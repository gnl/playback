;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/) which can
;; be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns playback.core
  #?(:cljs (:require-macros playback.core))
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            #?@(:clj  [[cljs.analyzer.api :as ana]
                       [debux.core :as debux :refer [dbg dbgn]]
                       [portal.api :as portal]]
                :cljs [[debux.cs.core :as debux :refer-macros [dbg dbgn]]
                       [portal.web :as portal]])))


;;; Vars and Init ;;;


(debux/set-source-info-mode! false)
(debux/set-tap-output! true)


(def ^:no-doc *fn-arg-cache (atom {}))
(def ^:no-doc ^:dynamic **portal-value* nil)

(def ^:private ^:dynamic *env*)
(def ^:private ^:dynamic *form*)

(def ^:private form-types->ops
  {::defn             #{'clojure.core/defn 'cljs.core/defn
                        'clojure.core/defn- 'cljs.core/defn-}
   ::>defn            #{'com.fulcrologic.guardrails.core/>defn
                        'com.fulcrologic.guardrails.core/>defn-}
   ::defmethod        #{'clojure.core/defmethod 'cljs.core/defmethod}
   ;; TODO
   ::fn               #{'clojure.core/fn 'cljs.core/fn}
   ::re-frame-handler #{'re-frame.core/reg-event-fx 're-frame.core/reg-event-db}
   ::fn-registration  #{'re-frame.core/reg-sub 're-frame.core/reg-sub-raw}
   ::transducer       #{'clojure.core/transduce 'cljs.core/transduce}
   ::defn-traced      #{'day8.re-frame.tracing/defn-traced}
   ::do-not-trace     #{'day8.re-frame.tracing/fn-traced}})

(def ^:private ops->form-types
  (into {}
        (for [[form-type syms] form-types->ops
              sym syms]
          [sym form-type])))


;;; Specs ;;;


(s/def ::form any?)
(s/def ::result any?)
(s/def ::indent-level nat-int?)
(s/def ::info any?)

(s/def ::debux-eval
  (s/and (s/keys :req-un [::form ::result ::indent-level]
                 :opt-un [::info])
         #(<= (count %) 4)))


;;; Utils ;;;


(defn- prepend-indent
  [indent-level]
  ;; Alternative symbols: #_"⤴ｏ｜⬆ ～￣＿"
  (-> (repeat indent-level "⬆")
      string/join
      (str " ")))


(defn- format-portal-label
  ([label]
   (format-portal-label label 1))
  ([label indent-level]
   (-> (prepend-indent indent-level)
       (str label)
       symbol)))


(defn ^:no-doc log-data
  ([data]
   (log-data data false))
  ([data label?]
   (println data)
   (flush)
   (if label?
     (tap> (format-portal-label data))
     (tap> data))))


#?(:clj
   (defn- cljs-env? [env] (boolean (:ns env))))


#?(:clj
   (defn- get-ns-meta [env]
     (if (cljs-env? env)
       (or (meta *ns*) (some-> env :ns :meta))
       (meta *ns*))))


#?(:clj
   (defn- get-ns-name [env]
     (if (cljs-env? env)
       (or (.-name *ns*) (some-> env :ns :name))
       (.-name *ns*))))


#?(:clj
   (defn- get-qualified-sym [fn-name env]
     (symbol (str (get-ns-name env)) (name fn-name))))


#?(:clj
   (defn- get-quoted-qualified-sym [fn-name env]
     `(quote ~(get-qualified-sym fn-name env))))


#?(:clj
   (defn- resolve-sym-clj* [env sym]
     (if-let [sym-var (resolve sym)]
       (if (class? sym-var)
         sym
         (symbol sym-var)
         #_(let [m    (meta sym-var)
                 ns   (str (ns-name (:ns m)))
                 name (str (:name m))]
             (symbol ns name)))
       sym)))


#?(:clj
   ;; Code gratefully borrowed from philoskim/debux
   (defn- resolve-sym-cljs* [env sym]
     (if-let [ast-node (ana/resolve env sym)]
       ;; normal symbol
       (if (:local ast-node)
         sym
         (let [[ns name] (string/split (str (:name ast-node)) #"/")]
           ;; The special symbol `.` must be handled in the following special symbol part.
           ;; However, the special symbol `.` returns meta {:name / :ns nil}, which may be a bug.
           (if (nil? ns)
             sym
             (symbol ns name))))
       ;; special symbols except for `.`
       sym)))


#?(:clj
   (defn- resolve-sym
     [env sym]
     (if (cljs-env? env)
       (resolve-sym-cljs* env sym)
       (resolve-sym-clj* env sym))))


;;; Tracing Implementation ;;;


#?(:clj
   (defn- split-fn
     [fn-form]
     (split-with (every-pred (complement vector?)           ; parameters
                             (complement list?))            ; multi-arity
                 fn-form)))


#?(:clj
   ;; REVIEW: refactor as multimethod
   (defn- arg-cache-key
     [[op sym & [?dispatch-val] :as fn-form] form-type]
     (case form-type
       ::defn (get-qualified-sym sym *env*)
       ::fn-registration [op (keyword (get-qualified-sym sym *env*))]
       ::defmethod [(get-qualified-sym sym *env*)
                    ?dispatch-val])))


#?(:clj
   (defn- traced-op
     [op form-type]
     (case form-type
       ::defmethod 'defn
       op)))


#?(:clj
   (defn- wrapping-op
     [op]
     (let [form-type (get ops->form-types (resolve-sym *env* op))]
       (case form-type
         ::defn-traced 'defn
         ::>defn 'defn
         op))))


#?(:clj
   (defn- stripped-fn-head-rest
     [fn-head-rest form-type]
     (case form-type
       ::defmethod (drop 1 fn-head-rest)
       fn-head-rest)))


#?(:clj
   (defn- generate-capture-defn
     [fn-form trace-level form-type fn-arg-cache-key]
     (let [[fn-head fn-tail] (split-fn fn-form)
           [op fn-name & fn-head-rest] fn-head
           fn-head-rest   (stripped-fn-head-rest fn-head-rest form-type)
           traced-fn-name (gensym (str fn-name "__"))]
       `(do
          ;; needed for recursion and multi-arity functions
          (declare ~fn-name)
          (~(traced-op op form-type) ~traced-fn-name ~@fn-head-rest ~@fn-tail)
          (let [definition#
                (~(wrapping-op op) ~@(rest fn-head)
                 [& args#]
                 (swap! *fn-arg-cache assoc '~fn-arg-cache-key args#)
                 (log-data (symbol (str "▷ ︎"
                                        ~(if (cljs-env? *env*)
                                           `(.toLocaleTimeString (js/Date.))
                                           `(.format (java.text.SimpleDateFormat. "HH:mm:ss") (java.util.Date.)))
                                        " "
                                        ~(string/join (repeat 32 "＿")))))
                 (log-data (str ~(str "#"
                                      (string/join (repeat trace-level ">"))
                                      " ")
                                '~fn-arg-cache-key)
                           true)
                 (when (> ~trace-level 1)
                   (log-data :args true)
                   (log-data (vec args#)))
                 (let [result# (apply ~traced-fn-name args#)]
                   (log-data :ret true)
                   (log-data result#)
                   result#))]
            ~(when-not (string/ends-with? (str fn-name) "!")
               `(when-let [args# (get @*fn-arg-cache '~fn-arg-cache-key)]
                  (apply ~fn-name args#)))
            definition#)))))


#?(:clj
   (defn- trace-form-dispatch [form trace-level]
     (if-not (seq? form)
       ::default
       (let [op (first form)]
         (if (symbol? op)
           (let [qualified-sym (resolve-sym *env* op)]
             (get ops->form-types qualified-sym ::default))
           ::default)))))


#?(:clj
   (defmulti ^:private trace-form* trace-form-dispatch))


#?(:clj
   (defmethod trace-form* ::defn
     [form trace-level]
     (let [fn-arg-cache-key (arg-cache-key form ::defn)]
       (generate-capture-defn form trace-level ::defn fn-arg-cache-key))))


#?(:clj
   (defmethod trace-form* ::>defn
     [form trace-level]
     (let [fn-arg-cache-key (arg-cache-key form ::defn)]
       (generate-capture-defn form trace-level ::>defn fn-arg-cache-key))))


#?(:clj
   (defmethod trace-form* ::defmethod
     [form trace-level]
     (let [fn-arg-cache-key (arg-cache-key form ::defmethod)]
       (generate-capture-defn form trace-level ::defmethod fn-arg-cache-key))))


#?(:clj
   (defn- wrap-fn-reg
     [form trace-level rf-handler?]
     (let [[op id :as reg-head] (butlast form)
           handler-fn       (last form)
           [_ handler-fn-tail] (split-fn handler-fn)
           ;; FIXME: This won't cache correctly due to the ever-changing gensym
           ;; Pass explicit arg-cache-key to generate-trace-defn and create from handler ID
           fn-name          (gensym (str (name id) "_"))
           fn-arg-cache-key (arg-cache-key form ::fn-registration)
           rf10x?           (and rf-handler? (= trace-level 3))]
       `(do
          ~(generate-capture-defn `(~(if rf10x? 'day8.re-frame.tracing/defn-traced 'defn)
                                    ~fn-name
                                    ~@handler-fn-tail)
                                  trace-level
                                  ::defn
                                  fn-arg-cache-key)
          (~@reg-head ~fn-name)))))


#?(:clj
   (defmethod trace-form* ::re-frame-handler
     [form trace-level]
     (wrap-fn-reg form trace-level true)))


#?(:clj
   (defmethod trace-form* ::fn-registration
     [form trace-level]
     (wrap-fn-reg form trace-level false)))


;; TODO
#?(:clj
   (defmethod trace-form* ::defn-traced
     [form trace-level]
     form))

;; TODO
#?(:clj
   (defmethod trace-form* ::do-not-trace
     [form trace-level]
     form))


;; TODO
#?(:clj
   (defmethod trace-form* ::transducer
     [form trace-level]
     form))


#?(:clj
   (defmethod trace-form* ::default
     [form trace-level]
     (case trace-level
       1 `(dbg ~form :simple)
       2 `(dbg ~form)
       3 `(dbgn ~form))))


;;; Data Reader Functions and Macros ;;;


(defmacro ^:no-doc trace>
  [form]
  (binding [*env* &env]
    (trace-form* form 1)))


(defmacro ^:no-doc trace>>
  [form]
  (binding [*env* &env]
    (trace-form* form 2)))


#_(defmacro ^:no-doc trace>>>
    [form]
    (binding [*env* &env
              #_*form* #_&form]
      (trace-form* form 3)))


(defn ^:no-doc trace-o [form] `(trace> ~form))
(defn ^:no-doc trace-io [form] `(trace>> ~form))
#_(defn ^:no-doc trace-deep [form] `(trace>>> ~form))
(defn ^:no-doc get-portal-data [_] `@playback.core/**portal-value*)


;;; Public ;;;


(defn portal-tap
  [value]
  (if-not (s/valid? ::debux-eval value)
    (portal/submit value)
    (let [{:keys [info form indent-level result]} value]
      (when info
        (portal/submit (format-portal-label info indent-level)))
      (when form
        (portal/submit (format-portal-label form indent-level)))
      (when result
        (portal/submit result)))))


(defn open-portal!
  []
  (if (some? **portal-value*)
    (println "Portal is already open.")
    (let [portal-instance (portal/open)]
      #?(:clj  (alter-var-root #'**portal-value* (constantly portal-instance))
         :cljs (set! **portal-value* portal-instance))
      portal-instance)))


;;; DEBUG ;;;


(defmacro ^:no-doc test-resolve-sym
  [sym]
  `(println (quote ~(resolve-sym &env sym))))


(defmacro ^:no-doc print-env
  []
  `(def ~'debug-environment (quote ~&env)))
