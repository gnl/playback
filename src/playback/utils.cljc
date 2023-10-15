(ns ^:no-doc playback.utils
  (:require
   [clojure.string :as string]))


(defn prepend-indent
  [indent-level]
  ;; Alternative symbols: #_"⤴ｏ｜⬆ ～￣＿"
  (-> (repeat indent-level "⬆")
      string/join
      (str " ")))


(defn format-portal-label
  ([label]
   (format-portal-label label 1))
  ([label indent-level]
   (-> (prepend-indent indent-level)
       (str label)
       symbol)))


(defn ^:no-doc log-data
  ([data]
   (log-data data nil 1))
  ([data label?]
   (log-data data label? 1))
  ([data label? indent]
   (println data)
   (flush)
   (if label?
     (tap> (format-portal-label data indent))
     (tap> data))))


#?(:clj
   (defn cljs-env? [env] (boolean (:ns env))))


#?(:clj
   (defn get-ns-meta [env]
     (if (cljs-env? env)
       (or (meta *ns*) (some-> env :ns :meta))
       (meta *ns*))))


#?(:clj
   (defn get-ns-name [env]
     (if (cljs-env? env)
       (or (ns-name *ns*) (some-> env :ns :name))
       (ns-name *ns*))))


#?(:clj
   (defn get-qualified-sym [fn-name env]
     (symbol (str (get-ns-name env)) (name fn-name))))


#?(:clj
   (defn get-quoted-qualified-sym [fn-name env]
     `(quote ~(get-qualified-sym fn-name env))))


#?(:clj
   (defn resolve-sym
     [env sym]
     (if (cljs-env? env)
       (let [maybe-sym (:name ((requiring-resolve 'cljs.analyzer.api/resolve) env sym))]
         (when (qualified-symbol? maybe-sym)
           maybe-sym))
       (when-let [sym-var (resolve sym)]
         (symbol sym-var)))))


#?(:clj
   (defn resolve-var
     [env sym]
     (if (cljs-env? env)
       ((requiring-resolve 'cljs.analyzer.api/resolve) env sym)
       (resolve sym))))


#?(:clj
   (defn invert-optype->ops
     [optype->ops]
     (into {}
           (for [[optype ops] optype->ops
                 op ops]
             [op optype]))))


#?(:clj
   (defn unique-playback-sym
     [env sym]
     (when-let [maybe-duplicate-sym (resolve-var env sym)]
       (let [metadata (if (cljs-env? env)
                        (:meta maybe-duplicate-sym)
                        (meta maybe-duplicate-sym))]
         (when-not (:playback.core/playback? metadata)
           (throw (Exception. (str "Found existing def using Playback suffix, refusing to overwrite: " sym))))))
     sym))
