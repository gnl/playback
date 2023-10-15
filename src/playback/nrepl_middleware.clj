;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/) which can
;; be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns playback.nrepl-middleware
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint]
            [com.fulcrologic.guardrails.core :refer [>defn ? | =>]]
            [clojure.spec.alpha :as s]))


;; In my brief experience writing and debugging nREPL middleware, the process
;; was obscure and fragile in the extreme, including sporadic and seemingly
;; non-deterministic failures on superficially innocuous code changes like
;; trying to set the correct middleware description. Unclear if any of that was
;; related to Shadow CLJS. Tread with caution.


(def ^:private ^:dynamic *refreshing-fn-syms* [])
(def ^:private ^:dynamic *refreshable-ns-prefixes* [])

(>defn init-refresh-on-eval!
  [refreshing-fn-syms refreshable-ns-prefixes]
  [(s/coll-of qualified-symbol?) (s/coll-of string?) => any?]
  (alter-var-root #'*refreshing-fn-syms* (constantly refreshing-fn-syms))
  (alter-var-root #'*refreshable-ns-prefixes* (constantly refreshable-ns-prefixes)))


(def ^:private ^:dynamic *refresh-on-traced-fn?* false)
(defn set-refresh-on-traced-fn!
  [refresh-on-traced-fn?]
  (alter-var-root #'*refresh-on-traced-fn?* (constantly refresh-on-traced-fn?)))


(def ^:private ^:dynamic *debug?* false)
(defn set-debug!
  [debug?]
  (alter-var-root #'*debug?* (constantly debug?)))


(defn ^:no-doc refresh-on-eval [handler]
  (fn [{:keys [op code ns] :as msg}]
    (if (and (= op "eval")
             (some #(string/starts-with? (str ns) %) *refreshable-ns-prefixes*)
             (or *refresh-on-traced-fn?*
                 (if-let [found-traced-fn
                          ;; REVIEW: Un-hardcode this and generate it from
                          ;; `playback.core/form-types->ops`
                          (first (re-find #"#>>?\s*\(\s*(>?defn-?|defmethod)\s+\S+" (str code)))]
                   (when *debug?*
                     (println (format "\nNo refresh due to found traced fn: `%s`"
                                      found-traced-fn)))
                   :no-traced-fn)))
      (do
        (when *debug?*
          (println (str "\nEval and refresh in " ns ":\n"))
          (pprint/pprint msg))
        (handler (assoc msg :code (str "(let [result (do " code ")]"
                                       "  (doseq [refresh-fn " *refreshing-fn-syms* "]"
                                       "    (refresh-fn))"
                                       "  result)"))))
      (handler msg))))
