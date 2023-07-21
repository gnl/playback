;; Copyright (c) George Lipov. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 2.0 (https://choosealicense.com/licenses/epl-2.0/) which can
;; be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns ^:no-doc build
  "
  1. Update version in build.clj
  2. `clj -T:build install` (install jar locally and test)
  3. `git commit -aS`
  4. `git tag -s <v0.1.1>`
  5. `clj -T:build deploy` (optional: `:sign false`)"
  (:require [clojure.string :as string]
            [clojure.tools.build.api :as b]))


(def version "0.3.10-SNAPSHOT")

(def lib 'com.github.gnl/playback)
(def gpg-signing-key "EA4E52DD8F4A7AD2F6D1F1FBE1A18DCD43ECC7C9")

(def class-dir "target/classes")
(def basis (b/create-basis))
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def version-tag-regex #"^v(\d+\.\d+\.\d+)(-\w+)?$")
(def snapshot? (string/ends-with? version "-SNAPSHOT"))


(defn- step [s]
  (print (str s "... "))
  (flush))


(defn- done []
  (println "DONE"))


(defn- validate-version []
  (step (format "Validating version %s" version))
  (assert (re-matches version-tag-regex (str "v" version))
          "Invalid version format – should look like 0.1.1 or 0.1.1-qualifier.")
  (done))


(defn- pre-deploy-checks []
  (when-not snapshot?
    (step "Refreshing pom.xml and running pre-deploy checks")
    (b/process {:command-args ["clj" "-A:pom-provided" "-Spom"]})
    (assert (string/blank? (b/git-process {:git-args ["status" "--porcelain"]}))
            "Git working tree is not clean.")
    (let [git-tag (b/git-process {:git-args ["describe" "--tags"]})]
      (assert (not (string/blank? git-tag))
              "Missing Git version tag.")
      (assert (re-matches #"^v(\d+\.\d+\.\d+)(-\w+)?$" git-tag)
              "Invalid Git tag – should be v0.1.1 or v0.1.1-qualifier.")
      (assert (= (str "v" version) git-tag)
              "Git tag doesn't match version in build.clj.")))
  (done))


(defn- auth-environment []
  (println "\n")
  (let [console (System/console)
        user    (.readLine console "Clojars username: " nil)
        token   (apply str (.readPassword console "Clojars deploy token: " nil))]
    ;; Make sure the Clojars username and password in your Maven settings are
    ;; set to ${env.CLOJARS_USERNAME} and ${env.CLOJARS_PASSWORD} respectively.
    {"CLOJARS_USERNAME" user
     "CLOJARS_PASSWORD" token}))


(defn clean [_]
  (step "Cleaning build")
  (b/delete {:path "target"})
  (done))


(defn pom [_]
  (validate-version)
  (step "Syncing pom.xml template with deps.edn")
  (b/process {:command-args ["clj" "-A:pom-provided" "-Spom"]})
  (done)
  (step "Generating pom.xml from template")
  (b/write-pom {:basis     basis
                :lib       lib
                :version   version
                :class-dir class-dir
                :scm       (when-not snapshot? {:tag (str "v" version)})})
  (done))


(defn jar [_]
  (clean nil)
  (pom nil)
  (step "Building jar")
  (b/copy-dir {:src-dirs   ["src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file})
  (done))


(defn install [_]
  (jar nil)
  (step "Installing jar to local repo")
  (b/install {:basis     basis
              :lib       lib
              :version   version
              :class-dir class-dir
              :jar-file  jar-file})
  (done))


(defn deploy
  "Securely handles the Clojars credentials by prompting the user and passing
  them on to Maven in environment vars. gpg-agent should be set up correctly as
  well so that the signing key passphrase doesn't have to be stored or provided
  to Maven in an insecure manner (or at all)."
  [{:keys [sign]}]
  (assert (or (nil? sign) (boolean? sign))
          ":sign has to be `true` or `false`")
  (let [sign? (and gpg-signing-key
                   (not (false? sign))
                   (not snapshot?))]
    (pre-deploy-checks)
    (jar nil)
    (step (format "\nDeploying %s jar to Clojars"
                  (if sign? "signed" "unsigned")))
    (b/process {:env (auth-environment)
                :command-args
                (remove nil?
                        ["mvn"
                         (if sign?
                           "gpg:sign-and-deploy-file"
                           "deploy:deploy-file")
                         (str "-Dfile=" jar-file)
                         (format "-DpomFile=%s/META-INF/maven/%s/pom.xml"
                                 class-dir
                                 lib)
                         "-DrepositoryId=clojars"
                         "-Durl=https://clojars.org/repo"
                         (when sign?
                           (str "-Dgpg.keyname=" gpg-signing-key))])})
    (println "\n")
    (doseq [hash-algo ["sha512" "rmd160"]]
      (b/process {:command-args ["openssl" hash-algo jar-file]}))))
