(ns build
  "build electric.jar library artifact and demos"
  (:require
   [css-gen]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]

   [org.corfield.build :as bb]
   [babashka.fs :as fs]
   [shadow.css.build :as cb]
   [shadow.cljs.devtools.api :as shadow-api]
   [shadow.cljs.devtools.server :as shadow-server]))

(def lib 'benjamin/ftlm-hearts)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [opts]
  (bb/clean opts))

(def class-dir "target/classes")
(defn default-jar-name [{:keys [version] :or {version version}}]
  (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean-cljs [_]
  (b/delete {:path "resources/public/js"}))

(defn css-release []
  (let [build-state
        (-> (cb/start)
            (cb/index-path css-gen/index-path {})
            (cb/generate css-gen/generate-opts)
            (cb/write-outputs-to css-gen/output))]

    (doseq [mod (:outputs build-state)
            {:keys [warning-type] :as warning} (:warnings mod)]

      (prn [:CSS (name warning-type) (dissoc warning :warning-type)]))))

(defn build-client [{:keys [optimize debug verbose version]
                     :or {optimize true, debug false, verbose false, version version}}]
  (println "Building client. Version:" version)
  (shadow-server/start!)
  (shadow-api/release
   :client
   {:debug debug,
    :verbose verbose,
    :config-merge [{:compiler-options {:optimizations (if optimize :advanced :simple)}}]})
  (shadow-server/stop!))

(defn uberjar [{:keys [jar-name version optimize debug verbose]
                :or   {version version, optimize true, debug false, verbose false}}]
  (println "Cleaning up before build")
  (clean nil)

  (println "Cleaning cljs compiler output")
  (clean-cljs nil)

  (build-client {:optimize optimize, :debug debug, :verbose verbose, :version version})

  (println "Bundling sources")
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})

  (println "Compiling server. Version:" version)
  (b/compile-clj {:basis      basis
                  :src-dirs   ["src"]
                  :ns-compile '[ftlm.hearts.prod]
                  :class-dir  class-dir})

  (let [uber-file (str (or jar-name (default-jar-name {:version version})))]
    (println "Building uberjar")
    (b/uber {:class-dir class-dir
             :uber-file  uber-file
             :basis     basis
             :main      'ftlm.hearts.prod})

    (println "Setting up run scripts")
    (fs/delete-if-exists "release.jar")
    (fs/create-sym-link "release.jar" uber-file)))

(defn noop [_])                         ; run to preload mvn deps

(comment
  (css-release))
