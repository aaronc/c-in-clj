(defproject c-in-clj "0.3.0-SNAPSHOT"
  :dependencies
  [[org.clojure/clojure "1.5.1"]]
  :source-paths ["src" "src-jvm"]
  :codox {:include [c-in-clj.lang.api]}
  :plugins [[codox "0.6.4"]
            [lein-marginalia "0.7.1"]])
