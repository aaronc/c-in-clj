(defproject c-in-clj "0.3.0-SNAPSHOT"
  :dependencies
  [[org.clojure/clojure "1.5.1"]
   [org.clojure/tools.logging "0.2.6"]]
  :profiles
  {:dev
   {:dependencies
    [[ch.qos.logback/logback-classic "1.0.13"]
     [hiccup "1.0.3"]]}}
  :source-paths ["src" "src-jvm"]
  :test-paths ["test" "test-jvm"]
  :codox {:include [c-in-clj.lang.api]}
  :plugins [[codox "0.6.4"]
            [lein-marginalia "0.7.1"]])
