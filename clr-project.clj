{:name "c-in-clj" :version "0.3.0-alpha1-*"
 :dependencies [["ClojureClrEx"]]
 :disable-default-aot true
 :dll-exclusions [#"c_in_clj/gcc.*" #"c_in_clj/type-system.*"]
 :source-paths ["src" "clr-src"]
 :key-file "%CLOJURE_SNK%"}
