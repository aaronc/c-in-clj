;; (add-ns-load-mapping "c_in_clj" "c:/dev/c-in-clj/src/c_in_clj")
;; (add-ns-load-mapping "c_in_clj" "c:/dev/c-in-clj/src-clr/c_in_clj")
;; (add-ns-load-mapping "c_in_clj" "c:/dev/c-in-clj/test-clr/c_in_clj")
(ns c-in-clj.msvc-test
  (:use [c-in-clj core msvc]))

(msvc-module TestModule1)

(cpackage TestModule1 Test1)
