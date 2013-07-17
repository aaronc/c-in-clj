(ns c-in-clj.core-test
  (:use [c-in-clj core2 runtime2]
        [c-in-clj.lang api]))

(csource-module TestModule :dev true)

(cpackage TestModule TestPackage)

(cdefn test1 ^i32 [^i32 a ^i32 b]
       (def ^i32 c (+ a b))
       c)


