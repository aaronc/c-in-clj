(ns c-in-clj.doc
  (:use [c-in-clj core2]
        [c-in-clj.lang api intrinsics]))

(csource-module DocModule :dev true)

(cpackage DocModule DocPackage)

(cdef ^i32 x)

(cdef ^i32 y)

(cdef ^i32 i)

(cdef ^i32 j)

(cdef ^i32 len)

(def expressions
  '[(+ x y)
    (- x y)
    (* x y)
    (/ x y)
    (mod x y)
    (= x y)
    (not= x y)
    (< x y)
    (> x y)
    (<= x y)
    (>= x y)
    (or x y)
    (and x y)
    (bit-and x y)
    (bit-or x y)
    (bit-xor x y)
    (bit-shift-left x y)
    (bit-shift-right x y)
    (set! x y)
    (bit-and= x y)
    (bit-or= x y)
    (bit-xor= x y)
    (bit-not= x y)
    (inc x)
    (post-inc x)
    (dec x)
    (post-dec x)
    (not x)
    (bit-not x)
    ;;(.x y)
    (ref x)
    (deref x)
    ;;@x
    (aget x i)
    (aset x i y)
    ;;(cast some_type x)
    (sizeof x)])

(defn print-expr [expr]
  (reduce-parens (expr-write (->expr expr))))

(defn print-expressions [exprs]
  (doseq [expr exprs]
    (println expr (print-expr expr))))

(cdefn a ^void [])
(cdefn b ^void [])
(cdefn c ^void [])
(cdefn d ^void [])

(def statements
  '[(do
      (a)
      (b)
      (c))
    (if (> x y) (a))

    (if (> x y) (a) (b))

    (if (> x y)
      (do
        (a)
        (b))
      (do
        (c)
        (d)))

(case x
 0 (return y)
 1 (b)
 (c))

(while (> x y)
 (if (not (a x)) (break);
     (dec x)))


(for (set! i 0) (< i len) (inc i)
 (if (a i) (continue))
 (b i))

(for [(set! i 0) (set! j 0)] (and (< i x) (< j y)) [(inc i) (inc j)]
 (a i j))

(let [^i32 x 0
      ^double y 1.0]
  (a x y) (b y x))

])

(print-expressions expressions)
(print-expressions statements)
