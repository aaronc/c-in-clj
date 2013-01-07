(ns c-in-clj.core
  (:require [clojure.string :as str]))

(defprotocol IExpression
  (get-type [this])
  (write [this])
  (is-block? [this])
  (wrap-last [this func]))

(defmacro defliteral [name ctype]
  `(defrecord ~name [value#]
       IExpression
       (write [_] (pr-str value#))
       (is-block? [_] false)
       (get-type [_] 'ctype)))

(defliteral Int32Literal 'int32_t)
(defliteral Int64Literal 'int64_t)
(defliteral DoubleLiteral 'double)
(defliteral BooleanLiteral 'bool)
(defliteral StringLiteral 'char*)

(def null-literal
  (reify IExpression
    (write [this] "0")
    (is-block? [_] false)
    (get-type [this])))

(defprotocol IFunction
  (get-return-type [this]))

(defrecord Function [func-name return-type params]
  IFunction
  (get-return-type [this] return-type)
  clojure.lang.Named
  (getName [this] func-name))

(defrecord FunctionCallExpression [func args]
  IExpression
  (write [this]
    (str (name func)
         "(" (str/join "," (map write args)) ")"))
  (is-block? [_] false)
  (get-type [this] (get-return-type func)))

(defrecord AnonymousFunctionCallExpression [func-name args]
  IExpression
  (write [this]
    (str func-name
         "(" (str/join "," (map write args)) ")"))
  (is-block? [_] false)
  (get-type [this]))

(defrecord ComputedFunctionCallExpression [func-expr args]
  IExpression
  (write [this]
    (str func-expr
         "(" (str/join "," (map write args)) ")"))
  (is-block? [_] false)
  (get-type [this]))

(defrecord Variable [var-name var-type]
  clojure.lang.Named
  (getName [this] var-name))

(defrecord VariableRefExpression [variable]
  IExpression
  (write [this] (name variable))
  (is-block? [_] false)
  (get-type [this] (:var-type variable)))

(defrecord AnonymousVariableRefExpression [var-name]
  IExpression
  (write [this] var-name)
  (is-block? [_] false)
  (get-type [this]))

(defn cexpand-num [x]
  (let [ntype (type x)]
    (cond
     (= ntype Int64)
     (if (and (>= x Int32/MinValue) (<= x Int32/MaxValue))
       (Int32Literal. x)
       (Int64Literal. x))
     (= ntype Double)
     (DoubleLiteral. x))))

(defn lookup-symbol [sym-name])

(defn lookup-type [type-name])

(defn lookup-macro [macro-name])

(def ^:private cintrinsics (atom {}))

(def ^:dynamic *locals* [])

(defn cexpand-op-sym [sym args]
  (if-let [intrinsic (@cintrinsics sym)]
    (apply intrinsic args)
    (if-let [macro (lookup-macro sym)]
      (cexpand (apply macro args))
      (if-let [local-func (get *locals* sym)]
        (AnonymousFunctionCallExpression. (name sym) (map cexpand args))
        (if-let [defined (lookup-symbol sym)]
          (FunctionCallExpression. defined (map cexpand args))
          (throw (ArgumentException. (str "Don't know how to handle list symbol " sym))))))))

(defn cexpand-list [[op & args]]
  (cond
   (symbol? op) (cexpand-op-sym op args)
   (keyword? op) (AnonymousFunctionCallExpression. (name op) (map cexpand args))
   (list? op)
   (loop [expanded (cexpand-list op)]
     (cond
      (satisfies? IExpression expanded) (ComputedFunctionCallExpression. expanded (map cexpand args))
      (list? expanded) (recur (cexpand-list expanded))
      :default (cexpand-list expanded args)))
   :default (throw (ArgumentException. (str "Don't know how to handle list starting with" op)))))

(defn cexpand [form]
  (cond
   (nil? form) null-literal
   (number? form) (cexpand-num form)
   (= Boolean (type form)) (BooleanLiteral. form)
   (string? form) (StringLiteral. form)
   (symbol? form) (lookup-symbol form)
   (keyword? form) (AnonymousVariableRefExpression. (name form))
   (list? form) (cexpand-list form)
   :default (throw (ArgumentException. (str "Don't know how to handle " form " of type " (type form))))))

(defn cintrinsic* [sym func]
  (swap! cintrinsics assoc sym func))

(defmacro cintrinsic [sym args & body]
  `(cintrinsic* '~sym
               (fn ~sym ~args
                 (let [~@(reduce (fn [res x] (into res [x `(cexpand ~x)])) [] args)]
                   ~@body))))

(defmacro cop [sym args & body]
  `(cintrinsic ~sym ~args
               (let [~@(reduce (fn [res x] (into res [x `(cexpand ~x)])) [] args)]
                 (reify
                   IExpression
                   (is-block? [_] false)
                   ~@body))))

(defn get-bin-op-type [x y]
  (let [xt (get-type x)
        yt (get-type y)]
    (when (= xt yt)
      xt)))

(defmacro cbinop [sym]
  `(cop ~sym [x# y#]
        (get-type [this#] (get-bin-op-type x# y#))
        (write [this#] (str "(" (write x#) " " ~(name sym) " " (write y#) ")"))))

(defmacro cbinop* [sym expr]
  `(cop ~sym [x# y#]
        (get-type [this#] (get-bin-op-type x# y#))
        (write [this#] (str "(" (write x#) " " ~expr " " (write y#) ")"))))

(defmacro cbinops [& syms]
  `(do ~@(for [x syms] `(cbinop ~x))))

(defmacro compop [sym]
  `(cop ~sym [x# y#]
        (get-type [this#] 'bool)
        (write [this#] (str "(" (write x#) " " ~(name sym) " " (write y#) ")"))))

(defmacro compops [& syms]
  `(do ~@(for [x syms] `(compop ~x))))

(defmacro compop* [sym expr]
  `(cop ~sym [x# y#]
        (get-type [this#] 'bool)
        (write [this#] (str "(" (write x#) " " ~expr " " (write y#) ")"))))

(defmacro cassignop [sym expr]
  (let [assign-rec-name (str "AssignExpression_" (name sym))
        assign-rec-ctr (symbol (str assign-rec-name "."))]
    `(do
       (defrecord ~(symbol assign-rec-name)
           [x# y#]
         IExpression
         (get-type [_])
         (is-block? [_] false)
         (write [this#] (str "(" (write x#) " " ~expr " " (write y#) ")")))
       (cintrinsic
        ~sym
        [target# source#]
        (if (is-block? source#)
          (wrap-last source#
                     (fn [x#] (~assign-rec-ctr target# x#)))
          (~assign-rec-ctr target# source#))))))
  
(cbinops + - * /  += -= *= /= %= <<= >>=)
(compops += -= *= /= %= <<= >>=)
(compop* = "==")
(compop* not= "!=")
(cbinop* mod "%")
(compop* or "||")
(compop* and "&&")
(cbinop* bit-or "|")
(cassignop bit-or= "|=")
(cbinop* bit-and "&")
(cassignop bit-and= "&=")
(cbinop* bit-shift-left "<<")
(cassignop bit-shift-left="<<=")
(cbinop* bit-shift-right ">>")
(cassignop bit-shift-right= ">>=")
(cbinop* bit-xor "^")
(cassignop bit-xor= "^=")
(cbinop* bit-not "~")
(cassignop bit-not= "~=")
(cassignop set! "=")

(defmacro cunop [name arg & body]
  `(cop ~name ~arg
        (get-type [this#] (get-type ~(first arg)))
        (write [this#] ~@body)))

(defn write-str [& args]
  (apply str (for [arg args]
               (if (string? arg)
                 arg
                 (write arg)))))

(cunop inc [x] (write-str "++" x))
(cunop dec [x] (write-str "--" x))
(cunop post-inc [x] (write-str x "++"))
(cunop post-dec [x] (write-str x "--"))

(cop not [x]
     (get-type [_] 'bool)
     (write [_] (write-str "!" write x)))

(cop sizeof [x]
     (get-type [_] 'size_t)
     (write [_] (write-str "sizeof(" x ")")))

;; (defn c* [& args]
;;   (let [expanded (for [arg args]
;;                    (if (string? arg)
;;                      arg
;;                      (cexpand arg)))]
;;     (apply str expanded)))

;; (cintrinsic 'c* c*)

(cintrinsic* '.
     (fn [& args]
       (let [args (map cexpand args)]
         (reify
           IExpression
           (get-type [_])
           (is-block? [_] false)
           (write [_]
             (str/join "." (map write args)))))))

(cintrinsic* '->
            (fn [& args]
              (let [args (map cexpand args)]
                (reify
                  IExpression
                  (get-type [_])
                  (is-block? [_] false)
                  (write [_]
                    (str/join "->" (map write args)))))))

(cop aget [x y]
     (get-type [_])
     (write [_] (write-str x "[" y "]")))

(defrecord ArraySetExpression [target idx value]
  IExpression
  (is-block? [_] false)
  (get-type [_] (get-type target))
  (write [_] (write-str target "[" idx "] = " value)))

(cintrinsic aset [target idx value]
            (if (is-block? value)
              (wrap-last value (fn [x] (ArraySetExpression. target idx x)))
              (ArraySetExpression. target idx value)))

(cop ref [x]
     (get-type [_])
     (write [_] (write-str "(&" x ")")))

(cop deref [x]
     (get-type [_])
     (write [_] (write-str "*" x)))

(def ^:dynamic *indent* 0)

(defn indent [] (str/join (for [x (range *indent*)] " ")))

(defn reduce-parens [^String expr]
  (when (nil? expr) (throw (ArgumentNullException. "expr")))
  (comment (if (and (.StartsWith expr "(") (.EndsWith expr ")"))
            (.Substring expr 1 (- (.Length expr) 2))
            expr))
  expr)

(defrecord Statement [expr noindent]
  IExpression
  (get-type [_] (get-type expr))
  (is-block? [_] (is-block? expr))
  (wrap-last [_ func]
    (println "wrapping" expr)
    (if (is-block? expr)
      (Statement. (wrap-last expr func) noindent)
      (Statement. (func expr) noindent)))
  (write [_]
    (let [res (str (when-not noindent (indent)) (reduce-parens (write expr)))
          res (if (or
                   (.EndsWith res "}")
                   (.EndsWith res "})")
                   (.EndsWith res ";")
                   (.EndsWith res "*/"))
                res
                (str res ";"))]
      res)))

(defn cstatement [expr & {:keys [noindent]}]
  (Statement. (cexpand expr) noindent))

(defn cstatements [statements]
  (let [statements (for [st statements] (cstatement st))]
    (reify
      IExpression
      (get-type [_] (get-type (last statements)))
      (is-block? [_] true)
      (write [_] (str/join "\n" (map write statements)))
      (wrap-last [_ func]
        (let [[statements last-st] statements]
          (cstatements
           (conj statements
                 (if (is-block? last-st)
                   (wrap-last last-st func)
                   (func last-st)))))))))

(defrecord CaseExpression [test cases]
  IExpression
  (get-type [_])
  (is-block? [_] true)
  (write [_]
    (let [cases
          (binding [*indent* (inc *indent*)]
            (for [[expr block] cases]
              (if block
                (let [block (binding [*indent* (inc *indent*)]
                              (write block))]
                  (str (indent) "case " (write expr) ":\n" block "\n" (indent) "break;\n"))
                (str (indent) "default:" (write expr) "\n" (indent) "break;\n"))))]
      (str "switch(" (write test) ") {\n" (str/join "\n" cases) (indent) "\n}")))
  (wrap-last [_ func]
    (CaseExpression.
     test
     (for [[expr block] cases]
       (if block
         [expr (wrap-last block func)]
         [(wrap-last expr func)])))))

(cintrinsic*
 'case
 (fn [test & args]
   (let [test (cexpand test)
         cases (partition-all 2 args)
         cases
         (for [[expr block] cases]
           (if block
             [(cexpand expr)
              (cstatement block)]
             [(cstatement expr)]))]
     (CaseExpression. test cases))))

(defrecord ReturnExpression [expr]
  IExpression
  (get-type [_] (get-type expr))
  (is-block? [_] false)
  (write [_] (if expr
               (str "return " (reduce-parens (write expr)))
               "return")))

(cintrinsic*
 'return
 (fn
   ([] (ReturnExpression. nil))
   ([expr]
      (let [expr (cexpand expr)]
        (if (is-block? expr)
          (wrap-last expr (fn [x] (ReturnExpression. x)))
          (ReturnExpression. expr))))))

;; (defn cblock [statements]
;;   (str (indent) "{\n"
;;        (binding [*indent* (inc *indent*)]
;;          (cstatements statements))
;;        "\n" (indent) "}"))

;; (defn child-block [statements]
;;   (let [nsts (count statements)]
;;                      (cond
;;                       (= 0 nsts) "{ }"
;;                       ;; (= 1 nsts) (binding [*indent* (inc *indent*)]
;;                       ;;              (cstatement (first statements)))
;;                       :default (cblock statements))))

(defrecord IfExpression [expr then else]
  IExpression
  (is-block? [_] true)
  (get-type [_])
  (write [_]
    (println expr then else)
    (str "if(" (reduce-parens (write expr)) ")\n"
         (write then)
         (when else "\n" "else " (write else))))
  (wrap-last [_ func]
    (IfExpression.
     expr
     (wrap-last then func)
     (when else (wrap-last else func)))))

(cintrinsic* 'if
             (fn
               ([expr then]
                  (IfExpression. (cexpand expr)
                                 (cstatement then)
                                 nil))
              ([expr then else]
                 (IfExpression. (cexpand expr)
                                (cstatement then)
                                (cstatement else)))))



