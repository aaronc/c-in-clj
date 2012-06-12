(ns c-in-clj.core
  (:require [clojure.string :as str]))

(defprotocol ICModuleContext
  (beginfn [this name ret args])
  (resolve-sym [this sym])
  (compilefn [this body])
  (clean [this] [this func]))

(defn null-module-context []
  (reify
    ICModuleContext
    (beginfn [this name ret args])
    (resolve-sym [this sym])
    (compilefn [this body])
    (clean [this])
    (clean [this func])))

(def ^:dynamic *cmodule-context* (null-module-context))

(def ^:private cintrinsics (atom {}))

(def ^:dynamic *indent* 0)

(def ^:dynamic *locals* [])

(declare cexpand)

(defn c-func-call [func args]
  (str func "(" (str/join "," (for [x args] (cexpand x))) ")"))

(defn cexpand-op-sym [sym args]
  (if-let [intrinsic (@cintrinsics sym)]
    (apply intrinsic args)
    (if (contains? *locals* sym)
      (c-func-call (name sym) args)
      (if-let [foreign (resolve-sym *cmodule-context* sym)]
        (c-func-call foreign args)
        (throw (ArgumentException. (str "Don't know how to handled list symbol " sym)))))))

(defn cexpand-list
  ([form]
      (cexpand-list (first form) (rest form)))
  ([op args]
     (when op
       (cond
        (symbol? op) (cexpand-op-sym op args)
        (keyword? op) (c-func-call (name op) args)
        (list? op)
        (loop [expanded (cexpand-list op)]
          (cond (string? expanded) (c-func-call expanded args)
                (list? expanded) (recur (cexpand-list expanded))
                :default (cexpand-list expanded args)))
        :default (throw (ArgumentException. (str "Don't know how to handle list starting with" op)))))))

(defn cexpand-sym [sym]
  (cond
   (contains? *locals* sym) (name sym)
   :default (throw (ArgumentException. (str "Unresolved symbol " sym)))))

(defn cexpand [form]
  (cond
   (number? form) form
   (string? form) (pr-str form)
   (list? form) (cexpand-list form)
   (keyword? form) (name form)
   (symbol? form) (cexpand-sym form)
   :default (throw (ArgumentException. (str "Don't know how to handle " form " of type " (type form))))))

(defn cintrinsic [sym func]
  (swap! cintrinsics assoc sym func))

(defmacro cop [sym args & body]
  `(cintrinsic '~sym
          (fn ~sym ~args
            (let [~@(reduce (fn [res x] (into res [x `(cexpand ~x)])) [] args)]
              ~@body))))

(defmacro cbinop [sym] `(cop ~sym [x# y#] (str "(" x# " " ~(name sym) " " y# ")")))

(defmacro cbinop* [sym expr] `(cop ~sym [x# y#] (str "(" x# " " ~expr " " y# ")")))

(defmacro cbinops [& syms]
  `(do ~@(for [x syms] `(cbinop ~x))))

(cbinops + - / % == != > >= < <= >> >> = += -= *= /= %= <<= >>=)

(cbinop* bor "|")
(cbinop* or "||")
(cbinop* or= "|=")
(cbinop* band "&")
(cbinop* and "&&")
(cbinop* and= "&=")
(cbinop* xor "^")
(cbinop* xor= "^=")
(cbinop* comma ",")

(cop ++ [x] (str "++" x))
(cop -- [x] (str "--" x))
(cop ++' [x] (str x "++"))
(cop --' [x] (str x "--"))

(defn indent [] (str/join (for [x (range *indent*)] " ")))

(defn reduce-parens [^String expr]
  (when (nil? expr) (throw (ArgumentNullException. "expr")))
  (if (and (.StartsWith expr "(") (.EndsWith expr ")"))
    (.Substring expr 1 (- (.Length expr) 2))
    expr))

(defn cexpand-reduce [form] (reduce-parens (cexpand form)))

(defn cstatement [expr & {:keys [noindent]}]
  (let [res (str (when-not noindent (indent)) (cexpand-reduce expr))
        res (if (or (.EndsWith res "}") (.EndsWith res ";")) res (str res ";"))]
    res))

(defn cstatements [statements]
  (let [statements (for [st statements] (cstatement st))]
    (str/join "\n" statements)))

(defn cblock [statements]
  (str (indent) "{\n"
       (binding [*indent* (inc *indent*)]
         (cstatements statements))
       "\n" (indent) "}"))

(defn child-block [statements]
  (let [nsts (count statements)]
                     (cond
                      (= 0 nsts) "{ }"
                      (= 1 nsts) (binding [*indent* (inc *indent*)]
                                   (cstatement (first statements)))
                      :default (cblock statements))))

(cintrinsic '*
            (fn [x] (let [x (cexpand x)] (str "*" x)))
            (fn [x y] (let [x (cexpand x)
                           y (cexpand y)]
                       (str "(" x " * " y ")"))))

(cintrinsic 'if
            (fn [expr & statements]
              (str "if(" (reduce-parens (cexpand expr)) ")\n"
                   (child-block statements))))

(cintrinsic 'else
            (fn [& statements]
              (str "else "
                   (let [nsts (count statements)]
                     (cond
                      (= 0 nsts) "{ }"
                      (= 1 nsts) (cstatement (first statements) :noindent true)
                      :default (cblock statements))))))

(cintrinsic 'for
            (fn [init test each & statements]
              (str "for("
                   (cexpand-reduce init) "; "
                   (cexpand-reduce test) "; "
                   (cexpand-reduce each) ")\n"
                   (child-block statements))))

(cintrinsic 'while
            (fn [test & statements]
              (str "while("
                   (cexpand-reduce test) ")\n"
                   (child-block statements))))

(cintrinsic 'return
            (fn ([] "return")
              ([x] (str "return " (cexpand-reduce x)))))

(cintrinsic 'break (fn [] "break"))
(cintrinsic 'continue (fn [] "continue"))
(cintrinsic 'label (fn [x] (str (name x) ":")))
(cintrinsic 'goto (fn [x] (str "goto " (name x))))

(def ^:private ctypes (atom {}))

(defmacro add-ctypes [& type-map]
  (let [type-map (flatten (for [[id ctype clr-type] (partition 3 type-map)]
                            [ (name id) {:ctype (name ctype) :clr-type clr-type} ]))]
    (swap! ctypes #(apply assoc % type-map))))

(add-ctypes
 u8 uint8_t Byte
 u16 uint16_t UInt16
 u32 uint32_t UInt32
 u64 uint64_t UInt64
 i8 int8_t SByte
 i16 int16_t Int16
 i32 int32_t Int32
 i64 int64_t Int64
 float float Single
 double double Double
 void void nil)

(defn get-ctype [type-name]
  (let [type-name (name type-name)
        ptr-idx (.IndexOf type-name "*")
        ptr-part (when (> ptr-idx 0) (.Substring type-name ptr-idx))
        type-name (if ptr-part (.Substring type-name 0 ptr-idx) type-name)
        ctype (get-in @ctypes [type-name :ctype])]
    (str ctype ptr-part)))

(cintrinsic 'decl (fn ([type name init] (let [init (cexpand init)] (str (get-ctype type) " " name " = " init)))
                    ([type name] (let [res (str (get-ctype type) " " name)] (println res) res))))

(defn cfnsig [name ret args]
  (str (get-ctype ret) " "
       name "("
       (str/join ", "
                 (for [[t n] (partition 2 args)]
                   (str (get-ctype t) " " n)))
       ")"))

(defn extract-locals [args locals]
  (let [argnames (for [[_ n] (partition 2 args)] n)
        localnames (for [local locals] (second local))]
    (apply hash-set (into argnames localnames))))

(defn compile-cfn [name ret args body]
  (beginfn *cmodule-context* name ret args)
  (let [sig-txt (cfnsig name ret args)
        init (first body)
        locals (when (vector? init) init)
        local-decls (vec (map (fn [x] (into x '(decl))) locals))
        body (if locals
               (do
                 (println local-decls)
                 (into local-decls (rest body)))
               body)
        local-names (extract-locals args locals)
        body-txt (binding [*locals* local-names]
                   (println *locals*)
                   (println body)
                   (cblock body))
        fn-txt (str sig-txt "\n" body-txt "\n")
        compiled (compilefn *cmodule-context* fn-txt)]
    (println)
    (print fn-txt)
    compiled))

(defmacro cfn [name ret args & body]
  (compile-cfn name ret args body))

(cinclude "stdio.h")

(cfn test1 i32 [i32 x]
     (if (> 0 1)
       (= x (- (+ x 1) 5)))
     (else (if (< 0 1)
             (while (= 0 1)
               (++ x)
               (-- x))))
     (return x))

(ptest1 23895623589)
LoadLibrary -> dll handle
GetProcAddress -> func ptr
Alloc ptr to ptr, Set ptr = func ptr
resolve test1 -> ptr to func ptr

(cfn test2 i32 [i32 x i32 y]
     (return (+ (test1 x) (test1 y))))

(cfn avg double [double* data u64 len]
     [(u64 i 0) (double sum 0) (double* pdata data)]
     (while (< i len)
       (+= sum (* (++' pdata)))
       (++ i))
     (return (/ sum len)))