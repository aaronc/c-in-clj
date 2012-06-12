(ns c-in-clj.core
  (:require [clojure.string :as str])
  (:import [System.IO StringReader]))

(defprotocol ICModuleContext
  (beginfn [this name ret args])
  (resolve-sym [this sym])
  (compilefn [this name ret args body])
  (clean [this] [this func]))

(defn null-module-context []
  (reify
    ICModuleContext
    (beginfn [this name ret args])
    (resolve-sym [this sym])
    (compilefn [this name ret args body]
      (println body))
    (clean [this])
    (clean [this func])))

(def ^:dynamic *cmodule-context* (atom (null-module-context)))

(def ^:private cintrinsics (atom {}))

(def ^:dynamic *indent* 0)

(def ^:dynamic *header*)

(def ^:dynamic *locals* [])

(def ^:private cmodules (atom {}))

(def ^:private cmodules-by-ns (atom {}))

(def ^:private defined-types (atom {}))

(defmacro cmodule [name]
  (let [mod (atom {:name name :ns *ns* :includes #{} :deftypes {}})]
    (swap! cmodules assoc name mod)
    (swap! cmodules-by-ns assoc *ns* mod)
    nil))

(defn cinclude [header]
  (when-let [mod (@cmodules-by-ns *ns*)]
    (swap! mod update-in [:includes] conj (str "<" header ">"))))

(declare cexpand)

(defn c-func-call [func args]
  (str func "(" (str/join "," (for [x args] (cexpand x))) ")"))

(defn cexpand-op-sym [sym args]
  (if-let [intrinsic (@cintrinsics sym)]
    (apply intrinsic args)
    (if (contains? *locals* sym)
      (c-func-call (name sym) args)
      (if-let [foreign (resolve-sym @*cmodule-context* sym)]
        (c-func-call foreign args)
        (throw (ArgumentException. (str "Don't know how to handle list symbol " sym)))))))

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
   (number? form) (pr-str form)
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

(cop sizeof [x] (str "sizeof(" x ")"))

(cintrinsic '. (fn [& args]
                   (str/join "." (map cexpand args))))
(cintrinsic '-> (fn [& args]
                   (str/join "->" (map cexpand args))))

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

(defn fn-ptr-sig [fn-ptr-vec name]
  (let [ret (first fn-ptr-vec)
        params (rest fn-ptr-vec)]
    (str ret "(*" name ")(" (str/join "," params) ")")))

(def ^:dynamic *local-def-types* {})

(defn get-def-type [type-name]
  (if-let [type-name* (*local-def-types* type-name)]
    type-name*
    (when-let [typedef (@defined-types type-name)]
      (set! *header* (str *header* "\n" typedef "\n"))
      (set! *local-def-types* (assoc *local-def-types* type-name type-name))
      type-name)))

(defn get-ctype [type-name]
  (if (vector? type-name)
    (fn-ptr-sig type-name nil)
    (let [type-name (name type-name)
          ptr-idx (.IndexOf type-name "*")
          ptr-part (when (> ptr-idx 0) (.Substring type-name ptr-idx))
          type-name (if ptr-part (.Substring type-name 0 ptr-idx) type-name)
          ctype (or (get-in @ctypes [type-name :ctype])
                    (get-def-type type-name))]
      (str ctype ptr-part))))

(defn get-clr-type [type-name]
  (if (vector? type-name)
    IntPtr
    (let [type-name (name type-name)
          ptr-idx (.IndexOf type-name "*")
          ptr-part (when (> ptr-idx 0) (.Substring type-name ptr-idx))
          type-name (if ptr-part (.Substring type-name 0 ptr-idx) type-name)
          info (@ctypes type-name)
          def-info (get-def-type type-name)]
      (if info
        (if ptr-part
          IntPtr
          (eval (:clr-type info)))
        (when (and def-info ptr-part) IntPtr)))))

(defn c-type-name-pair [t n]
  (if (vector? t)
    (fn-ptr-sig t n)
    (str (get-ctype t) " " n)))

(defn cdecl
  ([type name init]
     (set! *locals* (conj *locals* name))
     (let [tn (c-type-name-pair type name)]
       (str tn " = " (cexpand init))))
  ([type name]
     (set! *locals* (conj *locals* name))
     (c-type-name-pair type name)))

(cintrinsic 'decl cdecl)

(defn ccast [ty nm]
  (str "((" (get-ctype ty) ")" (cexpand nm) ")"))

(cintrinsic 'cast ccast)

(cintrinsic 'decl (fn ([type name init] (let [init (cexpand init)] (str (get-ctype type) " " name " = " init)))
                    ([type name] (let [res (str (get-ctype type) " " name)] (println res) res))))

(defn cfnsig [name ret args]
  (str (get-ctype ret) " "
       name "("
       (str/join ", "
                 (for [[t n] (partition 2 args)]
                   (if (vector? t)
                     (fn-ptr-sig t n)
                     (str (get-ctype t) " " n))))
       ")"))

(defn extract-locals [args]
  (apply hash-set (for [[_ n] (partition 2 args)] n)))


(defn print-numbered [txt]
  (let [rdr (StringReader. txt)]
    (loop [line (.ReadLine rdr)
           i 0]
      (when line
        (println (str i "  " line))
        (recur (.ReadLine rdr) (inc i))))))

(defmacro cstruct [name & members]
  (let [name (str name)]
    (binding [*header* ""
              *local-def-types* {name (str "struct " name)}]
      (let [body-txt
            (str/join (map #(str " " (apply c-type-name-pair %) ";\n")
                           (partition 2 (map str members))))
            struct-txt
            (str "typedef struct " name " {\n"
                 body-txt
                 "} " name ";")
            mod (@cmodules-by-ns *ns*)]
        (swap! defined-types assoc name struct-txt)
        (swap! mod assoc-in [:deftypes name] struct-txt)
        (println struct-txt)))))

(defn init-header []
  (set! *header* (str *header* "#include <stdint.h>\n"))
  (when-let [mod (@cmodules-by-ns *ns*)]
    (let [includes
          (str/join
           "\n" (for [include (:includes @mod)]
                  (str "#include " include)))]
      (set! *header* (str *header* includes "\n")))))

(defn compile-cfn [name ret args body]
  (beginfn @*cmodule-context* name ret args)
  (binding [*header* ""
            *local-def-types* {}
            *locals* (extract-locals args)]
    (init-header)
    (let [sig-txt (cfnsig name ret args)
          body-txt (cblock body)
          fn-txt (str sig-txt "\n" body-txt "\n")
          compiled (compilefn @*cmodule-context* name ret args fn-txt)]
      (intern *ns* name compiled))))

(defmacro cdefn [name ret args & body]
  (compile-cfn name ret args body))

;(cinclude "stdio.h")
(comment
  (cdefn add double [double x double y]
         (return (+ x y)))

  (cdefn test2 double [double x]
         (return (add (* 7 x) (/ 47.435 x))))

  (cdefn test3 double [double x]
         (return (test2 x)))

  (cdefn test4 double [double x [double double double] func]
         (return (func x x))))

;(ptest1 23895623589)
;LoadLibrary -> dll handle
;GetProcAddress -> func ptr
;Alloc ptr to ptr, Set ptr = func ptr
;resolve test1 -> ptr to func ptr

(comment (cdefn test2 i32 [i32 x i32 y]
                (return (+ (test1 x) (test1 y)))))