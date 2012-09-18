(ns c-in-clj.core
  (:require [clojure.string :as str])
  (:import [System.IO StringReader]))

(defprotocol ICModuleContext
  (resolve-sym [this sym])
  (compilefn [this name ret args body])
  (compilefns [this funcs])
  (clean [this] [this func]))

(defn null-module-context []
  (reify
    ICModuleContext
    (resolve-sym [this sym])
    (compilefn [this name ret args body]
      (println body))
    (compilefns [this funcs]
      (doseq [{:keys [body]} funcs]
        (println body)))
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

(def cmacros (atom {}))

(defmacro cmodule [name]
  (let [mod (atom {:name name :ns *ns* :includes #{} :headers [] :deftypes {}})]
    (swap! cmodules assoc name mod)
    (swap! cmodules-by-ns assoc *ns* mod)
    nil))

(defn cinclude [header]
  (when-let [mod (@cmodules-by-ns *ns*)]
    (let [value (str "<" header ">")]
      (swap! mod update-in [:includes] conj value)
      (swap! mod update-in [:headers] conj (str "#include " value)))))

(declare cexpand)

(defn c-func-call [func args]
  (str func "(" (str/join "," (for [x args] (cexpand x))) ")"))

(defn cexpand-op-sym [sym args]
  (if-let [intrinsic (@cintrinsics sym)]
    (apply intrinsic args)
    (if-let [macro (@cmacros sym)]
      (cexpand (apply macro args))
      (if (contains? *locals* sym)
        (c-func-call (name sym) args)
        (if-let [foreign (resolve-sym @*cmodule-context* sym)]
          (c-func-call foreign args)
          (throw (ArgumentException. (str "Don't know how to handle list symbol " sym))))))))

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
  (if
   (contains? *locals* sym) (name sym)
   (if-let [foreign
            (resolve-sym @*cmodule-context* sym)]
     foreign
     (throw (ArgumentException. (str "Unresolved symbol " sym))))))

(defn cexpand [form]
  (cond
   (number? form) (pr-str form)
   (string? form) (pr-str form)
   (seq? form) (cexpand-list form)
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

(cbinops + - * / % == != > >= < <= >> >> = += -= *= /= %= <<= >>=)

(cbinop* bit-or "|")
(cbinop* or "||")
(cbinop* or= "|=")
(cbinop* bit-and "&")
(cbinop* bit-shift-left "<<")
(cbinop* bit-shift-right ">>")
(cbinop* and "&&")
(cbinop* and= "&=")
(cbinop* xor "^")
(cbinop* xor= "^=")
(cbinop* comma ",")
(cbinop* set! "=")

(cop ++ [x] (str "++" x))
(cop -- [x] (str "--" x))
(cop ++' [x] (str x "++"))
(cop --' [x] (str x "--"))
(cop bit-not [x] (str "~" x))

(cop ? [x y z] (str "(" x " ? " y " : " z ")"))

(cop sizeof [x] (str "sizeof(" x ")"))

(cintrinsic '. (fn [& args]
                   (str/join "." (map cexpand args))))
(cintrinsic '-> (fn [& args]
                  (str/join "->" (map cexpand args))))

(cintrinsic 'aget (fn [x y]
                    (str (cexpand x) "[" (cexpand y) "]")))

(cintrinsic 'aset (fn [target idx value]
                    (str (cexpand target) "[" (cexpand idx) "] = "
                         (cexpand value))))

(cintrinsic 'ref (fn [x] (str "(&" (cexpand x) ")")))

(cintrinsic 'deref (fn [x] (str "*" (cexpand x))))

(defn c* [& args]
  (let [expanded (for [arg args]
                   (if (string? arg)
                     arg
                     (cexpand arg)))]
    (apply str expanded)))

(cintrinsic 'c* c*)

(defn indent [] (str/join (for [x (range *indent*)] " ")))

(defn reduce-parens [^String expr]
  (when (nil? expr) (throw (ArgumentNullException. "expr")))
  (comment (if (and (.StartsWith expr "(") (.EndsWith expr ")"))
            (.Substring expr 1 (- (.Length expr) 2))
            expr))
  expr)

(defn cexpand-reduce [form] (reduce-parens (cexpand form)))

(defn cstatement [expr & {:keys [noindent]}]
  (let [res (str (when-not noindent (indent)) (cexpand-reduce expr))
        res (if (or
                 (.EndsWith res "}")
                 (.EndsWith res "})")
                 (.EndsWith res ";")
                 (.EndsWith res "*/"))
              res
              (str res ";"))]
    res))

(defn cstatements [statements]
  (let [statements (for [st statements] (cstatement st))]
    (str/join "\n" statements)))

(cintrinsic 'case
            (fn [test & args]
              (let [cases (partition-all 2 args)
                    cases (binding [*indent* (inc *indent*)]
                            (for [[expr block] cases]
                              (if block
                                (let [block (binding [*indent* (inc *indent*)]
                                              (cstatement block))]
                                  (str (indent) "case " (cexpand expr) ":\n" block))
                                (str (indent) "default:" (cexpand expr)))))]
                (str "switch(" (cexpand test) ") {\n" (str/join "\n" cases) (indent) "\n}"))))

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

(cintrinsic 'comment (fn [x] (str "/*" x "*/")))

(cintrinsic 'block (fn [& statements] (cblock statements)))

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

(declare get-ctype)

(defn fn-ptr-sig [fn-ptr-vec name]
  (let [ret (first fn-ptr-vec)
        params (rest fn-ptr-vec)]
    (str (get-ctype ret) "(__stdcall *" name ")(" (str/join "," (map get-ctype params)) ")")))

(def ^:dynamic *local-def-types* {})

(defn get-def-type [type-name]
  (if-let [type-name* (*local-def-types* type-name)]
    type-name*
    (when-let [typedef (@defined-types type-name)]
      (set! *header* (str *header* "\n" (:text typedef) "\n"))
      (set! *local-def-types* (assoc *local-def-types* type-name type-name))
      type-name)))

(defn get-ctype [type-name]
  (cond
   (vector? type-name) (fn-ptr-sig type-name nil)
   :default
   (let [type-name (name type-name)
         ptr-idx (.IndexOf type-name "*")
         ptr-part (when (> ptr-idx 0) (.Substring type-name ptr-idx))
         type-name (if ptr-part (.Substring type-name 0 ptr-idx) type-name)
         ctype (or (get-in @ctypes [type-name :ctype])
                    (get-def-type type-name)
                    type-name)]
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
        (when ptr-part
          IntPtr)))))

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

(cintrinsic 'new
            (fn [target & args]
              (str "new " (get-ctype target) "("
                  (str/join "," (map cexpand args)) ")")))

(cintrinsic 'delete
            (fn [target]
              (c* "delete " target)))


(defn ccast [ty nm]
  (str "((" (get-ctype ty) ")" (cexpand nm) ")"))

(cintrinsic 'cast ccast)

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

(defn- cstruct-member [member]
  (let [ty (first member)
        name (second member)
        bits (first (nnext member))]
    (str " " (c-type-name-pair ty name) (when bits (str ":" bits)) ";\n")))

(defn add-def-type [name info]
  (swap! defined-types assoc name info)
  (swap! (@cmodules-by-ns *ns*) assoc-in [:deftypes name] info))

(defn- compile-cstruct [name members]
  (let [name (str name)]
    (binding [*header* ""
              *local-def-types* {name (str "struct " name)}]
      (let [body-txt
            (str/join (map #(cstruct-member %) members))
            struct-txt
            (str "typedef struct " name " {\n"
                 body-txt
                 "} " name ";")
            mod (@cmodules-by-ns *ns*)
            info {:text struct-txt}]
        (add-def-type name info)
        (println struct-txt)))))
  
(defmacro cstruct [name & members]
  (compile-cstruct name members))

(defn init-header []
  (set! *header* (str *header* "#include <stdint.h>\n"))
  (when-let [mod (@cmodules-by-ns *ns*)]
    (let [headers
          (str/join
           "\n" (:headers @mod))]
      (set! *header* (str *header* headers "\n")))))

(defn wrap-compile-cfn [func]
  (binding [*header* ""
            *local-def-types* {}]
    (init-header)
    (func)))

(defn create-cfn-body [name ret args body]
  (binding [*locals* (extract-locals args)]
    (let [sig-txt (cfnsig name ret args)
          body-txt (cblock body)
          fn-txt (str sig-txt "\n" body-txt "\n")]
      fn-txt)))

(defn compile-cfn [name ret args body]
  (wrap-compile-cfn
   (fn []
     (let [fn-txt (create-cfn-body name ret args body)
           compiled (compilefn @*cmodule-context* name ret args fn-txt)]
       (intern *ns* name compiled)))))

(def test123 (atom nil))

(defn compile-cfns [funcs]
  (wrap-compile-cfn
   (fn []
     (let [funcs (for [[name ret args & body] funcs]
                   {:name name :ret ret :args args :body (create-cfn-body name ret args body)})
           compiled (compilefns @*cmodule-context* funcs)]
       (reset! test123 compiled)
       (doseq [[name func] compiled]
         (intern *ns* name func))))))

(defmacro cdefn [name ret args & body]
  (compile-cfn name ret args body))

(defmacro cdefns [& funcs]
  (compile-cfns funcs))

(comment (def c-unquote)
         (def c-unquote-splicing)

         (defn expand-cmacro-form [form]
           (let [op (first form)
                 more (rest form)]
             (cond 
              (= op 'clojure.core/unquote) [(cexpand more)]
              (= op 'clojure.core/unquote-splicing) (map cexpand more)
              (seq? form) (reduce into (map expand-cmacro-form form))
              :default form)))

         (defmacro c' [& forms]
           `(expand-cmacro ~forms)))

(defn unqualify-symbols [form]
  (if (seq? form)
    (for [f form]
      (unqualify-symbols f))
    (if (symbol? form)
      (symbol (name form))
      form)))

(defmacro cmacro [name params & body]
  `(do
     (swap! c-in-clj.core/cmacros assoc '~name (fn ~params
                                                 (unqualify-symbols
                                                        (do ~@body))))))

(defn cheader [value]
  (when-let [mod (@cmodules-by-ns *ns*)]
    (swap! mod update-in [:headers] conj value)))

(defn cdefine [key value]
  (cheader (str "#define " (name key) " " value)))

(defmacro ctypedef [name value]
  (add-def-type (str name) value))

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

; try, catch, finally, raii for c: http://code.google.com/p/libex/
; good c hash map, tree, etc. lib: https://github.com/attractivechaos/klib

(comment (cdefn test2 i32 [i32 x i32 y]
                (return (+ (test1 x) (test1 y)))))