(ns c-in-clj.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import
   [System.IO Path File]))

(defprotocol IHasType
  (get-type [this]))

(defprotocol IExpression
  (write [this])
  (expr-category [this])
  (wrap-last [this func]))

(defprotocol IField
  (get-field-type [this])
  (get-bitfield-width [this]))

(defprotocol IType
  (write-type [this])
  (write-decl-expr [this var-name])
  (is-reference-type? [this])
  (create-new-expr [this args])
  (common-denominator-type [this other-type])
  (create-implicit-cast-expr [this expr])
  (create-explicit-cast-expr [this expr])
  (get-fields [this])
  (create-field-access-expr [this instance-expr field-name]))

(defprotocol IDeclaration
  (write-decl [this])
  (write-impl [this])
  (decl-package [this]))

(defrecord CompileSource [source body-source filename])

(defprotocol ICompileContext
  (write-hook [hook-name expr])
  (compile-decls [decls compile-source])
  (resolve-ext-sym [sym-name])
  (resolve-ext-type [type-name]))

(def null-compile-context
  (reify ICompileContext
    (write-hook [hook-name expr])
    (compile-decls [decls source-file-path] {})
    (resolve-ext-sym [sym-name])
    (resolve-ext-type [type-name])))

(defrecord Module [name
                   compile-ctxt
                   src-output-path
                   temp-output-path
                   preamble
                   cpp-mode
                   packages
                   symbols
                   types])

(defrecord Package [name module declarations private-symbols private-types])

(defprotocol ILoadContext
  (load-symbol [package-name symbol-name]))

(def null-loader-context
  (reify ILoadContext
    (load-symbol [package-name symbol-name])))

(defrecord RuntimeModule [name loader packages])

(defrecord RuntimePackage [name module])

(def ^:private packages-by-ns (atom {}))

(defn dev-env? []
  (= (Environment/GetEnvironmentVariable "C_IN_CLJ_DEV") "true"))

(def default-c-preamble
  "#include <stddef.h>\n#include <stdint.h>\n#include <stdbool.h>\n")

(def default-cpp-preamble
  "#include <cstddef>\n#include <cstdint>\n")

(defn create-module [module-name init-compile-ctxt-fn init-load-ctxt-fn {:keys [dev] :as opts}]
  (if (if (not (nil? dev)) dev (dev-env?))
    (let [{:keys [src-output-path temp-output-path cpp-mode preamble]} opts
          cpp-mode (or cpp-mode false)
          preamble (or preamble (if cpp-mode
                                  default-cpp-preamble
                                  default-c-preamble))
          ;;TODO default output paths
          ]
      (Module. module-name (init-compile-ctxt-fn)
               src-output-path temp-output-path
               preamble cpp-mode
               (atom {}) (atom {}) (atom {})))
    (RuntimeModule. module-name (init-load-ctxt-fn) (atom {}))))

(defmacro csource-module [module-name & {:as opts}]
  `(def ~module-name
     (create-module
      ~(name module-name)
      (constantly null-compile-context)
      (constantly null-loader-context)
      ~opts)))

(defn cpackage [module package-name]
  (swap! packages-by-ns assoc *ns*
         (cond (instance? Module module)
               (Package. package-name module (atom []) (atom {}) (atom {}))
               (instance? RuntimeModule module)
               (RuntimePackage. package-name module))))

(defn get-package [] (get @packages-by-ns *ns*))

(defn dev-mode? [module] (instance? Module module))

(defn- find-index-of [items pred]
  (loop [[item & more] items
         idx 0]
    (when item
      (if (pred item)
        idx
        (recur more (inc idx))))))

(defn package-add-decl [{:keys [declarations] :as package} decl]
  (let [decl-name (name decl)
        existing-idx (find-index-of @declarations #(= (name %) decl-name))]
    (if existing-idx
      (swap! declarations assoc existing-idx decl)
      (swap! declarations conj decl))))

(defn module-add-symbol [{:keys [symbols] :as module} decl]
  (swap! symbols assoc (name decl) decl))

(defn package-add-symbol [{:keys [private-symbols] :as package} decl]
  (package-add-decl package decl)
  (if (:private (meta decl))
    (swap! private-symbols assoc (name decl) decl)
    (module-add-symbol (:module package) decl)))

(defn module-add-type [{:keys [types] :as module} decl]
  (swap! types assoc (name decl) decl))

(defn package-add-type [{:keys [private-types] :as package} decl]
  (package-add-decl package decl)
  (if (:private (meta decl))
    (swap! private-types assoc (name decl) decl)
    (module-add-type (:module package) decl)))

(def ^:private ^:dynamic *locals* nil)

(def ^:private ^:dynamic *local-decls* nil)

(def ^:private ^:dynamic *referenced-decls*)

(def ^:dynamic *dynamic-compile* false)

(declare lookup-type)

(defmacro defliteral [name ctype]
  (let [ctype (lookup-type ctype)]
    `(defrecord ~name [value#]
       IExpression
       (write [_] (pr-str value#))
       (expr-category [_] :literal)
       IHasType
       (get-type [_] ~ctype))))

(defliteral Int32Literal int32_t)
(defliteral Int64Literal int64_t)
(defliteral DoubleLiteral double)
(defliteral BooleanLiteral bool)
(defliteral StringLiteral char*)

(def null-literal
  (reify IExpression
    (write [this] "NULL")
    (expr-category [_] :literal)
    IHasType
    (get-type [this])))

(def ^:private primitive-types (atom {}))

(def ^:private type-aliases (atom {}))

(defrecord PrimitiveType [type-name]
  clojure.lang.Named
  (getName [_] type-name)
  IType
  (write-type [_] type-name)
  (write-decl-expr [_ var-name] (str type-name " " var-name))
  (is-reference-type? [_] false)
  (common-denominator-type [_ _]))

(defmacro defprimitive [type-name]
  (let [type-name (name type-name)]
    `(swap! primitive-types
            assoc
            ~type-name
            (PrimitiveType. ~type-name))))

(defprimitive void)
(defprimitive int8_t)
(defprimitive int16_t)
(defprimitive int32_t)
(defprimitive int64_t)
(defprimitive uint8_t)
(defprimitive uint16_t)
(defprimitive uint32_t)
(defprimitive uint64_t)
(defprimitive size_t)
(defprimitive char)
(defprimitive float)
(defprimitive double)
(defprimitive bool)

(defmacro ctype-alias [type-alias type-name]
  `(swap! type-aliases
          assoc
          ~(name type-alias)
          ~(name type-name)))

(ctype-alias i8 int8_t)
(ctype-alias i16 int16_t)
(ctype-alias i32 int32_t)
(ctype-alias i64 int64_t)
(ctype-alias u8 uint8_t)
(ctype-alias u16 uint16_t)
(ctype-alias u32 uint32_t)
(ctype-alias u64 uint64_t)

(defrecord PointerType [type]
  clojure.lang.Named
  (getName [_] (str (name type) "*"))
  IType
  (write-type [_] (str (write-type type) "*"))
  (write-decl-expr [_ var-name] (str (write-type type) "* " var-name))
  (is-reference-type? [_] true)
  (get-fields [_]))

(defrecord AnonymousType [type-name]
  clojure.lang.Named
  (getName [_] type-name)
  IType
  (write-type [_] type-name)
  (write-decl-expr [_ var-name] (str type-name " " var-name))
  (is-reference-type? [_] false))

(defn lookup-type [type-name]
  (cond
   (satisfies? IType type-name) type-name
   (keyword? type-name) (AnonymousType. (name type-name))
   :default
    (let [type-name (name type-name)]
      (if-let [primitive (get @primitive-types type-name)]
        primitive
        (if-let [alias (get @type-aliases type-name)]
          (lookup-type alias)
          (if (.EndsWith type-name "*")
            (PointerType. (lookup-type (.Substring type-name 0 (dec (.Length type-name)))))
            (comment TODO)))))))

(defrecord FunctionParameter [param-name param-type metadata]
  clojure.lang.Named
  (getName [_] param-name)
  IHasType
  (get-type [_] param-type)
  IExpression
  (expr-category [_] :local)
  (write [_] (write-decl-expr param-type param-name)))

(defn- write-function-type [{:keys [return-type params]}
                            pointer-depth name?]
  (str (write-type return-type) " (" name?
       (apply str (repeat pointer-depth "*")) ")("
       (str/join ", " (map write params)) ")"))

(defrecord FunctionType [return-type params]
  IType
  (write-type [this]
    (write-function-type this 0 nil))
  (write-decl-expr [this var-name]
    (write-function-type this 0 var-name))
  (is-reference-type? [this] false))

(defn write-function-signature [function-name {:keys [return-type params]}]
  (str (write-type return-type) " "
       function-name "(" (str/join ", " (map write params)) ")"))

(defrecord FunctionDeclaration [package function-name function-type body referenced-decls metadata]
  clojure.lang.Named
  (getName [this] function-name)
  IHasType
  (get-type [this] function-type)
  IDeclaration
  (write-decl [this]
    (str (write-function-signature function-name function-type) ";"))
  (write-impl [this]
    (str (write-function-signature function-name function-type) "\n"
         (write body)))
  (decl-package [this] package))

(defrecord FunctionCallExpression [func args]
  IExpression
  (write [this]
    (str (name func)
         "(" (str/join "," (map write args)) ")"))
  (expr-category [_])
  IHasType
  (get-type [this] (:return-type (get-type func))))

(defrecord AnonymousFunctionCallExpression [func-name args]
  IExpression
  (write [this]
    (str func-name
         "(" (str/join "," (map write args)) ")"))
  (expr-category [_])
  IHasType
  (get-type [this]))

(defrecord ComputedFunctionCallExpression [func-expr args]
  IExpression
  (write [this]
    (str (write func-expr)
         "(" (str/join "," (map write args)) ")"))
  (expr-category [_])
  IHasType
  (get-type [this]))

(defrecord VariableDeclaration [var-name var-type]
  clojure.lang.Named
  (getName [_] var-name)
  IHasType
  (get-type [_] var-type)
  IExpression
  (expr-category [_])
  (write [_] (write-decl-expr var-type var-name)))

(defrecord VariableRefExpression [variable]
  IExpression
  (write [_] (name variable))
  (expr-category [_])
  IHasType
  (get-type [_] (get-type variable)))

(defrecord AnonymousVariableRefExpression [var-name]
  IExpression
  (write [_] var-name)
  (expr-category [_])
  IHasType
  (get-type [_]))

(declare cexpand)

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

(defn lookup-macro [macro-name])

(def ^:private cintrinsics (atom {}))

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
   (satisfies? IExpression form) form
   (nil? form) null-literal
   (number? form) (cexpand-num form)
   (= Boolean (type form)) (BooleanLiteral. form)
   (string? form) (StringLiteral. form)
   (symbol? form) (lookup-symbol form)
   (keyword? form) (AnonymousVariableRefExpression. (name form))
   (list? form) (cexpand-list form)
   :default (throw (ArgumentException. (str "Don't know how to handle " form " of type " (type form))))))

(defn is-block? [expr]
  (let [cat (expr-category expr)]
    (or (= :statement* cat) (= :block cat))))

(defn cintrinsic* [sym func]
  (swap! cintrinsics assoc sym func))

(defmacro cintrinsic [sym args & body]
  `(cintrinsic* '~sym
               (fn ~sym ~args
                 (let [~@(reduce (fn [res x] (into res [x `(cexpand ~x)])) [] args)]
                   ~@body))))

(defn- sanitize-class-name [name]
  (reduce (fn [name [orig sub]] (str/replace name orig sub))
          (partition 2
                     ["+" "PLUS"
                      "=" "EQ"
                      ">" "GT"
                      "<" "LT"
                      "!" "BANG"
                      ""])))

(defn- get-expr-record-sym [sym]
  (symbol (clojure.lang.Compiler/munge
           (str (name sym) "Expression"))))

(defmacro cop [sym args & body]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
      (defrecord ~rec-sym ~args
        IExpression
        (expr-category [_])
        ~@body)
      (cintrinsic ~sym ~args
                  (new ~rec-sym ~@args)))))

(defn get-bin-op-type [x y]
  (let [xt (get-type x)
        yt (get-type y)]
    (when (= xt yt)
      xt)))

(defmacro cbinop [sym]
  `(cop ~sym [x# y#]
        (write [_] (str "(" (write x#) " " ~(name sym) " " (write y#) ")"))
        IHasType
        (get-type [_] (get-bin-op-type x# y#))))

(defmacro cbinop* [sym expr]
  `(cop ~sym [x# y#]
        (write [_] (str "(" (write x#) " " ~expr " " (write y#) ")"))
        IHasType
        (get-type [_] (get-bin-op-type x# y#))))

(defmacro cbinops [& syms]
  `(do ~@(for [x syms] `(cbinop ~x))))

(defmacro compop [sym]
  `(cop ~sym [x# y#]
        (write [this#] (str "(" (write x#) " " ~(name sym) " " (write y#) ")"))
        IHasType
        (get-type [this#] 'bool)))

(defmacro compops [& syms]
  `(do ~@(for [x syms] `(compop ~x))))

(defmacro compop* [sym expr]
  `(cop ~sym [x# y#]
        (write [this#] (str "(" (write x#) " " ~expr " " (write y#) ")"))
        IHasType
        (get-type [this#] 'bool)))

(defmacro cassignop [sym expr]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
       (defrecord ~rec-sym
           [x# y#]
         IHasType
         (get-type [_])
         IExpression
         (expr-category [_])
         (write [this#] (str "(" (write x#) " " ~expr " " (write y#) ")")))
       (cintrinsic
        ~sym
        [target# source#]
        (if (is-block? source#)
          (wrap-last source#
                     (fn [x#] (new ~rec-sym target# x#)))
          (new ~rec-sym target# source#))))))

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
        (write [this#] ~@body)
        IHasType
        (get-type [this#] (get-type ~(first arg)))))

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
     (write [_] (write-str "!" write x))
     IHasType
     (get-type [_] 'bool))

(cop sizeof [x]
     (write [_] (write-str "sizeof(" x ")"))
     IHasType
     (get-type [_] 'size_t))

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
           IHasType
           (get-type [_])
           IExpression
           (expr-category [_])
           (write [_]
             (str/join "." (map write args)))))))

(cintrinsic* '->
            (fn [& args]
              (let [args (map cexpand args)]
                (reify
                  IHasType
                  (get-type [_])
                  IExpression
                  (expr-category [_])
                  (write [_]
                    (str/join "->" (map write args)))))))

(cop aget [x y]
     (write [_] (write-str x "[" y "]"))
     IHasType
     (get-type [_]))

(defrecord ArraySetExpression [target idx value]
  IHasType
  (get-type [_] (get-type target))
  IExpression
  (expr-category [_])
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
  (expr-category [_] :statement)
  (wrap-last [_ func]
    (Statement. (func expr) noindent))
  (write [_]
    (str (when-not noindent (indent)) (reduce-parens (write expr)) ";")))

(defn cstatement [expr & {:keys [noindent]}]
  (let [expr (cexpand expr)]
    (if (or (is-block? expr) (instance? Statement expr))
      expr
      (Statement. (cexpand expr) noindent))))

(defn- wrap-statements [func statements]
  (conj (vec (drop-last statements))
        (let [last-st (last statements)]
          (if (is-block? last-st)
            (wrap-last last-st func)
            (func last-st)))))

(defrecord Statements [statements]
  IExpression
  (get-type [_] (get-type (last statements)))
  (expr-category [_] :statement*)
  (write [_] (str/join "\n" (map write statements)))
  (wrap-last [_ func] (wrap-statements func statements)))

(defn cstatements [statements]
  (Statements. (for [st statements] (cstatement st))))

(defrecord CaseExpression [test cases]
  IExpression
  (get-type [_])
  (expr-category [_] :statement*)
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
  (expr-category [_])
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
  (expr-category [_] :statement*)
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

(defrecord DeclExpression [var-type var-name init-expr]
  IExpression
  (get-type [_] var-type)
  (write [_] (str (write-decl-expr var-type var-name) "=" (when init-expr (write init-expr))))
  (expr-category [_]))

(defrecord BlockExpression [statements]
  IExpression
  (get-type [_] (get-type (last statements)))
  (expr-category [_] :block)
  (wrap-last [_ func]
    (BlockExpression.
     (wrap-statements func statements)))
  (write [_]
    (str (indent) "{\n"
       (binding [*indent* (inc *indent*)]
         (str/join "\n" (map write statements)))
       "\n" (indent) "}")))

(defn cblock [& statements]
  (BlockExpression. (map cstatement statements)))

(cintrinsic* 'do cblock)

(defn- add-local [decl]
  (let [var-name (name decl)]
    (assert *locals* "No local variable context for let expression")
    (assert (not (get *locals* var-name))
            (str "Local variable named " var-name " already defined"))
    (set! *locals* (assoc *locals* var-name decl))))

(cintrinsic*
 'let
 (fn [& [forms]]
   (assert (even? (count forms)) "let expression must contain an even number of forms")
   (apply
    cblock
    (for [[decl expr-form] (partition 2 forms)]
      (let [init-expr (cexpand expr-form)
            explicit-tag (:tag (meta decl))
            decl-type (if explicit-tag
                        explicit-tag
                        (get-type init-expr))]
        (assert decl-type (str "unable to infer type for let binding: " decl " " expr-form))
        (let [decl-type (lookup-type decl-type)
              decl-expr (VariableDeclaration.
                         (name decl)
                         decl-type)]
          (add-local decl-expr)
          (if (is-block? init-expr)
            (wrap-last init-expr (fn [x] (set_BANG_Expression. (VariableRefExpression. decl-expr) x)))
            (set_BANG_Expression. (VariableRefExpression. decl-expr) init-expr))))))))


;; (defn create-cfn-body [name args body]
;;   (binding [*locals* (extract-locals args)]
;;     (let [sig-txt (cfnsig name ret args)
;;           body-txt (cblock body)
;;           fn-txt (str sig-txt "\n" body-txt "\n")]
;;       fn-txt)))

;; (defmacro cdefn [name ret args & body]
;;   (compile-cfn name ret args body))

(defrecord StructField [name type-name bits]
  clojure.lang.Named
  (getName [_] name)
  IField
  (get-field-type [_] (lookup-type type-name))
  (get-bitfield-width [_] bits))

(defrecord Struct [package struct-name fields]
  IDeclaration
  (write-decl [_]
    (str "typedef struct " struct-name " {\n"
       (str/join
        (for [field fields]
          (str " "
               (write-decl-expr (get-field-type field)
                                (name field))
               (when-let [bits (get-bitfield-width field)]
                 (str ":" bits)) ";\n")))
       "} " struct-name ";"))
  (write-impl [_])
  (decl-package [_] package))

(defn- compile-cstruct [struct-name members]
  (let [struct
        (Struct. (name struct-name)
                 (for [[type-name field-name bits] members]
                   (StructField. (name field-name) type-name bits)))]
    (comment TODO add struct to module/package)
    (print-struct struct)))

(defmacro cstruct [name & members]
  (compile-cstruct name members))

(defn parse-cfn [package [func-name & forms]]
  (let [f1 (first forms)
        doc-str (when (string? f1) f1)
        params (if doc-str (second forms) f1)
        body-forms (if doc-str (nnext forms) (next forms))
        params (for [param params]
                      (let [metadata (meta param)
                            param-name (name param)
                            param-type (:tag metadata)]
                        (FunctionParameter. param-name param-type metadata)))]
    (binding [*locals* (into {} (for [param params] [(name param) param]))
              *locals-decls* {}
              *referenced-decls* #{}]
      (let [body-statements (map cstatement body-forms)
            body-block (if (and (= 1 (count body-statements))
                                (= :block (expr-category (first body-statements))))
                         (first body-statements)
                         (BlockExpression. body-statements))
            func-metadata (meta func-name)
            func-name (name func-name)
            ret-type (:tag func-metadata)
            func-type (FunctionType. ret-type params)
            func-decl (FunctionDeclaration. package func-name func-type body-block *referenced-decls* func-metadata)]
        (package-add-symbol package func-decl)
        func-decl))))

(defn- output-dev-src [package decls cpp-mode preamble temp-output-path]
  (binding [*dynamic-compile* true]
    (let [anon-header (str/join "\n\n" (doall
                                        (for [decl (:declarations package)]
                                          (when-not (name decl)
                                            (write-decl decl)))))
          referenced (apply set/union (map :referenced-decls decls))
          header (str/join "\n\n" (doall (map write-impl referenced)))
          body (str/join "\n\n" (doall (map write-impl decls)))
          src (apply str/join "\n\n" preamble anon-header header body)
          filename (str/join "_" (map name decls))
          ;; TODO save id??
          filename (str filename (if cpp-mode ".cpp" ".c"))
          filename (Path/Combine temp-output-path filename)]
      (println src)
      (File/WriteAllText filename src)
      (CompileSource. src body filename))))

(defn print-numbered [txt]
  (let [lines (str/split-lines txt)]
    (doall
     (map-indexed
      (fn [i line] (println (str i "  " line))) lines))))

(defn compile-cfns [funcs]
  (let [{:keys [module] :as package} (get-package)]
    (if (dev-mode? module)
      (let [{:keys [compile-ctxt temp-output-path preamble cpp-mode]} module
            func-decls (doall (for [func funcs] (parse-cfn package func)))
            {:keys [body-source] :as src}
            (output-dev-src package func-decls cpp-mode preamble temp-output-path)]
        (when-let [compiled compile-decls]
          (println body-source)
          (doseq [[n v] compiled] (intern *ns* (name n) v))))
      (let [{:keys [loader]} module]
        (doseq [[func-name & forms] funcs]
          (load-symbol (:name package) func-name))))))

(defmacro cdefn [& forms]
  (compile-cfns [forms]))

(defmacro cdefn- [& forms]
  `(c-in-clj.core/cdefn ^:private ~@forms))

(defmacro cdefns [])


;;; Test code
(csource-module TestModule :dev true)

(cpackage TestModule "test1")

(cdefn ^void t1 [^i32 x])



