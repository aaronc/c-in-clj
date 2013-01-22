(ns c-in-clj.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import
   [System.IO Path File Directory]))

(defprotocol IHasType
  (get-type [this]))

(defprotocol IExpression
  (write [this])
  (expr-category [this])
  (wrap-last [this func]))

(defprotocol IField
  (get-bitfield-width [this]))

(defprotocol IType
  (write-type [this])
  (write-decl-expr
    [this var-name]
    [this var-name pointer-depth])
  (is-reference-type? [this])
  (is-function-type? [this])
  (create-new-expr
    [this args])
  (common-denominator-type [this other-type])
  (create-implicit-cast-expr [this expr])
  (create-explicit-cast-expr [this expr])
  (get-fields [this])
  (create-field-access-expr
    [this instance-expr field-name]
    [this instance-expr field-name pointer-depth]))

(defprotocol IDeclaration
  (write-decl [this])
  (write-impl [this])
  (decl-package [this]))

(defrecord CompileSource [source body-source filename])

(defprotocol ISymbolScope
  (add-symbol [this decl])
  (resolve-symbol [this symbol-name]))

(defprotocol ITypeScope
  (add-type [this decl])
  (resolve-type [this type-name]))

(defprotocol ICompileContext
  (write-hook [this hook-name expr])
  (compile-decls [this decls compile-source]))

(def null-compile-context
  (reify
    ICompileContext
    (write-hook [this hook-name expr])
    (compile-decls [this decls {:keys [source]}] (println source))
    ISymbolScope
    (resolve-symbol [this sym-name])
    ITypeScope
    (resolve-type [this type-name])))

(defrecord Module [name
                   compile-ctxt
                   src-output-path
                   temp-output-path
                   preamble
                   cpp-mode
                   packages]
  clojure.lang.Named
  (getName [_] name)
  ISymbolScope
  (resolve-symbol [_ sym-name]
    (resolve-symbol compile-ctxt sym-name))
  ITypeScope
  (resolve-type [_ type-name]
    (resolve-type compile-ctxt type-name)))

(defmethod print-method Module [o w]
  (print-simple (str "#" o (select-keys o [:name])) w))

(defn- find-index-of [items pred]
  (loop [[item & more] items
         idx 0]
    (when item
      (if (pred item)
        idx
        (recur more (inc idx))))))

(defprotocol IPackage
  (add-declaration [this decl]))

(defn look-in-referenced-packages [referenced-packages target-sym accessor]
  (loop [[rp & more] (vec @referenced-packages)]
    (when rp
      (if-let [s (get @(get rp accessor) target-sym)]
        s
        (recur more)))))

(defrecord Package [package-name module declarations public-symbols public-types private-symbols private-types referenced-packages]
  clojure.lang.Named
  (getName [_] package-name)
  IPackage
  (add-declaration [this decl]
    (when (satisfies? IDeclaration decl)
      (let [decl-name (name decl)
            existing-idx (find-index-of @declarations #(= (name %) decl-name))]
        (if existing-idx
          (swap! declarations assoc existing-idx decl)
          (swap! declarations conj decl)))))
  ISymbolScope
  (add-symbol [this decl]
    (add-declaration this decl)
    (if (:private (meta decl))
      (swap! private-symbols assoc (name decl) decl)
      (swap! public-symbols assoc (name decl) decl)))
  (resolve-symbol [_ sym-name]
    (or (get @private-symbols sym-name)
        (get @public-symbols sym-name)
        (look-in-referenced-packages referenced-packages
                                     sym-name
                                     :public-symbols)
        (resolve-symbol module sym-name)))
  ITypeScope
  (add-type [this decl]
    (add-declaration this decl)
    (if (:private (meta decl))
      (swap! private-types assoc (name decl) decl)
      (swap! public-types assoc (name decl) decl)))
  (resolve-type [_ type-name]
    (or (get @private-types type-name)
        (get @public-types type-name)
        (look-in-referenced-packages referenced-packages
                                     type-name
                                     :public-types)
        (resolve-type module type-name))))

(defmethod print-method Package [o w]
  (print-simple (str "#" o (select-keys o [:package-name :module])) w))

(defprotocol ILoadContext
  (load-symbol [this package-name symbol-name]))

(def null-loader-context
  (reify ILoadContext
    (load-symbol [this package-name symbol-name])))

(defrecord RuntimeModule [name loader packages])

(defrecord RuntimePackage [name module])

(def ^:private packages-by-ns (atom {}))

(defn dev-env? []
  (= (Environment/GetEnvironmentVariable "C_IN_CLJ_DEV") "true"))

(def default-c-preamble
  "#include <stddef.h>\n#include <stdint.h>\n#include <stdbool.h>\n")

(def default-cpp-preamble
  "#include <cstddef>\n#include <cstdint>\n")

(defn ensure-directory [path]
  (when-not (Directory/Exists path)
    (Directory/CreateDirectory path)))

(defn create-module [module-name init-compile-ctxt-fn init-load-ctxt-fn {:keys [dev] :as opts}]
  (if (if (not (nil? dev)) dev (dev-env?))
    (let [opts
          (merge
           {:temp-output-path (Path/Combine (Path/GetTempPath) "c-in-clj")
            :cpp-mode false}
           opts)
          opts (merge {:preamble (if (:cpp-mode opts)
                                  default-cpp-preamble
                                  default-c-preamble)}
                      opts)
          {:keys [src-output-path temp-output-path cpp-mode preamble]} opts]
      (ensure-directory temp-output-path)
      (when src-output-path
        (ensure-directory src-output-path))
      (Module. module-name (init-compile-ctxt-fn opts)
               src-output-path temp-output-path
               preamble cpp-mode
               (atom {})))
    (RuntimeModule. module-name (init-load-ctxt-fn opts) (atom {}))))

(defmacro csource-module [module-name & {:as opts}]
  `(def ~module-name
     (create-module
      ~(name module-name)
      (constantly null-compile-context)
      (constantly null-loader-context)
      ~opts)))

(defmacro cpackage [module package-sym]
  (let [module (eval module)
        package-name (name package-sym)
        package (cond (instance? Module module)
               (Package. package-name module (atom []) (atom {}) (atom {}) (atom {}) (atom {}) (atom #{}))
               (instance? RuntimeModule module)
               (RuntimePackage. package-name module))]
    (when (instance? Module module)
      (swap! (:packages module) assoc package-name package))
    (swap! packages-by-ns assoc *ns* package)
    `(def ~package-sym ~package)))

(defn get-package [] (get @packages-by-ns *ns*))

(defn get-module [] (:module (get-package)))

(defn apply-hook [hook-name expr]
  (let [compile-ctxt (:compile-ctxt (get-module))]
    (write-hook compile-ctxt hook-name expr)))

(defmacro defhooks [name]
  `(let [hook-map# (atom {})]
    (defmacro ~name
      {:hook-map hook-map#}
      [hook-name# args# & body#]
      (swap! hook-map# assoc
             hook-name#
             (eval
              `(fn ~(symbol (name hook-name#)) ~args#
                 ~@body#))))))

(defn dispatch-hook
  "Dispatches a hook to a hook map defined with defhooks.
Usage: (dispatch-hook #'hook-map)."
  [hooks hook-name ctxt expr]
  (when-let [hook-impl (get @(:hook-map (meta hooks)) hook-name)]
    (hook-impl ctxt expr)))

(defn dev-mode? [module] (instance? Module module))

(def ^:private ^:dynamic *locals* nil)

(def ^:private ^:dynamic *local-decls* nil)

(def ^:private ^:dynamic *referenced-decls* nil)

(def ^:dynamic *dynamic-compile* false)

(def ^:private primitive-types (atom {}))

(def ^:private type-aliases (atom {}))

(def ^:private save-id (atom 0))

(defn add-referenced-decl [resolved]
  ;; (println "trying to add ref to" resolved
  ;;          (satisfies? IDeclaration resolved)
  ;;          *referenced-decls*)
  (when (and (satisfies? IDeclaration resolved) *referenced-decls*)
    (set! *referenced-decls*
          (conj *referenced-decls* resolved))))

(defrecord DefaultCastExpression [target-type expr]
  IHasType
  (get-type [_] target-type)
  IExpression
  (write [_] (str "((" (write-type target-type) ")" (write expr) ")"))
  (expr-category [_]))

(defrecord PrimitiveType [type-name]
  clojure.lang.Named
  (getName [_] type-name)
  IType
  (write-type [_] type-name)
  (write-decl-expr [_ var-name] (str type-name " " var-name))
  (write-decl-expr [_ var-name pointer-depth]
    (str type-name (apply str (repeat pointer-depth "*")) " " var-name))
  (is-reference-type? [_] false)
  (is-function-type? [_] false)
  (common-denominator-type [_ _])
  (create-explicit-cast-expr [this expr]
    (DefaultCastExpression. this expr)))

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
  (write-decl-expr [_ var-name]
    (write-decl-expr type var-name 1))
  (write-decl-expr [_ var-name pointer-depth]
    (write-decl-expr type var-name (inc pointer-depth)))
  (is-function-type? [_]
    (is-function-type? type))
  (is-reference-type? [_] true)
  (get-fields [_])
  (create-explicit-cast-expr [this expr]
    (DefaultCastExpression. this expr))
  (create-field-access-expr [this instance-expr member-name]
    (create-field-access-expr this instance-expr member-name 1))
  (create-field-access-expr [this instance-expr member-name pointer-depth]
    (create-field-access-expr type instance-expr member-name pointer-depth)))

(defrecord AnonymousType [type-name]
  clojure.lang.Named
  (getName [_] type-name)
  IType
  (write-type [_] type-name)
  (write-decl-expr [_ var-name] (str type-name " " var-name))
  (is-reference-type? [_])
  (is-function-type? [_]))

(defrecord StaticArrayType [underlying-type array-length]
  clojure.lang.Named
  (getName [_] (str (name underlying-type) "[" array-length "]"))
  IType
  (write-type [_] (str (write-type underlying-type) "[" array-length "]"))
  (write-decl-expr [_ var-name] (str (write-decl-expr underlying-type var-name)
                                     "[" array-length "]"))
  (is-reference-type? [_] true)
  (is-function-type? [_] false))

(defn lookup-type [type-name]
  (let [resolved-type
        (cond
         (satisfies? IType type-name) type-name
         (keyword? type-name) (AnonymousType. (name type-name))
         :default
         (let [type-name (name type-name)]
           (if-let [[_ type-name array-len] (re-matches #"(.*)!([0-9])*" type-name)]
             (let [underlying-type (lookup-type type-name)
                   array-len (when-not (empty? array-len)
                               (int array-len))]
               (if array-len
                 (StaticArrayType. underlying-type array-len)
                 (PointerType. underlying-type)))
             (if-let [primitive (get @primitive-types type-name)]
               primitive
               (if-let [alias (get @type-aliases type-name)]
                 (lookup-type alias)
                 (if (.EndsWith type-name "*")
                   (PointerType. (lookup-type (.Substring type-name 0 (dec (.Length type-name)))))
                   (resolve-type (get-package) type-name)))))))]
    (add-referenced-decl resolved-type)
    resolved-type))

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

(defrecord CharLiteral [value]
       IExpression
       (write [_] (str "'" value "'"))
       (expr-category [_] :literal)
       IHasType
       (get-type [_] "char"))

(def null-literal
  (reify IExpression
    (write [this] "NULL")
    (expr-category [_] :literal)
    IHasType
    (get-type [this])))

(defrecord FunctionParameter [param-name param-type]
  clojure.lang.Named
  (getName [_] param-name)
  IHasType
  (get-type [_] param-type)
  IExpression
  (expr-category [_] :local)
  (write [_] (write-decl-expr param-type param-name)))

(defn- write-function-type [{:keys [return-type params]}
                            pointer-depth name?]
  (str (write-type return-type) " ("
       (apply str (repeat pointer-depth "*"))
       name? ")("
       (str/join ", " (map write params)) ")"))

(defrecord FunctionType [return-type params]
  IType
  (write-type [this]
    (write-function-type this 0 nil))
  (write-decl-expr [this var-name]
    (write-function-type this 0 var-name))
  (write-decl-expr [this var-name pointer-depth]
    (write-function-type this pointer-depth var-name))
  (is-reference-type? [this] false)
  (is-function-type? [_] true))

(defn write-function-signature [{:keys [function-name function-type] :as decl} ]
  (let [{:keys [return-type params]} function-type]
    (str
     (apply-hook :before-function-signature decl)
     (write-type return-type) " "
     function-name "(" (str/join ", " (map write params)) ")")))

(defrecord FunctionDeclaration [package function-name function-type body referenced-decls]
  clojure.lang.Named
  (getName [this] function-name)
  IHasType
  (get-type [this] function-type)
  IDeclaration
  (write-decl [this]
    (or (apply-hook :alternate-function-declaration this)
        (str
         (when-let [doc (:doc (meta this))]
           (str "/**\n" 
                (apply str (map #(str "* " % "\n") (str/split-lines doc)))
                "*/\n"))
         (write-function-signature this) ";")))
  (write-impl [this]
    (str (write-function-signature this) "\n"
         (write body)))
  (decl-package [this] package))

(defmethod print-method FunctionDeclaration [o w]
  (print-simple
   (str "#" o (into {} (update-in o [:referenced-decls] #(map name %)))) w))

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

(defn create-var-decl [var-name var-type]
  (let [var-type (lookup-type var-type)]
    (VariableDeclaration. var-name var-type)))

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

(defn lookup-symbol [sym-name]
  (let [resolved-symbol
        (if (keyword? sym-name)
          (name sym-name)
          (let [sym-name (name sym-name)]
            (if-let [local (get *locals* sym-name)]
              (VariableRefExpression. local)
              (resolve-symbol (get-package) sym-name))))]
    (add-referenced-decl resolved-symbol)
    resolved-symbol))

(defn lookup-macro [macro-name])

(def ^:private cintrinsics (atom {}))

(defn- cexpand-member-access [sym args]
  (let [sym-name (name sym)
        target (first args)
        args (rest args)
        target (lookup-symbol target)]
    ))

(defn cexpand-op-sym [sym args]
  (let [sym-name (name sym)]
    (if-let [intrinsic (@cintrinsics sym)]
      (apply intrinsic args)
      (if (.StartsWith sym-name ".") ;; Member access
        (cexpand-op-sym '. (apply vector
                                  (first args)
                                  (.Substring sym-name 1)
                                  (rest args)))
        (if-let [macro (lookup-macro sym)]
          (cexpand (apply macro args))
          (if-let [local-func (get *locals* sym)]
            (AnonymousFunctionCallExpression. (name sym) (map cexpand args))
            (if-let [defined (lookup-symbol sym)]
              (FunctionCallExpression. defined (map cexpand args))
              (throw (ArgumentException. (str "Don't know how to handle list symbol " sym))))))))))

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

(defrecord InitializerList [values]
  IHasType
  (get-type [_])
  IExpression
  (expr-category [_])
  (write [_] (str "{" (str/join ", " (map write values)) "}")))

(defn cexpand-vector [values]
  (InitializerList. (map cexpand values)))

(defn cexpand [form]
  (cond
   (satisfies? IExpression form) form
   (nil? form) null-literal
   (char? form) (CharLiteral. form)
   (number? form) (cexpand-num form)
   (= Boolean (type form)) (BooleanLiteral. form)
   (string? form) (StringLiteral. form)
   (symbol? form) (lookup-symbol form)
   (keyword? form) (AnonymousVariableRefExpression. (name form))
   (list? form) (cexpand-list form)
   (vector? form) (cexpand-vector form)
   :default (throw (ArgumentException. (str "Don't know how to handle " form " of type " (type form))))))

(defn is-block? [expr]
  (let [cat (expr-category expr)]
    (or (= :statement* cat) (= :block cat))))

(defn cintrinsic* [sym func]
  (swap! cintrinsics assoc sym func))

(defmacro cintrinsic [sym args & body]
  `(cintrinsic* '~sym
               (fn ~sym ~args
                 (let [~@ (reduce (fn [res x] (into res [x `(cexpand ~x)])) [] args)]
                   ~@body))))

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

(defn get-bin-op-type [& args]
  (let [types (map get-type args)]
    (when (apply = types)
      (first types))))

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

(defn reduce-parens [^String expr]
  (when expr
    (if (.StartsWith expr "(")
      (let [len (.Length expr)
            matching-paren-idx
            (loop [idx 1
                   depth 1]
              (when (< idx len)
                (let [ch (nth expr idx)]
                  (cond
                   (= \( ch)
                   (recur (inc idx) (inc depth))
                   (= \) ch)
                   (if (= depth 1)
                     idx
                     (recur (inc idx) (dec depth)))
                   :default
                   (recur (inc idx) depth)))))]
        (if (= matching-paren-idx (- len 1))
          (.Substring expr 1 (- len 2))
          expr))
      expr)))

(defmacro cassignop [sym expr]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
       (defrecord ~rec-sym
           [x# y#]
         IHasType
         (get-type [_])
         IExpression
         (expr-category [_])
         (write [this#] (str "(" (write x#) " " ~expr " " (reduce-parens (write y#)) ")")))
       (cintrinsic
        ~sym
        [target# source#]
        (if (is-block? source#)
          (wrap-last source#
                     (fn [x#] (new ~rec-sym target# x#)))
          (new ~rec-sym target# source#))))))

(defmacro c*op [sym]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
       (defrecord ~rec-sym [~'args]
         IExpression
         (expr-category [_])
         (write [_]
           (if (= 1 (count ~'args))
             (str ~(name sym) (write (first ~'args)))
             (str "(" (str/join ~(str " " sym " ") (map write ~'args)) ")")))
         IHasType
         (get-type [_] (apply get-bin-op-type ~'args)))
       (cintrinsic* '~sym
                    (fn [& args#]
                      (new ~rec-sym (doall (map cexpand args#))))))))

(defmacro c*ops [& syms]
  `(do ~@(for [x syms] `(c*op ~x))))

(c*ops + - * /)
(cassignop += "+=")
(cassignop -= "-=")
(cassignop *= "*=")
(cassignop /= "/=")
(compops < > <= >=)
(compop* = "==")
(compop* not= "!=")
(cbinop* mod "%")
(cassignop mod= "%=")
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

(defrecord StructFieldAccessExpression [instance-expr member-info pointer-depth]
  IHasType
  (get-type [_] (get-type member-info))
  IExpression
  (expr-category [_])
  (write [_]
    (let [prefix-pointer (when (>= pointer-depth 2)
                           (- pointer-depth 2))
          pointer-depth (if prefix-pointer 1
                            pointer-depth)]
      (str
       (when prefix-pointer
         (apply str "(" (repeat prefix-pointer "*")))
       (write instance-expr)
       (when prefix-pointer ")")
       (case pointer-depth
         0 "."
         1 "->")
       (name member-info)))))

(cintrinsic* '.
             (fn [instance-expr member-name & args]
               (let [instance-expr (cexpand instance-expr)
                     instance-type (get-type instance-expr)
                     access-expr (create-field-access-expr
                    instance-type instance-expr member-name)]
                 (if args
                   (let [args (map cexpand args)]
                     )
                   access-expr))))

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
     (write [_] (write-str "(&" x ")"))
     IHasType
     (get-type [_]))

(cop deref [x]
     (write [_] (write-str "*" x))
     IHasType
     (get-type [_]))

(def ^:dynamic *indent* 0)

(defn indent [] (str/join (for [x (range *indent*)] "\t")))

(defrecord Statement [expr noindent]
  IExpression
  (expr-category [_] :statement)
  (wrap-last [_ func]
    (Statement. (func expr) noindent))
  (write [_]
    (str (when-not noindent (indent)) (reduce-parens (write expr)) ";"))
  IHasType
  (get-type [_] (get-type expr)))

(defn cstatement [expr & {:keys [noindent]}]
  (let [expr (cexpand expr)]
    (when expr
      (if (or (is-block? expr) (= :statement (expr-category expr)))
        expr
        (Statement. (cexpand expr) noindent)))))

(defn- wrap-statements [func statements]
  (conj (vec (drop-last statements))
        (wrap-last (last statements) func)))

(defrecord Statements [statements]
  IExpression
  (expr-category [_] :statement*)
  (write [_] (str/join "\n" (map write statements)))
  (wrap-last [_ func] (wrap-statements func statements))
  IHasType
  (get-type [_] (get-type (last statements))))

(defn cstatements [statements]
  (Statements. (doall (map cstatement (remove nil? statements)))))

(defrecord CaseExpression [test cases]
  IHasType
  (get-type [_])
  IExpression
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
  IHasType
  (get-type [_] (get-type expr))
  IExpression
  (expr-category [_])
  (write [_] (if expr
               (if-let [expr (write expr)]
                 (str "return " (reduce-parens expr))
                 "return")
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
  IHasType
  (get-type [_])
  IExpression
  (expr-category [_] :statement*)
  (write [_]
    (str (indent)
         "if(" (reduce-parens (write expr)) ")\n"
         (binding [*indent* (inc *indent*)] (write then))
         (when else
           (str "\n" (indent) "else\n"
                (binding [*indent* (inc *indent*)] (write else))))))
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
  IHasType
  (get-type [_] var-type)
  IExpression
  (write [_] (str (write-decl-expr var-type var-name) "=" (when init-expr (write init-expr))))
  (expr-category [_]))

(defrecord BlockExpression [statements]
  IHasType
  (get-type [_] (get-type (last statements)))
  IExpression
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
  (BlockExpression. (doall (map cstatement (remove nil? statements)))))

(cintrinsic* 'do cblock)

(defrecord ForStatement [init-expr test-expr each-expr body]
    IHasType
    (get-type [_])
    IExpression
    (expr-category [_] :statement)
    (write [_]
      (str (indent) "for("
           (reduce-parens (write init-expr)) "; "
           (reduce-parens (write test-expr)) "; "
           (reduce-parens (write each-expr)) ")\n"
           (write body)))
    (wrap-last [_ func] (throw (Exception. "Cannot take value of for statement"))))

(defrecord CommaExpression [expressions]
  IHasType
  (get-type [_])
  IExpression
  (expr-category [_] :noreturn)
  (write [_] (str/join ", " (map write expressions))))

(defrecord NopExpression []
  IHasType
  (get-type [_])
  IExpression
  (write [_])
  (expr-category [_]))

(defn- wrap-for-expressions [form]
  (cond
   (empty? form)
   (NopExpression.)
   (vector? form)
   (CommaExpression. (map cexpand form))
   :default
   (cexpand form)))

(cintrinsic* 'for
             (fn [init test each & body]
               (let [body (if (= (count body) 1)
                            (cstatement (first body))
                            (apply cblock body))]
                 (ForStatement.
                  (wrap-for-expressions init)
                  (wrap-for-expressions test)
                  (wrap-for-expressions each)
                  body))))

(defrecord BreakStatement []
  IHasType
  (get-type [_])
  IExpression
  (write [_] "break")
  (expr-category [_]))

(cintrinsic break [] (BreakStatement.))

(defrecord ContinueStatement []
  IHasType
  (get-type [_])
  IExpression
  (write [_] "continue")
  (expr-category [_]))

(cintrinsic continue [] (ContinueStatement.))

(defn- add-local [decl]
  (let [var-name (name decl)]
    (assert *locals* "No local variable context for let expression")
    (assert (not (get *locals* var-name))
            (str "Local variable named " var-name " already defined"))
    (set! *locals* (assoc *locals* var-name decl))
    (set! *local-decls* (conj *local-decls* decl))))

(cintrinsic*
 'let
 (fn [& forms]
   (let [let-forms (first forms)
         body-forms (rest forms)]
     (assert (even? (count let-forms)) "let expression must contain an even number of forms")
     (apply
      cblock
      (concat
       (for [[decl expr-form] (partition 2 let-forms)]
         (let [init-expr (cexpand expr-form)
               explicit-tag (:tag (meta decl))
               decl-type (if explicit-tag
                           explicit-tag
                           (get-type init-expr))]
           (assert decl-type (str "unable to infer type for let binding: " decl " " expr-form))
           (let [decl-type (lookup-type decl-type)
                 decl-expr (create-var-decl
                            (name decl)
                            decl-type)]
             (add-local decl-expr)
             (if (is-block? init-expr)
               (wrap-last init-expr (fn [x] (set_BANG_Expression. (VariableRefExpression. decl-expr) x)))
               (set_BANG_Expression. (VariableRefExpression. decl-expr) init-expr)))))
       (map cstatement body-forms))))))

(cintrinsic*
 'declare
 (fn [sym]
   (if-let [decl-type (:tag (meta sym))]
     (let [sym-name (name sym)]
       (add-local
        (create-var-decl
         sym-name
         decl-type))
       nil)
     (throw (ArgumentException.
             (str "Unable to infer type for declare expression of symbol" sym))))))


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
  IHasType
  (get-type [_] (lookup-type type-name))
  IField
  (get-bitfield-width [_] bits))

(defrecord Struct [package struct-name fields]
  clojure.lang.Named
  (getName [_] struct-name)
  IHasType
  (get-type [this] this)
  IType
  (write-type [this]
    (add-referenced-decl this)
    struct-name)
  (is-function-type? [_] false)
  (is-reference-type? [_] false)
  (write-decl-expr [this var-name]
    (write-decl-expr this var-name 0))
  (write-decl-expr [this var-name pointer-depth]
    (add-referenced-decl this)
    (str struct-name (apply str (repeat pointer-depth "*"))
         " " var-name))
  (create-explicit-cast-expr [this expr]
    (DefaultCastExpression. this expr))
  (get-fields [this] fields)
  (create-field-access-expr [this instance-expr field-name])
  (create-field-access-expr [this instance-expr field-name pointer-depth]
    (when-let [field (get fields (name field-name))]
      (StructFieldAccessExpression. instance-expr field pointer-depth)))
  IDeclaration
  (write-decl [_]
    (str "typedef struct " struct-name " {\n"
       (str/join
        (for [[field-name field] fields]
          (str "\t"
               (write-decl-expr (get-type field)
                                (name field))
               (when-let [bits (get-bitfield-width field)]
                 (str ":" bits)) ";\n")))
       "} " struct-name ";"))
  (write-impl [_])
  (decl-package [_] package))

(defn cstruct* [struct-name members]
  (let [package (get-package)
        struct
        (Struct.
         package
         (name struct-name)
         (into (array-map)
               (for [[type-name field-name bits] members]
                 [(name field-name)
                  (StructField. (name field-name) (name type-name) bits)]))
         (meta struct-name)
         nil)]
    (add-type package struct)
    (println (write-decl struct))))

(defmacro cstruct [name & members]
  (cstruct* name members))

(cintrinsic*
 'cast
 (fn [target-type expr]
   (let [target-type (lookup-type target-type)
         expr (cexpand expr)]
     (create-explicit-cast-expr target-type expr))))

(defn- parse-fn-params [params]
  (for [param params]
    (let [metadata (meta param)
          param-name (name param)
          param-type (lookup-type (:tag metadata))]
      (FunctionParameter. param-name param-type metadata nil))))

(defn parse-cfn [package [func-name & forms]]
  (let [f1 (first forms)
        doc-str (when (string? f1) f1)
        params (if doc-str (second forms) f1)
        body-forms (if doc-str (nnext forms) (next forms))]
    (binding [*referenced-decls* #{}]
      (let [params (parse-fn-params params)]
        (binding [*locals* (into {} (for [param params] [(name param) param]))
                  *local-decls* []]
          (let [body-forms (or body-forms [(ReturnExpression. nil)])
                body-statements (vec (remove nil? (doall (map cstatement body-forms)))) 
                local-decls (vec (doall (map cstatement *local-decls*)))
                body-statements (concat local-decls body-statements)
                body-block (if (and (= 1 (count body-statements))
                                    (= :block (expr-category (first body-statements))))
                             (first body-statements)
                             (BlockExpression. body-statements))
                body-block (wrap-last
                            body-block
                            (fn [expr]
                              (if (instance? ReturnExpression expr)
                                expr
                                (ReturnExpression. expr))))
                func-metadata (meta func-name)
                func-metadata (if doc-str
                                (assoc func-metadata :doc doc-str)
                                func-metadata)
                func-name (name func-name)
                ret-type (lookup-type (:tag func-metadata))

                func-type (FunctionType. ret-type params)
                func-decl (FunctionDeclaration. package func-name func-type body-block *referenced-decls* func-metadata nil)]
            (add-symbol package func-decl)
            func-decl))))))

(defn- write-dev-header [referenced-decls]
  (let [already-referenced *referenced-decls*]
    (let [header
          (str/join "\n\n" (doall (map write-decl referenced-decls)))
          already-referenced (set/union already-referenced referenced-decls)
          new-refs (set/difference *referenced-decls* already-referenced)]
      (if (empty? new-refs)
        header
        (str (write-dev-header new-refs) "\n\n" header)))))

(defn- output-dev-src [package decls cpp-mode preamble temp-output-path]
  (binding [*dynamic-compile* true]
    (let [anon-header (str/join "\n\n" (doall
                                        (remove
                                         nil?
                                         (for [decl @(:declarations package)]
                                           (when-not (name decl)
                                             (write-decl decl))))))
          ;;referenced (apply set/union (map :referenced-decls decls))
          [body referenced] (binding [*referenced-decls* #{}]
                              [(str/join "\n\n" (doall (map write-impl decls)))
                               *referenced-decls*])
          header (binding [*referenced-decls* #{}]
                   (write-dev-header referenced))
          header (str/trim header)
          src (str/join "\n" [preamble anon-header header "\n" body])
          filename (str/join  "_" (map name decls))
          ;; TODO save id??
          filename (str (name package) "_" filename "_" (swap! save-id inc) (if cpp-mode ".cpp" ".c"))
          filename (Path/Combine temp-output-path filename)]
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
        (when-let [compiled (compile-decls compile-ctxt func-decls src)]
          (println body-source)
          (doseq [[n v] compiled] (intern *ns* (symbol (name n)) v))))
      (let [{:keys [loader]} module]
        (doseq [[func-name & forms] funcs]
          (load-symbol loader (:name package) func-name))))))

(defn cdefn* [& forms]
  (compile-cfns [forms]))

(defmacro cdefn [& forms]
  (apply cdefn* forms))

(defmacro cdefn- [& forms]
  (let [[sym & forms] forms
        metadata (meta sym)
        metadata (assoc metadata :private true)
        sym (with-meta sym metadata)]
    `(c-in-clj.core/cdefn ~sym ~@forms)))

(defmacro cdefns [])

(defn unqualify-symbols [form]
  (if (seq? form)
    (apply list
           (for [f form]
             (unqualify-symbols f)))
    (if (symbol? form)
      (symbol (name form))
      form)))

(defrecord TypeDef [package typedef-name target-type]
  clojure.lang.Named
  (getName [_] typedef-name)
  IHasType
  (get-type [_] target-type)
  IDeclaration
  (write-decl [_] (str "typedef " (write-decl-expr target-type typedef-name) ";"))
  (write-impl [_])
  (decl-package [_] package)
  IType
  (write-type [this]
    (add-referenced-decl this)
    typedef-name)
  (write-decl-expr
    [this var-name] (write-decl-expr this var-name 0))
  (write-decl-expr
    [this var-name pointer-depth]
    (add-referenced-decl this)
    (str typedef-name (apply str (repeat pointer-depth "*"))
         " " var-name))
  (is-reference-type? [_] (is-reference-type? target-type))
  (is-function-type? [_] (is-function-type? target-type))
  (common-denominator-type [_ other-type]
    (common-denominator-type target-type other-type))
  (get-fields [_] (get-fields target-type))
  (create-field-access-expr [_ instance-expr field-name]
    (create-field-access-expr target-type instance-expr field-name)))

(defn ctypedef* [target-type typedef-name metadata]
  (let [package (get-package)
        typedef-name (name typedef-name)
        target-type (lookup-type target-type)
        decl (TypeDef. package typedef-name target-type metadata nil)]
    (add-type package decl)
    (write-decl decl)))

(defmacro ctypedef
  ([target-type typedef-name]
     (ctypedef* target-type typedef-name nil))
  ([metadata target-type typedef-name]
     (ctypedef* target-type typedef-name metadata)))

(defmacro ctypedeffn [typedef-name params]
  (let [metadata (meta typedef-name)
        return-type (lookup-type (:tag metadata))
        params (parse-fn-params params)
        func-ptr-type (PointerType. (FunctionType. return-type params))]
    (ctypedef* func-ptr-type typedef-name metadata)))

(defrecord EnumValue [name value base-type]
  clojure.lang.Named
  (getName [_] name)
  IHasType
  (get-type [_] base-type)
  IExpression
  (expr-category [_])
  (write [_] name))

(defrecord EnumType [package enum-name values]
  clojure.lang.Named
  (getName [_] enum-name)
  IDeclaration
  (write-decl [_] (str "enum " enum-name
                       " {"
                       (str/join
                        ", "
                        (map #(str (:name %) " = " (:value %)) values))
                       "};"))
  (write-impl [_])
  (decl-package [_] package)
  IType
  (write-type [this]
    (add-referenced-decl this)
    enum-name)
  (write-decl-expr
    [this var-name] (write-decl-expr this var-name 0))
  (write-decl-expr
    [this var-name pointer-depth]
    (add-referenced-decl this)
    (str enum-name (apply str (repeat pointer-depth "*"))
         " " var-name))
  (is-reference-type? [_] false)
  (is-function-type? [_] false)
  (common-denominator-type [_ other-type])
  (get-fields [_])
  (create-field-access-expr [_ instance-expr field-name]))

(defn- cenum* [enum-name values]
  (let [package (get-package)
        enum-name (name enum-name)
        values (partition 2 values)
        base-type (lookup-type "i32")
        values (map #(EnumValue. (name (first %))
                                 (second %)
                                 base-type)
                    values)
        enum (EnumType. package enum-name values)]
    (add-type package enum)
    (doseq [v values]
      (add-symbol package v))
    (println (write-decl enum))))

(defmacro cenum [enum-name values]
  (cenum* enum-name values))

(defrecord PackageIncludeDeclaration [package referenced-package]
  clojure.lang.Named
  (getName [_] (str (name referenced-package) ".h"))
  IDeclaration
  (decl-package [_] package)
  (write-decl [_] (str "#include \"" (name referenced-package) ".h\""))
  (write-impl [_]))

(defrecord IncludeDeclaration [package include-name]
  clojure.lang.Named
  (getName [_] include-name)
  IDeclaration
  (decl-package [_] package)
  (write-decl [_] (str "#include <" include-name ">"))
  (write-impl [_]))

(defn cinclude [package-or-include & {:as opts}]
  (let [package (get-package)]
    (if (satisfies? IPackage package-or-include)
      (let [referenced-pkg package-or-include
            decl (PackageIncludeDeclaration. package referenced-pkg opts nil)]
        (swap! (:referenced-packages (get-package)) conj package-or-include)
        (add-declaration package decl)
        decl)
      (let [include-name package-or-include
            decl (IncludeDeclaration. package include-name opts nil)]
        (add-declaration package decl)
        decl))))

(defrecord GlobalVariableDeclaration [package var-name var-type init-expr]
  clojure.lang.Named
  (getName [_] var-name)
  IHasType
  (get-type [_] var-type)
  IDeclaration
  (write-decl [_] (str "extern " (write-decl-expr var-type var-name) ";"))
  (write-impl [_] (str (write-decl-expr var-type var-name) (when init-expr (str " = " (write init-expr))) ";")))

(defn cdef*
  ([var-name init-expr]
     (let [package (get-package)
           metadata (meta var-name)
           var-name (name var-name)
           var-type (lookup-type (:tag metadata))
           init-expr (when init-expr (cexpand init-expr))
           var-decl (GlobalVariableDeclaration. package var-name var-type
                                                init-expr metadata nil)]
       (add-symbol package var-decl)
       (println (write-impl var-decl))))
  ([var-name] (cdef* var-name nil)))

(defn write-package-source [{:keys [declarations module] :as package} & {:keys [src-output-path cpp-mode]}]
  (let [{:keys [preamble]} module
        pkg-name (name package)
        header-name (str pkg-name ".h")
        include-guard-name (.ToUpper
                            (str "_" (name module) "__" pkg-name "__H_"))
        public-decls (filter #(not (:private (meta %))) @declarations)
        private-decls (filter #(:private (meta %)) @declarations)
        decl-src
        (str "#ifndef " include-guard-name "\n"
             "#define " include-guard-name "\n\n"
             preamble
             "\n"
             (str/join "\n\n"
                       (remove empty?
                               (for [decl public-decls]
                                 (write-decl decl))))
             "\n\n#endif // " include-guard-name "\n")
       
        impl-src
        (str
         "#include \"" header-name "\"\n\n"
         (str/join "\n\n"
                   (remove empty?
                           (concat
                            (for [decl private-decls]
                              (write-decl decl))
                            (for [decl @declarations]
                              (write-impl decl))))))]
    (when src-output-path
      (let [decl-path (Path/Combine src-output-path header-name)
            impl-path (Path/Combine src-output-path
                                    (str pkg-name
                                         (if cpp-mode ".cpp" ".c")))]

        (println "Writing" decl-path)
        (println "Writing" impl-path)
        (File/WriteAllText decl-path decl-src)
        (File/WriteAllText impl-path impl-src)))))

(defn write-module-source [{:keys [packages cpp-mode src-output-path] :as module}]
  (doseq [[_ pkg] @packages]
    (write-package-source pkg :cpp-mode cpp-mode :src-output-path src-output-path)))

(defmacro cdef [& forms] (apply cdef* forms))

;; ;;; Test code
(csource-module TestModule :dev true)

(cpackage TestModule test1)

;; (cdefn ^void t1 [^i32 x]
;;        (+ x 1))
