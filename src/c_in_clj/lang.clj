(ns c-in-clj.lang
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [c-in-clj.platform :as platform]))

(defprotocol IHasType
  (get-type [this]))

(defmulti expr-category class)

(defmethod expr-category :default [_] nil)

(defprotocol IExpression
  (write-expr [this])
  (wrap-last [this func]))

(defprotocol IType)

(defmulti write-type class)

(defmethod write-type ::Type [{:keys [type-name]}] type-name)

(defmulti create-explicit-cast-expr (fn [this expr] (class this)))

(defrecord DefaultCastExpression [target-type expr]
  IHasType
  (get-type [_] target-type)
  IExpression
  (write-expr [_] (str "((" (write-type target-type) ")" (write-expr expr) ")")))

(defmethod create-explicit-cast-expr :default [this expr]
  (DefaultCastExpression. this expr))

(defmulti is-reference-type? class)

(defmethod is-reference-type? ::Type [_] false)

(defmulti is-function-type? class)

(defmethod is-function-type? ::Type [_] false)

(defmulti get-fields class)

(defmethod get-fields ::Type [_])

(defmulti write-decl-expr (fn [this & args] (class this)))

(defmethod write-decl-expr ::Type
  ([{:keys [type-name]} var-name] (str type-name " " var-name))
  ([{:keys [type-name]} var-name pointer-depth]
     (str type-name (apply str (repeat pointer-depth "*")) " " var-name)))

(defmulti expand-list-args (fn [this args & opts] (class this)))

(defmulti common-denominator-type (fn [this & args] (class this)))

(defmethod common-denominator-type ::Type [_ other-type])

(defmulti create-field-access-expr (fn [this & args] (class this)))

(defmulti dereferenced-type class)

(defprotocol IDeclaration
  (write-decl [this])
  (write-impl [this]))

(defrecord CompileSource [source body-source filename])

(defprotocol ISymbolScope
  (resolve-symbol [this symbol-name]))

(defprotocol ITypeScope
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

(derive Module ::Module)

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

(defn- look-in-referenced-packages [referenced-packages target-sym]
  (loop [[rp & more] (vec @referenced-packages)]
    (when rp
      (if-let [decl (get @(:symbols rp) target-sym)]
        (if-not (:private (meta decl))
          decl
          (recur more))
        (recur more)))))

(defrecord Package [package-name module declarations symbols referenced-packages]
  clojure.lang.Named
  (getName [_] package-name)
  IPackage
  (add-declaration [this decl]
    (when (satisfies? IDeclaration decl)
      (let [decl-name (name decl)
            existing-idx (find-index-of @declarations #(= (name %) decl-name))]
        (if existing-idx
          (swap! declarations assoc existing-idx decl)
          (swap! declarations conj decl))))
    (when-let [decl-name (name decl)]
      (swap! symbols assoc decl-name decl)))
  ISymbolScope
  (resolve-symbol [_ sym-name]
    (or (get @symbols sym-name)
        (look-in-referenced-packages referenced-packages
                                     sym-name)
        (resolve-symbol module sym-name))))

(defmethod print-method Package [o w]
  (print-simple (str "#" o (select-keys o [:package-name :module])) w))

(defprotocol ILoadContext
  (load-symbol [this package-name symbol-info]))

(def null-loader-context
  (reify ILoadContext
    (load-symbol [this package-name symbol-name])))

(defrecord RuntimeModule [module-name loader packages])

(derive RuntimeModule ::RuntimeModule)

(defmethod print-method RuntimeModule [o w]
  (print-simple (str "#" o (select-keys o [:module-name :loader])) w))

(defrecord RuntimePackage [package-name module symbols referenced-packages]
  clojure.lang.Named
  (getName [_] package-name)
  IPackage
  (add-declaration [this decl]
    (when-let [decl-name (name decl)]
      (swap! symbols assoc decl-name decl)))
  ISymbolScope
  (resolve-symbol [_ sym-name]
    (or (get @symbols sym-name)
        (look-in-referenced-packages referenced-packages
                                     sym-name)
        (resolve-symbol module sym-name))))

(defmethod print-method RuntimePackage [o w]
  (print-simple (str "#" o (select-keys o [:package-name :module])) w))

(def ^:private packages-by-ns (atom {}))

(defn add-package [module package]
  (swap! (:packages module) assoc (name package) package)
  (swap! packages-by-ns assoc *ns* package))

(def ^:dynamic *c-in-clj-dev-mode* false)

(defn dev-env? []
  (or *c-in-clj-dev-mode* (when-let [dev-var (Environment/GetEnvironmentVariable "C_IN_CLJ_DEV")]
                        (not= "0" dev-var))))

(def default-c-preamble
  "#include <stddef.h>\n#include <stdint.h>\n#include <stdbool.h>\n")

(def default-cpp-preamble
  "#include <cstddef>\n#include <cstdint>\n")

(defn create-module [module-name init-compile-ctxt-fn init-load-ctxt-fn {:keys [dev] :as opts}]
  (if (if (not (nil? dev)) dev (dev-env?))
    (let [opts
          (merge
           {:temp-output-path (platform/path-combine (platform/get-temp-path) "c-in-clj")
            :cpp-mode false}
           opts)
          opts (merge {:preamble (if (:cpp-mode opts)
                                  default-cpp-preamble
                                  default-c-preamble)}
                      opts)
          {:keys [src-output-path temp-output-path cpp-mode preamble]} opts]
      (platform/ensure-directory temp-output-path)
      (when src-output-path
        (platform/ensure-directory src-output-path))
      (Module. module-name (init-compile-ctxt-fn opts)
               src-output-path temp-output-path
               preamble cpp-mode
               (atom {})))
    (RuntimeModule. module-name (init-load-ctxt-fn opts) (atom {}))))

(def ^:dynamic *package* nil)

(defn get-package [] (or *package* (get @packages-by-ns *ns*)))

(defn get-module [] (:module (get-package)))

(defn- apply-hook [hook-name expr]
  (let [compile-ctxt (:compile-ctxt (get-module))]
    (write-hook compile-ctxt hook-name expr)))

(defmacro defhooks [name]
  `(let [hook-map# (atom {})]
    (defmacro ~name
      {:hook-map hook-map#}
      [hook-name# args# & body#]
      `(swap! hook-map# assoc
             hook-name#
             (fn ~(symbol (name hook-name#)) ~args#
               ~@body#)))))

(defmacro defhooks [name]
  `(def ~name (atom {})))

(defn add-hook [hooks hook-name hook-func]
  (swap! hooks assoc (keyword (name hook-name)) hook-func)
  nil)

(defmacro defhook [hooks hook-name args & body]
  `(do
     (swap! ~hooks assoc
            ~(keyword (name hook-name))
            (fn ~(symbol (name hook-name)) ~args ~@body))
     nil))

(defn dispatch-hook
  "Dispatches a hook to a hook map defined with defhooks.
Usage: (dispatch-hook hooks)."
  [hooks hook-name ctxt expr]
  (when-let [hook-impl (get @hooks hook-name)]
    (hook-impl ctxt expr)))

(defn dev-mode?
  ([]
     (if-let [pkg (get-package)]
       (dev-mode? (get-module))
       (dev-env?)))
  ([module] (instance? Module module)))

(def ^:private ^:dynamic *locals* nil)

(def ^:private ^:dynamic *local-decls* nil)

(def ^:private ^:dynamic *referenced-decls* nil)

(def ^:dynamic *dynamic-compile* false)

(def ^:private primitive-types (atom {}))

(def ^:private type-aliases (atom {}))

(def ^:private symbol-aliases (atom {}))

(def ^:private save-id (atom 0))

(def ^:private ^:dynamic *dynamic-compile-header* nil)

(defn- add-referenced-decl [resolved]
  ;; (println "trying to add ref to" (name resolved)
  ;;          (type resolved)
  ;;          (satisfies? IDeclaration resolved)
  ;;          *referenced-decls*)
  (when (and (satisfies? IDeclaration resolved) *referenced-decls*)
    (when-not (contains? *referenced-decls* resolved)
      (set! *referenced-decls*
            (conj *referenced-decls* resolved))
      (when *dynamic-compile-header*
        (when-let [decl-text (write-decl resolved)]
          (set! *dynamic-compile-header*
                (str *dynamic-compile-header* "\n\n" decl-text)))))))

(defrecord PrimitiveType [type-name]
  clojure.lang.Named
  (getName [_] type-name)
  IType)

(derive PrimitiveType ::Type)

(defmulti default-initializer class)

(defmethod default-initializer :default [_])

(defmulti requires-initialization class)

(defmethod requires-initialization :default [_] false)

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

(declare lookup-type)

;; PointerType

(defrecord PointerType [type-name]
  clojure.lang.Named
  (getName [_] (str (name type-name) "*"))
  IType)

(derive PointerType ::Type)

(defmethod write-decl-expr PointerType
  ([{:keys [type-name]} var-name]
     (write-decl-expr (lookup-type type-name) var-name 1))
  ([{:keys [type-name]} var-name pointer-depth]
     (write-decl-expr (lookup-type type-name) var-name (inc pointer-depth))))

(defmethod is-function-type? PointerType [{:keys [type-name]}]
  (is-function-type? (lookup-type type-name)))

(defmethod is-reference-type? PointerType [_] true)

(defmethod write-type PointerType
  [{:keys [type-name]}]
  (str (write-type (lookup-type type-name)) "*"))

(defmethod create-field-access-expr PointerType
  ([this instance-expr member-name]
     (create-field-access-expr this instance-expr member-name 1))
  ([{:keys [type-name]} instance-expr member-name pointer-depth]
     (create-field-access-expr (lookup-type type-name) instance-expr member-name pointer-depth)))

(defrecord AnonymousFieldAccessExpression [instance-expr member-name pointer-depth]
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_]
    (let [prefix-pointer (when (>= pointer-depth 2)
                           (- pointer-depth 2))
          pointer-depth (if prefix-pointer 1
                            pointer-depth)]
      (str
       (when prefix-pointer
         (apply str "(" (repeat prefix-pointer "*")))
       (write-expr instance-expr)
       (when prefix-pointer ")")
       (case pointer-depth
         0 "."
         1 "->")
       (name member-name)))))

;; AnonymousType 

(defrecord AnonymousType [type-name]
  clojure.lang.Named
  (getName [_] type-name)
  IType)

(defmethod create-field-access-expr AnonymousType
  ([this instance-expr member-name]
    (create-field-access-expr this instance-expr member-name 0))
  ([this instance-expr member-name pointer-depth]
    (AnonymousFieldAccessExpression. instance-expr member-name pointer-depth)))

(derive AnonymousType ::Type)

;; StaticArrayType

(defrecord StaticArrayType [element-type-name array-length]
  clojure.lang.Named
  (getName [_] (str element-type-name "[" array-length "]"))
  IType)

(derive StaticArrayType ::Type)

(defmethod write-decl-expr StaticArrayType
  [{:keys [element-type-name array-length]} var-name]
  (str (write-decl-expr (lookup-type element-type-name) var-name)
       "[" array-length "]"))

(defmethod is-reference-type? StaticArrayType [_] true)

(defmethod write-type StaticArrayType
  [{:keys [element-type-name array-length]}]
  (str (write-type (lookup-type element-type-name)) "[" array-length "]"))

(defmethod dereferenced-type StaticArrayType
  [{:keys [element-type-name]}]
  (lookup-type element-type-name))

(declare lookup-symbol)

(declare lookup-type)

(defn- static-array-type? [type-name]
  (when-let [[_ type-name array-len] (re-matches #"(.*)!([0-9]*)" type-name)]
    (let [underlying-type (lookup-type type-name)
          array-len (when-not (empty? array-len)
                      (int array-len))]
      (if array-len
        (StaticArrayType. underlying-type array-len)
        (PointerType. underlying-type)))))

(defn lookup-type [type-name]
  (let [resolved-type
        (cond
         (isa? (type type-name) ::Type) type-name
         (keyword? type-name) (AnonymousType. (name type-name))
         :default
         (let [type-name (name type-name)]
           (or
            (static-array-type? type-name)
            (if-let [primitive (get @primitive-types type-name)]
              primitive
              (if-let [alias (get @type-aliases type-name)]
                (lookup-type alias)
                (if (= (last type-name) \*)
                  (PointerType. (lookup-type (subs type-name 0 (dec (count type-name)))))
                  (when-let [typ (resolve-symbol (get-package) type-name)]
                    (when (isa? (type typ) ::Type) typ))))))))]
    (add-referenced-decl resolved-type)
    resolved-type))

(declare cexpand)

(defmacro defliteral [name ctype]
  (let [ctype (lookup-type ctype)]
    `(do
       (defrecord ~name [value#]
         IExpression
         (write-expr [_] (pr-str value#))
         IHasType
         (get-type [_] ~ctype))
       (derive ~name :c-in-clj.core/Literal))))

(defmethod expr-category ::Literal [_] :literal)

(defliteral Int32Literal int32_t)
(defliteral Int64Literal int64_t)
(defliteral DoubleLiteral double)
(defliteral BooleanLiteral bool)
(defliteral StringLiteral char*)

(defrecord CharLiteral [value]
       IExpression
       (write-expr [_] (str "'" value "'"))
       IHasType
       (get-type [_] "char"))

(derive CharLiteral ::Literal)

(defrecord NullLiteral []
  IExpression
  (write-expr [this] "NULL")
  IHasType
  (get-type [this]))

(derive NullLiteral ::Literal)

(def null-literal (NullLiteral.))

(defrecord FunctionParameter [param-name param-type]
  clojure.lang.Named
  (getName [_] param-name)
  IHasType
  (get-type [_] (lookup-type param-type))
  IExpression
  (write-expr [_] (write-decl-expr (lookup-type param-type) param-name)))

(defmethod expr-category FunctionParameter [_] :local)

(defn- write-function-type [{:keys [return-type params]}
                            pointer-depth name?]
  (str (write-type (lookup-type return-type)) " ("
       (apply str (repeat pointer-depth "*"))
       name? ")("
       (str/join ", " (map write-expr params)) ")"))

(defrecord FunctionType [type-name return-type params]
  clojure.lang.Named
  (getName [_] type-name)
  IType)

(derive FunctionType ::Type)

(defmethod is-function-type? FunctionType [_] true)

(defmethod write-decl-expr FunctionType
  ([this var-name]
    (write-function-type this 0 var-name))
  ([this var-name pointer-depth]
    (write-function-type this pointer-depth var-name)))

(defmethod write-type FunctionType
  [this]
  (write-function-type this 0 nil))

(defn- write-function-signature [{:keys [function-name function-type] :as decl} ]
  (let [{:keys [return-type params]} function-type]
    (str
     (apply-hook :before-function-signature decl)
     (write-type (lookup-type return-type)) " "
     function-name "(" (str/join ", " (map write-expr params)) ")")))

(defrecord FunctionDeclaration [package function-name function-type body referenced-decls locals]
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
         (binding [*locals* locals]
           (write-expr body)))))

(defrecord FunctionCallExpression [func args]
  IExpression
  (write-expr [this]
    (str (name (lookup-symbol func))
         "(" (str/join "," (map write-expr args)) ")"))
  IHasType
  (get-type [this] (:return-type (get-type (lookup-symbol func)))))

(defmethod expand-list-args FunctionDeclaration
  [func args]
  (FunctionCallExpression. (name func) (map cexpand args)))

(defmethod print-method FunctionDeclaration [o w]
  (print-simple
   (str "#" o (into {} (update-in o [:referenced-decls] #(map name %)))) w))

(defrecord AnonymousFunctionCallExpression [func-name args]
  IExpression
  (write-expr [this]
    (str (name func-name)
         "(" (str/join "," (map write-expr args)) ")"))
  IHasType
  (get-type [this]))

(defrecord ComputedFunctionCallExpression [func-expr args]
  IExpression
  (write-expr [this]
    (str (write-expr func-expr)
         "(" (str/join "," (map write-expr args)) ")"))
  IHasType
  (get-type [this] (get-type func-expr)))

(defrecord VariableDeclaration [var-name var-type]
  clojure.lang.Named
  (getName [_] var-name)
  IHasType
  (get-type [_] (lookup-type var-type))
  IExpression
  (write-expr [_]
    (let [var-type (lookup-type var-type)]
      (str (write-decl-expr var-type var-name)
           (when-let [init (requires-initialization var-type)]
             (str " = " (default-initializer var-type)))))))

(defn- create-var-decl
  ([var-name var-type]
     (VariableDeclaration. var-name var-type)))

(defrecord VariableRefExpression [variable]
  IExpression
  (write-expr [_] (name (lookup-symbol variable)))
  IHasType
  (get-type [_] (get-type (lookup-symbol variable))))

(defrecord AnonymousVariableRefExpression [var-name]
  IExpression
  (write-expr [_] (name var-name))
  IHasType
  (get-type [_]))

(defn- cexpand-num [x]
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
              local
              (resolve-symbol (get-package) sym-name))))]
    (add-referenced-decl resolved-symbol)
    resolved-symbol))

(defrecord CMacro [name func]
  clojure.lang.Named
  (getName [_] name))

(defmethod expand-list-args CMacro
  [{:keys [func]} args]
  (apply func args))

(def ^:private cintrinsics (atom {}))

(defmacro csymbol-alias [symbol-alias symbol-name]
  `(swap! symbol-aliases
          assoc
          '~symbol-alias
          '~symbol-name))

(defrecord RawCMacro [package macro-name body]
  clojure.lang.Named
  (getName [_] macro-name)
  IDeclaration
  (write-decl [_] (str "#define " macro-name " " body "\n"))
  (write-impl [_]))

(defmethod expand-list-args RawCMacro
  [{:keys [macro-name]} args]
  (AnonymousFunctionCallExpression. macro-name (map cexpand args)))

;; List Symbol Expansion

(declare cexpand-op-sym)

;; TODO remove alias, macro distinction

(defn- op-sym-alias? [sym args]
  (when-let [alias (@symbol-aliases sym)]
    (cexpand-op-sym alias args)))

(defn ^:dynamic lookup-intrinsic [sym]
  (@cintrinsics sym))

(defn- op-sym-intrinsic? [sym args]
  (when-let [intrinsic (lookup-intrinsic sym)]
    (apply intrinsic args)))

(defn- op-sym-member-access? [sym args]
  (when-let [[_ member] (re-matches #"\.(.*)" sym)]
    (cexpand-op-sym
     '.
     (apply vector
            (first args)
            member
            (rest args)))))

(defn- op-sym-local-func? [sym args]
  (when-let [local-func (get *locals* sym)]
    (AnonymousFunctionCallExpression. (name sym) (map cexpand args))))

(defn- op-sym-defined-symbol? [sym args]
  (when-let [resolved (lookup-symbol sym)]
    (cexpand (expand-list-args resolved args))))

(defn- cexpand-op-sym [sym args]
  (let [sym-name (name sym)]
    (or
     (op-sym-alias? sym args)
     (op-sym-intrinsic? sym args)
     (op-sym-member-access? sym-name args)
     (op-sym-local-func? sym args)
     (op-sym-defined-symbol? sym-name args)
     (throw (ArgumentException. (str "Don't know how to handle list symbol " sym))))))

(defn- cexpand-list [[op & args]]
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
  (write-expr [_] (str "{" (str/join ", " (map write-expr values)) "}")))

(defn- cexpand-vector [values]
  (InitializerList. (map cexpand values)))

(defn- boolean? [x] (or (= x true) (= x false)))

(defn ^:dynamic cexpand [form]
  (cond
   (nil? form) null-literal
   (char? form) (CharLiteral. form)
   (number? form) (cexpand-num form)
   (boolean? form) (BooleanLiteral. form)
   (string? form) (StringLiteral. form)
   (symbol? form) (when-let [resolved (lookup-symbol form)]
                    (if (instance? RawCMacro resolved)
                      (AnonymousVariableRefExpression. (name form))
                      (VariableRefExpression. form)))
   (keyword? form) (AnonymousVariableRefExpression. (name form))
   (list? form) (cexpand-list form)
   (vector? form) (cexpand-vector form)
   (satisfies? IExpression form) form
   :default (throw (ArgumentException. (str "Don't know how to handle " form " of type " (type form))))))

(defn- is-block? [expr]
  (let [cat (expr-category expr)]
    (or (= :statement* cat) (= :block cat))))

(defn- cintrinsic* [sym func]
  (swap! cintrinsics assoc sym func))

(defmacro ^:private cintrinsic [sym args & body]
  `(cintrinsic* '~sym
               (fn ~sym ~args
                 (let [~@ (doall (reduce (fn [res x] (into res [x `(cexpand ~x)])) [] args))]
                   ~@body))))

(defn- get-expr-record-sym [sym]
  (symbol (clojure.lang.Compiler/munge
           (str (name sym) "Expression"))))

(defmacro cop [sym args & body]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
      (defrecord ~rec-sym ~args
        IExpression
        ~@body)
      (cintrinsic ~sym ~args
                  (new ~rec-sym ~@args)))))

(defn- get-bin-op-type [& args]
  (let [types (map get-type args)]
    (when (apply = types)
      (first types))))

(defmacro cbinop [sym]
  `(cop ~sym [x# y#]
        (write-expr [_] (str "(" (write-expr x#) " " ~(name sym) " " (write-expr y#) ")"))
        IHasType
        (get-type [_] (get-bin-op-type x# y#))))

(defmacro cbinop* [sym expr]
  `(cop ~sym [x# y#]
        (write-expr [_] (str "(" (write-expr x#) " " ~expr " " (write-expr y#) ")"))
        IHasType
        (get-type [_] (get-bin-op-type x# y#))))

(defmacro cbinops [& syms]
  `(do ~@(for [x syms] `(cbinop ~x))))

(defmacro compop [sym]
  `(cop ~sym [x# y#]
        (write-expr [this#] (str "(" (write-expr x#) " " ~(name sym) " " (write-expr y#) ")"))
        IHasType
        (get-type [this#] 'bool)))

(defmacro compops [& syms]
  `(do ~@(for [x syms] `(compop ~x))))

(defmacro compop* [sym expr]
  `(cop ~sym [x# y#]
        (write-expr [this#] (str "(" (write-expr x#) " " ~expr " " (write-expr y#) ")"))
        IHasType
        (get-type [this#] 'bool)))

(defn- reduce-parens [^String expr]
  (when expr
    (if (= (first expr) \()
      (let [len (count expr)
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
          (subs expr 1 (- len 1))
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
         (write-expr [this#] (str "(" (write-expr x#) " " ~expr " " (reduce-parens (write-expr y#)) ")")))
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
         (write-expr [_]
           (if (= 1 (count ~'args))
             (str ~(name sym) (write-expr (first ~'args)))
             (str "(" (str/join ~(str " " sym " ") (map write-expr ~'args)) ")")))
         IHasType
         (get-type [_] (apply get-bin-op-type ~'args)))
       (cintrinsic* '~sym
                    (fn [& args#]
                      (new ~rec-sym (doall (map cexpand args#))))))))

(defmacro comp*op* [sym expr]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
       (defrecord ~rec-sym [~'args]
         IExpression
         (write-expr [_]
           (str "(" (str/join ~(str " " expr " ") (map write-expr ~'args)) ")"))
         IHasType
         (get-type [_] (lookup-type 'bool)))
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
(comp*op* or "||")
(comp*op* and "&&")
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
(cassignop bit-not= "~=")
(cassignop set! "=")

(defmacro cunop [name arg & body]
  `(cop ~name ~arg
        (write-expr [this#] ~@body)
        IHasType
        (get-type [this#] (get-type ~(first arg)))))

(defn- write-str [& args]
  (apply str (for [arg args]
               (if (string? arg)
                 arg
                 (write-expr arg)))))

(cunop inc [x] (write-str "++" x))
(cunop dec [x] (write-str "--" x))
(cunop post-inc [x] (write-str x "++"))
(cunop post-dec [x] (write-str x "--"))
(cunop bit-not [x] (write-str "~" x))

(cop not [x]
     (write-expr [_] (str "!" (write-expr x)))
     IHasType
     (get-type [_] (lookup-type 'bool)))

(defrecord SizeofExpression [x]
  IExpression
  (write-expr [_] (let [type (lookup-type x)]
               (str "sizeof(" (write-type type) ")")))
  IHasType
  (get-type [_] (lookup-type 'size_t)))

(cintrinsic*
 'sizeof (fn [x]
           (SizeofExpression. x)))

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
  (write-expr [_]
    (let [prefix-pointer (when (>= pointer-depth 2)
                           (- pointer-depth 2))
          pointer-depth (if prefix-pointer 1
                            pointer-depth)]
      (str
       (when prefix-pointer
         (apply str "(" (repeat prefix-pointer "*")))
       (write-expr instance-expr)
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
                     (ComputedFunctionCallExpression. access-expr args))
                   access-expr))))

(cintrinsic* '->
            (fn [& args]
              (let [args (map cexpand args)]
                (reify
                  IHasType
                  (get-type [_])
                  IExpression
                  (write-expr [_]
                    (str/join "->" (map write-expr args)))))))

(cop aget [x y]
     (write-expr [_] (write-str x "[" y "]"))
     IHasType
     (get-type [_]
               (lookup-type (dereferenced-type (get-type x)))))

(defrecord ArraySetExpression [target idx value]
  IHasType
  (get-type [_] (get-type target))
  IExpression
  (write-expr [_] (write-str target "[" idx "] = " value)))

(cintrinsic aset [target idx value]
            (if (is-block? value)
              (wrap-last value (fn [x] (ArraySetExpression. target idx x)))
              (ArraySetExpression. target idx value)))

(cop ref [x]
     (write-expr [_] (write-str "(&" x ")"))
     IHasType
     (get-type [_]))

(cop deref [x]
     (write-expr [_] (write-str "*" x))
     IHasType
     (get-type [_]))

(csymbol-alias 'clojure.core/deref deref)

(defrecord CVerbatim [args]
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_]
    (apply write-str args)))

(cintrinsic* 'c*
            (fn [& args]
              (CVerbatim.
               (map (fn [x]
                      (if (string? x) x (cexpand x)))
                    args))))

(def ^:dynamic *indent* 0)

(defn- indent [] (str/join (for [x (range *indent*)] "\t")))

(defrecord Statement [expr noindent]
  IExpression
  (wrap-last [_ func]
    (Statement. (func expr) noindent))
  (write-expr [_]
    (str (when-not noindent (indent)) (reduce-parens (write-expr expr)) ";"))
  IHasType
  (get-type [_] (get-type expr)))

(defmethod expr-category Statement [_] :statement)

(defn- cstatement [expr & {:keys [noindent]}]
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
  (write-expr [_] (str/join "\n" (map write-expr statements)))
  (wrap-last [_ func]
    (Statements.
     (wrap-statements func statements)))
  IHasType
  (get-type [_] (get-type (last statements))))

(defmethod expr-category Statements [_] :statement*)

(defn- cstatements [statements]
  (Statements. (doall (map cstatement (remove nil? statements)))))

(defrecord CaseExpression [test cases]
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_]
    (let [cases
          (binding [*indent* (inc *indent*)]
            (for [[expr block] cases]
              (if block
                (let [block (binding [*indent* (inc *indent*)]
                              (write-expr block))]
                  (str (indent) "case " (write-expr expr) ":\n" block "\n" (indent) "break;\n"))
                (str (indent) "default:" (write-expr expr) "\n" (indent) "break;\n"))))]
      (str "switch(" (write-expr test) ") {\n" (str/join "\n" cases) (indent) "\n}")))
  (wrap-last [_ func]
    (CaseExpression.
     test
     (for [[expr block] cases]
       (if block
         [expr (wrap-last block func)]
         [(wrap-last expr func)])))))

(defmethod expr-category CaseExpression [_] :statement)

(cintrinsic*
 'case
 (fn [test & args]
   (let [test (cexpand test)
         cases (partition 2 args)
         has-default (odd? (count args))
         cases
         (vec
          (for [[expr block] cases]
            [(cexpand expr)
             (cstatement block)]))
         cases (if has-default
                 (conj cases [(cstatement (last args))])
                 cases)]
     (CaseExpression. test cases))))

(defrecord ReturnExpression [expr]
  IHasType
  (get-type [_] (get-type expr))
  IExpression
  (write-expr [_]
    (if expr
      (if-let [expr (write-expr expr)]
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

(declare cblock)

(defrecord IfExpression [expr then else]
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_]
    (str (indent)
         "if(" (reduce-parens (write-expr expr)) ")\n"
         (write-expr then)
         (when else
           (str "\n" (indent) "else\n"
                (write-expr else)))))
  (wrap-last [_ func]
    (IfExpression.
     expr
     (wrap-last then func)
     (when else (wrap-last else func)))))

(defmethod expr-category IfExpression [_] :statement*)

(cintrinsic* 'if
             (fn
               ([expr then]
                  (IfExpression. (cexpand expr)
                                 (cblock then)
                                 nil))
              ([expr then else]
                 (IfExpression. (cexpand expr)
                                (cblock then)
                                (cblock else)))))

(defrecord DeclExpression [var-type var-name init-expr]
  IHasType
  (get-type [_] var-type)
  IExpression
  (write-expr [_] (str (write-decl-expr var-type var-name) "=" (when init-expr (write-expr init-expr)))))

(defrecord BlockExpression [statements]
  IHasType
  (get-type [_] (get-type (last statements)))
  IExpression
  (wrap-last [_ func]
    (BlockExpression.
     (wrap-statements func statements)))
  (write-expr [_]
    (str (indent) "{\n"
       (binding [*indent* (inc *indent*)]
         (str/join "\n" (map write-expr statements)))
       "\n" (indent) "}")))

(defmethod expr-category BlockExpression [_] :block)

(defn- cblock [& statements]
  (BlockExpression. (doall (map cstatement (remove nil? statements)))))

(cintrinsic* 'do cblock)

(defrecord ForStatement [init-expr test-expr each-expr body]
    IHasType
    (get-type [_])
    IExpression
    (write-expr [_]
      (str (indent) "for("
           (reduce-parens (write-expr init-expr)) "; "
           (reduce-parens (write-expr test-expr)) "; "
           (reduce-parens (write-expr each-expr)) ")\n"
           (write-expr body)))
    (wrap-last [_ func] (throw (Exception. "Cannot take value of for statement"))))

(defmethod expr-category ForStatement [_] :statement)

(defrecord CommaExpression [expressions]
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_] (str/join ", " (map write-expr expressions))))

(defmethod expr-category CommaExpression [_] :statement)

(defrecord NopExpression []
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_]))

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
               (let [body (apply cblock body)]
                 (ForStatement.
                  (wrap-for-expressions init)
                  (wrap-for-expressions test)
                  (wrap-for-expressions each)
                  body))))

(defrecord WhileStatement [test-expr body]
    IHasType
    (get-type [_])
    IExpression
    (write-expr [_]
      (str (indent) "while(" (reduce-parens (write-expr test-expr)) ")\n"
           (write-expr body)))
    (wrap-last [_ func] (throw (Exception. "Cannot take value of while statement"))))

(defmethod expr-category WhileStatement [_] :statement)

(cintrinsic* 'while
             (fn [test & body]
               (let [body (apply cblock body)]
                 (WhileStatement.
                  (cexpand test)
                  body))))


(defrecord BreakStatement []
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_] "break"))

(cintrinsic break [] (BreakStatement.))

(defrecord ContinueStatement []
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_] "continue"))

(cintrinsic continue [] (ContinueStatement.))

(defrecord LabelStatement [label]
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_] (str (name label) ":")))

(defmethod expr-category LabelStatement [_] :statement)

(cintrinsic* 'label (fn [x] (LabelStatement. x)))

(defrecord GotoExpression [label]
  IHasType
  (get-type [_])
  IExpression
  (write-expr [_] (str "goto " (name label))))

(cintrinsic* 'goto (fn [x] (GotoExpression. x)))

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
     (cstatements
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

(defn- get-var-type-tag [metadata]
  (let [tag (:tag metadata)]
    (if (string? tag) (keyword tag) tag)))

(defn- declare-fn [sym]
  (if-let [decl-type (get-var-type-tag (meta sym))]
    (do
      (add-local
       (create-var-decl (name sym) decl-type))
      (NopExpression.))
     (throw (ArgumentException.
             (str "Unable to infer type for declare expression of symbol" sym)))))

(cintrinsic* 'declare declare-fn)

(def ^:private set!-fn (get @cintrinsics 'set!))

(cintrinsic*
 'def
 (fn def-fn [sym init-expr]
   (declare-fn sym)
   (set!-fn sym init-expr)))

;; (defn create-cfn-body [name args body]
;;   (binding [*locals* (extract-locals args)]
;;     (let [sig-txt (cfnsig name ret args)
;;           body-txt (cblock body)
;;           fn-txt (str sig-txt "\n" body-txt "\n")]
;;       fn-txt)))

;; (defmacro cdefn [name ret args & body]
;;   (compile-cfn name ret args body))

(defrecord StructField [name field-type bits]
  clojure.lang.Named
  (getName [_] name)
  IHasType
  (get-type [_] (lookup-type field-type)))

(defn write-struct-field [field]
  (str "\t"
       (write-decl-expr (get-type field)
                        (name field))
       (when-let [bits (:bits field)]
         (str ":" bits)) ";\n"))

(defrecord Struct [package type-name fields field-map]
  clojure.lang.Named
  (getName [_] type-name)
  IHasType
  (get-type [this] this)
  IType
  IDeclaration
  (write-decl [_]
    (str "typedef struct " type-name " {\n"
       (str/join
        (map write-struct-field fields))
       "} " type-name ";"))
  (write-impl [_]))

(derive Struct ::Type)
(derive Struct ::Struct)

(defmethod get-fields ::Struct [{:keys [field-map]}] field-map)

(defmethod create-field-access-expr ::Struct
  create-field-access-expr-struct
  ([this instance-expr field-name]
     (create-field-access-expr this instance-expr field-name 0))
  ([{:keys [field-map]} instance-expr field-name pointer-depth]
   (when-let [field (get field-map (name field-name))]
     (StructFieldAccessExpression. instance-expr field pointer-depth)))) 

(cintrinsic*
 'cast
 (fn [target-type expr]
   (let [target-type (lookup-type target-type)
         expr (cexpand expr)]
     (create-explicit-cast-expr target-type expr))))

;; VarArgsType

(defrecord VarArgsType []
  clojure.lang.Named
  (getName [_] "...")
  IType)

(derive VarArgsType ::Type)

(defmethod write-decl-expr VarArgsType
  [_ var-name] "...")

(defmethod write-type VarArgsType[_])

(defn parse-fn-params
  ([params] (parse-fn-params params nil))
  ([params fn-name]
     (let [fn-name-ret-type (get-var-type-tag (meta fn-name))
           params-meta (meta params)
           ret-type (or (get-var-type-tag params-meta)
                        fn-name-ret-type)
            params
            (for [param params]
              (let [metadata (meta param)
                    param-name (name param)]
                (if (= param-name "...")
                  (FunctionParameter. param-name (VarArgsType.) metadata nil)
                  (let [param-type (get-var-type-tag metadata)]
                    (FunctionParameter. param-name param-type metadata nil)))))]
       (FunctionType. (name fn-name) ret-type params params-meta nil))))

(defn- parse-cfn [[func-name & forms]]
  (let [package (get-package)
        f1 (first forms)
        doc-str (when (string? f1) f1)
        params (if doc-str (second forms) f1)
        body-forms (if doc-str (nnext forms) (next forms))]
    (binding [*referenced-decls* #{}]
      (let [func-type (parse-fn-params params func-name)
            params (:params func-type)]
        (binding [*locals* (into {} (for [param params] [(name param) param]))
                  *local-decls* []]
          (let [func-metadata (meta func-name)
                ret-type (:return-type func-type)
                has-return (not (= "void" (name ret-type)))
                body-forms (or body-forms (when has-return [(ReturnExpression. nil)]))
                body-statements (vec (remove nil? (doall (map cstatement body-forms)))) 
                local-decls (vec (doall (map cstatement *local-decls*)))
                body-statements (concat local-decls body-statements)
                body-block (if (and (= 1 (count body-statements))
                                    (instance? BlockExpression (first body-statements)))
                             (first body-statements)
                             (BlockExpression. body-statements))
                body-block
                (if (and has-return (not (:disable-default-return func-metadata)))
                  (wrap-last
                   body-block
                   (fn [expr]
                     (if (instance? ReturnExpression expr)
                       expr
                       (ReturnExpression. expr))))
                  body-block)
                func-metadata (if doc-str
                                (assoc func-metadata :doc doc-str)
                                func-metadata)
                func-name (name func-name)
                func-decl (FunctionDeclaration. package func-name func-type body-block *referenced-decls* *locals* func-metadata nil)]
            (add-declaration package func-decl)
            func-decl))))))

(defn- load-cfn [[func-name params & _]]
  (let [func-metadata (meta func-name)
        fn-type (parse-fn-params params func-name)
        fn-info {:name func-name :func-type fn-type :type :function}
        {:keys [loader]} (get-module)
        func (load-symbol loader (:name (get-package)) fn-info)]
    (intern *ns* (symbol (name func-name)) func)))

;; (defn- write-dev-header [referenced-decls]
;;   (doseq [decl referenced-decls]
;;     (set! *dynamic-compile-header*
;;           (str *dynamic-compile-header* "\n\n"
;;                (write-decl decl))))
;;   *dynamic-compile-header*)

(defrecord IncludeDeclaration [package include-name]
  clojure.lang.Named
  (getName [_] include-name)
  IDeclaration
  (write-decl [_] (str "#include <" include-name ">"))
  (write-impl [_]))

(defn- output-dev-src [package decls cpp-mode preamble temp-output-path]
  (binding [*dynamic-compile* true
            *dynamic-compile-header* ""
            *referenced-decls* #{}]
    (let [anon-header (str/join "\n\n" (doall
                                        (remove
                                         nil?
                                         (for [decl @(:declarations package)]
                                           (when (or (not (name decl))
                                                     (instance? IncludeDeclaration decl))
                                             (write-decl decl))))))
          ;;referenced (apply set/union (map :referenced-decls decls))
          body (str/join "\n\n" (doall (map write-impl decls)))
          ;;header (str/trim (write-dev-header referenced))
          header (str/trim *dynamic-compile-header*)
          src (str/join "\n" [preamble anon-header header "\n" body])
          filename (str/join  "_" (map name decls))
          ;; TODO save id??
          filename (str (name package) "_" filename "_" (swap! save-id inc) (if cpp-mode ".cpp" ".c"))
          filename (platform/path-combine temp-output-path filename)]
      (platform/write-text-file filename src)
      (CompileSource. src body filename))))

(defn print-numbered [txt]
  (let [lines (str/split-lines txt)]
    (doall
     (map-indexed
      (fn [i line] (println (str i "  " line))) lines))))

(defn do-compile-decls [decls parse-fn load-fn]
  (let [{:keys [module] :as package} (get-package)]
    (if (dev-mode? module)
      (let [{:keys [compile-ctxt temp-output-path preamble cpp-mode]} module
            decls (doall (map parse-fn decls))
            {:keys [body-source] :as src}
            (output-dev-src package decls cpp-mode preamble temp-output-path)]
        (when-let [compiled (compile-decls compile-ctxt decls src)]
          (println body-source)
          (doseq [[n v] compiled] (intern *ns* (symbol (name n)) v))))
      (doseq [decl decls]
        (load-fn decl)))))

(defn compile-cfns [funcs]
  (do-compile-decls funcs #'parse-cfn #'load-cfn))

(defn unqualify-symbols [form]
  (if (seq? form)
    (apply list
           (for [f form]
             (unqualify-symbols f)))
    (if (symbol? form)
      (symbol (name form))
      form)))

;; TypeDef's

(defrecord TypeDef [package type-name target-type]
  clojure.lang.Named
  (getName [_] type-name)
  IHasType
  (get-type [_] target-type)
  IDeclaration
  (write-decl [_] (str "typedef " (write-decl-expr target-type type-name) ";"))
  (write-impl [_])
  IType)

(derive TypeDef ::Type)

(defmethod is-reference-type? TypeDef [{:keys [target-type]}] (is-reference-type? target-type))

(defmethod is-function-type? TypeDef [{:keys [target-type]}] (is-function-type? target-type))

(defmethod get-fields TypeDef [{:keys [target-type]}] (get-fields target-type))

(defmethod common-denominator-type TypeDef
  [{:keys [target-type]} other-type]
  (common-denominator-type target-type other-type))

(defmethod create-field-access-expr TypeDef
  [{:keys [target-type]} instance-expr field-name]
  (create-field-access-expr target-type instance-expr field-name))

;; Enums

(defrecord EnumValue [name value base-type enum-type-name]
  clojure.lang.Named
  (getName [_]
    (lookup-type enum-type-name)
    name)
  IHasType
  (get-type [_] base-type)
  IExpression
  (write-expr [_]
    (lookup-type enum-type-name)
    name))

(defrecord EnumType [package type-name values]
  clojure.lang.Named
  (getName [_] type-name)
  IDeclaration
  (write-decl [_] (str "typedef enum " 
                       "{"
                       (str/join
                        ", "
                        (map #(str (:name %) " = " (:value %)) values))
                       "} " type-name ";"))
  (write-impl [_])
  IType)

(derive EnumType ::Type)

;; Includes

(defrecord PackageIncludeDeclaration [package referenced-package]
  clojure.lang.Named
  (getName [_] (str (name referenced-package) ".h"))
  IDeclaration
  (write-decl [_] (str "#include \"" (name referenced-package) ".h\""))
  (write-impl [_]))

(defrecord GlobalVariableDeclaration [package var-name var-type init-expr]
  clojure.lang.Named
  (getName [_] var-name)
  IHasType
  (get-type [_] (lookup-type var-type))
  IDeclaration
  (write-decl [this]
    (let [var-type (lookup-type var-type)]
      (or (apply-hook :alternate-global-variable-declaration this)
          (str (or (apply-hook :before-global-variable-declaration this) "extern ") (write-decl-expr var-type var-name) ";"))))
  (write-impl [this]
    (let [var-type (lookup-type var-type)]
      (str (apply-hook :before-global-variable-declaration this) (write-decl-expr var-type var-name) (when init-expr (str " = " (write-expr init-expr))) ";"))))

(defmethod expand-list-args GlobalVariableDeclaration
  [{:keys [var-name]} args]
  (FunctionCallExpression. var-name (map cexpand args)))

(defn parse-cdef [[var-name init-expr]]
  (let [package (get-package)
        metadata (meta var-name)
        var-name (name var-name)
        tag (get-var-type-tag metadata)
        var-type tag
        init-expr (when init-expr (cexpand init-expr))
        var-decl (GlobalVariableDeclaration. package var-name var-type init-expr metadata nil)]
    (add-declaration package var-decl)
    var-decl))

(defn load-cdef [[var-name & _]])

(defrecord RawCHeader [package txt]
  clojure.lang.Named
  (getName [_])
  IDeclaration
  (write-decl [_] (str txt "\n"))
  (write-impl [_]))

