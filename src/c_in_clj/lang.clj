(ns c-in-clj.lang
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [c-in-clj.platform :as platform]))

(defn- is-an-instance? [x t] (isa? (type x) t))

(defmulti get-name class)
(defmethod get-name :default [x] (when (instance? clojure.lang.Named x) (name x)))

(defn derive-scope [cls] (derive cls ::Scope))
(defn is-scope? [x] (is-an-instance? x ::Scope))
(defmulti scope-add (fn [scope element] (class scope)))
(defmulti scope-lookup (fn [scope sym] (class scope)))
(defmulti scope-get-Allocator class)
(defmulti scope-hook (fn [scope hook-name & args] class))
(def ^:dynamic *scope* nil)
(def ^:private scopes-by-ns (atom {}))
(defn get-scope [])
(defn get-ns-scope [])
(defn set-ns-scope [scope])
(defn add-to-scope [element])
(defn add-to-ns-scope [element])

(defmulti list->expr (fn [this args & opts] (class this)))

(defmulti sym->expr class)

(defn derive-expr [cls])
(defn is-expr? [x])
(defmulti expr-write class)
(defmulti expr-category class)
(defmethod expr-category :default [_] nil)

(defmulti get-type class)
(defmethod get-type :default [_] nil)

(defn derive-type [cls] (derive cls ::Type))

(defn is-type? [x] (is-an-instance? x ::Type))

(defmulti type-common-denominator (fn [this & args] (class this)))
(defmethod type-common-denominator ::Type [_ other-type])

(defmulti type-create-field-access-expr (fn [this & args] (class this)))

(defmulti type-dereferenced-type class)

(defmulti type-write class)
(defmethod type-write ::Type [{:keys [type-name]}] type-name)

(defmulti type-is-reference? class)
(defmethod type-is-reference? ::Type [_] false)

(defmulti type-is-function? class)
(defmethod type-is-function? ::Type [_] false)

(defmulti type-get-fields class)
(defmethod type-get-fields ::Type [_])

(defmulti type-write-decl-expr (fn [this & args] (class this)))
(defmethod type-write-decl-expr ::Type
  ([{:keys [type-name]} var-name] (str type-name " " var-name))
  ([{:keys [type-name]} var-name pointer-depth]
     (str type-name (apply str (repeat pointer-depth "*")) " " var-name)))

(defrecord DefaultCastExpression [target-type expr])
(defmethod get-type DefaultCastExpression [x] (:target-type x))
(defmethod expr-write DefaultCastExpression [{:keys [expr target-type]}]
  (str "((" (type-write target-type) ")" (expr-write expr) ")"))

(defmulti type-create-explicit-cast-Expr (fn [this expr] (class this)))
(defmethod type-create-explicit-cast-Expr :default [this expr]
  (DefaultCastExpression. this expr))

(defmulti type-default-initializer class)
(defmethod type-default-initializer :default [_])

(defmulti type-requires-initialization class)
(defmethod type-requires-initialization :default [_] false)

(defn derive-decl [cls])

(defn is-decl? [x])

(defmulti decl-write class)

(defmulti decl-write-impl class)
(defmethod decl-write-impl :default [_])

(defn derive-allocator [cls] (derive cls ::Allocator))

(defn is-allocator? [x] (is-an-instance? x ::Allocator))

(defmulti allocator-alloc (fn [allocator & args] (class allocator)))

(defmulti allocator-free (fn [allocator & args] (class allocator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IHasType
  (get-type [this]))

(defprotocol IExpression
  (expr-write [this])
  (wrap-last [this func]))

(defn derive-Expr [t]
  (derive t ::Expression))

(defprotocol IType)

(defn derive-Type [t]
  (derive t ::Type))

(defprotocol IDeclaration
  (decl-write [this])
  (decl-write-impl [this]))

(defn derive-Decl [t]
  (derive t ::Declaration))

(defprotocol ISymbolScope
  (resolve-symbol [this symbol-name]))

(defprotocol ITypeScope
  (resolve-type [this type-name]))

(defprotocol IPackage
  (add-declaration [this decl]))

(defprotocol ICompileContext
  (write-hook [this hook-name expr])
  (compile-decls [this decls compile-source]))

(def ^:private packages-by-ns (atom {}))

(defn add-package [module package]
  (swap! (:packages module) assoc (name package) package)
  (swap! packages-by-ns assoc *ns* package))

(def ^:dynamic *package* nil)

(defn get-package [] (or *package* (get @packages-by-ns *ns*)))

(defn get-module [] (:module (get-package)))

(defn- apply-hook [hook-name expr]
  (let [compile-ctxt (:compile-ctxt (get-module))]
    (write-hook compile-ctxt hook-name expr)))

(def ^:dynamic *locals* nil)

(def ^:dynamic *local-decls* nil)

(def ^:dynamic *referenced-decls* nil)

(def ^:private primitive-types (atom {}))

(def ^:private type-aliases (atom {}))

(def ^:private symbol-aliases (atom {}))

(def ^:dynamic *dynamic-compile-header* nil)

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
        (when-let [decl-text (decl-write resolved)]
          (set! *dynamic-compile-header*
                (str *dynamic-compile-header* "\n\n" decl-text)))))))

(defrecord PrimitiveType [type-name]
  clojure.lang.Named
  (getName [_] type-name)
  IType)

(derive PrimitiveType ::Type)

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

(defmethod type-write-decl-expr PointerType
  ([{:keys [type-name]} var-name]
     (type-write-decl-expr (lookup-type type-name) var-name 1))
  ([{:keys [type-name]} var-name pointer-depth]
     (type-write-decl-expr (lookup-type type-name) var-name (inc pointer-depth))))

(defmethod type-is-function? PointerType [{:keys [type-name]}]
  (type-is-function? (lookup-type type-name)))

(defmethod type-is-reference? PointerType [_] true)

(defmethod type-write PointerType
  [{:keys [type-name]}]
  (str (type-write (lookup-type type-name)) "*"))

(defmethod type-create-field-access-expr PointerType
  ([this instance-expr member-name]
     (type-create-field-access-expr this instance-expr member-name 1))
  ([{:keys [type-name]} instance-expr member-name pointer-depth]
     (type-create-field-access-expr (lookup-type type-name) instance-expr member-name pointer-depth)))

(defrecord AnonymousFieldAccessExpression [instance-expr member-name pointer-depth]
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_]
    (let [prefix-pointer (when (>= pointer-depth 2)
                           (- pointer-depth 2))
          pointer-depth (if prefix-pointer 1
                            pointer-depth)]
      (str
       (when prefix-pointer
         (apply str "(" (repeat prefix-pointer "*")))
       (expr-write instance-expr)
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

(defmethod type-create-field-access-expr AnonymousType
  ([this instance-expr member-name]
    (type-create-field-access-expr this instance-expr member-name 0))
  ([this instance-expr member-name pointer-depth]
    (AnonymousFieldAccessExpression. instance-expr member-name pointer-depth)))

(derive AnonymousType ::Type)

;; StaticArrayType

(defrecord StaticArrayType [element-type-name array-length]
  clojure.lang.Named
  (getName [_] (str element-type-name "[" array-length "]"))
  IType)

(derive StaticArrayType ::Type)

(defmethod type-write-decl-expr StaticArrayType
  [{:keys [element-type-name array-length]} var-name]
  (str (type-write-decl-expr (lookup-type element-type-name) var-name)
       "[" array-length "]"))

(defmethod type-is-reference? StaticArrayType [_] true)

(defmethod type-write StaticArrayType
  [{:keys [element-type-name array-length]}]
  (str (type-write (lookup-type element-type-name)) "[" array-length "]"))

(defmethod type-dereferenced-type StaticArrayType
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

(declare ->expr)

(defmacro ^:private defliteral [name ctype]
  (let [ctype (lookup-type ctype)]
    `(do
       (defrecord ~name [value#]
         IExpression
         (expr-write [_] (pr-str value#))
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
       (expr-write [_] (str "'" value "'"))
       IHasType
       (get-type [_] "char"))

(derive CharLiteral ::Literal)

(defrecord NullLiteral []
  IExpression
  (expr-write [this] "NULL")
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
  (expr-write [_] (type-write-decl-expr (lookup-type param-type) param-name)))

(defmethod expr-category FunctionParameter [_] :local)

(defn- write-function-type [{:keys [return-type params]}
                            pointer-depth name?]
  (str (type-write (lookup-type return-type)) " ("
       (apply str (repeat pointer-depth "*"))
       name? ")("
       (str/join ", " (map expr-write params)) ")"))

(defrecord FunctionType [type-name return-type params]
  clojure.lang.Named
  (getName [_] type-name)
  IType)

(derive FunctionType ::Type)

(defmethod type-is-function? FunctionType [_] true)

(defmethod type-write-decl-expr FunctionType
  ([this var-name]
    (write-function-type this 0 var-name))
  ([this var-name pointer-depth]
    (write-function-type this pointer-depth var-name)))

(defmethod type-write FunctionType
  [this]
  (write-function-type this 0 nil))

(defn- write-function-signature [{:keys [function-name function-type] :as decl} ]
  (let [{:keys [return-type params]} function-type]
    (str
     (apply-hook :before-function-signature decl)
     (type-write (lookup-type return-type)) " "
     function-name "(" (str/join ", " (map expr-write params)) ")")))

(defrecord FunctionDeclaration [package function-name function-type body referenced-decls locals]
  clojure.lang.Named
  (getName [this] function-name)
  IHasType
  (get-type [this] function-type)
  IDeclaration
  (decl-write [this]
    (or (apply-hook :alternate-function-declaration this)
        (str
         (when-let [doc (:doc (meta this))]
           (str "/**\n" 
                (apply str (map #(str "* " % "\n") (str/split-lines doc)))
                "*/\n"))
         (write-function-signature this) ";")))
  (decl-write-impl [this]
    (str (write-function-signature this) "\n"
         (binding [*locals* locals]
           (expr-write body)))))

(defrecord FunctionCallExpression [func args]
  IExpression
  (expr-write [this]
    (str (name (lookup-symbol func))
         "(" (str/join "," (map expr-write args)) ")"))
  IHasType
  (get-type [this] (:return-type (get-type (lookup-symbol func)))))

(defmethod list->expr FunctionDeclaration
  [func args]
  (FunctionCallExpression. (name func) (map ->expr args)))

(defmethod print-method FunctionDeclaration [o w]
  (print-simple
   (str "#" o (into {} (update-in o [:referenced-decls] #(map name %)))) w))

(defrecord AnonymousFunctionCallExpression [func-name args]
  IExpression
  (expr-write [this]
    (str (name func-name)
         "(" (str/join "," (map expr-write args)) ")"))
  IHasType
  (get-type [this]))

(defrecord ComputedFunctionCallExpression [func-expr args]
  IExpression
  (expr-write [this]
    (str (expr-write func-expr)
         "(" (str/join "," (map expr-write args)) ")"))
  IHasType
  (get-type [this] (get-type func-expr)))

(defrecord VariableDeclaration [var-name var-type]
  clojure.lang.Named
  (getName [_] var-name)
  IHasType
  (get-type [_] (lookup-type var-type))
  IExpression
  (expr-write [_]
    (let [var-type (lookup-type var-type)]
      (str (type-write-decl-expr var-type var-name)
           (when-let [init (type-requires-initialization var-type)]
             (str " = " (type-default-initializer var-type)))))))

(defn- create-var-decl
  ([var-name var-type]
     (VariableDeclaration. var-name var-type)))

(defrecord VariableRefExpression [variable]
  IExpression
  (expr-write [_] (name (lookup-symbol variable)))
  IHasType
  (get-type [_] (get-type (lookup-symbol variable))))

(defrecord AnonymousVariableRefExpression [var-name]
  IExpression
  (expr-write [_] (name var-name))
  IHasType
  (get-type [_]))

(defn- ->expr-num [x]
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

(defmethod list->expr CMacro
  [{:keys [func]} args]
  (apply func args))

(def ^:private cintrinsics (atom {}))

(defmacro ^:private csymbol-alias [symbol-alias symbol-name]
  `(swap! symbol-aliases
          assoc
          '~symbol-alias
          '~symbol-name))

(defrecord RawCMacro [package macro-name body]
  clojure.lang.Named
  (getName [_] macro-name)
  IDeclaration
  (decl-write [_] (str "#define " macro-name " " body "\n"))
  (decl-write-impl [_]))

(defmethod list->expr RawCMacro
  [{:keys [macro-name]} args]
  (AnonymousFunctionCallExpression. macro-name (map ->expr args)))

;; List Symbol Expansion

(declare ->expr-op-sym)

;; TODO remove alias, macro distinction

(defn- op-sym-alias? [sym args]
  (when-let [alias (@symbol-aliases sym)]
    (->expr-op-sym alias args)))

(defn ^:dynamic lookup-intrinsic [sym]
  (@cintrinsics sym))

(defn- op-sym-intrinsic? [sym args]
  (when-let [intrinsic (lookup-intrinsic sym)]
    (apply intrinsic args)))

(defn- op-sym-member-access? [sym args]
  (when-let [[_ member] (re-matches #"\.(.*)" sym)]
    (->expr-op-sym
     '.
     (apply vector
            (first args)
            member
            (rest args)))))

(defn- op-sym-local-func? [sym args]
  (when-let [local-func (get *locals* sym)]
    (AnonymousFunctionCallExpression. (name sym) (map ->expr args))))

(defn- op-sym-defined-symbol? [sym args]
  (when-let [resolved (lookup-symbol sym)]
    (->expr (list->expr resolved args))))

(defn- ->expr-op-sym [sym args]
  (let [sym-name (name sym)]
    (or
     (op-sym-alias? sym args)
     (op-sym-intrinsic? sym args)
     (op-sym-member-access? sym-name args)
     (op-sym-local-func? sym args)
     (op-sym-defined-symbol? sym-name args)
     (throw (ArgumentException. (str "Don't know how to handle list symbol " sym))))))

(defn- ->expr-list [[op & args]]
  (cond
   (symbol? op) (->expr-op-sym op args)
   (keyword? op) (AnonymousFunctionCallExpression. (name op) (map ->expr args))
   (list? op)
   (loop [expanded (->expr-list op)]
     (cond
      (satisfies? IExpression expanded) (ComputedFunctionCallExpression. expanded (map ->expr args))
      (list? expanded) (recur (->expr-list expanded))
      :default (->expr-list expanded args)))
   :default (throw (ArgumentException. (str "Don't know how to handle list starting with" op)))))

(defrecord InitializerList [values]
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_] (str "{" (str/join ", " (map expr-write values)) "}")))

(defn- ->expr-vector [values]
  (InitializerList. (map ->expr values)))

(defn ^:dynamic ->expr [form]
  (cond
   (nil? form) null-literal
   (char? form) (CharLiteral. form)
   (number? form) (->expr-num form)
   (boolean? form) (BooleanLiteral. form)
   (string? form) (StringLiteral. form)
   (symbol? form) (when-let [resolved (lookup-symbol form)]
                    (if (instance? RawCMacro resolved)
                      (AnonymousVariableRefExpression. (name form))
                      (VariableRefExpression. form)))
   (keyword? form) (AnonymousVariableRefExpression. (name form))
   (list? form) (->expr-list form)
   (vector? form) (->expr-vector form)
   (satisfies? IExpression form) form
   :default (throw (ArgumentException. (str "Don't know how to handle " form " of type " (type form))))))

(defn expand-Expr)

(defn- is-block? [expr]
  (let [cat (expr-category expr)]
    (or (= :statement* cat) (= :block cat))))

(defn- cintrinsic* [sym func]
  (swap! cintrinsics assoc sym func))

(defmacro ^:private cintrinsic [sym args & body]
  `(cintrinsic* '~sym
               (fn ~sym ~args
                 (let [~@ (doall (reduce (fn [res x] (into res [x `(->expr ~x)])) [] args))]
                   ~@body))))

(defn- get-expr-record-sym [sym]
  (symbol (clojure.lang.Compiler/munge
           (str (name sym) "Expression"))))

(defmacro ^:private cop [sym args & body]
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

(defmacro ^:private cbinop [sym]
  `(cop ~sym [x# y#]
        (expr-write [_] (str "(" (expr-write x#) " " ~(name sym) " " (expr-write y#) ")"))
        IHasType
        (get-type [_] (get-bin-op-type x# y#))))

(defmacro ^:private cbinop* [sym expr]
  `(cop ~sym [x# y#]
        (expr-write [_] (str "(" (expr-write x#) " " ~expr " " (expr-write y#) ")"))
        IHasType
        (get-type [_] (get-bin-op-type x# y#))))

(defmacro ^:private cbinops [& syms]
  `(do ~@(for [x syms] `(cbinop ~x))))

(defmacro ^:private compop [sym]
  `(cop ~sym [x# y#]
        (expr-write [this#] (str "(" (expr-write x#) " " ~(name sym) " " (expr-write y#) ")"))
        IHasType
        (get-type [this#] 'bool)))

(defmacro ^:private compops [& syms]
  `(do ~@(for [x syms] `(compop ~x))))

(defmacro ^:private compop* [sym expr]
  `(cop ~sym [x# y#]
        (expr-write [this#] (str "(" (expr-write x#) " " ~expr " " (expr-write y#) ")"))
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

(defmacro ^:private cassignop [sym expr]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
       (defrecord ~rec-sym
           [x# y#]
         IHasType
         (get-type [_])
         IExpression
         (expr-write [this#] (str "(" (expr-write x#) " " ~expr " " (reduce-parens (expr-write y#)) ")")))
       (cintrinsic
        ~sym
        [target# source#]
        (if (is-block? source#)
          (wrap-last source#
                     (fn [x#] (new ~rec-sym target# x#)))
          (new ~rec-sym target# source#))))))

(defmacro ^:private c*op [sym]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
       (defrecord ~rec-sym [~'args]
         IExpression
         (expr-write [_]
           (if (= 1 (count ~'args))
             (str ~(name sym) (expr-write (first ~'args)))
             (str "(" (str/join ~(str " " sym " ") (map expr-write ~'args)) ")")))
         IHasType
         (get-type [_] (apply get-bin-op-type ~'args)))
       (cintrinsic* '~sym
                    (fn [& args#]
                      (new ~rec-sym (doall (map ->expr args#))))))))

(defmacro ^:private comp*op* [sym expr]
  (let [rec-sym (get-expr-record-sym sym)]
    `(do
       (defrecord ~rec-sym [~'args]
         IExpression
         (expr-write [_]
           (str "(" (str/join ~(str " " expr " ") (map expr-write ~'args)) ")"))
         IHasType
         (get-type [_] (lookup-type 'bool)))
       (cintrinsic* '~sym
                    (fn [& args#]
                      (new ~rec-sym (doall (map ->expr args#))))))))

(defmacro ^:private c*ops [& syms]
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

(defmacro ^:private cunop [name arg & body]
  `(cop ~name ~arg
        (expr-write [this#] ~@body)
        IHasType
        (get-type [this#] (get-type ~(first arg)))))

(defn- write-str [& args]
  (apply str (for [arg args]
               (if (string? arg)
                 arg
                 (expr-write arg)))))

(cunop inc [x] (write-str "++" x))
(cunop dec [x] (write-str "--" x))
(cunop post-inc [x] (write-str x "++"))
(cunop post-dec [x] (write-str x "--"))
(cunop bit-not [x] (write-str "~" x))

(cop not [x]
     (expr-write [_] (str "!" (expr-write x)))
     IHasType
     (get-type [_] (lookup-type 'bool)))

(defrecord SizeofExpression [x]
  IExpression
  (expr-write [_] (let [type (lookup-type x)]
               (str "sizeof(" (type-write type) ")")))
  IHasType
  (get-type [_] (lookup-type 'size_t)))

(cintrinsic*
 'sizeof (fn [x]
           (SizeofExpression. x)))

;; (defn c* [& args]
;;   (let [expanded (for [arg args]
;;                    (if (string? arg)
;;                      arg
;;                      (->expr arg)))]
;;     (apply str expanded)))

;; (cintrinsic 'c* c*)

(defrecord StructFieldAccessExpression [instance-expr member-info pointer-depth]
  IHasType
  (get-type [_] (get-type member-info))
  IExpression
  (expr-write [_]
    (let [prefix-pointer (when (>= pointer-depth 2)
                           (- pointer-depth 2))
          pointer-depth (if prefix-pointer 1
                            pointer-depth)]
      (str
       (when prefix-pointer
         (apply str "(" (repeat prefix-pointer "*")))
       (expr-write instance-expr)
       (when prefix-pointer ")")
       (case pointer-depth
         0 "."
         1 "->")
       (name member-info)))))

(cintrinsic* '.
             (fn [instance-expr member-name & args]
               (let [instance-expr (->expr instance-expr)
                     instance-type (get-type instance-expr)
                     access-expr (type-create-field-access-expr
                                  instance-type instance-expr member-name)]
                 (if args
                   (let [args (map ->expr args)]
                     (ComputedFunctionCallExpression. access-expr args))
                   access-expr))))

(cintrinsic* '->
            (fn [& args]
              (let [args (map ->expr args)]
                (reify
                  IHasType
                  (get-type [_])
                  IExpression
                  (expr-write [_]
                    (str/join "->" (map expr-write args)))))))

(cop aget [x y]
     (expr-write [_] (write-str x "[" y "]"))
     IHasType
     (get-type [_]
               (lookup-type (type-dereferenced-type (get-type x)))))

(defrecord ArraySetExpression [target idx value]
  IHasType
  (get-type [_] (get-type target))
  IExpression
  (expr-write [_] (write-str target "[" idx "] = " value)))

(cintrinsic aset [target idx value]
            (if (is-block? value)
              (wrap-last value (fn [x] (ArraySetExpression. target idx x)))
              (ArraySetExpression. target idx value)))

(cop ref [x]
     (expr-write [_] (write-str "(&" x ")"))
     IHasType
     (get-type [_]))

(cop deref [x]
     (expr-write [_] (write-str "*" x))
     IHasType
     (get-type [_]))

(csymbol-alias 'clojure.core/deref deref)

(defrecord CVerbatim [args]
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_]
    (apply write-str args)))

(cintrinsic* 'c*
            (fn [& args]
              (CVerbatim.
               (map (fn [x]
                      (if (string? x) x (->expr x)))
                    args))))

(def ^:dynamic *indent* 0)

(defn- indent [] (str/join (for [x (range *indent*)] "\t")))

(defrecord Statement [expr noindent]
  IExpression
  (wrap-last [_ func]
    (Statement. (func expr) noindent))
  (expr-write [_]
    (str (when-not noindent (indent)) (reduce-parens (expr-write expr)) ";"))
  IHasType
  (get-type [_] (get-type expr)))

(defmethod expr-category Statement [_] :statement)

(defn cstatement [expr & {:keys [noindent]}]
  (let [expr (->expr expr)]
    (when expr
      (if (or (is-block? expr) (= :statement (expr-category expr)))
        expr
        (Statement. (->expr expr) noindent)))))

(defn- wrap-statements [func statements]
  (conj (vec (drop-last statements))
        (wrap-last (last statements) func)))

(defrecord Statements [statements]
  IExpression
  (expr-write [_] (str/join "\n" (map expr-write statements)))
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
  (expr-write [_]
    (let [cases
          (binding [*indent* (inc *indent*)]
            (for [[expr block] cases]
              (if block
                (let [block (binding [*indent* (inc *indent*)]
                              (expr-write block))]
                  (str (indent) "case " (expr-write expr) ":\n" block "\n" (indent) "break;\n"))
                (str (indent) "default:" (expr-write expr) "\n" (indent) "break;\n"))))]
      (str "switch(" (expr-write test) ") {\n" (str/join "\n" cases) (indent) "\n}")))
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
   (let [test (->expr test)
         cases (partition 2 args)
         has-default (odd? (count args))
         cases
         (vec
          (for [[expr block] cases]
            [(->expr expr)
             (cstatement block)]))
         cases (if has-default
                 (conj cases [(cstatement (last args))])
                 cases)]
     (CaseExpression. test cases))))

(defrecord ReturnExpression [expr]
  IHasType
  (get-type [_] (get-type expr))
  IExpression
  (expr-write [_]
    (if expr
      (if-let [expr (expr-write expr)]
        (str "return " (reduce-parens expr))
        "return")
      "return")))

(derive ReturnExpression ::ReturnExpression)

(cintrinsic*
 'return
 (fn
   ([] (ReturnExpression. nil))
   ([expr]
      (let [expr (->expr expr)]
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
  (expr-write [_]
    (str (indent)
         "if(" (reduce-parens (expr-write expr)) ")\n"
         (expr-write then)
         (when else
           (str "\n" (indent) "else\n"
                (expr-write else)))))
  (wrap-last [_ func]
    (IfExpression.
     expr
     (wrap-last then func)
     (when else (wrap-last else func)))))

(defmethod expr-category IfExpression [_] :statement*)

(cintrinsic* 'if
             (fn
               ([expr then]
                  (IfExpression. (->expr expr)
                                 (cblock then)
                                 nil))
              ([expr then else]
                 (IfExpression. (->expr expr)
                                (cblock then)
                                (cblock else)))))

(defrecord DeclExpression [var-type var-name init-expr]
  IHasType
  (get-type [_] var-type)
  IExpression
  (expr-write [_] (str (type-write-decl-expr var-type var-name) "=" (when init-expr (expr-write init-expr)))))

(defrecord BlockExpression [statements]
  IHasType
  (get-type [_] (get-type (last statements)))
  IExpression
  (wrap-last [_ func]
    (BlockExpression.
     (wrap-statements func statements)))
  (expr-write [_]
    (str (indent) "{\n"
       (binding [*indent* (inc *indent*)]
         (str/join "\n" (map expr-write statements)))
       "\n" (indent) "}")))

(defmethod expr-category BlockExpression [_] :block)

(defn- cblock [& statements]
  (BlockExpression. (doall (map cstatement (remove nil? statements)))))

(cintrinsic* 'do cblock)

(defrecord ForStatement [init-expr test-expr each-expr body]
    IHasType
    (get-type [_])
    IExpression
    (expr-write [_]
      (str (indent) "for("
           (reduce-parens (expr-write init-expr)) "; "
           (reduce-parens (expr-write test-expr)) "; "
           (reduce-parens (expr-write each-expr)) ")\n"
           (expr-write body)))
    (wrap-last [_ func] (throw (Exception. "Cannot take value of for statement"))))

(defmethod expr-category ForStatement [_] :statement)

(defrecord CommaExpression [expressions]
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_] (str/join ", " (map expr-write expressions))))

(defmethod expr-category CommaExpression [_] :statement)

(defrecord NopExpression []
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_]))

(defn- wrap-for-expressions [form]
  (cond
   (empty? form)
   (NopExpression.)
   (vector? form)
   (CommaExpression. (map ->expr form))
   :default
   (->expr form)))

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
    (expr-write [_]
      (str (indent) "while(" (reduce-parens (expr-write test-expr)) ")\n"
           (expr-write body)))
    (wrap-last [_ func] (throw (Exception. "Cannot take value of while statement"))))

(defmethod expr-category WhileStatement [_] :statement)

(cintrinsic* 'while
             (fn [test & body]
               (let [body (apply cblock body)]
                 (WhileStatement.
                  (->expr test)
                  body))))


(defrecord BreakStatement []
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_] "break"))

(cintrinsic break [] (BreakStatement.))

(defrecord ContinueStatement []
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_] "continue"))

(cintrinsic continue [] (ContinueStatement.))

(defrecord LabelStatement [label]
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_] (str (name label) ":")))

(defmethod expr-category LabelStatement [_] :statement)

(cintrinsic* 'label (fn [x] (LabelStatement. x)))

(defrecord GotoExpression [label]
  IHasType
  (get-type [_])
  IExpression
  (expr-write [_] (str "goto " (name label))))

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
         (let [init-expr (->expr expr-form)
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

(defn get-var-type-tag [metadata]
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
       (type-write-decl-expr (get-type field)
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
  (decl-write [_]
    (str "typedef struct " type-name " {\n"
       (str/join
        (map write-struct-field fields))
       "} " type-name ";"))
  (decl-write-impl [_]))

(derive Struct ::Type)
(derive Struct ::Struct)

(defmethod type-get-fields ::Struct [{:keys [field-map]}] field-map)

(defmethod type-create-field-access-expr ::Struct
  type-create-field-access-expr-struct
  ([this instance-expr field-name]
     (type-create-field-access-expr this instance-expr field-name 0))
  ([{:keys [field-map]} instance-expr field-name pointer-depth]
   (when-let [field (get field-map (name field-name))]
     (StructFieldAccessExpression. instance-expr field pointer-depth)))) 

(cintrinsic*
 'cast
 (fn [target-type expr]
   (let [target-type (lookup-type target-type)
         expr (->expr expr)]
     (type-create-explicit-cast-Expr target-type expr))))

;; VarArgsType

(defrecord VarArgsType []
  clojure.lang.Named
  (getName [_] "...")
  IType)

(derive VarArgsType ::Type)

(defmethod type-write-decl-expr VarArgsType
  [_ var-name] "...")

(defmethod type-write VarArgsType[_])

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

(defrecord IncludeDeclaration [package include-name]
  clojure.lang.Named
  (getName [_] include-name)
  IDeclaration
  (decl-write [_] (str "#include <" include-name ">"))
  (decl-write-impl [_]))

(derive IncludeDeclaration ::IncludeDeclaration)

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
  (decl-write [_] (str "typedef " (type-write-decl-expr target-type type-name) ";"))
  (decl-write-impl [_])
  IType)

(derive TypeDef ::Type)

(defmethod type-is-reference? TypeDef [{:keys [target-type]}] (type-is-reference? target-type))

(defmethod type-is-function? TypeDef [{:keys [target-type]}] (type-is-function? target-type))

(defmethod type-get-fields TypeDef [{:keys [target-type]}] (type-get-fields target-type))

(defmethod type-common-denominator TypeDef
  [{:keys [target-type]} other-type]
  (type-common-denominator target-type other-type))

(defmethod type-create-field-access-expr TypeDef
  [{:keys [target-type]} instance-expr field-name]
  (type-create-field-access-expr target-type instance-expr field-name))

;; Enums

(defrecord EnumValue [name value base-type enum-type-name]
  clojure.lang.Named
  (getName [_]
    (lookup-type enum-type-name)
    name)
  IHasType
  (get-type [_] base-type)
  IExpression
  (expr-write [_]
    (lookup-type enum-type-name)
    name))

(defrecord EnumType [package type-name values]
  clojure.lang.Named
  (getName [_] type-name)
  IDeclaration
  (decl-write [_] (str "typedef enum " 
                       "{"
                       (str/join
                        ", "
                        (map #(str (:name %) " = " (:value %)) values))
                       "} " type-name ";"))
  (decl-write-impl [_])
  IType)

(derive EnumType ::Type)

;; Includes

(defrecord PackageIncludeDeclaration [package referenced-package]
  clojure.lang.Named
  (getName [_] (str (name referenced-package) ".h"))
  IDeclaration
  (decl-write [_] (str "#include \"" (name referenced-package) ".h\""))
  (decl-write-impl [_]))

(defrecord GlobalVariableDeclaration [package var-name var-type init-expr]
  clojure.lang.Named
  (getName [_] var-name)
  IHasType
  (get-type [_] (lookup-type var-type))
  IDeclaration
  (decl-write [this]
    (let [var-type (lookup-type var-type)]
      (or (apply-hook :alternate-global-variable-declaration this)
          (str (or (apply-hook :before-global-variable-declaration this) "extern ") (type-write-decl-expr var-type var-name) ";"))))
  (decl-write-impl [this]
    (let [var-type (lookup-type var-type)]
      (str (apply-hook :before-global-variable-declaration this) (type-write-decl-expr var-type var-name) (when init-expr (str " = " (expr-write init-expr))) ";"))))

(defmethod list->expr GlobalVariableDeclaration
  [{:keys [var-name]} args]
  (FunctionCallExpression. var-name (map ->expr args)))

(defrecord RawCHeader [package txt]
  clojure.lang.Named
  (getName [_])
  IDeclaration
  (decl-write [_] (str txt "\n"))
  (decl-write-impl [_]))

