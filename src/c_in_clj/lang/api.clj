(ns c-in-clj.lang.api
  "The idea here is to create an API that will let us define the elements of
a language like C in a very flexible, extensible way.  This API would probably
be sufficient to define many other languages besides C.

It consists of three basic type classes type, expr (expression)
and decl (declaration). It uses almost exclusively Clojure multimethods and
hierarchies defined using the derive function.  Each type class follows the
following pattern:
  For the type class X:
   * there are functions derive-X and is-X? which both allow us to derive
     some other class from X and test if a given object derives from X
     (ex. derive-type and is-type?)
   * any function prefixed with X- is a multimethod which extends the
     functionality of types derived from X.
   * functions which include X in their name but do not start with X,
     are helper functions

It is suggested that you use defrecords as the concrete type, expr, and decl
types.

These multimethods can be defined on any type, expr, or decl:
get-name, get-name, sym->expr, list->expr")


(defn- is-an-instance? [x t] (isa? (type x) t))

;; Methods which can apply to all c-in-clj elements

(defmulti get-name class)

(defmethod get-name :default [x] (when (instance? clojure.lang.Named x) (name x)))

(defmulti get-type class)

(defmethod get-type :default [_] nil)

(defmulti list->expr (fn [this args & opts] (class this)))

(defmulti sym->expr class)

;; Expressions

(defn derive-expr [cls] (derive cls ::Expression))

(defn is-expr? [x] (is-an-instance? x ::Expression))

(defmulti expr-write class)

(defmulti expr-wrap-last (fn [expr func] (class expr)))

(defmethod expr-wrap-last ::Expression [expr func] (func expr))

(defmulti expr-category class)

(defmethod expr-category :default [_] nil)

;; Types

(defn derive-type [cls] (derive cls ::Type))

(defn is-type? [x] (is-an-instance? x ::Type))

(defmulti type-common-denominator (fn [this & args] (class this)))

(defmulti type-create-field-access-expr (fn [this & args] (class this)))

(defmulti type-dereferenced-type class)

(defmulti type-write class)

(defmulti type-is-reference? class)

(defmulti type-is-function? class)

(defmulti type-get-fields class)

(defmulti type-write-decl-expr (fn [this & args] (class this)))

(defmulti type-create-explicit-cast-Expr (fn [this expr] (class this)))

(defmulti type-default-initializer class)

(defmulti type-requires-initialization class)

;; Type method defaults
(defmethod get-name ::Type [x] (:type-name x))

(defmethod type-common-denominator ::Type [_ other-type])

(defmethod type-write ::Type [{:keys [type-name]}] type-name)

(defmethod type-is-reference? ::Type [_] false)

(defmethod type-is-function? ::Type [_] false)

(defmethod type-get-fields ::Type [_])

(defmethod type-default-initializer :default [_])

(defmethod type-requires-initialization :default [_] false)

;; Declarations

(defn derive-decl [cls])

(defn is-decl? [x])

(defmulti decl-write class)

(defmulti decl-write-impl class)

(defmethod decl-write-impl :default [_])

;; Scopes

(defn derive-scope [cls] (derive cls ::Scope))

(defn is-scope? [x] (is-an-instance? x ::Scope))

(defmulti scope-add (fn [scope element] (class scope)))

(defmulti scope-lookup-symbol (fn [scope sym] (class scope)))

(defmulti scope-lookup-type (fn [scope sym] (class scope)))

(defmulti scope-form->expr (fn [scope sym] (class scope)))

(defmulti scope-get-allocator class)

(defmulti scope-hook (fn [scope hook-name & args] class))

(def ^:dynamic *scope* nil)

(def ^:private scopes-by-ns (atom {}))

(defn get-ns-scope [] (get scopes-by-ns *ns*))

(defn get-scope [] (or *scope* (get-ns-scope)))

(defn set-ns-scope [scope] (swap! scopes-by-ns assoc *ns* scope))

(defn add-to-scope [element] (scope-add (get-scope) element))

(defn add-to-ns-scope [element] (scope-add (get-ns-scope) element))

;; Helper functions to be used in writing expr's, type's, and decl's

(defn lookup-type [sym & {:as opts}]
  (or (scope-lookup-type (get-scope) sym)
      (when (:throw opts)
        (throw (ex-info (str "Unable to resolve type" sym)
                        {:type ::unresolved-type
                         :symbol sym})))))

(defn lookup-symbol [sym & {:as opts}]
  (or (scope-lookup-symbol (get-scope) sym)
      (when (:throw opts)
        (throw (ex-info (str "Unable to resolve symbol" sym)
                        {:type ::unresolved-symbol
                         :symbol sym})))))

(defn ->expr [form] (scope-form->expr (get-ns-scope) form))

;; Some other useful constructs which will likely be used across languages

;; Allocators

(defn derive-allocator [cls] (derive cls ::Allocator))

(defn is-allocator? [x] (is-an-instance? x ::Allocator))

(defmulti allocator-alloc (fn [allocator & args] (class allocator)))

(defmulti allocator-free (fn [allocator & args] (class allocator)))

;; Aliases

(defrecord Alias [target-sym])

(defmethod sym->expr Alias [alias] (:target-sym alias))

