(ns c-in-clj.lang.declarations
  (:require [clojure.string :as str])
  (:use [c-in-clj.lang api intrinsics]))

;;;;; Declarations

(defrecord AnonymousFunctionCallExpression [func-name args])
(defmethod expr-write AnonymousFunctionCallExpression
  [{:keys [func-name args]}]
  (str (name func-name)
       "(" (str/join "," (map expr-write args)) ")"))

;; (defn- add-local [decl]
;;   (let [var-name (name decl)]
;;     (assert *locals* "No local variable context for let expression")
;;     (assert (not (get *locals* var-name))
;;             (str "Local variable named " var-name " already defined"))
;;     (set! *locals* (assoc *locals* var-name decl))
;;     (set! *local-decls* (conj *local-decls* decl))))

(defrecord LocalVariableScope [parent-scope local-decls local-syms])
(derive-scope LocalVariableScope)
(defmethod scope-lookup-symbol LocalVariableScope
  [{:keys [parent-scope local-syms]} sym]
  (or (get local-syms sym)
      (scope-lookup-symbol parent-scope)))
(defmethod scope-lookup-type LocalVariableScope
  [{:keys [parent-scope local-syms]} sym]
  (or (get local-syms sym)
      (scope-lookup-type parent-scope)))
(defmethod scope-form->expr LocalVariableScope
  [{:keys [parent-scope]} form]
  (scope-form->expr parent-scope form))

(defrecord CMacro [name func]
  clojure.lang.Named
  (getName [_] name))
(defmethod list->expr CMacro [{:keys [func]} args]
  (apply func args))

(defrecord FunctionParameter [param-name param-type]
  clojure.lang.Named
  (getName [_] param-name))
(defmethod get-type FunctionParameter
  [{:keys [param-type]}]
  (lookup-type param-type))
(defmethod expr-write FunctionParameter
  [{:keys [param-type param-name]}]
  (type-write-decl-expr (lookup-type param-type) param-name))
(defmethod expr-category FunctionParameter [_] :local)

(defn- write-function-type [{:keys [return-type params]}
                            pointer-depth name?]
  (str (type-write (lookup-type return-type)) " ("
       (apply str (repeat pointer-depth "*"))
       name? ")("
       (str/join ", " (map expr-write params)) ")"))

(defrecord FunctionType [type-name return-type params]
  clojure.lang.Named
  (getName [_] type-name))
(derive-ctype FunctionType)
(defmethod type-is-function? FunctionType [_] true)
(defmethod type-write-decl-expr FunctionType
  ([this var-name]
    (write-function-type this 0 var-name))
  ([this var-name pointer-depth]
    (write-function-type this pointer-depth var-name)))
(defmethod type-write FunctionType
  [this]
  (write-function-type this 0 nil))

(defn apply-hook [hook & args] (apply scope-hook (get-scope) args))

(defn write-function-signature [{:keys [function-name function-type] :as decl} ]
  (let [{:keys [return-type params]} function-type]
    (str
     (apply-hook :before-function-signature decl)
     (type-write (lookup-type return-type)) " "
     function-name "(" (str/join ", " (map expr-write params)) ")")))

(defrecord FunctionDeclaration [function-name function-type body referenced-decls locals]
  clojure.lang.Named
  (getName [this] function-name))
(defmethod get-type FunctionDeclaration
  [{:keys [function-type]}]
  function-type)
(defmethod decl-write FunctionDeclaration
  [{:keys [function-name function-type body referenced-decls locals] :as this}]
  (or (apply-hook :alternate-function-declaration this)
      (str
       (when-let [doc (:doc (meta this))]
         (str "/**\n" 
              (apply str (map #(str "* " % "\n") (str/split-lines doc)))
              "*/\n"))
       (write-function-signature this) ";")))
(defmethod decl-write-impl FunctionDeclaration
  [{:keys [function-name function-type body referenced-decls locals] :as this}]
  (str (write-function-signature this) "\n"
       (binding [*scope* locals]
         (expr-write body))))

(defrecord RawCMacro [macro-name body]
  clojure.lang.Named
  (getName [_] macro-name))
(defmethod decl-write RawCMacro [{:keys [macro-name body]}] (str "#define " macro-name " " body "\n"))
(defmethod list->expr RawCMacro
  [{:keys [macro-name]} args]
  (AnonymousFunctionCallExpression. macro-name (map ->expr args)))

(defrecord StructFieldAccessExpression [instance-expr member-info pointer-depth])
(derive-expr StructFieldAccessExpression)
(defmethod get-type StructFieldAccessExpression
  [{:keys [member-info]}]
  (get-type member-info))
(defmethod expr-write StructFieldAccessExpression
  [{:keys [instance-expr member-info pointer-depth]}]
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
     (name member-info))))

