(ns c-in-clj.lang.intrinsics
  (:use [c-in-clj.lang.api])
  (:require
   [clojure.string :as str]))

(derive-type ::CType)

(defn derive-ctype [cls] cls ::CType)

(defmethod type-write-decl-expr ::CType
  ([{:keys [type-name]} var-name] (str type-name " " var-name))
  ([{:keys [type-name]} var-name pointer-depth]
     (str type-name (apply str (repeat pointer-depth "*")) " " var-name)))

(defrecord DefaultCastExpression [target-type expr])

(defmethod get-type DefaultCastExpression [x] (:target-type x))

(defmethod expr-write DefaultCastExpression [{:keys [expr target-type]}]
  (str "((" (type-write target-type) ")" (expr-write expr) ")"))

(defmethod type-create-explicit-cast-Expr ::CType [this expr]
  (DefaultCastExpression. this expr))

(defrecord PrimitiveType [type-name])

(derive-ctype PrimitiveType)

(def ^:private primitive-types (atom {}))

(defn add-primitives [& type-syms]
  (doseq [sym type-syms]
    (let [type-name (name sym)]
      (swap! primitive-types
             assoc
             type-name
             (PrimitiveType. type-name)))))

(add-primitives
 'void 'size_t 'ptrdiff_t 
 'float 'double 'bool 'char
 'int8_t 'int16_t 'int32_t 'int64_t
 'uint8_t 'uint16_t 'uint32_t 'uint64_t)

;; Aliases

(defn add-primitive-type-alias [alias-sym target-sym]
  (swap! primitive-types
         assoc
         alias-sym
         (->Alias target-sym)))

(add-primitive-type-alias 'i8 'int8_t)
(add-primitive-type-alias 'i16 'int16_t)
(add-primitive-type-alias 'i32 'int32_t)
(add-primitive-type-alias 'i64 'int64_t)
(add-primitive-type-alias 'u8 'uint8_t)
(add-primitive-type-alias 'u16 'uint16_t)
(add-primitive-type-alias 'u32 'uint32_t)
(add-primitive-type-alias 'u64 'uint64_t)

(defrecord PointerType [type-name])

(defmethod get-name PointerType [{:keys [type-name]}]
  (str (name type-name) "*"))

(derive-ctype PointerType)

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

(derive-ctype ::Literal)

(defn derive-literal [cls] (derive cls ::Literal))

(defmethod expr-category ::Literal [_] :literal)

(defrecord Literal [ctype value])
(derive-literal Literal)
(defmethod expr-write Literal [{:keys [value]}] (pr-str value))
(defmethod get-type Literal [{:keys [ctype]}] (lookup-type ctype))

(defrecord CharLiteral [value])
(derive-literal CharLiteral)
(defmethod expr-write CharLiteral [{:keys [value]}] (str "'" value "'"))
(defmethod get-type Literal [{:keys [ctype]}] (lookup-type 'char))

(defrecord NullLiteral [])
(derive-literal NullLiteral)
(defmethod expr-write NullLiteral [_] "NULL")
(def null-literal (NullLiteral.))

(defn cnum->expr [x]
  (let [ntype (type x)]
    (cond
     (= ntype Int64)
     (if (and (>= x Int32/MinValue) (<= x Int32/MaxValue))
       (Literal. 'i32 x)
       (Literal. 'i64 x))
     (= ntype Double)
     (Literal. 'double x))))

(defn cmember-access-list->expr [sym args]
  (when-let [[_ member] (re-matches #"\.(.*)" (name sym))]
    (->expr
     (apply list
            '.
            (first args)
            member
            (rest args)))))

(defn clist->expr [[sym & args]]
  (or
   (member-access-list->expr sym args)
   (when-let [resolved (lookup-symbol sym)]
     (->expr (list->expr resolved args)))
   (throw (ArgumentException. (str "Don't know how to handle list symbol " sym)))))

(defn boolean? [x] (or (= x true) (= x false)))


(defrecord InitializerList [values])
(defmethod expr-write InitializerList
  [{:keys [values]}]
  (str "{" (str/join ", " (map expr-write values)) "}"))

(defn cvec->expr [values]
  (InitializerList. (map ->expr values)))

(defn cform->expr [form]
  (cond
   (nil? form) null-literal
   (char? form) (CharLiteral. form)
   (number? form) (cnum->expr form)
   (boolean? form) (Literal. 'bool form)
   (string? form) (Literal. 'char* form)
   (symbol? form) (cform->expr (lookup-symbol form :throw true))
   (keyword? form) (AnonymousVariableRefExpression. (name form))
   (list? form) (clist->expr form)
   (vector? form) (cvec->expr form)
   (is-expr? form) form
   :default
   (if-let [as-expr (get-method sym->expr form)]
     (cform->expr (as-expr form))
     (throw (ArgumentException. (str "Don't know how to handle " form " of type " (type form)))))))

;; Intrinsics

(def ^:private cintrinsics (atom {}))

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

(defn reduce-parens [^String expr]
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

;;;;; Operators

(defn- add-intrinsic [sym f] (swap! cintrinsics assoc sym f))

(defn get-bin-op-type [& args]
  (let [types (map get-type args)]
    (when (apply = types)
      (first types))))

;;;; Simple Binary Operators

(defrecord BinOp [sym x y])
(derive-expr BinOp)
(defmethod expr-write BinOp
  [{:keys [sym x y]}]
  (str "(" (expr-write x) " " (name sym) " " (expr-write y) ")"))
(defmethod get-type BinOp
  [{:keys [sym x y]}]
  (get-bin-op-type x y))

(defn parse-bin-op [sym x y]
  (BinOp. sym (->expr x) (->expr x)))

(defn- add-bin-op [op-sym output-sym]
  (add-intrinsic op-sym (partial parse-bin-op output-sym)))

(add-bin-op 'mod "%")
(add-bin-op 'bit-or "|")
(add-bin-op 'bit-and "&")
(add-bin-op 'bit-shift-left "<<")
(add-bin-op 'bit-shift-right ">>")
(add-bin-op 'bit-xor "^")

;;;; Binary Comparison Operators 

(defrecord CompOp [sym x y])
(derive-expr CompOp)
(defmethod expr-write CompOp
  [{:keys [sym x y]}]
  (str "(" (expr-write x) " " (name sym) " " (expr-write y) ")"))
(defmethod get-type CompOp [_] 'bool)

(defn parse-comp-op [sym x y]
  (CompOp. sym (->expr x) (->expr x)))

(doseq [s '[< > <= >=]]
  (add-intrinsic s (partial parse-comp-op s)))  

(add-intrinsic '= (partial parse-comp-op "=="))
(add-intrinsic not= (partial parse-comp-op "!="))

;;;; Assignment Operators

(defrecord AssignOp [sym target source])
(derive-expr AssignOp)
(defmethod expr-write AssignOp
  [{:keys [sym x y]}]
  (str "(" (expr-write target) " " sym " " (reduce-parens (expr-write source)) ")"))

(defn parse-assign-op [sym target source]
  (expr-wrap-last source (fn [x] (AssignOp. target x))))

(defn add-assign-op [op-sym output-sym]
  (add-intrinsic op-sym (partial parse-assign-op output-sym)))

(add-assign-op '+= "+=")
(add-assign-op '-= "-=")
(add-assign-op '*= "*=")
(add-assign-op '/= "/=")
(add-assign-op 'mod= "%=")
(add-assign-op 'bit-or= "|=")
(add-assign-op 'bit-and= "&=")
(add-assign-op 'bit-shift-left="<<=")
(add-assign-op 'bit-shift-right= ">>=")
(add-assign-op 'bit-xor= "^=")
(add-assign-op 'bit-not= "~=")
(add-assign-op 'set! "=")

(defrecord NOperation [sym args])
(derive-expr NOperation)
(defmethod expr-write NOperation
  [{:keys [sym args]}]
  (if (= 1 (count args))
    (str (name sym) (expr-write (first args)))
             (str "(" (str/join ~(str " " sym " ") (map expr-write args)) ")"))  )
(defmethod get-type NOperation
  [{:keys [sym args]}]
  (apply get-bin-op-type args))

(defn parse-*op [sym & args]
  (NOperation. sym (doall (map ->expr args))))

(doseq [s '[+ - * /]] (add-intrinsic s (partial parse-*op s)))

(defrecord NCompOperation [sym args])
(derive-expr NCompOperation)
(defmethod expr-write NCompOperation
  [{:keys [sym args]}]
  (str "(" (str/join ~(str " " sym " ") (map expr-write args)) ")")  )
(defmethod get-type NCompOperation [_] 'bool)

(defn parse-comp*op [sym & args]
  (NCompOperation. sym (doall (map ->expr args))))

(add-intrinsic 'or (partial parse-comp*op "||"))
(add-intrinsic 'and (partial parse-comp*op "&"))

(defrecord UnaryOperation [func x])
(derive-expr UnaryOperation)
(defmethod expr-write UnaryOperation
  [{:keys [func x]}]
  (func (expr-write x)))


;;;; TODO:

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

;;;;; Declarations

(defrecord AnonymousFunctionCallExpression [func-name args])
(defmethod expr-write AnonymousFunctionCallExpression
  [{:keys [func-name args]}]
  (str (name func-name)
       "(" (str/join "," (map expr-write args)) ")"))

(defrecord ComputedFunctionCallExpression [func-expr args])
(defmethod expr-write ComputedFunctionCallExpression
  [{:keys [func-expr args]}]
  (str (expr-write func-expr)
       "(" (str/join "," (map expr-write args)) ")"))
(defmethod get-type ComputedFunctionCallExpression
  [{:keys [func-expr]}]
  (get-type func-expr))

(defrecord VariableRefExpression [variable])
(defmethod expr-write VariableRefExpression
  [{:keys [variable]}]
  (get-name variable))
(defmethod get-type VariableRefExpression
  [{:keys [variable]}]
  (get-type variable))

(defrecord VariableDeclaration [var-name var-type]
  clojure.lang.Named
  (getName [_] var-name))
(defmethod get-type VariableDeclaration
  [{:keys [var-type]}] (lookup-type var-type))
(defmethod expr-write VariableDeclaration
  [{:keys [var-type var-name]}]
  (let [var-type (lookup-type var-type)]
    (str (type-write-decl-expr var-type var-name)
         (when-let [init (type-requires-initialization var-type)]
           (str " = " (type-default-initializer var-type))))))
(defmethod sym->expr VariableDeclaration
  [this]
  (VariableRefExpression. this))

(defrecord AnonymousVariableRefExpression [var-name])
(defmethod expr-write AnonymousVariableRefExpression
  [{:keys [var-name]}] (get-name var-name))

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

(defn- write-function-signature [{:keys [function-name function-type] :as decl} ]
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
  [{:keys [function-name function-type body referenced-decls locals]}]
  (or (apply-hook :alternate-function-declaration this)
      (str
       (when-let [doc (:doc (meta this))]
         (str "/**\n" 
              (apply str (map #(str "* " % "\n") (str/split-lines doc)))
              "*/\n"))
       (write-function-signature this) ";")))
(defmethod decl-write-impl FunctionDeclaration
  [{:keys [function-name function-type body referenced-decls locals]}]
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
