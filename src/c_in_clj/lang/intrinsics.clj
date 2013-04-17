(ns c-in-clj.lang.intrinsics
  (:use [c-in-clj.lang.api])
  (:require
   [clojure.string :as str]
   [c-in-clj.platform :as platform]))

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
  (let [cnum-type (platform/get-c-number-type x)]
    (Literal. cnum-type x)))

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
   (cmember-access-list->expr sym args)
   (when-let [resolved (lookup-symbol sym)]
     (if-let [func (get-method list->expr resolved)]
       (->expr (func resolved args))
       (when (::macro (meta resolved))
         (->expr (apply resolved args)))))
   (throw (ex-info (str "Don't know how to handle list symbol " sym)
                   {:type ::list-parse-exception
                    :symbol sym
                    :args args}))))

(defn boolean? [x] (or (= x true) (= x false)))

(defrecord InitializerList [values])
(defmethod expr-write InitializerList
  [{:keys [values]}]
  (str "{" (str/join ", " (map expr-write values)) "}"))

(defn cvec->expr [values]
  (InitializerList. (map ->expr values)))

(defrecord AnonymousVariableRefExpression [var-name])
(derive-expr AnonymousVariableRefExpression)
(defmethod expr-write AnonymousVariableRefExpression
  [{:keys [var-name]}]
  (name var-name))

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
     (throw (ex-info (str "Don't know how to handle " form " of type " (type form))
                     {:type ::expression-parse-error
                      :form form})))))

;;;;;; Intrinsics

(def ^:private cintrinsics (atom {}))

(defn- add-intrinsic [sym f]
  (swap! cintrinsics
         assoc (name sym)
         (with-meta f {::macro true})))

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
(add-intrinsic 'not= (partial parse-comp-op "!="))

;;;; Assignment Operators

(defrecord AssignOp [sym target source])
(derive-expr AssignOp)
(defmethod expr-write AssignOp
  [{:keys [sym target source]}]
  (str "(" (expr-write target) " " sym " " (reduce-parens (expr-write source)) ")"))

(defn parse-assign-op [sym target source]
  (expr-wrap-last source (fn [x] (AssignOp. sym target x))))

(defn add-assign-op [op-sym output-sym]
  (add-intrinsic op-sym (partial parse-assign-op output-sym)))

(add-assign-op '+= "+=")
(add-assign-op '-= "-=")
(add-assign-op '*= "*=")
(add-assign-op 'div= "/=")
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
             (str "(" (str/join (str " " sym " ") (map expr-write args)) ")"))  )
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
  (str "(" (str/join (str " " sym " ") (map expr-write args)) ")")  )
(defmethod get-type NCompOperation [_] 'bool)

(defn parse-comp*op [sym & args]
  (NCompOperation. sym (doall (map ->expr args))))

(add-intrinsic 'or (partial parse-comp*op "||"))
(add-intrinsic 'and (partial parse-comp*op "&&"))

(defrecord UnaryOperation [func x])
(derive-expr UnaryOperation)
(defmethod expr-write UnaryOperation
  [{:keys [func x]}]
  (func (expr-write x)))

(defn parse-un-op [func x] (UnaryOperation. func (->expr x)))

(defn add-un-op [sym func]
  (add-intrinsic sym (partial parse-un-op)))

(add-un-op 'inc #(str "++" %))
(add-un-op 'dec #(str "--" %))
(add-un-op 'post-inc #(str % "++"))
(add-un-op 'post-dec #(str % "--"))
(add-un-op 'bit-not #(str "~" %))

(defrecord NotExpression [x])
(derive-expr NotExpression)
(defmethod expr-write NotExpression
  [{:keys [x]}]
  (str "!" (expr-write x)))
(defmethod get-type NotExpression [_] 'bool)
(defn parse-not-op [x] (NotExpression. (->expr x)))
(add-intrinsic 'not parse-not-op)

(defrecord SizeofExpression [x])
(derive-expr SizeofExpression)
(defmethod expr-write SizeofExpression
  [{:keys [x]}]
  (let [type (lookup-type x)]
    (str "sizeof(" (type-write type) ")")))
(defmethod get-type SizeofExpression [_] 'size_t)
(add-intrinsic 'sizeof (fn [x] (SizeofExpression. x)))

(defrecord ComputedFunctionCallExpression [func-expr args])
(derive-expr ComputedFunctionCallExpression)
(defmethod expr-write ComputedFunctionCallExpression
  [{:keys [func-expr args]}]
  (str (expr-write func-expr)
       "(" (str/join "," (map expr-write args)) ")"))
(defmethod get-type ComputedFunctionCallExpression
  [{:keys [func-expr]}]
  (get-type func-expr))

(add-intrinsic '.
               (fn [instance-expr member-name & args]
                 (let [instance-expr (->expr instance-expr)
                       instance-type (get-type instance-expr)
                       access-expr (type-create-field-access-expr
                                    instance-type instance-expr member-name)]
                   (if args
                     (let [args (map ->expr args)]
                       (ComputedFunctionCallExpression. access-expr args))
                     access-expr))))

(defrecord StructureDereferenceExpression [source target])
(derive-expr StructureDereferenceExpression)
(defmethod expr-write StructureDereferenceExpression
  [{:keys [source target]}]
  (str (expr-write source) "->" (expr-write target)))
(add-intrinsic '.->
             (fn [source target] (StructureDereferenceExpression. (->expr source) (->expr target))))

(defrecord ArrayGetExpression [target idx])
(derive-expr ArrayGetExpression)
(defmethod expr-write ArrayGetExpression
  [{:keys [target idx]}]
  (str (expr-write target) "[" (expr-write idx) "]"))
(defmethod get-type ArrayGetExpression
  [{:keys [target idx]}]
  (lookup-type (type-dereferenced-type (get-type target))))
(add-intrinsic 'aget (fn [target idx] (ArrayGetExpression. (->expr target) (->expr idx))))

(defrecord ArraySetExpression [target idx value])
(derive-expr ArraySetExpression)
(defmethod expr-write ArraySetExpression
  [{:keys [target idx value]}]
  (str (expr-write target) "[" (expr-write idx) "] = " (expr-write value)))
(add-intrinsic 'aset
               (fn [target idx value]
                 (expr-wrap-last value (fn [x] (ArraySetExpression. (->expr target) (->expr idx) x)))))

(defrecord RefExpression [x])
(derive-expr RefExpression)
(defmethod expr-write RefExpression
  [{:keys [x]}]
  (str  "(&" (expr-write x) ")"))
(add-intrinsic 'ref (fn [x] (RefExpression. (->expr x))))

(defrecord DerefExpression [x])
(derive-expr DerefExpression)
(defmethod expr-write DerefExpression
  [{:keys [x]}]
  (str  "*" (expr-write x)))
(add-intrinsic 'deref (fn [x] (DerefExpression. (->expr x))))

(defrecord CVerbatim [args])
(derive-expr CVerbatim)
(defmethod expr-write CVerbatim
  [{:keys [args]}]
  (apply str (map (fn [x] (if (string? x) x (expr-write x))) args)))
(add-intrinsic 'c*
            (fn [& args]
              (CVerbatim.
               (map (fn [x]
                      (if (string? x) x (->expr x)))
                    args))))

;;;; Statements

(def ^:dynamic *indent* 0)

(defn indent [] (str/join (for [x (range *indent*)] "\t")))

(defn is-block? [expr]
  (let [cat (expr-category expr)]
    (or (= :statement* cat) (= :block cat))))

(defrecord Statement [expr noindent])
(derive-expr Statement)
(defmethod expr-wrap-last Statement
  [{:keys [expr noindent]} func]
  (Statement. (func expr) noindent))
(defmethod expr-write Statement
  [{:keys [expr noindent]}]
  (str (when-not noindent (indent)) (reduce-parens (expr-write expr)) ";"))
(defmethod get-type Statement
  [{:keys [expr noindent]}] (get-type expr))
(defmethod expr-category Statement [_] :statement)

(defn cstatement [expr & {:keys [noindent]}]
  (let [expr (->expr expr)]
    (when expr
      (if (or (is-block? expr) (= :statement (expr-category expr)))
        expr
        (Statement. (->expr expr) noindent)))))

(defn wrap-statements [func statements]
  (conj (vec (drop-last statements))
        (expr-wrap-last (last statements) func)))

(defrecord Statements [statements])
(derive-expr Statements)
(defmethod expr-write Statements
  [{:keys [statements]}]
  (str/join "\n" (map expr-write statements)))
(defmethod expr-wrap-last Statements
  [{:keys [statements]} func]
  (Statements.
   (wrap-statements func statements)))
(defmethod get-type Statements
  [{:keys [statements]}]
  (get-type (last statements)))
(defmethod expr-category Statements [_] :statement*)

(defn cstatements [statements]
  (Statements. (doall (map cstatement (remove nil? statements)))))

(defrecord CaseExpression [test cases])
(derive-expr CaseExpression)
(defmethod expr-write CaseExpression
  [{:keys [test cases]}]
  (let [cases
        (binding [*indent* (inc *indent*)]
          (for [[expr block] cases]
            (if block
              (let [block (binding [*indent* (inc *indent*)]
                            (expr-write block))]
                (str (indent) "case " (expr-write expr) ":\n" block "\n" (indent) "break;\n"))
              (str (indent) "default:" (expr-write expr) "\n" (indent) "break;\n"))))]
    (str "switch(" (expr-write test) ") {\n" (str/join "\n" cases) (indent) "\n}")))
(defmethod expr-wrap-last CaseExpression
  [{:keys [test cases]} func]
  (CaseExpression.
   test
   (for [[expr block] cases]
     (if block
       [expr (expr-wrap-last block func)]
       [(expr-wrap-last expr func)]))))
(defmethod expr-category CaseExpression [_] :statement)
(add-intrinsic
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

(defrecord ReturnExpression [expr])
(derive-expr ReturnExpression)
(defmethod get-type ReturnExpression
  [{:keys [expr]}]
  (get-type expr))
(defmethod expr-write ReturnExpression
  [{:keys [expr]}]
  (if expr
    (if-let [expr (expr-write expr)]
      (str "return " (reduce-parens expr))
      "return")
    "return"))
(add-intrinsic
 'return
 (fn
   ([] (ReturnExpression. nil))
   ([expr]
      (let [expr (->expr expr)]
        (expr-wrap-last expr (fn [x] (ReturnExpression. x)))))))

(declare cblock)

(defrecord IfExpression [expr then else])
(derive-expr IfExpression)
(defmethod expr-write IfExpression
  [{:keys [expr then else]}]
  (str (indent)
       "if(" (reduce-parens (expr-write expr)) ")\n"
       (expr-write then)
       (when else
         (str "\n" (indent) "else\n"
              (expr-write else)))))
(defmethod expr-wrap-last IfExpression
  [{:keys [expr then else]} func]
                (IfExpression.
                 expr
                 (expr-wrap-last then func)
                 (when else (expr-wrap-last else func))))
(defmethod expr-category IfExpression [_] :statement*)

(add-intrinsic 'if
             (fn
               ([expr then]
                  (IfExpression. (->expr expr)
                                 (cblock then)
                                 nil))
              ([expr then else]
                 (IfExpression. (->expr expr)
                                (cblock then)
                                (cblock else)))))

(defrecord DeclExpression [var-type var-name init-expr])
(derive-expr DeclExpression)
(defmethod get-type DeclExpression
  [{:keys [var-type]}]
  var-type)
(defmethod expr-write DeclExpression
  [{:keys [var-type var-name init-expr]}] (str (type-write-decl-expr var-type var-name) "=" (when init-expr (expr-write init-expr))))

(defrecord BlockExpression [statements])
(derive-expr BlockExpression)
(defmethod get-type BlockExpression
  [{:keys [statements]}]
  (get-type (last statements)))
(defmethod expr-wrap-last BlockExpression
  [{:keys [statements]} func]
  (BlockExpression.
   (wrap-statements func statements)))
(defmethod expr-write BlockExpression
  [{:keys [statements]}]
  (str (indent) "{\n"
       (binding [*indent* (inc *indent*)]
         (str/join "\n" (map expr-write statements)))
       "\n" (indent) "}"))
(defmethod expr-category BlockExpression [_] :block)

(defn- cblock [& statements]
  (BlockExpression. (doall (map cstatement (remove nil? statements)))))

(add-intrinsic 'do cblock)

(defrecord ForStatement [init-expr test-expr each-expr body])
(derive-expr ForStatement)
(defmethod expr-write ForStatement
  [{:keys [init-expr test-expr each-expr body]}]
  (str (indent) "for("
       (reduce-parens (expr-write init-expr)) "; "
       (reduce-parens (expr-write test-expr)) "; "
       (reduce-parens (expr-write each-expr)) ")\n"
       (expr-write body)))
(defmethod expr-wrap-last ForStatement
  [_ _] (throw (Exception. "Cannot take value of for statement")))
(defmethod expr-category ForStatement [_] :statement)

(defrecord CommaExpression [expressions])
(derive-expr CommaExpression)
(defmethod expr-write  CommaExpression [{:keys [expressions]}] (str/join ", " (map expr-write expressions)))
(defmethod expr-category CommaExpression [_] :statement)

(defrecord NopExpression [])
(derive-expr NopExpression)
(defmethod expr-write NopExpression [_])

(defn- wrap-for-expressions [form]
  (cond
   (empty? form)
   (NopExpression.)
   (vector? form)
   (CommaExpression. (map ->expr form))
   :default
   (->expr form)))

(add-intrinsic 'for
             (fn [init test each & body]
               (let [body (apply cblock body)]
                 (ForStatement.
                  (wrap-for-expressions init)
                  (wrap-for-expressions test)
                  (wrap-for-expressions each)
                  body))))

(defrecord WhileStatement [test-expr body])
(derive-expr WhileStatement)
(defmethod expr-write WhileStatement
  [{:keys [test-expr body]}]
  (str (indent) "while(" (reduce-parens (expr-write test-expr)) ")\n"
       (expr-write body)))
(defmethod expr-wrap-last WhileStatement [_ _] (throw (Exception. "Cannot take value of while statement")))
(defmethod expr-category WhileStatement [_] :statement)

(add-intrinsic 'while
             (fn [test & body]
               (let [body (apply cblock body)]
                 (WhileStatement.
                  (->expr test)
                  body))))


(defrecord BreakStatement [])
(derive-expr BreakStatement)
(defmethod expr-write BreakStatement [_] "break")
(add-intrinsic 'break (fn [] (BreakStatement.)))

(defrecord ContinueStatement [])
(derive-expr ContinueStatement)
(defmethod expr-write ContinueStatement [_] "continue")
(add-intrinsic 'break (fn [] (ContinueStatement.)))

(defrecord LabelStatement [label])
(derive-expr LabelStatement)
(defmethod expr-write LabelStatement
  [{:keys [label]}]
  (str (name label) ":"))
(defmethod expr-category LabelStatement [_] :statement)
(add-intrinsic 'label (fn [x] (LabelStatement. x)))

(defrecord GotoStatement [label])
(derive-expr GotoStatement)
(defmethod expr-write GotoStatement
  [{:keys [label]}]
  (str "goto" (name label)))
(defmethod expr-category GotoStatement [_] :statement)
(add-intrinsic 'goto (fn [x] (GotoStatement. x)))

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

(def ^:private set!-fn (get @cintrinsics "set!"))

(add-intrinsic
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
                 decl-expr (VariableDeclaration.
                            (name decl)
                            (name decl-type))]
             (scope-add (get-scope) decl-expr)
             (if (is-block? init-expr)
               (expr-wrap-last init-expr (fn [x] (AssignOp. "=" (VariableRefExpression. decl-expr) x)))
               (AssignOp. "=" (VariableRefExpression. decl-expr) init-expr)))))
       (map cstatement body-forms))))))

(defn get-var-type-tag [metadata]
  (let [tag (:tag metadata)]
    (if (string? tag) (keyword tag) tag)))

(defn- declare-fn [sym]
  (if-let [decl-type (get-var-type-tag (meta sym))]
    (do
      (scope-add
       (get-scope)
       (VariableDeclaration. (name sym) decl-type))
      (NopExpression.))
     (throw (ex-info
             (str "Unable to infer type for declare expression of symbol" sym)
             {:type ::declaration-type-inference-error
              :symbol sym}))))

(add-intrinsic 'declare declare-fn)

(add-intrinsic
 'def
 (fn def-fn [sym init-expr]
   (declare-fn sym)
   (set!-fn sym init-expr)))

(defn get-intrinsics []
  @cintrinsics)

(defrecord StaticArrayType [element-type-name array-length]
  clojure.lang.Named
  (getName [_] (str element-type-name "[" array-length "]")))
(derive-type StaticArrayType)
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

(defn parse-static-array-type [sym]
  (let [type-name (str sym)]
    (when-let [[_ type-name array-len] (re-matches #"(.*)!([0-9]*)" type-name)]
      (let [underlying-type (lookup-type sym)
            array-len (when-not (empty? array-len)
                        (int array-len))]
        (if array-len
          (StaticArrayType. underlying-type array-len)
          (PointerType. underlying-type))))))

(defn parse-pointer-type [sym]
  (let [type-name (str sym)]
    (when (= (last type-name) \*)
      (PointerType.
       (lookup-type (symbol (subs type-name 0 (dec (count type-name))))
                    :throw true)))))

(defrecord AnonymousFieldAccessExpression [instance-expr member-name pointer-depth])
(defmethod expr-write AnonymousFieldAccessExpression
  [{:keys [instance-expr member-name pointer-depth]}]
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
     (name member-name))))

(defrecord AnonymousType [type-name]
  clojure.lang.Named
  (getName [_] type-name))
(derive-type AnonymousType)
(defmethod type-create-field-access-expr AnonymousType
  ([this instance-expr member-name]
    (type-create-field-access-expr this instance-expr member-name 0))
  ([this instance-expr member-name pointer-depth]
    (AnonymousFieldAccessExpression. instance-expr member-name pointer-depth)))

(defrecord CLanguageScope [])

(defmethod scope-lookup-symbol
  CLanguageScope
  [_ sym]
  (get @cintrinsics (name sym)))

(defmethod scope-lookup-type
  CLanguageScope
  [_ type-name]
  (cond
   (is-type? type-name) type-name
   (keyword? type-name) (AnonymousType. (name type-name))
   :default
   (or
    (parse-static-array-type type-name)
    (parse-pointer-type type-name)
    (get @primitive-types (name type-name)))))

(defmethod scope-form->expr
  CLanguageScope
  [_ form]
  (cform->expr form))

(def default-c-preamble
  "#include <stddef.h>\n#include <stdint.h>\n#include <stdbool.h>\n")

(def default-cpp-preamble
  "#include <cstddef>\n#include <cstdint>\n")
