(ns c-in-clj.type-system
  (:use [c-in-clj.core]
        [c-in-clj.lang]
        [c-in-clj.msvc])
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defrecord InterfaceMethod [method-name fn-type default-impl])

(defrecord InterfaceMethodGroup [method-name overload-map])

(defrecord Interface [package type-name extends method-map vtable-struct interface-field field-map]
  clojure.lang.Named
  (getName [_] type-name)
  IDeclaration
  (write-decl [this]
    (str "typedef struct " type-name " {\n"
        (write-struct-field interface-field) 
       "} " type-name ";"))
  (write-impl [_])
  IType)

(derive Interface :c-in-clj.core/Type)

(derive Interface :c-in-clj.core/Struct)

(defn- parse-interface-overload [method-name overload]
  (cond
   (vector? overload)
   (let [overload-meta (meta overload)
         this-symbol (with-meta (symbol "__this") {:tag 'void*})
         overload (with-meta (cons this-symbol overload) overload-meta)
         fn-type (parse-fn-params overload method-name)
         metadata (meta overload)]
     (InterfaceMethod. (name method-name) fn-type nil metadata nil))
   (list? overload)
   (throw (ex-info "Default interface implementation not supported" {:method-name method-name :overload overload}))
   :default
   (throw (ex-info "Expected vector or list for interface method specificiation" {:method-name method-name :overload overload}))))

(defn- arity [overload]
  (count (get-in overload [:fn-type :params])))

(defn- parse-interface-method [[method-name & overloads]]
  (let [metadata (meta method-name)
        overloads (map (partial parse-interface-overload method-name) overloads)
        overload-arity-map
        (into {} (for [o overloads] [(arity o) o]))]
    (InterfaceMethodGroup. (name method-name) overload-arity-map metadata nil)))

(defn- create-vtable-overload-field [{:keys [method-name fn-type]} single-arity?]
  (let [field-name (if single-arity?
                     method-name
                     (str method-name "__" (count (:params fn-type))))]
    (->StructField field-name (->PointerType fn-type) nil)))

(defn- create-interface-vtable-struct [interface-name extends method-map]
  (let [extends-fields (map #(with-meta
                               (->StructField (str % "_OFFSET")
                                                 'size_t nil)
                               {:offset-for %}) extends)
        fields
        (apply
         concat
         extends-fields
         (for [[mname {:keys [overload-map]}] method-map]
           (if (= (count overload-map) 1)
             [(create-vtable-overload-field
               (second (first overload-map)) true)]
             (map #(create-vtable-overload-field
                    (second %) false)
                  overload-map))))
        field-map (into {} (for [{:keys [name] :as f} fields]
                             [name f]))
        struct (->Struct (get-package) (str interface-name "Interface") fields field-map)]
    (add-declaration (get-package) struct)
    (println (write-decl struct))
    struct))

(defn- create-interface-field [vtable-struct]
  (let [name (name vtable-struct)
        type (->PointerType vtable-struct)]
    (->StructField (str "_" name) type nil)))

(defn cdefinterface* [interface-name extends methods]
  (when (dev-mode?)
    (let [interface-name (name interface-name)
          extends (map lookup-type extends)
          extends (set/difference (set (map name extends))
                                  (apply set/union
                                         (map :extends extends)))
          methods (map parse-interface-method methods)
          method-map (into {} (for [m methods]
                                [(:method-name m) m]))
          vtable-struct (create-interface-vtable-struct interface-name extends method-map)
          interface-field (create-interface-field vtable-struct)
          interface (Interface. (get-package) interface-name extends method-map vtable-struct interface-field nil)]
      (add-declaration (get-package) interface)
      (println (write-decl interface)))))

(defn- parse-interface-class-body [body]
  (let [extends? (first body)
        extends (when (vector? extends?) extends?)
        body (if extends (rest body) body)]
    [extends body]))

(defmacro cdefinterface [name & body]
  (apply cdefinterface* name (parse-interface-class-body body)))

(defn- write-class-body [{:keys [members base-class new-interfaces]}]
  (str
   (when base-class
     (write-class-body (lookup-type base-class)))
   (str/join
    (map write-struct-field members))
   (str/join
    (map write-struct-field (map :interface-field
                                 (map lookup-type new-interfaces))))))

(defrecord InterfaceVTable [class-name interface-name fn-map])

(defrecord Class [package type-name base-class interfaces new-interfaces members
                  vtables field-map]
  clojure.lang.Named
  (getName [_] type-name)
  IDeclaration
  (write-decl [this]
    (str "typedef struct " type-name " {\n"
         (write-class-body this)
         "} " type-name ";"))
  (write-impl [_])
  IType)

(derive Class :c-in-clj.core/Type)
(derive Class :c-in-clj.core/Struct)

(defmethod default-initializer Class [cls] "{}")

(defmethod requires-initialization Class [cls] true)

(defn- get-all-extends [interface]
  (let [interface (lookup-type interface)
        extends (:extends interface)]
    (apply set/union
           extends
           (map get-all-extends extends))))

(defn- get-new-all-interfaces [base-class interfaces]
  (if base-class
    (let [base-interfaces (set (:interfaces (lookup-type base-class)))
          interfaces (set (map lookup-type interfaces))
          interfaces (apply set/union
                            (set (map name interfaces))
                            (map get-all-extends interfaces))]
      [(vec (set/difference interfaces base-interfaces))
       (vec (set/union interfaces base-interfaces))])
    [interfaces interfaces]))

(defn- class-interface-offset-name [class-name iface-name]
  (str class-name "_" iface-name "_" "Offset"))

(defn- write-vtable-init [vtable-struct {:keys [class-name interface-name]}]
  (let [cls (lookup-type class-name)]
    (str "{" (str/join
              ","
              (for [f (:fields vtable-struct)]
                (let [{:keys [offset-for]} (meta f)]
                  (if offset-for
                    (let [offset-macro-name (class-interface-offset-name class-name interface-name)
                          offset-macro (lookup-symbol offset-macro-name)]
                      offset-macro-name)
                    "0")))) "}")))

(defrecord VTableInitExpression [vtable-struct interface-vtable]
  IHasType
  (get-type [_] (lookup-type vtable-struct))
  IExpression
  (write-expr [this]
    (write-vtable-init vtable-struct interface-vtable)))

(defn cdefclass* [class-name extends members]
  (let [metadata (meta class-name)
        class-name (name class-name)
        extends (map lookup-type extends)
        base-class? (first extends)
        base-class (when (instance? Class base-class?)
                     base-class?)
        interfaces (if base-class (rest extends) extends)
        interfaces (map name interfaces)
        base-class (when base-class (name base-class))
        [new-interfaces interfaces]
        (get-new-all-interfaces base-class interfaces)
        members (for [[type-name field-name bits] members]
                  (->StructField (name field-name) type-name bits))
        vtables (zipmap interfaces (map #(InterfaceVTable. class-name % (atom {}))
                                        interfaces))
        cls (Class. (get-package) class-name base-class interfaces
                    new-interfaces members vtables nil metadata nil)]
    (add-declaration (get-package) cls)
    (doseq [{:keys [interface-name interface-field vtable-struct] :as iface} (map lookup-type interfaces)]
      (let [vtable (get vtables interface-name)]
        (cdef* (with-meta (symbol (str class-name "_" (name vtable-struct)))
                 {:tag (symbol (name vtable-struct))})
               (VTableInitExpression. vtable-struct vtable)))
      (cdefine (class-interface-offset-name class-name (name iface))
               (str "offsetof(" class-name "," (name interface-field) ")")))
    (println (write-decl cls))))

(defmacro cdefclass [name & body]
  (apply cdefclass* name (parse-interface-class-body body)))

(defn cvirtualfn* [name target-class args body])

(defmacro cvirtualfn [name target-class args & body])

(msvc-module TestTypeSystem :dev true)

(cpackage TestTypeSystem Test1)

(cdefn test1 ^void [])

(cdefinterface ITest1
               (^void test1 [^i32 a])
               (test2 ^void [^i32 a] ^i32 [^i32 b ^i32 c])
               ;;(^i32 test2 ([] 0))
               )

(cdefinterface ITest2 [ITest1]
               (abc1 ^void []))

(cdefinterface ITest3 [ITest2 ITest1]
               (def1 ^void []))

(cdefinterface ITest4 [ITest3])

(cdefclass TestA [ITest1]
           (i32 x))

(cdefclass TestB [TestA ITest4]
           (double y))

(cdefn test2 ^void []
       (declare ^TestA a))

(cvirtualfn
 test2 TestB [b c]
 (+ b c))

