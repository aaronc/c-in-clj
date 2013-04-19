(ns c-in-clj.core
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [c-in-clj.platform :as platform])
  (:use
   [c-in-clj.lang]
   [c-in-clj.runtime]))

(defmacro csource-module [module-name & {:as opts}]
  `(def ~module-name
     (c-in-clj.runtime/create-module
      ~(name module-name)
      (constantly null-compile-context)
      (constantly null-loader-context)
      ~opts)))

(defmacro cpackage [module package-sym]
  (let [module (eval module)
        package-name (name package-sym)
        package
        (cond (isa? (type module) :c-in-clj.runtime/Module)
              (->Package package-name module (atom []) (atom {}) (atom #{}))
              (isa? (type module) :c-in-clj.runtime/RuntimeModule)
              (->RuntimePackage package-name module (atom {}) (atom #{})))]
    (add-package module package)
    `(def ~package-sym ~package)))

(defn cpackage-clone [module {:keys [declarations symbols
                                     referenced-packages package-name] :as package}]
  (when (isa? (type module) ::Module)
    (let [ndecls (atom [])
          nref-pkgs (atom #{})
          new-syms (atom {})
          new-pkg (->Package package-name module ndecls new-syms nref-pkgs)]
      (doseq [[k v] @symbols]
        (let [nsym (assoc v :package new-pkg)]
          (swap! new-syms assoc k nsym)))
      (doseq [decl @declarations]
        (let [ndecl (or (get @symbols (name decl))
                        (assoc decl :package new-pkg))]
          (swap! ndecls conj ndecl)))
      (doseq [{:keys [package-name]} @referenced-packages]
        (if-let [rpkg (get @(:packages module) package-name)]
          (swap! nref-pkgs conj rpkg)
          (throw (ex-info (str "In order to clone package " (name new-pkg) " package "
                               package-name " must be cloned as well.") {}))))
      (swap! (:packages module) assoc package-name new-pkg)
      new-pkg)))

(defn cinclude [package-or-include & {:as opts}]
  (let [package (get-package)]
    (when (satisfies? IPackage package-or-include)
      (swap! (:referenced-packages (get-package)) conj package-or-include))
    (when (dev-mode?)
      (if (satisfies? IPackage package-or-include)
        (let [referenced-pkg package-or-include
              decl (with-meta (->PackageIncludeDeclaration package referenced-pkg) opts) ]
          (add-declaration package decl)
          decl)
        (let [include-name package-or-include
              decl (with-meta (->IncludeDeclaration package include-name) opts)]
          (add-declaration package decl)
          decl)))))

(defn- parse-cfn [[func-name & forms]]
  (let [package (get-package)
        f1 (first forms)
        doc-str (when (string? f1) f1)
        params (if doc-str (second forms) f1)
        body-forms (if doc-str (nnext forms) (next forms))]
    (binding [*referenced-decls* #{}]
      (let [func-type (parse-fn-params params func-name)
            params (:params func-type)]
        (println func-type)
        (binding [*locals* (into {} (for [param params] [(name param) param]))
                  *local-decls* []]
          (let [func-metadata (meta func-name)
                ret-type (:return-type func-type)
                has-return (not (= "void" (name ret-type)))
                body-forms (or body-forms (when has-return [(->ReturnExpression nil)]))
                body-statements (vec (remove nil? (doall (map cstatement body-forms)))) 
                local-decls (vec (doall (map cstatement *local-decls*)))
                body-statements (concat local-decls body-statements)
                body-block (if (and (= 1 (count body-statements))
                                    (= :block (expr-category (first body-statements))))
                             (first body-statements)
                             (->BlockExpression body-statements))
                body-block
                (if (and has-return (not (:disable-default-return func-metadata)))
                  (wrap-last
                   body-block
                   (fn [expr]
                     (if (isa? (type expr) :c-in-clj.lang/ReturnExpression)
                       expr
                       (->ReturnExpression expr))))
                  body-block)
                func-metadata (if doc-str
                                (assoc func-metadata :doc doc-str)
                                func-metadata)
                func-name (name func-name)
                func-decl (with-meta (->FunctionDeclaration package func-name func-type body-block *referenced-decls* *locals*) func-metadata)]
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

(def ^:private save-id (atom 0))

(defn- output-dev-src [package decls cpp-mode preamble temp-output-path]
  (binding [*dynamic-compile* true
            *dynamic-compile-header* ""
            *referenced-decls* #{}]
    (let [anon-header (str/join "\n\n" (doall
                                        (remove
                                         nil?
                                         (for [decl @(:declarations package)]
                                           (when (or (not (name decl))
                                                     (isa? (type decl) :c-in-clj.lang/IncludeDeclaration))
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
      (->CompileSource src body filename))))

(defn- do-compile-decls [decls parse-fn load-fn]
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

(defn- compile-cfns [funcs]
  (do-compile-decls funcs #'parse-cfn #'load-cfn))

(defn- parse-cdef [[var-sym init-expr]]
  (let [package (get-package)
        metadata (meta var-sym)
        var-type (get-var-type-tag metadata)
        var-name (name var-sym)
        init-expr (when init-expr (cexpand init-expr))
        var-decl (with-meta (->GlobalVariableDeclaration package var-name var-type init-expr) metadata)]
    (println (meta var-sym))
    (add-declaration package var-decl)
    var-decl))

(defn- load-cdef [[var-name & _]])

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

(defn cdefns* [& fns]
  (compile-cfns fns))

(defmacro cdefns [& fns]
  (apply cdefns* fns))

(defn cdef* [& forms]
  (do-compile-decls [forms] parse-cdef load-cdef))

(defmacro cdef [& forms]
  (apply cdef* forms))

(defn cmacro* [macro-name func]
  (when (dev-mode?)
    (let [macro (->CMacro macro-name func)]
      (add-declaration (get-package) macro)
      macro)))

(defmacro cmacro [macro-name params & body]
  (when (dev-mode?)
    `(let [func#
           (fn ~(symbol (str macro-name "-macro")) ~params
             (c-in-clj.lang/unqualify-symbols (do ~@body)))]
       (c-in-clj.core/cmacro* ~(name macro-name) func#))))

(defn cstruct* [struct-name members]
  (let [package (get-package)
        fields (vec
                (for [[type-name field-name bits] members]
                  (->StructField (name field-name) type-name bits)))
        struct
        (with-meta
          (->Struct
           package
           (name struct-name)
           fields
           (into {} (for [f fields] [(name f) f])))
          (meta struct-name))]
    (add-declaration package struct)
    (when (dev-mode?)
      (println (write-decl struct)))))

(defmacro cstruct [name & members]
  (cstruct* name members))

(defn ctypedef* [target-type typedef-name metadata]
  (let [package (get-package)
        typedef-name (name typedef-name)
        target-type (lookup-type target-type)
        decl (with-meta (->TypeDef package typedef-name target-type) metadata)] 
    (add-declaration package decl)
    (when (dev-mode?)
      (println (write-decl decl)))))

(defmacro ctypedef
  ([target-type typedef-name]
     (ctypedef* target-type typedef-name nil))
  ([metadata target-type typedef-name]
     (ctypedef* target-type typedef-name metadata)))

(defmacro ctypedeffn [typedef-name params]
  (let [metadata (meta typedef-name)
        func-type (parse-fn-params params typedef-name)
        func-ptr-type (->PointerType func-type)]
    (ctypedef* func-ptr-type typedef-name metadata)))

(defn cenum* [enum-name values]
  (when (dev-mode?)
    (let [package (get-package)
          enum-name (name enum-name)
          values (partition 2 values)
          base-type (lookup-type "i32")
          values (map #(->EnumValue (name (first %))
                                   (second %)
                                   base-type
                                   enum-name)
                      values)
          enum (->EnumType package enum-name values)]
      (add-declaration package enum)
      (doseq [v values]
        (add-declaration package v))
      (println (write-decl enum)))))

(defmacro cenum [enum-name values]
  (cenum* enum-name values))

(defn cdefine [macro-name body]
  (when (dev-mode?)
    (let [raw-macro (->RawCMacro (get-package) macro-name body)]
      (add-declaration (get-package) raw-macro)
      (println (write-decl raw-macro)))))

(defn cheader [raw-header]
  (when (dev-mode?)
    (add-declaration (get-package) (->RawCHeader (get-package) raw-header))))

(defn write-package-source [{:keys [declarations module] :as package} & {:keys [src-output-path cpp-mode]}]
  (binding [*package* package]
    (let [{:keys [preamble]} module
          pkg-name (name package)
          header-name (str pkg-name ".h")
          include-guard-name (str/upper-case
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
                                (write-impl decl)))))
           "\n")]
      (when src-output-path
        (let [decl-path (platform/path-combine src-output-path header-name)
              impl-path (platform/path-combine src-output-path
                                      (str pkg-name
                                           (if cpp-mode ".cpp" ".c")))]

          (println "Writing" decl-path)
          (println "Writing" impl-path)
          (platform/write-text-file decl-path decl-src)
          (platform/write-text-file impl-path impl-src))))))

(defn write-module-source [{:keys [packages cpp-mode src-output-path] :as module}]
  (doseq [[_ pkg] @packages]
    (write-package-source pkg :cpp-mode cpp-mode :src-output-path src-output-path)))
