(ns c-in-clj.core2
  (:use [c-in-clj.runtime2]
        [c-in-clj.lang api intrinsics declarations]))

(defn create-c-module [module-name init-compile-ctxt-fn init-load-ctxt-fn opts]
  (create-module
   module-name
   init-compile-ctxt-fn
   init-load-ctxt-fn
   opts
   (fn [] (->CLanguageScope))
   (fn setup-c-opts [opts]
     (let [opts (merge
                 {:cpp-mode false}
                 opts)
           opts (merge {:preamble (if (:cpp-mode opts)
                                    default-cpp-preamble
                                    default-c-preamble)}
                       opts)]
       opts))))

(defrecord CSourceModuleContext [])
(defmethod scope-add CSourceModuleContext [ctxt decl]
  (println (decl-write-impl decl))
  decl)

(defmacro csource-module [module-name & {:as opts}]
  `(def ~module-name
     (c-in-clj.core2/create-c-module
      ~(name module-name)
      (fn [opts#] (->CSourceModuleContext))
      (constantly nil)
      ~opts)))

(defmacro cpackage [module package-sym]
  `(def ~package-sym
     (c-in-clj.runtime2/create-package ~module ~(name package-sym))))

(defn- parse-fn-params
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
                 (with-meta (->FunctionParameter param-name (->VarArgsType)) metadata)
                 (let [param-type (get-var-type-tag metadata)]
                   (with-meta (->FunctionParameter param-name param-type) metadata)))))]
       (with-meta (->FunctionType (name fn-name) ret-type params) params-meta))))

(defn cdefn* [[func-name & forms]]
  (let [package (get-ns-scope)
        f1 (first forms)
        doc-str (when (string? f1) f1)
        params (if doc-str (second forms) f1)
        body-forms (if doc-str (nnext forms) (next forms))]
    (let [func-type (parse-fn-params params func-name)
          params (:params func-type)]
      (binding [*scope* (->LocalVariableScope
                         package
                         (atom (into {} (for [param params]
                                          [(symbol (get-name param)) param]))))]
        (let [func-metadata (meta func-name)
              ret-type (:return-type func-type)
              has-return (not (= "void" (name ret-type)))
              body-forms (or body-forms (when has-return [(->ReturnExpression nil)]))
              body-statements (vec (remove nil? (doall (map cstatement body-forms))))
              body-block (if (and (= 1 (count body-statements))
                                  (= :block (expr-category (first body-statements))))
                           (first body-statements)
                           (->BlockExpression body-statements))
              body-block
              (if (and has-return (not (:disable-default-return func-metadata)))
                (expr-wrap-last
                 body-block
                 (fn return-wrapper [expr]
                   (if (isa? (type expr) :c-in-clj.lang/ReturnExpression)
                     expr
                     (->ReturnExpression expr))))
                body-block)
              func-metadata (if doc-str
                              (assoc func-metadata :doc doc-str)
                              func-metadata)
              func-name (name func-name)
              func-decl (with-meta (->FunctionDeclaration func-name func-type body-block nil *scope*) func-metadata)]
          (scope-add package func-decl))))))

(defmacro cdefn [& forms]
  (cdefn* forms))

(defn cdef* [[var-sym init-expr]]
  (let [package (get-ns-scope)
        metadata (meta var-sym)
        var-type (get-var-type-tag metadata)
        var-name (name var-sym)
        init-expr (when init-expr (->expr init-expr))
        var-decl (with-meta (->GlobalVariableDeclaration package var-name var-type init-expr) metadata)]
    (scope-add package var-decl)))

(defmacro cdef [& forms]
  (cdef* forms))

;; (csource-module TestModule :dev true)

;; (cpackage TestModule TestPackage)

;; (cdefn test1 ^i32 [^i32 a ^i32 b]
;;        (+ a b))

;; (cdef ^i32 x 0)

