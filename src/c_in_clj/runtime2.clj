(ns c-in-clj.runtime2
  (:use
   [c-in-clj.lang.api])
  (:require
   [clojure.string :as str]
   [c-in-clj.platform :as platform]))

;;;; Module

(derive-scope ::Module)

(defn derive-module [cls] (derive cls ::Module))

(defn is-module? [x] (is-an-instance? x ::Module))

(defrecord Module [module-name compiler packages lang-scope])

(derive-module Module)

(defmethod+ get-name Module [{:keys [module-name]}] module-name)

(defmethod+ scope-add Module
  [{:keys [compiler]} decl]
  (if compiler
    (scope-add compiler decl)
    decl))

(defmethod+ scope-lookup-symbol Module
  [{:keys [lang-scope]} sym]
  (scope-lookup-symbol lang-scope sym))

(defmethod+ scope-lookup-type Module
  [{:keys [lang-scope]} sym]
  (scope-lookup-type lang-scope sym))

(defmethod+ scope-form->expr Module
  [{:keys [lang-scope]} form]
  (scope-form->expr lang-scope form))

(defmethod+ print-method Module [o w]
  (print-simple (str "#" o (select-keys o [:name])) w))

;;;; NSScope

(defrecord NSScope [package-name module declarations package-ns])

(defn- find-index-of [items pred]
  (loop [[item & more] items
         idx 0]
    (when item
      (if (pred item)
        idx
        (recur more (inc idx))))))

(defmethod+ scope-add NSScope
  [{:keys [module package-ns declarations]} decl]
  (let [processed (scope-add module decl)
        decl-name (get-name decl)]
    (when processed
      (when (is-decl? decl)
        (let [existing-idx (find-index-of @declarations #(= (get-name %) decl-name))]
          (if existing-idx
            (swap! declarations assoc existing-idx decl)
            (swap! declarations conj decl))))
      (when decl-name
        (intern package-ns
                (with-meta (symbol decl-name)
                  {::Declaration decl})
                processed)))))

(defn- lookup-defined-symbol [package-ns sym]
  (when-let [resolved (ns-resolve package-ns sym)]
     (::Declaration (meta resolved))))

(defn- lookup-defined-type [package-ns sym]
  (if (is-type? sym)
    sym
    (when-let [resolved (lookup-defined-symbol package-ns sym)]
      (when (is-type? resolved)
        resolved))))

(defmethod+ scope-lookup-symbol NSScope
  [{:keys [package-ns module]} sym]
  (or
   (lookup-defined-symbol package-ns sym)
   (scope-lookup-symbol module sym)))

(defmethod+ scope-lookup-type NSScope
  [{:keys [package-ns module]} sym]
  (or
   (lookup-defined-type package-ns sym)
   (scope-lookup-type module sym)))

(defmethod+ scope-form->expr NSScope
  [{:keys [module]} form]
  (scope-form->expr module form))

(defmethod+ print-method NSScope [o w]
  (print-simple (str "#" o (select-keys o [:package-name :module])) w))

;; Development mode stuff

(def ^:dynamic *c-in-clj-dev-mode* false)

(defn dev-env? []
  (or *c-in-clj-dev-mode*
      (when-let [dev-var (platform/get-env-variable "C_IN_CLJ_DEV")]
        (not= "0" dev-var))))

(defn dev-mode?
  ([]
     (if-let [pkg (get-ns-scope)]
       (dev-mode? (:module pkg))
       (dev-env?)))
  ([module] (instance? Module module)))

;; Runtime Modules

(defrecord RuntimeModule [module-name loader packages])

(defmethod+ print-method RuntimeModule [o w]
  (print-simple (str "#" o (select-keys o [:module-name :loader])) w))

(defrecord RuntimeNSScope [package-name module symbols referenced-packages]
  clojure.lang.Named
  (getName [_] package-name)
  ;; INSScope
  ;; (add-declaration [this decl]
  ;;   (when-let [decl-name (name decl)]
  ;;     (swap! symbols assoc decl-name decl)))
  ;; ISymbolScope
  ;; (resolve-symbol [_ sym-name]
  ;;   (or (get @symbols sym-name)
  ;;       (look-in-referenced-packages referenced-packages
  ;;                                    sym-name)
  ;;       (resolve-symbol module sym-name)))
  )

(defmethod+ print-method RuntimeNSScope [o w]
  (print-simple (str "#" o (select-keys o [:package-name :module])) w))

;; Module creation

(defn create-module [module-name init-compile-ctxt-fn init-load-ctxt-fn
                             {:keys [dev] :as opts}
                             lang-scope-fn
                             opts-fn]
  (if (if (not (nil? dev)) dev (dev-env?))
    (let [opts (opts-fn opts)]
      (Module. module-name
               (init-compile-ctxt-fn opts)
               (atom {})
               (lang-scope-fn)
               nil
               opts))
    (RuntimeModule. module-name (init-load-ctxt-fn opts) (atom {}))))

(defn create-package [{:keys [packages] :as module} package-name]
  (let [package (cond (is-module? module)
                      (NSScope. package-name module (atom []) *ns*))]
    (swap! packages assoc package-name package)
    (set-ns-scope package)
    package))
