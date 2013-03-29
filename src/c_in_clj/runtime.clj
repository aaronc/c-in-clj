(ns c-in-clj.runtime
  (:use
   [c-in-clj.lang])
  (:require
   [clojure.string :as str]
   [c-in-clj.platform :as platform]))

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

(def default-c-preamble
  "#include <stddef.h>\n#include <stdint.h>\n#include <stdbool.h>\n")

(def default-cpp-preamble
  "#include <cstddef>\n#include <cstdint>\n")


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

(defn- look-in-referenced-packages [referenced-packages target-sym]
  (loop [[rp & more] (vec @referenced-packages)]
    (when rp
      (if-let [decl (get @(:symbols rp) target-sym)]
        (if-not (:private (meta decl))
          decl
          (recur more))
        (recur more)))))

(defn- find-index-of [items pred]
  (loop [[item & more] items
         idx 0]
    (when item
      (if (pred item)
        idx
        (recur more (inc idx))))))

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

(def ^:dynamic *c-in-clj-dev-mode* false)

(defn dev-env? []
  (or *c-in-clj-dev-mode* (when-let [dev-var (Environment/GetEnvironmentVariable "C_IN_CLJ_DEV")]
                        (not= "0" dev-var))))

(defn dev-mode?
  ([]
     (if-let [pkg (get-package)]
       (dev-mode? (get-module))
       (dev-env?)))
  ([module] (instance? Module module)))

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

(def ^:dynamic *dynamic-compile* false)

(defrecord CompileSource [source body-source filename])

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

(defn parse-cdef [[var-name init-expr]]
  (let [package (get-package)
        metadata (meta var-name)
        var-name (name var-name)
        var-type (get-var-type-tag var-name)
        init-expr (when init-expr (cexpand init-expr))
        var-decl (with-meta (->GlobalVariableDeclaration package var-name var-type init-expr) metadata)]
    (add-declaration package var-decl)
    var-decl))

(defn load-cdef [[var-name & _]])


