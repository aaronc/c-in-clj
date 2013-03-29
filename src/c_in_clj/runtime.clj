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

(def ^:dynamic *dynamic-compile* false)

(defrecord CompileSource [source body-source filename])

(defn print-numbered [txt]
  (let [lines (str/split-lines txt)]
    (doall
     (map-indexed
      (fn [i line] (println (str i "  " line))) lines))))

