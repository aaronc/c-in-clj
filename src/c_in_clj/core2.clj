(ns c-in-clj.core
  (:use [c-in-clj.runtime2]
        [c-in-clj.lang api intrinsics declarations]))

(defn create-c-module [module-name init-compile-ctxt-fn init-load-ctxt-fn opts]
  (create-generic-module
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

(defmacro csource-module [module-name & {:as opts}]
  `(def ~module-name
     (c-in-clj.core/create-c-module
      ~(name module-name)
      (constantly nil)
      (constantly nil)
      ~opts)))

(defmacro cpackage [module package-sym]
  `(def ~package-sym
     (c-in-clj.runtime2/create-package ~module ~(name package-sym))))


(defn cdefn* )


