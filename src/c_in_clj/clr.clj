(ns c-in-clj.clr
  (:import [System.Diagnostics Process ProcessStartInfo]
           [System.IO Path File Directory]
           [System.Runtime.InteropServices Marshal GCHandle GCHandleType])
  (:require [clojure.string :as str])
  (:use [c-in-clj.core]
        [clojure.clr pinvoke emit]))

(defn- write-file [name text]
  (File/WriteAllText name text))

(dllimports "kernel32.dll"
            (LoadLibrary IntPtr [String])
            (GetProcAddress IntPtr [IntPtr String])
            (FreeLibrary nil [IntPtr]))

(def ^:dynamic *msvc-compiler-context*)

(def vc-root "C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\VC")

(def ^:private dg-type-cache (atom {}))

(defn run-cl [work-dir cl-path args body text]
  (let [psi (ProcessStartInfo. cl-path args)]
    (.set_UseShellExecute psi false)
    (.set_RedirectStandardOutput psi true)
    (.set_CreateNoWindow psi true)
    (.set_WorkingDirectory psi work-dir)
    (let [process (Process/Start psi)
          output (.ReadToEnd (.StandardOutput process))]
      (.WaitForExit process)
      (let [exit-code (.ExitCode process)]
        (if (not= 0 exit-code)
          (do
            (println)
            (print-numbered text)
            (println output)
            (println "Exited with " exit-code))
          (println body))
        exit-code))))

(defn clean-fn-files [fname]
  (try
    (File/Delete (Path/ChangeExtension fname ".dll"))
    (File/Delete (Path/ChangeExtension fname ".c"))
    (File/Delete (Path/ChangeExtension fname ".cpp"))
    (File/Delete (Path/ChangeExtension fname ".obj"))
    (catch Exception ex
      (println ex))))

(defn- count-args-size [args]
  (when (even? (count args))
    (let [param-types (map get-clr-type (take-nth 2 args))]
      (reduce + (map #(let [^Type t %] (Marshal/SizeOf t)) param-types)))))

(defn get-proc-name [name args]
  (let [args-size (count-args-size args)]
    (if args-size
      (str "_" name "@" args-size)
      name)))

(defn update-fn-ref [name ret args fname existing]
  (let [dll-path (Path/ChangeExtension fname ".dll")
        new-dll-handle (LoadLibrary dll-path)
        proc-name (get-proc-name name args)
        new-fn-ptr (GetProcAddress new-dll-handle proc-name)]
    (when (= IntPtr/Zero new-fn-ptr)
      (throw (Exception. (str "Unable to load " proc-name))))
    (println "Loaded" proc-name "at" new-fn-ptr)
    (let [dir-name (Path/GetDirectoryName fname)
          cache-file-name (Path/Combine dir-name name)]
      (write-file cache-file-name fname))
    (if existing
      (let [fn-ptr (:fn-ptr existing)
            fn-ptr-ptr (:fn-ptr-ptr existing)
            dll-handle (:dll-handle existing)
            last-fname (:filename existing)
            invoker (:invoker existing)]
        (reset! invoker nil)
        (reset! fn-ptr new-fn-ptr)
        (Marshal/WriteIntPtr fn-ptr-ptr new-fn-ptr)
        (FreeLibrary dll-handle)
        ;(clean-fn-files last-fname)
        (assoc existing :filename fname :dll-handle new-dll-handle
               :return ret :params args))
      (let [fn-ptr-ptr (Marshal/AllocHGlobal IntPtr/Size)]
        (Marshal/WriteIntPtr fn-ptr-ptr new-fn-ptr)
        {:fn-ptr (atom new-fn-ptr)
         :fn-ptr-ptr fn-ptr-ptr
         :dll-handle new-dll-handle
         :filename fname
         :return ret
         :params args
         :invoker (atom nil)}))))

(defn- get-dg-type [ret-type param-types]
  (let [dg-sig (into [ret-type] param-types)]
    (or (@dg-type-cache dg-sig)
        (let [dg-type (clr-delegate* ret-type param-types)]
          (swap! dg-type-cache assoc dg-sig dg-type)
          dg-type))))

(defn- make-invoker [fn-ptr ret args]
  (binding [*header* ""
            *local-def-types* {}]
    (when (even? (count args))
      (let [ret-type (get-clr-type ret)
            param-types (map get-clr-type (take-nth 2 args))
            dg-type (get-dg-type ret-type param-types)
            dg (Marshal/GetDelegateForFunctionPointer fn-ptr dg-type)
            invoke-method (.GetMethod dg-type "Invoke")]
        (fn [& args]
          (.Invoke invoke-method dg (to-array args)))))))

(defn get-invoker [ctxt name]
  (when-let [fn-info (@(:fn-index ctxt) name)]
    (make-invoker @(:fn-ptr fn-info) (:return fn-info) (:params fn-info))))

(defmacro gen-c-delegate [ret params args & body]
  (let [dg-type (get-dg-type (eval ret) (eval params))]
    `(let [dg# (gen-delegate ~dg-type ~args ~@body)
           gch# (System.Runtime.InteropServices.GCHandle/Alloc
                 dg#)
           fp# (System.Runtime.InteropServices.Marshal/GetFunctionPointerForDelegate dg#)]
       {:dg dg#
        :gch gch#
        :fp fp#})))

(defn msvc-compile-file [{:keys [compile-path cl-path] :as ctxt} filename text body on-compile]
  (write-file filename text)
  (if (= 0 (run-cl compile-path cl-path (str "user32.lib gdi32.lib /EHs /Gz " "/LD " filename) body text))
    (on-compile)
    (clean-fn-files filename)))

(defn msvc-on-compile [ctxt filename name ret args]
  (let [fn-index (:fn-index ctxt)
        fn-info (update-fn-ref name ret args filename (@fn-index name))
        invoker (:invoker fn-info)]
    (swap! fn-index assoc name fn-info)
    (with-meta
      (fn [& args]
        (if-let [ivk @invoker]
          (apply ivk args)
          (let [ivk (reset! invoker (get-invoker ctxt name))]
            (apply ivk args))))
      {:fn-info fn-info})))

(defn check-cache [ctxt name text ret args]
  (when (not (contains? @(:fn-index ctxt) name))
    (println "Checking cache for" name)
    (let [compile-path (:compile-path ctxt)
          cache-file-name (Path/Combine compile-path name)]
      (when (File/Exists cache-file-name)
        (let [cached-c-file-name (slurp cache-file-name :encoding "UTF8")
              cached-text (when (File/Exists cached-c-file-name) (slurp cached-c-file-name :encoding "UTF8"))]
          (when (= cached-text text)
            (println "Loading" name "from cache")
            (msvc-on-compile
             ctxt cached-c-file-name name ret args)))))))

(defn msvc-compile [ctxt name ret args body]
  (let [name (str name)
        compile-path (:compile-path ctxt)
        filename (Path/Combine compile-path (str name "__" (swap! (:save-id ctxt) inc) ".cpp"))
        text (str *header* "\n\nextern \"C\" __declspec(dllexport) " body)]
    (or
     (check-cache ctxt name text ret args)
     (msvc-compile-file
      ctxt
      filename
      text
      body
      (fn [] (msvc-on-compile ctxt filename name ret args))))))

(defn msvc-compile-multiple [ctxt funcs]
  (let [body (str/join "\n\n" (map (fn [x] (str "extern \"C\" __declspec(dllexport) " (:body x))) funcs))
        name (str/join "_" (map :name funcs))
        compile-path (:compile-path ctxt)
        filename (Path/Combine compile-path (str name "__" (swap! (:save-id ctxt) inc) ".cpp"))
        text (str *header* body)]
    (msvc-compile-file ctxt filename text body
                       (fn []
                         (reduce
                          (fn [res {:keys [name ret args]}]
                            (assoc res name (msvc-on-compile ctxt filename
                                                             (str name) ret args)))
                          {} funcs)))))

(defn msvc-resolvesym [ctxt sym]
  (let [fn-name (name sym)]
    (when-let [fn-info (@(:fn-index ctxt) fn-name)]
      (let [{:keys [fn-ptr-ptr return params]} fn-info
            cret (get-ctype return)
            cparams (map get-ctype (take-nth 2 params))
            param-sig (str/join "," cparams)
            fn-const (str "(*(" cret "(**)(" param-sig "))" fn-ptr-ptr ")")
            fn-def (str "#define " fn-name " " fn-const "\n")]
        (set! *header* (str *header* fn-def))
        sym))))

(defn msvc-clean ([this]) ([this func]))

(defrecord MSVCDevContext [compile-path cl-path save-id fn-index])

(extend MSVCDevContext
  ICModuleContext
  {:init-module (constantly nil)
   :compilefn #'msvc-compile
   :compilefns #'msvc-compile-multiple
   :resolve-sym #'msvc-resolvesym
   :clean #'msvc-clean})

(defn init-msvc-context [& {:keys [msvc-path compile-path]}]
  (let [compile-path (or compile-path (Path/Combine (Path/GetTempPath) "c-in-clj"))
        msvc-path (or msvc-path vc-root)
        cl-bat-path (Path/Combine compile-path "cl.bat")
        x64 (= IntPtr/Size 8)]
    (when-not (Directory/Exists compile-path)
      (Directory/CreateDirectory compile-path))
    (write-file cl-bat-path
          (String/Format
           "@echo off\r\ncall \"{0}\" {1}\r\n{2} %*\r\n"
           (Path/Combine msvc-path "vcvarsall.bat")
           (if x64 "x64" "x86")
           "cl.exe"))
    (MSVCDevContext. compile-path cl-bat-path (atom 0) (atom {}))))

(reset! *cmodule-context* (init-msvc-context))

(defn compile-init-module [{:keys [modules]} module-name package-name]
  (swap! modules assoc-in [module-name package-name] {:header-text "" :impl-text ""}))

(defrecord MSVCCompileContext [modules])

(defn compile-init-context []
  (MSVCCompileContext. (atom {})))

(defn dll-init-module [{:keys [modules] :as ctxt} module-name package-name]
  (let [module (or (get modules module-name)
                   (let [dll (LoadLibrary module-name)
                         module {:dll dll}]
                     (swap! modules assoc module-name module)
                     module))]
    (reset! (:cur-module ctxt) module)))

(defn dll-load-fn [{:keys [cur-module]} name ret args body]
  (let [proc-name (get-proc-name name args)
        proc-address (GetProcAddress (:dll cur-module) proc-name)]
    (make-invoker)))

(defn dll-load-fns [ctxt funcs]
  (into {}
        (for [{:keys [name ret args body]} funcs]
          [name (dll-load-fn ctxt name ret args body)])))

(defn dll-resolve-sym [ctxt sym] sym)

(defn dll-clean [{:keys [modules cur-module]}]
  (reset! cur-module nil)
  (doseq [[name mod] @modules]
    (FreeLibrary mod))
  (reset! cur-module nil))

(defrecord DllImportContext [modules cur-module])

(extend DllImportContext
  ICModuleContext
  {:init-module #'dll-init-module
   :compilefn #'dll-load-fn
   :compilefns #'dll-load-fns
   :resolve-sym #'dll-resolve-sym
   :clean (constantly nil)})

(defn dll-init-context []
  (DllImportContext. (atom {}) (atom nil)))

