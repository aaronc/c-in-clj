(ns c-in-clj.msvc
  (:import [System.Diagnostics Process ProcessStartInfo]
           [System.IO Path File Directory]
           [System.Runtime.InteropServices Marshal GCHandle GCHandleType])
  (:require [clojure.string :as str])
  (:use [c-in-clj.core]
        [clojure.clr pinvoke emit]))

(dllimports "kernel32.dll"
            (LoadLibrary IntPtr [String])
            (GetProcAddress IntPtr [IntPtr String])
            (FreeLibrary nil [IntPtr]))

(def default-msvc-path (Environment/ExpandEnvironmentVariables "%PROGRAMFILES(x86)%\\Microsoft Visual Studio 10.0\\VC"))

(def default-msvc-args "user32.lib gdi32.lib kernel32.lib /EHs /Gz /LD")

(def ^:private dg-type-cache (atom {}))

(defn run-cl [work-dir cl-path cl-args filename src]
  (let [args (str cl-args " " filename)
        psi (ProcessStartInfo. cl-path args)]
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
            (print-numbered src)
            (println output)
            (println "Exited with " exit-code))
          true)))))

(defrecord DynamicDllInfo [dll-handle source-file references])

(defrecord CompiledSymbolRef [symbol-name fn-ptr-ptr cur-decl cur-fn-ptr cur-dll-info invoker])

(defn get-clr-type [ctype]
  (if (is-reference-type? ctype)
    IntPtr
    (let [type-name (name ctype)]
      (case type-name
        "int8_t" SByte
        "int16_t" Int16
        "int32_t" Int32
        "int64_t" Int64
        "uint8_t" Byte
        "uint16_t" UInt16
        "uint32_t" UInt32
        "uint64_t" UInt64
        "bool" Boolean
        "size_t" UIntPtr))))

(defn get-clr-params [params]
  (map (comp get-clr-type get-type) params))

(defn- get-proc-name [decl]
  (let [decl-type (get-type decl)]
    (if (is-function-type? decl-type)
      (let [{:keys [params]} decl-type
            clr-params (get-clr-params params)
            args-size (reduce + (map #(min (let [^Type t %] (Marshal/SizeOf t)) 4) clr-params))]
        (str "_" (name decl) "@" args-size)))))

(defn- get-dg-type [ret-type param-types]
  (let [dg-sig (into [ret-type] param-types)]
    (or (@dg-type-cache dg-sig)
        (let [dg-type (clr-delegate* ret-type param-types)]
          (swap! dg-type-cache assoc dg-sig dg-type)
          dg-type))))

(defn- make-invoker [fn-ptr decl]
  (let [decl-type (get-type decl)]
    (if (is-function-type? decl-type)
      (let [{:keys [params return-type]} decl-type
            clr-ret (get-clr-type return-type)
            clr-params (map (comp get-clr-type get-type) params)
            dg-type (get-dg-type clr-ret clr-params)
            dg (Marshal/GetDelegateForFunctionPointer fn-ptr dg-type)
            invoke-method (.GetMethod dg-type "Invoke")]
        (fn [& args]
          (.Invoke invoke-method dg (to-array args)))))))

(defn- msvc-compile-decls [{:keys [temp-output-path cl-args cl-bat-path compiled-symbols dll-handles] :as ctxt} decls {:keys [source filename]}]
  (when (run-cl temp-output-path
                cl-bat-path
                cl-args
                filename
                source)
    (let [dll-path (Path/ChangeExtension filename ".dll")
          new-dll-handle (LoadLibrary dll-path)
          new-dll-info (DynamicDllInfo. new-dll-handle filename (atom #{}))
          sym-refs
          (doall
           (for [decl decls]
             (let [sym-name (name decl)
                   proc-name (get-proc-name decl)
                   new-fn-ptr (GetProcAddress new-dll-handle proc-name)]
               (when (= IntPtr/Zero new-fn-ptr)
                 (throw (Exception. (str "Unable to load " proc-name))))
               (println "Loaded" proc-name "at" new-fn-ptr)
               (let [new-invoker (make-invoker new-fn-ptr decl)]
                 (if-let [{:keys [fn-ptr-ptr cur-fn-ptr cur-dll-info cur-decl invoker] :as sym-ref} (get @compiled-symbols sym-name)]
                   (let [{:keys [source-file dll-handle references]} @cur-dll-info]
                     (println "Found existing symbol ref for" sym-name)
                     (swap! references disj @cur-decl)
                     (when (empty? @references)
                       (println "Cleaning up" source-file)
                       (swap! dll-handles disj @cur-dll-info)
                       (FreeLibrary dll-handle)
                       (File/Delete source-file))
                     (reset! invoker new-invoker)
                     (reset! cur-fn-ptr new-fn-ptr)
                     (reset! cur-dll-info new-dll-info)
                     (reset! cur-decl decl)
                     (Marshal/WriteIntPtr fn-ptr-ptr new-fn-ptr)
                     sym-ref)
                   (let [fn-ptr-ptr (Marshal/AllocHGlobal IntPtr/Size)
                         sym-ref (CompiledSymbolRef. sym-name
                                                     fn-ptr-ptr
                                                     (atom decl)
                                                     (atom new-fn-ptr)
                                                     (atom new-dll-info)
                                                     (atom new-invoker))]
                     (println "Creating new symbol ref for" sym-name)
                     (Marshal/WriteIntPtr fn-ptr-ptr new-fn-ptr)
                     (swap! compiled-symbols assoc sym-name sym-ref)
                     sym-ref))))))]
      (when-not (empty? @(:references new-dll-info))
        (swap! dll-handles conj new-dll-info))
      (into {}
            (for [sym-ref sym-refs]
              [(:symbol-name sym-ref) @(:invoker sym-ref)])))))

(defhooks msvc-hook)

(msvc-hook
 :before-function-signature
 [{:keys [cpp-mode]} expr]
 (str (when cpp-mode "extern \"C\" ") "__declspec(dllexport) "))

(msvc-hook
 :alternate-function-declaration
 [ctxt decl]
 (when *dynamic-compile*))

(defn- msvc-write-hook [ctxt hook-name expr]
  (dispatch-hook #'msvc-hook hook-name ctxt expr))

(defrecord MSVCCompileContext [compiled-symbols dll-handles]
  ICompileContext
  (write-hook [this hook-name expr]
    (msvc-write-hook this hook-name expr))
  (compile-decls [this decls compile-source]
    (msvc-compile-decls this decls compile-source)))

(defn- init-cl-bat [temp-output-path msvc-path]
  (let [cl-bat-path (Path/Combine temp-output-path "cl.bat")
        x64 (= IntPtr/Size 8)]
    (ensure-directory temp-output-path)
    (File/WriteAllText
     cl-bat-path
     (String/Format
      "@echo off\r\ncall \"{0}\" {1}\r\n{2} %*\r\n"
      (Path/Combine msvc-path "vcvarsall.bat")
      (if x64 "x64" "x86")
      "cl.exe"))
    cl-bat-path))

(defn create-msvc-compile-context [opts]
  (let [{:keys [msvc-path temp-output-path] :as opts}
        (merge {:cl-args default-msvc-args
                :msvc-path default-msvc-path}
               opts)
        opts
        (merge {:cl-bat-path
                (init-cl-bat temp-output-path msvc-path)}
               opts)]
    (MSVCCompileContext. (atom {}) (atom #{}) nil opts)))

(defn dll-load-symbol [{:keys [opts]} package-name symbol-name])

(defrecord DllLoader [opts]
  ILoadContext
  (load-symbol [this package-name symbol-name]
    (dll-load-symbol this package-name symbol-name)))

(defn create-dll-loader [{:keys [] :as opts}]
  (DllLoader. opts))

(defmacro msvc-module
  [module-name & {:as opts}]
  (let [opts (merge {:cpp-mode true} opts)]
    `(def ~module-name
       (create-module
        ~(name module-name)
        #'create-msvc-compile-context
        #'create-dll-loader
        ~opts))))

(msvc-module TestMsvcModule :dev true)

(cpackage TestMsvcModule "test1")

(cdefn ^i32 t1 [^i32 x] (+ x 1))
