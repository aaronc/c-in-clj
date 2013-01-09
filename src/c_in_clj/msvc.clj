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

(defn- msvc-compile-decls [{:keys [temp-output-path cl-args cl-bat-path] :as ctxt} decls {:keys [source filename]}]
  (when (run-cl temp-output-path
                cl-bat-path
                cl-args
                filename
                source)))

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

(defrecord MSVCCompileContext [compiled-symbols]
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
    (MSVCCompileContext. (atom {}) nil opts)))

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
