(ns c-in-clj.clr
  (:import [System.Diagnostics Process ProcessStartInfo]
           [System.IO Path File Directory]
           [System.Runtime.InteropServices Marshal])
  (:require [clojure.string :as str])
  (:use [c-in-clj.core]
        [clojure.clr pinvoke emit]))

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
    (File/Delete (Path/ChangeExtension fname ".obj"))
    (catch Exception ex
      (println ex))))

(defn update-fn-ref [name ret args fname existing]
  (let [new-dll-handle (LoadLibrary (Path/ChangeExtension fname ".dll"))
        new-fn-ptr (GetProcAddress new-dll-handle name)]
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
        (clean-fn-files last-fname)
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
    (let [ret-type (get-clr-type ret)
          param-types (map get-clr-type (take-nth 2 args))
          dg-type (get-dg-type ret-type param-types)
          dg (Marshal/GetDelegateForFunctionPointer fn-ptr dg-type)
          invoke-method (.GetMethod dg-type "Invoke")]
      (fn [& args]
        (.Invoke invoke-method dg (to-array args))))))

(defn get-invoker [ctxt name]
  (when-let [fn-info (@(:fn-index ctxt) name)]
    (make-invoker @(:fn-ptr fn-info) (:return fn-info) (:params fn-info))))

(defmacro gen-c-delegate [ret params args & body]
  (let [dg-type (get-dg-type (eval ret) (eval params))]
    `(let [dg# (gen-delegate ~dg-type ~args ~@body)]
        {:dg dg#
         :fp (System.Runtime.InteropServices.Marshal/GetFunctionPointerForDelegate dg#)})))

(defn msvc-compile [ctxt name ret args body]
  (let [name (str name)
        compile-path (:compile-path ctxt)
        filename (Path/Combine compile-path (str name "__" (swap! (:save-id ctxt) inc) ".c"))
        text (str *header* "\n\n_declspec(dllexport) " body)]
    (spit filename text)
    (if (= 0 (run-cl compile-path (:cl-path ctxt) (str "/LD " filename) body text))
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
          {:fn-info fn-info}))
      (clean-fn-files filename))))

(defn msvc-beginfn [ctxt name ret args])

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

(defrecord MSVCCompileContext [compile-path cl-path save-id fn-index])

(extend MSVCCompileContext
  ICModuleContext
  {:compilefn #'msvc-compile
   :beginfn #'msvc-beginfn
   :resolve-sym #'msvc-resolvesym
   :clean #'msvc-clean})

(defn init-msvc-context [& {:keys [msvc-path compile-path]}]
  (let [compile-path (or compile-path (Path/Combine (Path/GetTempPath) "c-in-clj"))
        msvc-path (or msvc-path vc-root)
        cl-bat-path (Path/Combine compile-path "cl.bat")
        x64 (= IntPtr/Size 8)]
    (when-not (Directory/Exists compile-path)
      (Directory/CreateDirectory compile-path))
    (spit cl-bat-path
          (String/Format
           "@echo off\r\ncall \"{0}\" {1}\r\n{2} %*\r\n"
           (Path/Combine msvc-path "vcvarsall.bat")
           (if x64 "x64" "x86")
           "cl.exe"))
    (MSVCCompileContext. compile-path cl-bat-path (atom 0) (atom {}))))