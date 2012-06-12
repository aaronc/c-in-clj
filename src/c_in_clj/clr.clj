(ns c-in-clj.clr
  (:import [System.Diagnostics Process ProcessStartInfo]
           [System.IO Path File Directory]
           [System.Runtime.InteropServices Marshal])
  ;(:use [c-in-clj.core])
  )


(def ^:dynamic *msvc-compiler-context*)

(def vc-root "C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\VC")

(defn run-cl [ctxt args]
  (let [cl-path (:cl-path ctxt)
        psi (ProcessStartInfo. cl-path args)]
    (.set_UseShellExecute psi false)
    (.set_RedirectStandardOutput psi true)
    (.set_CreateNoWindow psi true)
    (let [process (Process/Start psi)
          output (.ReadToEnd (.StandardOutput process))]
      (.WaitForExit process)
      (let [exit-code (.ExitCode process)]
        (when (not= 0 exit-code)
          (println output)
          (println "Exited with " exit-code))))))

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
    {:compile-path compile-path
     :cl-path cl-bat-path}))

(defn set-msvc-compiler-context)

(defn msvc-module-context [name & {:keys [cl-path cl-args]}]
  (reify ICModuleContext))
