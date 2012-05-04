(ns c-in-clj.clr
  (:import [System.Diagnostics Process ProcessStartInfo]
           [System.IO Path])
  (:use [c-in-clj.core]))


(def ^:dynamic *msvc-compiler-context*)

(def vc-root "C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\VC\\BIN")
(def vc-root-x64 (Path/Combine vc-root "amd64"))
(def vc-path "")

(defn run-cl [cl-file args]
  (let [psi (ProcessStartInfo. cl-file args)]
    (.set_UseShellExecute psi false)
    (.set_RedirectStandardOutput psi true)
    (.set_CreateNoWindow psi true)
    (let [process (Process/Start psi)
          output (.ReadToEnd (.StandardOutput process))]
      (.WaitForExit process)
      (let [exit-code (.ExitCode process)]
        (println output)
        (when (not= 0 exit-code) (println "Exited with " exit-code))))))

(defn set-msvc-compiler-context)

(defn msvc-module-context [name & {:keys [cl-path cl-args]}]
  (reify ICModuleContext))
