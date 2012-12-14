(ns c-in-clj.gcc.nios
  (:require
   [c-in-clj.gcc.remote-link :as link]
   [c-in-clj.gcc.elf :as elf]
   [c-in-clj.gcc.gdb-server :as gdb])
  (:use
   [c-in-clj.gcc.elf :only [read-elf32]])
  (:import [System.Diagnostics Process ProcessStartInfo
            DataReceivedEventHandler]
           [System.IO Path Directory
            File]))

(defn- expand-env [path]
  (Environment/ExpandEnvironmentVariables path))

(defn- nios-cmd-shell-path []
   (expand-env "%SOPC_KIT_NIOS2%\\Nios II Command Shell.bat"))

(defn- get-temp-dir []
  (let [dir (expand-env "%TEMP%/c-in-clj.nios")]
    (when-not (Directory/Exists dir)
      (Directory/CreateDirectory dir))
    dir))

(defn run-gcc [gcc-args {:keys [env work-dir]}]
  (let [psi (ProcessStartInfo. (nios-cmd-shell-path))
        args (apply str "nios2-elf-gcc " gcc-args)]
    (set! (.Arguments psi) args)
    (doseq [[k v] env]
      (.Add (.EnvironmentVariables psi) k v))
    (.set_UseShellExecute psi false)
    (.set_RedirectStandardOutput psi true)
    (when work-dir (.set_WorkingDirectory psi work-dir))
    (let [process (Process/Start psi)
          output (.ReadToEnd (.StandardOutput process))]
      (.WaitForExit process)
      (let [exit-code (.ExitCode process)]
        (if (not= 0 exit-code)
          (do
            ;;(println)
            ;;(print-numbered text)
            (println output)
            (println "Exited with " exit-code))
          ;;(println body)
          )
        exit-code))))

(defn- win-path->cyg-path [path]
  (let [drive (.Substring (Path/GetPathRoot path) 0 1)
        cygpath (.Substring (.Replace path "\\" "/") 3)]
    (str "/cygdrive/" drive "/" cygpath)) )

(defn cache-lib-file [libfilename & {:keys [temp-dir]}]
  (let [temp-dir (or temp-dir (get-temp-dir))
        filename (Path/GetFileName libfilename)
        tempfile (Path/Combine temp-dir filename)
        cyfile (win-path->cyg-path tempfile)
        psi (ProcessStartInfo.
             (nios-cmd-shell-path)
             (str "ar x " cyfile))]
    (File/Copy libfilename tempfile true)
    (.set_WorkingDirectory psi temp-dir)
    (.set_RedirectStandardOutput psi true)
    (.set_UseShellExecute psi false)
    (let [ps (Process/Start psi)]
      (println (.ReadToEnd (.StandardOutput ps)))
      (link/cache-obj-dir temp-dir))))

(cache-lib-file (expand-env "%SOPC_KIT_NIOS2%/bin/gnu/H-i686-mingw32/nios2-elf/lib/libsmallc.a"))

(defn nios-config []
  {:bsp-path "c:/tmp/de0-nano-bsp"})

(defn compile-file [filename]
  (let [{:keys [bsp-path]} (nios-config)
        include-path (str bsp-path "/HAL/inc:" bsp-path "/drivers/inc")
        gcc-args (str filename " -c -I"
                      bsp-path "/HAL/inc -I"
                      bsp-path "/drivers/inc")]
    (run-gcc gcc-args {:env {"C_INCLUDE_PATH" include-path}
                       :work-dir (Path/GetDirectoryName filename)})))

(defn- add-bootstrap-sym-refs [elf-filename]
  (let [elf (elf/read-elf32 elf-filename)
        global-symbols (elf/find-global-symbols elf)
        data-symbols (elf/filter-data-symbols global-symbols)]
    (doseq [{:keys [name st_value]} data-symbols]
      ;;(link/set-symbol-addr name st_value)
      (println name))))

(defn bootstrap-nios [elf-filename]
  (let [port (+ 4444 (rand-int 5555))
        gdb-server (Process/Start
                    (nios-cmd-shell-path)
                    (str "nios2-gdb-server --tcpport " port))
        gdb-client (gdb/start-gdb (nios-cmd-shell-path) "nios2-elf-gdb --interpreter=mi")]
    (binding [gdb/*gdb-client* gdb-client]
      (gdb/gdb-connect-localhost port)
      ;;(link/cache-obj-file elf-filename)
      (add-bootstrap-sym-refs elf-filename)
      (gde "-file-exec-and-symbols " elf-filename)
      (gde "-target-download")
      )
    ;;(gdb/kill-gdb gdb-client)
    ))

(defn send-byte [byte]
  (let [buf (make-array Byte 1)
        {:keys [stream]} (gdb-server)]
    (aset buf 0 byte)
    (.Write stream buf 0 1)))

(defn stop []
  (send-byte 0x03))

(defn continue []
  (gre "c  ll"))

