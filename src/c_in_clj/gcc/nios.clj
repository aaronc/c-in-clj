(ns c-in-clj.gcc.nios
  (:require
   [c-in-clj.gcc.remote-link :as link]
   [c-in-clj.gcc.elf :as elf]
   [c-in-clj.gcc.gdb-server :as gdb])
  (:use
   [c-in-clj.gcc.elf :only [read-elf32]]
   [c-in-clj.gcc.gdb-server])
  (:import
   [System.Diagnostics Process ProcessStartInfo DataReceivedEventHandler]
   [System.IO Path Directory File]
   [System.Threading Thread]))

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

;;(cache-lib-file (expand-env "%SOPC_KIT_NIOS2%/bin/gnu/H-i686-mingw32/nios2-elf/lib/libsmallc.a"))

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

(defn- bootstrap-elf [elf-filename]
  (let [elf (elf/read-elf32 elf-filename)
        global-symbols (elf/find-global-symbols elf)
        data-symbols (elf/filter-data-symbols global-symbols)]
    (doseq [{:keys [p_type p_offset p_vaddr p_paddr p_filesz p_memsz p_flags p_align data] :as phdr} (:program-headers elf)]
      (when (= :PT_LOAD p_type)
        (gdb/write-mem p_paddr data)))
    (doseq [{:keys [name st_value st_size st_type st_shndx] :as sym} data-symbols]
      (link/set-fixed-symbol-addr name st_value st_size))))

(comment
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
      )))

(defn bootstrap-nios [elf-filename]
  (let [port (+ 4444 (rand-int 5555))
        gdb-server-process (Process/Start
                            (nios-cmd-shell-path)
                            (str "nios2-gdb-server --tcpport " port))]
    (Thread/Sleep 1000)
    (let [conn (gdb/gdb-server-connect port)]
      (bootstrap-elf elf-filename))))

(defn send-byte [byte]
  (let [buf (make-array Byte 1)
        {:keys [stream]} (gdb/gdb-server)]
    (aset buf 0 byte)
    (.Write stream buf 0 1)))

(defn break []
  (send-byte 0x03))

(defn write-word [addr word]
  (gdb/write-mem addr (BitConverter/GetBytes (uint word))))

(defn get-scratch-offset []
  (:addr (link/find-sym-in-memory "scratch_area")))

(defn write-scratch-word [offset word]
  (when-let [{:keys [addr size]} (link/find-sym-in-memory "scratch_area")]
    (when (< offset size)
      (write-word (+ addr offset) word))))

(defn save-state* [func]
  (let [regs (gdb/read-regs)]
    (func)
    (gdb/write-regs regs)))

(defmacro save-state [& body]
  `(save-state* (fn [] ~@body)))

(defn set-pc [addr]
  (gdb/write-reg 0x20 addr))

(defn step-one []
  (gdb/gre "s"))

(defn invoke-func [name args]
  (break)
  (save-state
   (when-let [{:keys [addr]} (link/find-sym-in-memory name)]
     (let [call-word (bit-shift-left (bit-shift-right addr 2) 6)
           nargs (count args)]
       (write-scratch-word 0 call-word)
       (if (<= nargs 4)
         (dotimes [i nargs]
           (let [arg (nth args i)]
             (gdb/write-reg (+ 4 i) arg)))
         (throw (ArgumentException. (str "Too many arguments (" nargs ") for invoke-func"))))
       (set-pc (get-scratch-offset))
       (step-one)
       (read-reg 2)))))

;;(add-bootstrap-sym-refs "c:/tmp/test1.elf")
;;(bootstrap-nios "c:/tmp/test1.elf")
(comment (defn continue []
           (gre "c  ll")))

(def ^:private nios-relocs (atom {}))

(defn- nios-calc-reloc [R B M X]
  (bit-or (bit-and (bit-shift-left R B) M)
          (bit-and X (bit-not M))))

(defmacro nrel [name value overflow-check? rel-addr-expr bit-mask bit-shift]
  `(swap! nios-relocs assoc ~value
         {:name ~name
          :value ~value
          :overflow-check? ~overflow-check?
          :bit-mask ~bit-mask
          :bit-shift ~bit-shift
          :calc-reloc-fn (fn ~(symbol (str (name name) "-calc-rel"))
                            [X# ~'S ~'A ~'PC ~'GP]
                            (let [R# ~rel-addr-expr]
                              (nios-calc-reloc R# ~bit-shift
                                               ~bit-mask X#)))}))

(defn- Adj [x]
  (bit-and
   (+ (bit-and (bit-shift-right x 16) 0xFFFF)
      (bit-and (bit-shift-right x 15) 0x1))
   0xFFFF))

(nrel :R_NIOS2_S16 1 true (+ S A)  0x003FFFC0 6)
(nrel :R_NIOS2_U16 2 true (+ S A) 0x003FFFC0 6)
(nrel :R_NIOS2_PCREL16 3 true (- (- (+ S A) 4) PC) 0x003FFFC0 6)
(nrel :R_NIOS2_CALL26 4 false (bit-shift-right (+ S A) 2) 0xFFFFFFC0 6)
(nrel :R_NIOS2_IMM5 5 true (bit-and (+ S A) 0x1F) 0x000007C0 6)
(nrel :R_NIOS2_CACHE_OPX 6 true (bit-and (+ S A) 0x1F)  0x07C00000 22)
(nrel :R_NIOS2_IMM6 7 true (bit-and (+ S A) 0x3F) 0x00000FC0 6)
(nrel :R_NIOS2_IMM8 8 true (bit-and (+ S A) 0xFF) 0x00003FC0 6)
(nrel :R_NIOS2_HI16 9 false (bit-and (bit-shift-right (+ S A) 16) 0xFFFF) 0x003FFFC0 6)
(nrel :R_NIOS2_LO16 10 false (bit-and (+ S A) 0xFFFF) 0x003FFFC0 6)
(nrel :R_NIOS2_HIADJ16 11 false (Adj (+ S A)) 0x003FFFC0 6)
(nrel :R_NIOS2_BFD_RELOC_32 12 false (+ S A) 0xFFFFFFFF 0)
(nrel :R_NIOS2_BFD_RELOC_16 13 true (bit-and (+ S A) 0xFFFF) 0x0000FFFF 0)
(nrel :R_NIOS2_BFD_RELOC_8 14 true (bit-and (+ S A) 0xFF) 0x000000FF 0)
(nrel :R_NIOS2_GPREL 15 false (bit-and (- (+ S A) GP) 0xFFFF) 0x003FFFC0 6)
;;:R_NIOS2_GNU_VTINHERIT 16
;;:R_NIOS2_GNU_VTENTRY 17
;; :R_NIOS2_UJMP 18 false
;; ((S + A) >> 16) & 0xFFFF,
;; (S + A + 4) & 0xFFFF
;; 0x003FFFC0
;; 6
;; :R_NIOS2_CJMP
;; 19
;; No
;; ((S + A) >> 16) & 0xFFFF,
;; (S + A + 4) & 0xFFFF
;; 0x003FFFC0
;; 6
;; :R_NIOS2_CALLR
;; 20
;; No
;; ((S + A) >> 16) & 0xFFFF)
;; (S + A + 4) & 0xFFFF
;; 0x003FFFC0
;; 6


(defn- perform-nios-relocation
  [this rela section-addr target-addr])

(defn create-nios-target []
  (reify link/IRemoteLinkerTarget
    (do-relocation [this rela section-addr target-addr]
      (perform-nios-relocation this rela section-addr target-addr))))

