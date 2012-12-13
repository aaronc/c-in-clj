(ns c-in-clj.gcc.remote-link
  (:require [c-in-clj.gcc.gdb :as gdb]
            [c-in-clj.gcc.elf :as elf])
  (:import
   [System.IO Directory SearchOption]))

(comment
  (defn read-memory [addr count]
    (gdb/gdb-sync-exec "-data-read-memory u 1 " addr " " count)))

(def ^:private linker-data (atom {}))

(defn- get-data []
  (or (gdb/get-client-data) linker-data))

(defn- write-memory* [addr data type multiplier]
  (doall
   (map-indexed
    (fn [i x]
      (let [addri (+ addr (* i multiplier))]
        (gdb/gdb-sync-exec "set {" type "}0x" (format "%x" addri) "=" x)))
    data))
  nil)

(defn write-memory-bytes [addr bytes]
  (write-memory* addr bytes "unsigned char" 1))

(defn write-memory-words [addr words]
  (write-memory* addr words "unsigned int" 4))

(defn get-register-map []
  (let [data (gdb/get-client-data)]
    (if-let [regs (:register-map @data)]
      regs
      (let [res (gdb/gdb-sync-exec "-data-list-register-names")
            reg-list (get-in res [:data :register-names])
            regs (zipmap (map keyword reg-list) (range (count reg-list)))]
        (swap! data assoc :register-map regs)))))

(defn read-reg [reg & {:keys [format]}]
  (let [format (or format "u")
        reg (if (number? reg)
              reg
              (get (get-register-map) reg))
        res (gdb/gdb-sync-exec "-data-list-register-values " format " " reg)]
    (long (-> res :data :register-values first :value))))

(defn set-reg [reg value]
  (gdb/gdb-sync-exec "set $" (name reg) "=" value))

(defn set-pc [addr] (set-reg :pc addr))

(defn stepi []
  (gdb/gdb-sync-exec "-exec-step-instruction"))

(defprotocol ILinkerTarget
  (calc-rela [rela])
  (alloc-mem [size]))

(defn cache-obj-file [filename]
  (let [elf (elf/read-elf32 filename)
        global-symbols (elf/find-global-symbols elf)
        defined-symbols (elf/filter-data-symbols global-symbols)]
    (doseq [{:keys [name] :as sym} defined-symbols]
      (println "Caching" name)
      (swap! (get-data) assoc-in [:symbol-cache name]
             {:symdata sym :obj-file elf}))))

(defn cache-obj-dir [path]
  (doseq [filename (Directory/EnumerateFiles path "*.o" SearchOption/AllDirectories)]
    (try
      (cache-obj-file filename)
      (catch Object ex
        (println "Error" ex)))))


(defn- find-sym-rel-sections [obj-file {:keys [st_shndx] :as symdata}]
  (when (number? st_shndx)
    (filter
     (fn [{:keys [sh_type sh_info] :as section}]
       (and (or (= :SHT_REL sh_type)
                (= :SHT_RELA sh_type))
            (= sh_info st_shndx)))
     (:sections obj-file))))

(defn- find-sym-section
  ([{:keys [obj-file symdata]}]
     (find-sym-section obj-file symdata))
  ([obj-file {:keys [st_shndx] :as symdata}]
     (when (number? st_shndx)
       (nth (:sections obj-file) st_shndx))))

(defn- resolve-sym [symbol-name]
  (get-in @(get-data) [:symbol-cache symbol-name]))

(defn- throw-unresolved-symbol [name]
  (throw (ex-info
          (str "Unresolved symbol " name)
          {:type ::unresolved-symbol :name name})))

(declare link-symbol)

(defn- find-relocation-target [obj-file {:keys [st_shndx name] :as r_sym}]
  (let [{:keys [st_bind st_type]} (:st_info r_sym)]
    (cond
     (and (= st_bind :STB_LOCAL)
          (= st_type :STT_SECTION)
          (number? st_shndx)) (find-sym-section obj-file r_sym)
     (= :SHN_UNDEF st_type) (or
                              (link-symbol name)
                              (throw-unresolved-symbol name))
     :default (throw-unresolved-symbol name))))

(defn- resolve-rel-entries [obj-file {:keys [sh_link data] :as rel-section}]
  (let [sym-tab (:data (nth (:sections obj-file) sh_link))]
    (for [{:keys [r_info] :as rel-entry} data]
      (let [r_sym (:r_sym r_info)
            r_sym (nth sym-tab r_sym)
            {:keys [st_bind st_type]} (:st_info r_sym)
            rel-target (find-relocation-target obj-file r_sym)]
        {:relocation-entry rel-entry
         :relocation-symbol r_sym
         :relocation-target rel-target}))))

(defn set-symbol-addr [symbol-name addr]
  (println "Setting symbol" symbol-name "address to" addr)
  (swap! (get-data) assoc-in [:symbol-table symbol-name addr] addr))

(defn link-symbol [symbol-name]
  (println "Trying to link" symbol-name)
  (let [{:keys [symdata obj-file]} (resolve-sym symbol-name)
        sym-section (find-sym-section obj-file symdata)
        rel-sections (find-sym-rel-sections obj-file symdata)
        rel-entries (apply concat
                           (map
                            (partial resolve-rel-entries obj-file)
                            rel-sections))]
    sym-section))

