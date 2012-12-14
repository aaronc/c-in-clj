(ns c-in-clj.gcc.remote-link
  (:require [c-in-clj.gcc.gdb-server :as gdb]
            [c-in-clj.gcc.elf :as elf]
            [clojure.set :as set])
  (:import
   [System.IO Directory SearchOption]))

(def ^:private linker-data (atom {}))

(defn- get-data []
  (or (:data (gdb/gdb-server)) linker-data))

(defprotocol IRemoteLinkerTarget
  (do-relocation [this rela section-addr target-addr])
  (alloc-mem [this size])
  (write-mem [this addr bytes])
  (zero-mem [this addr count]))

(defn cache-obj-file [filename & {:keys [cache-locals]}]
  (let [elf (elf/read-elf32 filename)
        symbols (if cache-locals
                  (elf/find-sym-table elf)
                  (elf/find-global-symbols elf))
        defined-symbols (elf/filter-data-symbols symbols)]
    (doseq [{:keys [name] :as sym} defined-symbols]
      (when name
        (println "Caching" name)
        (swap! (get-data) assoc-in [:symbol-cache name]
               {:symdata sym :obj-file elf})))))

(defn cache-obj-dir [path]
  (doseq [filename (Directory/EnumerateFiles path "*.o" SearchOption/AllDirectories)]
    (try
      (cache-obj-file filename)
      (catch Object ex
        (println "Error" ex)))))

(defn- find-sym-rel-sections [obj-file st_shndx]
  (when (number? st_shndx)
    (filter
     (fn [{:keys [sh_type sh_info] :as section}]
       (and (or (= :SHT_REL sh_type)
                (= :SHT_RELA sh_type))
            (= sh_info st_shndx)))
     (:sections obj-file))))

(defn- find-sym-section
  ([obj-file st_shndx]
     (when (number? st_shndx)
       (nth (:sections obj-file) st_shndx))))

(defn find-sym-section+rels
  [obj-file st_shndx]
  {:section (find-sym-section obj-file st_shndx)
   :rels (find-sym-rel-sections obj-file st_shndx)})


(defn find-section-globals [obj-file st_shndx]
  (filter (fn [sym] (= (:st_shndx sym) st_shndx))
          (elf/find-global-symbols obj-file)))

(defn process-section [obj-file st_shndx]
  (when (number? st_shndx)
    (let [{:keys [section rels]} (find-sym-section+rels obj-file st_shndx)
          section-globals (find-section-globals obj-file st_shndx)
          all-symbols (elf/find-sym-table obj-file)
          rels (apply concat (map :data rels))
          rels (for [{:keys [r_info] :as rel} rels]
                 (let [{:keys [r_sym]} r_info
                       {:keys [st_info st_shndx] :as rsym} (nth all-symbols r_sym)
                       target (case (:st_bind st_info)
                                :STB_GLOBAL (:name rsym)
                                :STB_LOCAL
                                {:symbol rsym
                                 :target-section (nth (:sections obj-file) st_shndx)})]
                   (assoc rel :target target)))
          global-symbols (for [{:keys [name st_value st_size]} section-globals]
            (let [sym-rels (filter (fn [{:keys [r_offset] :as rel}]
                                 (and (>= r_offset st_value)
                                      (<= r_offset (+ st_value st_size))))
                                   rels)]
              {:name name
               :offset st_value
               :size st_shndx
               :section section
               :rels sym-rels}))
          sym-rels (apply hash-set (apply concat (map :rels global-symbols)))
          section-rels (set/difference (apply hash-set rels) sym-rels)]
      {:section section
       :section-idx st_shndx
       :section-rels (vec section-rels)
       :global-symbols global-symbols})))

(defn- find-sym-in-cache [symbol-name]
  (get-in @(get-data) [:symbol-cache symbol-name]))

(defn- find-sym-in-memory [symbol-name]
  (get-in @(get-data) [:symbol-table symbol-name]))

(defn- find-section-in-memory [section]
  (get-in @(get-data) [:section-table section]))

(defn- throw-unresolved-symbol [name]
  (throw (ex-info
          (str "Unresolved symbol " name)
          {:type ::unresolved-symbol :name name})))

(defn- find-relocation-target [obj-file {:keys [st_shndx name] :as r_sym}]
  (println "Trying to locate" name)
  (let [{:keys [st_bind st_type]} (:st_info r_sym)]
    (cond
     (and (= st_bind :STB_LOCAL)
          ;;(= st_type :STT_SECTION)
          (number? st_shndx))
     (let [{:keys [section]} (find-sym-section+rels obj-file r_sym)
           in-mem (find-section-in-memory section)]
       (if in-mem
         {:type :linked :data in-mem}
         {:type :local
          :data {:symdata r_sym :obj-file obj-file}}))
     (= :SHN_UNDEF st_shndx) (or
                              (when-let [mem-sym (find-sym-in-memory name)]
                                {:type :linked :data mem-sym :name name})
                              (when-let [global (find-sym-in-cache name)]
                                {:type :global :data global :name name})
                              (throw-unresolved-symbol name))
     :default
     (do (println "Not found" r_sym)
         (throw-unresolved-symbol name)))))

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

(defn resolve-sym-refs [{:keys [symdata obj-file]} to-link]
  (let [{:keys [section rels section-idx] :as section-data} (find-sym-section+rels obj-file symdata)]
    (when-not (contains? @to-link section)
      (let [rel-entries (apply concat
                               (map
                                (partial resolve-rel-entries obj-file)
                                rels))]
        (swap! to-link assoc section
               {:rel-entries rel-entries
                :obj-file obj-file
                :section-idx section-idx})
        (doseq [{:keys [relocation-target]} rel-entries]
          (when (not= (:type relocation-target) :linked)
            (resolve-sym-refs (:data relocation-target) to-link)))))))

(def null-target
  (reify IRemoteLinkerTarget
    (do-relocation [this rela section-addr target-addr]
      (println "Relocating" rela section-addr target-addr))
    (alloc-mem [this count] (rand-int Int32/MaxValue))
    (write-mem [this addr bytes])
    (zero-mem [this addr count])))

(defn link-symbol [symbol-name target]
  (println "Trying to link" symbol-name)
  ;; Find sym in cache
  (if-let [{:keys [symdata obj-file] :as syminfo} (find-sym-in-cache symbol-name)]
    (let [to-link (atom {})]
      ;; Find references to sym -> list of linked symbols, list of cached symbols
      (resolve-sym-refs syminfo to-link)
      ;; If have all refs
      ;;  Write cached syms -> mem
      (let [updated-symbols (atom #{})
            newly-written
            (doall
             (for [[section {:keys [rel-entries obj-file section-idx]}] @to-link]
               (let [{:keys [sh_size data]} section
                     data-len (if data (.Length data) 0)
                     zero-len (- sh_size data-len)]
                 (println (:filename obj-file) (:name section) sh_size
                          data-len)
                 (when target
                   (let [addr (alloc-mem target sh_size)]
                     (when (> data-len 0) (write-mem target addr data))
                     (when (> zero-len 0) (write-mem target (+ addr data-len) zero-len))
                     (swap! (get-data) assoc-in [:section-table section]
                            {:addr addr :size sh_size})
                     (let [global-symbols (elf/find-global-symbols obj-file)
                           section-symbols
                           (filter (fn [sym] (= (:st_shndx sym) section-idx))
                                   global-symbols)]
                       (doseq [{:keys [name st_value st_size]} section-symbols]
                         (when name
                           (swap! updated-symbols conj name)
                           (swap! (get-data) update-in [:symbol-table name]
                                  merge
                                  {:addr (+ addr st_value)
                                   :size st_size}))))
                     ;; find all global symbols in this section, and
                     ;; update their ref in the symbol table
                     ;; if there are pieces of code that refer to those
                     ;; symbols, update their relocation entries
                     {:section section
                      :rel-entries rel-entries
                      :obj-file obj-file
                      :addr addr
                      :size sh_size})))))]
        ;; Updated references to updated symbols
        (doseq [sym updated-symbols]
          (let [{:keys [addr refs]} (find-sym-in-memory sym)]
            (doseq [[sym-name {:keys [addr rel-entries]}] refs]
              (do-relocation target relocation-entry section-addr target-addr))))
        ;;  Perform relocations on newly written sections
        (doseq [{:keys [rel-entries addr size]} newly-written]
          (doseq [{:keys [relocation-symbol relocation-entry relocation-target]} rel-entries]
            (let [section-addr addr
                  target-addr (case (:st_bind (:st_info relocation-symbol))
                                :STB_GLOBAL
                                (let [{:keys [addr refs]} (find-sym-in-memory (:name relocation-symbol))]
                                  addr)
                         :STB_LOCAL
                         (let [section (find-sym-section (:data relocation-target))
                               {:keys [addr mem-refs]} (find-section-in-memory section)
                               offset (:st_value relocation-symbol)
                               target-addr (+ addr offset)]
                           (swap! mem-refs conj section-addr)
                           (println "local" addr offset)
                           target-addr)
                         nil)]
              (do-relocation target relocation-entry section-addr target-addr)))))))
  ;;  If existing back refs to sym, update their reloc entries
  (throw-unresolved-symbol symbol-name))






