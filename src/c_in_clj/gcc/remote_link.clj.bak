(ns c-in-clj.gcc.remote-link
  (:require [c-in-clj.gcc.gdb-server :as gdb]
            [c-in-clj.gcc.elf :as elf]
            [clojure.set :as set])
  (:import
   [System.IO Directory SearchOption]))

(def ^:private linker-data (atom {}))

(defn get-data []
  (or (:data (gdb/gdb-server)) linker-data))

(defprotocol IRemoteLinkerTarget
  (do-relocation [this rela section-addr target-addr])
  (alloc-mem [this size])
  (write-mem [this addr bytes])
  (free-mem [this addr])
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
                             :target-idx st_shndx
                             :target-obj obj-file
                             :target-section (find-sym-section obj-file st_shndx)}
                            :STB_WEAK
                            (:name rsym))]
               (assoc rel :target target)))
      global-symbols (for [{:keys [name st_value st_size]} section-globals]
        (let [sym-rels (filter (fn [{:keys [r_offset] :as rel}]
                             (and (>= r_offset st_value)
                                  (<= r_offset (+ st_value st_size))))
                               rels)
              sym-rels (for [rel sym-rels]
                         (update-in rel [:r_offset] - st_value))]
          {:name name
           :offset st_value
           :size st_shndx
           :section section
           :rels sym-rels}))
      sym-rels (into #{} (apply concat (map :rels global-symbols)))
      section-rels (set/difference (apply hash-set rels) sym-rels)]
  {:section section
   :section-idx st_shndx
   :section-rels (vec section-rels)
   :rels rels
   :global-symbols global-symbols
   :obj-file obj-file})))

(defn- find-sym-in-cache [symbol-name]
(get-in @(get-data) [:symbol-cache symbol-name]))

(defn find-sym-in-memory [symbol-name]
(get-in @(get-data) [:symbol-table symbol-name]))

(defn- find-section-in-memory [section]
(get-in @(get-data) [:section-table section]))

(defn- throw-unresolved-symbol [name]
(throw (ex-info
      (str "Unresolved symbol " name)
      {:type ::unresolved-symbol :name name})))

(defn resolve-sym-refs [obj-file st_shndx to-link]
  (let [{:keys [section rels] :as section-info}
        (process-section obj-file st_shndx)]
    (when-not (contains? @to-link section-info)
      (swap! to-link conj section-info)
      (doseq [{:keys [target]} rels]
        (if (string? target)
          (when-not (find-sym-in-memory target)
            (println "Searching for" target)
            (if-let [{:keys [obj-file symdata]} (find-sym-in-cache target)]
              (resolve-sym-refs obj-file (:st_shndx symdata) to-link)
              (throw-unresolved-symbol target)))
          (resolve-sym-refs (:target-obj target) (:target-idx target) to-link))))))

(def null-target
  (reify IRemoteLinkerTarget
    (do-relocation [this rela section-addr target-addr]
      (println "Relocating" rela section-addr target-addr))
    (alloc-mem [this count] (rand-int Int32/MaxValue))
    (write-mem [this addr bytes])
    (free-mem [this addr]
      (println "Freeing" addr))
    (zero-mem [this addr count])))

(defn debug-clear []
  (swap! (get-data) merge {:symbol-table nil :section-table nil})
  nil)

(defn- remove-sym-ref [referencing-symbol referenced-symbol]
  (let [{:keys [referenced-by]} (find-sym-in-memory referenced-symbol)]
    (swap! referenced-by disj referencing-symbol)))

(defn- remove-section-ref [linker referencing-symbol target-section]
  (let [{:keys [referenced-by addr]} (find-section-in-memory target-section)]
    (swap! referenced-by disj referencing-symbol)
    (when (empty? @referenced-by)
      (free-mem linker addr)
      (swap! (get-data) update-in [:section-table] dissoc target-section))))

(defn set-fixed-symbol-addr [name addr size]
  (println name "@" addr)
  (swap! (get-data) assoc-in [:symbol-table name]
         {:addr addr :size size
          :fixed true}))

(defn link-symbol [symbol-name linker]
  (println "Trying to link" symbol-name)
  ;; Find sym in cache
  (if-let [{:keys [symdata obj-file] :as syminfo} (find-sym-in-cache symbol-name)]
    (let [to-link (atom #{})]
      ;; Find references to sym -> list of linked symbols, list of cached symbols
      (resolve-sym-refs obj-file (:st_shndx symdata) to-link)
      ;; If have all refs
      ;;  Write cached syms -> mem
      (let [linked
            (for [{:keys [section global-symbols section-rels rels obj-file] :as section-info} @to-link]
              (let [{:keys [sh_size data]} section
                    data-len (if data (.Length data) 0)
                    zero-len (- sh_size data-len)]
                (println (:filename obj-file) (:name section) sh_size
                         data-len)
                (when linker
                  (let [addr (alloc-mem linker sh_size)
                        section-info (merge section-info
                                            {:addr addr
                                             :size sh_size
                                             :referenced-by (atom #{})})]
                    (when (> data-len 0) (write-mem linker addr data))
                    (when (> zero-len 0) (write-mem linker (+ addr data-len) zero-len))
                    (swap! (get-data) assoc-in [:section-table section]
                           section-info)
                    (doseq [{:keys [name offset size sections rels] :as sym-info} global-symbols]
                      (println "updating symbol" name)
                      ;; Updating existing symbol references
                      (if-let [{:keys [rels]}
                               (get-in @(get-data) [:symbol-table name])]
                        (doseq [{:keys [target]} rels]
                          (if (string? target)
                            (remove-sym-ref name target)
                            (remove-section-ref linker name target)))
                        (swap! (get-data) assoc-in [:symbol-table name]
                               {:referenced-by (atom #{})}))
                      (swap! (get-data) update-in [:symbol-table name] merge sym-info))
                    ;; find all global symbols in this section, and
                    ;; update their ref in the symbol table
                    ;; if there are pieces of code that refer to those
                    ;; symbols, update their relocation entries
                    
                    section-info))))]
        
        (doseq [{:keys [section-rels global-symbols section addr]} linked]
          (doseq [{:keys [name rels]} global-symbols]
            (doseq [{:keys [target] :as rel} rels]
              (if (string? target)
                (println "rel target:" target)
                (println "rel target:" (keys target))))
            (doseq [back-ref @(:referenced-by (find-sym-in-memory name))]
              ;; TODO update back references to this symbol
              ;; with the new info
              )))
       ;; TODO have two relocation sections - one that relocated based
        ;; on symbols sending their back references to the symbols
        ;; that are being targeted (making sure offsets are correctly adjusted), and the other one relocating based
        ;; on sections with some reference to the section kept
        ;; somewhere for removal of that section from memory when it
        ;; is no longer being referenced
        ;; (doseq [{:keys [rels section addr]} linked]
        ;;   (doseq [{:keys [target] :as rel} rels]
        ;;     (let [target-addr
        ;;           (if (string? target)
        ;;             (let [mem-sym (find-sym-in-memory target)]
        ;;               (swap! mem-sym update-in [:refs] conj )
        ;;               (:addr mem-sym))
        ;;             (let [{:keys [symbol target-section]} target
        ;;                   {:keys [addr]} (find-section-in-memory target-section)]
        ;;               (+ (:st_value symbol) addr)))]
        ;;       (do-relocation linker (dissoc rel :target) addr target-addr)))))
      ;; (comment
      ;;   (let [updated-symbols (atom #{})
      ;;         newly-written
      ;;         (doall
      ;;          (for [[section {:keys [rel-entries obj-file section-idx]}] @to-link]
      ;;            (let [{:keys [sh_size data]} section
      ;;                  data-len (if data (.Length data) 0)
      ;;                  zero-len (- sh_size data-len)]
      ;;              (println (:filename obj-file) (:name section) sh_size
      ;;                       data-len)
      ;;              (when target
      ;;                (let [addr (alloc-mem target sh_size)]
      ;;                  (when (> data-len 0) (write-mem target addr data))
      ;;                  (when (> zero-len 0) (write-mem target (+ addr data-len) zero-len))
      ;;                  (swap! (get-data) assoc-in [:section-table section]
      ;;                         {:addr addr :size sh_size})
      ;;                  (let [global-symbols (elf/find-global-symbols obj-file)
      ;;                        section-symbols
      ;;                        (filter (fn [sym] (= (:st_shndx sym) section-idx))
      ;;                                global-symbols)]
      ;;                    (doseq [{:keys [name st_value st_size]} section-symbols]
      ;;                      (when name
      ;;                        (swap! updated-symbols conj name)
      ;;                        (swap! (get-data) update-in [:symbol-table name]
      ;;                               merge
      ;;                               {:addr (+ addr st_value)
      ;;                                :size st_size}))))
      ;;                  ;; find all global symbols in this section, and
      ;;                  ;; update their ref in the symbol table
      ;;                  ;; if there are pieces of code that refer to those
      ;;                  ;; symbols, update their relocation entries
      ;;                  {:section section
      ;;                   :rel-entries rel-entries
      ;;                   :obj-file obj-file
      ;;                   :addr addr
      ;;                   :size sh_size})))))]
      ;;     ;; Updated references to updated symbols
      ;;     (doseq [sym updated-symbols]
      ;;       (let [{:keys [addr refs]} (find-sym-in-memory sym)]
      ;;         (doseq [[sym-name {:keys [addr rel-entries]}] refs]
      ;;           (do-relocation target relocation-entry section-addr target-addr))))
      ;;     ;;  Perform relocations on newly written sections
      ;;     (doseq [{:keys [rel-entries addr size]} newly-written]
      ;;       (doseq [{:keys [relocation-symbol relocation-entry relocation-target]} rel-entries]
      ;;         (let [section-addr addr
      ;;               target-addr (case (:st_bind (:st_info relocation-symbol))
      ;;                             :STB_GLOBAL
      ;;                             (let [{:keys [addr refs]} (find-sym-in-memory (:name relocation-symbol))]
      ;;                               addr)
      ;;                             :STB_LOCAL
      ;;                             (let [section (find-sym-section (:data relocation-target))
      ;;                                   {:keys [addr mem-refs]} (find-section-in-memory section)
      ;;                                   offset (:st_value relocation-symbol)
      ;;                                   target-addr (+ addr offset)]
      ;;                               (swap! mem-refs conj section-addr)
      ;;                               (println "local" addr offset)
      ;;                               target-addr)
      ;;                             nil)]
      ;;           (do-relocation target relocation-entry section-addr target-addr))))))
      ))
    ;;  If existing back refs to sym, update their reloc entries
    (throw-unresolved-symbol symbol-name)))

;; (cache-obj-file "c:/tmp/test1.elf")
;; (cache-obj-dir "c:/tmp/de0-nano-bsp")
;; (link-symbol "alt_putstr" null-target)
;; (debug-clear)
