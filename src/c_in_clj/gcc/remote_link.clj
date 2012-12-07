(ns c-in-clj.gcc.remote-link
  (:require [c-in-clj.gcc.gdb :as gdb]))

(comment
  (defn read-memory [addr count]
    (gdb/gdb-sync-exec "-data-read-memory u 1 " addr " " count)))

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
