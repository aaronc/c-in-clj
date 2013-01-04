;;(add-ns-load-mapping "clj_parse" "c:/users/arc/dev/mtreader/apps/pharmaseqcommon/clj-parse/src/clj_parse")
;;(add-ns-load-mapping "c_in_clj" "c:/users/arc/dev/mtreader/apps/pharmaseqcommon/c-in-clj/src/c_in_clj")

(ns c-in-clj.gcc.gdb-server
  "Low level gdb remote protocol functions"
  (:import
   [System.Net.Sockets TcpClient]
   [System.Text Encoding]
   [System.Threading Thread EventWaitHandle EventResetMode]
   [System.Globalization NumberStyles]))

(def gdb-server-instance (atom nil))

(def ^:dynamic *gdb-server* nil)

(defn gdb-server []
  (or *gdb-server* @gdb-server-instance))

(def ^:private ^:const read-buf-size 8192)

(defn on-read [res]
  (try
    (let [{:keys [read-buf callback stream wait-handle res-data] :as connection} (.AsyncState res)
          read (.EndRead stream res)
          raw-data (.GetString Encoding/ASCII read-buf 0 read)
          data (or (second (re-matches #"\+\$(.*)\#.." raw-data)) raw-data)]
      (println "<-" data)
      (reset! res-data data)
      (when (or (.StartsWith raw-data "+") (.StartsWith raw-data "-"))
        (.Set wait-handle))
      (.BeginRead stream read-buf 0 read-buf-size
                   callback
                   connection))
    (catch Object ex
      (println "Error" ex))))

(defn gdb-server-connect
  [port & {:keys [host word-size] :as opts}]
  (let [host (or host "localhost")
        client (TcpClient. host port)
        stream (.GetStream client)
        read-buf (make-array Byte read-buf-size)
        callback (gen-delegate AsyncCallback [res] (on-read res))
        connection {:client client
                    :stream stream
                    :read-buf read-buf
                    :callback callback
                    :data (atom {})
                    :word-size (or word-size 4)
                    :res-data (atom nil)
                    :wait-handle (EventWaitHandle. false EventResetMode/AutoReset)}]
    (.BeginRead stream read-buf 0 read-buf-size
                callback
                connection)
    (reset! gdb-server-instance connection)))

(defn- calc-checksum [cmd]
  (let [sum (apply + (map char cmd))]
    (mod sum 256)))

(defn gre [cmd]
  (let [{:keys [stream wait-handle res-data]} (gdb-server)
        checksum (calc-checksum cmd)
        data (format "$%s#%x2" cmd checksum)
        bytes (.GetBytes Encoding/ASCII data)
        data-fragment (if (> (.Length data) 1024)
                        (str (.Substring data 0 1024) "...")
                        data)]
    (println "->" data-fragment)
    (.Write stream bytes 0 (.Length bytes))
    (when-not (.WaitOne wait-handle 1000)
      (throw (TimeoutException. "gdb server timed out")))
    @res-data))

(defn read-mem [addr length]
  (gre (str "m"
            (.ToString addr "x")
            ","
            (.ToString length "x"))))

(defn write-mem [addr bytes]
  (if (> (count bytes) 200)
    (doall
     (map-indexed
      (fn [i sub] (write-mem (+ addr (* i 200)) sub))
      (partition-all 200 bytes)))
    (gre (str "M"
              (.ToString addr "x")
              ","
              (.ToString (count bytes) "x")
              ":"
              (apply str (map (fn [x] (.ToString (byte x) "x2")) bytes))))))

(defn read-reg [n]
  (UInt64/Parse (gre (str "p" (.ToString n "x")))
                NumberStyles/HexNumber))

(defn get-word-size [] (:word-size (gdb-server)))

(defn- print-reg-value [r] (.ToString r (str "x" (* 2 (get-word-size)))))

(defn write-reg [n r]
  (gre (str "p" (.ToString n "x") "=" (print-reg-value r))))

(defn read-regs []
  (let [res (gre "g")
        regs (partition (* 2 (get-word-size)) res)]
    (for [reg regs]
      (if (.Contains reg "x")
        nil
        (UInt64/Parse (apply str reg) NumberStyles/HexNumber)))))

(defn write-regs [reg-values]
  (gre (apply str
              "G"
              (for [reg reg-values] (print-reg-value reg)))))
