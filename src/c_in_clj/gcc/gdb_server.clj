(ns c-in-clj.gcc.gdb-server
  "Low level gdb remote protocol functions"
  (:import
   [System.Net.Sockets TcpClient]
   [System.Text Encoding]))

(def gdb-server-instance (atom nil))

(def ^:dynamic *gdb-server* nil)

(defn gdb-server []
  (or *gdb-server* @gdb-server-instance))

(defn on-read [res]
  (try
    (let [{:keys [read-buf callback stream] :as connection} (.AsyncState res)
          read (.EndRead stream res)
          data (.GetString Encoding/ASCII read-buf 0 read)]
      (println "<-" data)
      (.BeginRead stream read-buf 0 1024
                   callback
                   connection))
    (catch Object ex
      (println "Error" ex))))

(defn gdb-server-connect
  ([port] (gdb-server-connect "localhost" port))
  ([host port]
     (let [client (TcpClient. host port)
           stream (.GetStream client)
           read-buf (make-array Byte 1024)
           callback (gen-delegate AsyncCallback [res] (on-read res))
           connection {:client client
                       :stream stream
                       :read-buf read-buf
                       :callback callback
                       :data (atom {})}]
       (.BeginRead stream read-buf 0 1024
                   callback
                   connection)
       (reset! gdb-server-instance connection))))

(defn- calc-checksum [cmd]
  (let [sum (apply + (map char cmd))]
    (mod sum 256)))

(defn gre [cmd]
  (let [{:keys [stream]} (gdb-server)
        checksum (calc-checksum cmd)
        data (format "$%s#%x" cmd checksum)
        bytes (.GetBytes Encoding/ASCII data)]
    (println "->" data)
    (.Write stream bytes 0 (.Length bytes))))



