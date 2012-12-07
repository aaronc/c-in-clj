;;(add-ns-load-mapping "clj_parse" "c:/users/arc/dev/mtreader/apps/pharmaseqcommon/clj-parse/src/clj_parse")
;;(add-ns-load-mapping "c_in_clj" "c:/users/arc/dev/mtreader/apps/pharmaseqcommon/c-in-clj/src/c_in_clj")

(ns c-in-clj.gcc.gdb
  (:use [clj-parse core helpers])
  (:require [clojure.string :as str])
  (:import [System.Diagnostics Process ProcessStartInfo
            DataReceivedEventHandler]))

(defn- start-process [path args & {:keys [no-shell-exec redirect-stdout redirect-stdin redirect-stderr create-no-wind]}]
  (let [psi (ProcessStartInfo.  (Environment/ExpandEnvironmentVariables path))]
    (set! (.Arguments psi) args)
    (when no-shell-exec (.set_UseShellExecute psi (not no-shell-exec)))
    (when redirect-stdout (.set_RedirectStandardOutput psi redirect-stdout))
    (when redirect-stdin (.set_RedirectStandardInput psi redirect-stdin))
    (when redirect-stderr (.set_RedirectStandardError psi redirect-stderr))
    (when create-no-wind (.set_CreateNoWindow psi create-no-wind))
    (Process/Start psi)))

;;gdb-server (start-process "%SOPC_KIT_NIOS2%\\Nios II Command
;;Shell.bat" "nios2-gdb-server --tcpport 54321 --tcppersist")

(def mi-token (mapply (comp int str) (m+ (mcharrange \0 \9))))

(def mi-result-class
  (mapply keyword
          (mor (mcharstr "done") (mcharstr "running") (mcharstr "connected")
               (mcharstr "error") (mcharstr "exit"))))

(def mi-variable (mapply (comp keyword str) (m+ (mexcept \=))))

(def mi-c-string
  (mapply
   (comp read-string str)
   (mseq \"
         (m* (m1extended
              (fn test-c-str [x ctxt]
                (or (not= \" x)
                    (= \\ (last (second ctxt)))))))
         \")))

(def mi-const mi-c-string)

(declare mi-result)

(def mi-tuple
  (mor
   (mconstantly [{}] (mcharstr "{}"))
   (mapply
    hash-map
    (mseq (mignore \{) mi-result
          (m* (mseq (mignore \,) mi-result))
          (mignore \})))))

(declare mi-value)

(def mi-list
  (mor
   (mconstantly [[]] (mcharstr "[]"))
   (mgroup
    (mseq (mignore \[)
          mi-value
          (m* (mseq (mignore \,) mi-value)) (mignore \])))
   (mapply
    hash-map
    (mseq (mignore \[) mi-result (m* (mseq (mignore \,) mi-result)) (mignore \])))))

(def mi-value (mor mi-const mi-tuple mi-list))

(def mi-result (mseq mi-variable (mignore \=) mi-value))

(defn- mi-create-res-fn [type]
  (fn mi-create-res [token res-class data]
    {:type type
      :token token
      :class res-class
      :data data}))

(def mi-result-record
  (mapply
   (mi-create-res-fn :result-record)
   (mseq
    (mdefault [nil] (m? mi-token))
    (mignore \^)
    mi-result-class
    (mapply
     hash-map
     (m* (mseq (mignore \,) mi-result))))))

(def mi-async-class
  (mapply (comp keyword str) (m+ (mexcept \,))))

(def mi-async-output
  (mseq mi-async-class (mapply hash-map (m* (mseq (mignore \,) mi-result)))))

(def mi-exec-async-output
  (mapply
   (mi-create-res-fn :exec-async-output)
   (mseq (mdefault [nil] (m? mi-token)) (mignore \*) mi-async-output)))

(def mi-status-async-output
  (mapply
   (mi-create-res-fn :status-async-output)
   (mseq (m? mi-token) (mignore \+) mi-async-output)))

(def mi-notify-async-output
  (mapply
   (mi-create-res-fn :notify-async-output)
   (mseq (m? mi-token) (mignore \=) mi-async-output)))

(def mi-async-record
  (mor
   mi-exec-async-output
   mi-status-async-output
   mi-notify-async-output))

(def mi-console-stream-output
  (mapply
   (fn [x] {:type :console-stream-output :data x})
   (mseq (mignore \~) mi-c-string)))

(def mi-target-stream-output
  (mapply
   (fn [x] {:type :target-stream-output :data x})
   (mseq (mignore \@) mi-c-string)))

(def mi-log-stream-output
  (mapply
   (fn [x] {:type :log-stream-output :data x})
   (mseq (mignore \&) mi-c-string)))

(def mi-stream-record
  (mor
   mi-console-stream-output
   mi-target-stream-output
   mi-log-stream-output))

(def mi-out-of-band-record
  (mor
   mi-async-record
   mi-stream-record))

(def gdb-mi-output-line
  (mor
   mi-out-of-band-record
   mi-result-record
   (mcharstr "(gdb)")))

(def glines (atom []))

(defn parse-gdb-mi-line [line]
  (try
    (first (gdb-mi-output-line (str/trim line)))
    (catch Object ex
      (throw ex))))

(defn gdb-data-received [sender args callbacks]
  (try
      (let [line (.Data args)]
        (let [res (parse-gdb-mi-line line)
              token (:token res)]
          (cond
           token (do
                   ;;(println "gdb:" line)
                   ;;(prn res)
                   ((get @callbacks token) res))
           (= "(gdb)" res) nil
           :default (do
                      (println "gdb:" line)
                      (prn res)))))
      (catch Object ex
        (println "Error"))))

(defn- wrap-gdb-data-received [callbacks]
  (fn on-gdb-data-received [sender args]
    (gdb-data-received sender args callbacks)))

(def gdb-client (atom nil))

(def ^:dynamic *gdb-client* nil)

(defn get-gdb-client []
  (or *gdb-client* @gdb-client))

(defn start-gdb []
  (let [;;gdb-server (start-process "%SOPC_KIT_NIOS2%\\Nios II Command
        ;;Shell.bat" "nios2-gdb-server --tcpport 54321 --tcppersist")
        gdb (Process.)
        psi (.StartInfo gdb)
        callbacks (atom {})
        data-received-fn (wrap-gdb-data-received callbacks)
        data-received-dg (gen-delegate DataReceivedEventHandler [s e]
                                       (data-received-fn s e))]
    (.set_FileName psi (Environment/ExpandEnvironmentVariables "%SOPC_KIT_NIOS2%\\Nios II Command Shell.bat"))
    (.set_Arguments psi "nios2-elf-gdb --interpreter=mi")
    (.set_UseShellExecute psi false)
    (.set_RedirectStandardOutput psi true)
    (.set_RedirectStandardInput psi true)
    (.set_RedirectStandardError psi true)
    ;;(.set_CreateNoWindow psi create-no-wind)
    (.add_OutputDataReceived gdb data-received-dg)
    (.Start gdb)
    (.BeginOutputReadLine gdb)
    (let [stdin (.StandardInput gdb)]
      (reset! gdb-client {:client gdb :stdin stdin
                          :callbacks callbacks
                          :data (atom {})}))))

(def ^:private gdb-token-idx (atom 0))

(defn gde [& cmds+callback]
  (let [callback? (first cmds+callback)
        callback (when (ifn? callback?) callback?)
        cmds (if callback (rest cmds+callback) cmds+callback)
        {:keys [stdin callbacks]} (get-gdb-client)
        token (when callback (swap! gdb-token-idx inc))]
    (when callback
      (swap! callbacks assoc token callback))
    (.WriteLine stdin (str token (apply str cmds)))
    token))

(defn unregister-callback [token]
  (let [callbacks (:callbacks (get-gdb-client))]
    (swap! callbacks dissoc token)))

(defn gdb-connect [port]
  (gde "-target-select remote localhost:" port))

(defn kill-gdb []
  (gde "quit")
  (let [{:keys [client callbacks]} (get-gdb-client)]
    (reset! callbacks nil)))

(defn get-client-data []
  (:data (get-gdb-client)))

(defn gdb-sync-exec [& cmds]
  (let [res-promise (promise)
        token (apply gde
           (fn [res] (deliver res-promise res))
           cmds)
        res @res-promise]
    (unregister-callback token)
    res))
