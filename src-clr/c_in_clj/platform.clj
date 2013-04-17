(ns c-in-clj.platform
  (:import
   [System.IO Path File Directory]))

(defn ensure-directory [path]
  (when-not (Directory/Exists path)
    (Directory/CreateDirectory path)))

(defn get-temp-path []
  (Path/GetTempPath))

(defn path-combine [path1 path2]
  (Path/Combine path1 path2))

(defn write-text-file [filename txt]
  (File/WriteAllText filename txt))

(defn get-c-number-type [x]
  (let [ntype (type x)]
    (cond
     (= ntype Int64)
     (if (and (>= x Int32/MinValue) (<= x Int32/MaxValue))
       'i32 
       'i64 )
     (= ntype Double)
     'double)))

(defn get-env-variable [var-name]
  (Environment/GetEnvironmentVariable var-name))
