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
