(ns c-in-clj.platform)

(defn get-c-number-type [x]
  (let [ntype (type x)]
    (cond
     (= ntype Long)
     (if (and (>= x Integer/MIN_VALUE) (<= x Integer/MAX_VALUE))
       'i32 
       'i64)
     (= ntype Double)
     'double)))

(defn get-env-variable [var-name] (System/getenv var-name))
