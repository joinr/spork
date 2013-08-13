(ns spork.cljgraph.data.alist)

(defn pair
  ([itm]
    (if (coll? itm)
      (pair (first itm) (fnext itm))
      (pair itm (list))))
  ([k v] (list k v)))

(def chunk-size 1024)

(defn add-count [l] (with-meta l {:count 0}))
(defn inc-count 
  [l c] (with-meta l {:count (+ (:count (meta l)) c)})
  [l] (inc-count l 1))

(defn acount [l] (:count (meta l)))

(defn alist [& pairs]
 (let [lc 
       (reduce (fn [[acc c] x] [(cons (pair x) acc) (inc c)]) [(list) 0] pairs)]
    (inc-count (first lc) (fnext lc))))

             (defn apush 
  ([alist pair] (inc-count (cons pair alist)))
  ([alist k v] (inc-count (cons (pair k v) alist))))

(defn get-alist [alist k]
  (loop [hd (first alist)
         tail (rest alist)]
    (cond (nil? hd) nil
          (= (first hd) k) (fnext hd)
          :else (recur (fnext tail) (rest tail)))))
