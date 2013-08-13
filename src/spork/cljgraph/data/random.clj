(ns spork.cljgraph.data.random)

(defrecord randomq [q idx])

(def emptyrq (randomq. [] -1))

(defn- conj-many [rq coll]
  (let [q (reduce conj (:q rq) coll)
        idx (rand-int (count q))]
    (randomq. q idx)))
  
(defn- conj-single
  [rq itm]
  (let [q (conj (:q rq) itm)
        idx (if (> 1 (count q))
              (rand-int (count q))
              0)]
    (randomq. q idx)))

(defn- pair? [coll] (= 2 (count coll)))

(defn conj-rand [rq itm]
  (if (and (coll? itm) (not (pair? itm)))
    (conj-many rq itm)
    (conj-single rq itm)))

(defn random-q [& args] 
  (if-let [s (first args)]
    (conj-rand emptyrq s)
    emptyrq))

(defn peek-rand [rq] 
  (if-let [q (:q rq)]
    (get q (:idx rq) )))

(defn pop-rand [rq] 
	  (let [old (:q rq)
          idx (:idx rq)
          c (count old)]
     (cond 
       (or (= 0 c) (= 1 c)) emptyrq           
       :else (randomq. (vec (concat (subvec old 0 idx) 
                                    (subvec old (inc idx) c))) 
                       (rand-int (dec c))))))

(defn peek-stream [rq]
  (map peek-rand 
        (take-while #(not= % emptyrq) 
                    (iterate pop-rand rq))))
