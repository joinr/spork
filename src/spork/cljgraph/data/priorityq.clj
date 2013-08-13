(ns spork.cljgraph.data.priorityq
  "The priorityq implementation is designed for use as an auxillary data
   structure.  It relies on the sorted-map from clojure.core, althogh I might 
   shift the implementation to use priority-map later.  The current 
   implementation maps a set of weights (keys in the sorted-map), to sequences
   of values.  This rises from the fact that many values may share the same 
   priority or weight.  Weight is assumed to be, although not enforced, a 
   numeric value, typically a floating point value.")

(def emptyq (sorted-map))
(def minq emptyq)
(def maxq (sorted-map-by >))

(defn conj-node
  "Conjoin node n onto priorityq  q with priority/weight w."
  ([q n w]
  (assoc q w
    (if-let [coll (get q w)] (conj coll n) [n])))
  ([q [n w]] (conj-node q n w)))

(defn conj-nodes
  "Conjoin many [node weight] pairs onto priorityq q."
  [q nws]
  (reduce conj-node q nws))

(defn next-node
  "Return {:node n :weight w} for the 
   highest-priority node." 
  [q]
  (if-let [[k v] (first q)]
    {:node (first v) :weight k}))

(defn drop-node
  "Return the priorityq resulting from disjoining the 
   highest priority node."
  [q]
  (let [[k v] (first q)]
    (cond
      (= 1 (count v)) (dissoc q k)
      :else (assoc q k (subvec v 1)))))

(defn alter-weight
  "Returns the result of disjoining node n, with weight wprev, and conjoining 
   node n with weight wnew, relative to the initial priorityq q"  
  [q n wprev wnew]
  (let [nodes (remove #{n} (get q wprev))
        qremaining
          (if (seq nodes)
                (assoc q wprev nodes)
                (dissoc q wprev))]
    (conj-node qremaining n wnew)))   


(defn node-stream
  "Return a sequence of popped {node weight} maps from 
   priorityq q."
  [q]
  (if-let [kv (next-node q)]
    (lazy-seq (cons kv (node-stream (drop-node q))))))




