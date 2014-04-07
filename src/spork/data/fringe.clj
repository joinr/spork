(ns spork.data.fringe
  "Stock implementations of the IFringe protocol from spork.protocols.fringe, as 
   inspired by Bob Sedgewick's Graph Algorithms in C."
   (:require [spork.data      [priorityq :as pq] 
                              [randq :as rq]]
             [spork.protocols [core :as generic]])
   (:import  [java.util PriorityQueue]))

(def emptyq clojure.lang.PersistentQueue/EMPTY)

;;Now using mutable priority queues for search fringe.  This ends up 
;;being faster than my persistent priorityqueue implementation.
(defn entry-comparer [^clojure.lang.MapEntry l ^clojure.lang.MapEntry r] 
  (let [pl (.key l)
        pr (.key r)]
    (cond (< pl pr) -1 
          (> pl pr) 1
          :else 0))) 

(defn ^PriorityQueue make-pq 
  ([] (PriorityQueue. 11 entry-comparer))
  ([compf] (PriorityQueue. 11 compf)))

(defn ^PriorityQueue pq [xs] 
  (reduce (fn [^PriorityQueue acc x]   
            (doto acc (.add x))) (make-pq) xs))
            
(defn ^PriorityQueue add-pq  [^PriorityQueue q obj]  (doto q (.add obj)))
(defn ^PriorityQueue pop-pq  [^PriorityQueue q    ]  (do (.poll q) q))


;;__TODO__ re-evaluate the use of entries as a standard api choice.
;;Do we really need access to the node weights?  Can't we just look them up?
;;The current scheme is fine if the cost of a weight function is high, but 
;;typically it'll just be a graph lookup...We might be introducing some 
;;overhead due to all the garbage creation for the entry objects.  Merits 
;;re-looking.

;;Implementations of basic stack (depth first) and queue (breadth first) 
;;fringes.
(extend-protocol generic/IFringe 
  nil 
  (conj-fringe [fringe n w] (conj '() n))
  (next-fringe [fringe]  nil)
  (pop-fringe  [fringe]  nil) 
  clojure.lang.PersistentQueue
  (conj-fringe [fringe n w] (conj  fringe n))
  (next-fringe [fringe]     (first fringe))
  (pop-fringe  [fringe]     (pop   fringe))
  clojure.lang.PersistentList
  (conj-fringe [fringe n w] (conj fringe n))
  (next-fringe [fringe]     (first fringe))
  (pop-fringe  [fringe]     (pop fringe))
  clojure.lang.PersistentList$EmptyList
  (conj-fringe [fringe n w] (conj fringe n))
  (next-fringe [fringe]     nil)
  (pop-fringe  [fringe]     nil)
  clojure.lang.Cons 
  (conj-fringe [fringe n w] (conj fringe n))
  (next-fringe [fringe]     (first fringe))
  (pop-fringe  [fringe]     (pop fringe))
  spork.data.randq.randomq
  (conj-fringe [fringe n w] (conj fringe n))
  (next-fringe [fringe]     (peek fringe))
  (pop-fringe  [fringe]     (pop fringe))
  java.util.PriorityQueue
  (conj-fringe [fringe n w] (doto fringe (.add (generic/entry w n))))
  (next-fringe [fringe]     (when-let [^clojure.lang.MapEntry e (.peek ^PriorityQueue fringe)] (.val e)))
  (pop-fringe  [fringe]     (doto fringe (.poll))))

(extend-protocol generic/IClearable
  nil 
  (-clear [x] x)
  clojure.lang.PersistentQueue
  (-clear [x] emptyq)
  clojure.lang.PersistentList
  (-clear [x] '())
  clojure.lang.PersistentList$EmptyList
  (-clear [x] x)
  clojure.lang.Cons 
  (-clear [x] '())
  spork.data.randq.randomq
  (-clear [x]  rq/emptyrq)
  java.util.PriorityQueue
  (-clear [x]  (doto x (.clear))))

;;we wrap a priorityq with a map to get a priority fringe.
;;Acts as an associative fringe, i.e. keeps exactly one instance of a value 
;;on the fringe at any time.  Could be supplanted by a priority map, or a 
;;cheaplist, or a stock priority queue that doesn't bother to eliminate stale
;;values when re-weighing.
;;OBSOLETE
(defrecord pfringe [priorities ^spork.data.priorityq.pqueue fringe]
  generic/IFringe
  (conj-fringe [pf n w]
    (let [w (or w 0)]
      (pfringe. (assoc priorities n w)
                (if-let [wold (get priorities n)]                  
                  ;update the entry in the priorityq.
                  (pq/alter-value fringe n wold w)
                  (pq/push-node fringe n w)))))
  (next-fringe [pf] (peek fringe))
  (pop-fringe  [pf] 
    (if (empty? priorities) fringe 
      (pfringe. (dissoc priorities (peek fringe)) (pop fringe)))))  

;;The four primitive fringes.  Just aliases for provided implementations.
(def breadth-fringe 
  "Builds a fringe that stores [node weight] entries in first-in-first-out 
   FIFO order.  Backed by a persistent queue"
    emptyq)
;;Currently not used, in favor of mutable priority queue.  May find
;;use again, if I can profile it and make it competitive.  It's not
;;terrible, but the implementation is weak compared to the mutable pq.
(def priority-fringe
  "Builds a fringe that stores [node weight] entries in priority order, 
   according to minimal weight.  Backed by a sorted map."
   (->pfringe {} pq/minq))
(def random-fringe
  "Builds a fringe that stores [node weight] entries in random order.  Backed 
   by a spork.data.randq.randomq"
   rq/emptyrq)
(def depth-fringe 
  "Builds a fringe that stores [node weight] entries in last-in-first-out 
   LIFO order.  Backed by a persistent list."
  (list))

;;Testing
(comment 
  (def nodes [[:a 2]
              [:b 3]
              [:c 10]
              [:d 11]
              [:e 0]])
  (defn load-fringe [f &{:keys [xs] :or {xs nodes}}] (reduce (fn [acc [n w]]
          (generic/conj-fringe acc n w))
        f xs))
  (assert (= (generic/fringe-seq (load-fringe priority-fringe))
             '([:e 0] [:a 2] [:b 3] [:c 10] [:d 11])))
  (assert (= (generic/fringe-seq (load-fringe depth-fringe))
             '([:e 0] [:d 11] [:c 10] [:b 3] [:a 2])))
  (assert (= (generic/fringe-seq (load-fringe breadth-fringe))
             '([:a 2] [:b 3] [:c 10] [:d 11] [:e 0])))
  
  
)
