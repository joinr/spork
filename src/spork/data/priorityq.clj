(ns spork.data.priorityq
  "The priorityq implementation is designed for use as an auxillary data
   structure.  It relies on the sorted-map from clojure.core, although I might 
   shift the implementation to use priority-map later.  The current 
   implementation maps a set of weights (keys in the sorted-map), to sequences
   of values.  This rises from the fact that many values may share the same 
   priority or weight.  Weight is assumed to be, although not enforced, a 
   numeric value, typically a floating point value.")

;;A simple type tagging mechanism.  Since priority queues use clojure's 
;;ordered maps, we can use this info for a second layer of type tagging.
(defn tag-as-pq [x]
  (with-meta x (assoc (meta x) ::priority-queue true)))

;;duplicated.
(defn- ^clojure.lang.MapEntry entry [k v] (clojure.lang.MapEntry. k v))

;;We map priorities to a queue of values.  These are stock queue manipulation 
;;functions to allow us to eliminate items from our queue containers.
(def empty-entries clojure.lang.PersistentQueue/EMPTY)
(defn- drop-entry [q x] 
  (loop [acc empty-entries
         xs  q]
    (if (empty? xs) acc    
      (if (= x (first x))
          (into acc (pop xs))
          (recur (conj acc (first xs)) (pop xs))))))

(defn- swap-entry [q x1 x2] 
  (loop [acc empty-entries
         xs  q]
    (if (empty? xs) acc    
      (if (= x1 (first xs))
          (into (conj acc x2) (pop xs))
          (recur (conj acc (first xs)) (pop xs))))))

(defn priority-queue? [m] (contains? (meta m) ::priority-queue))

(defn conj-node
  "Conjoin node n onto priorityq  q with priority/weight w."
  ([q n w]  (assoc q w
                   (if-let [coll (get q w)] 
                     (conj coll n) (conj empty-entries n))))
  ([q [n w]] (conj-node q n w)))

(defn conj-many
  "Conjoin many [node weight] pairs onto priorityq q."
  [q entries]
  (reduce conj-node q entries))

(defn next-val
  "Return the highest-priority value." 
  [pq]
  (first (first (vals pq))))

(defn next-entry 
  "Return [priority x] for the highest-priority node." 
  [pq]
  (when (seq pq)
    (entry (first (keys pq)) (next-val pq))))

(defn drop-first
  "Return the priorityq resulting from disjoining the 
   highest priority node."
  [pq]
  (if (empty? pq) pq
    (let [[w q] (first pq)]
      (cond
        (= 1  (count q))  (dissoc pq w)
        :else             (assoc pq w (pop q))))))

(defn priority-entries [pq]
  (map next-entry (take-while (complement empty?) (iterate drop-first pq))))

(defn priority-vals
  "Return a sequence of popped values from priorityq q."
  [pq]
  (map second (priority-entries pq)))

;;__TODO__ I think using the set as a filter is actually slow, from some forum
;;posts.  Look at optimizing that call in alter-weight to something else. 

(defn alter-value
  "Returns the result of disjoining node n, with weight wprev, and conjoining 
   node n with weight wnew, relative to the initial priorityq q.  Caller may 
   also supply an optional transformation to apply to the altered node, allowing
   for efficient changes to both weight and the values stored in the priority 
   queue."  
  ([pq n wprev wnew]  
    (let [nodes (drop-entry (get pq wprev) n)]
      (-> (if (empty? nodes) (dissoc pq wprev)
                             (assoc pq wprev nodes))
          (conj-node n wnew))))
  ([pq n wprev wnew transform]  
    (let [nodes (drop-entry (get pq wprev) n)]
      (-> (if (empty? nodes)
            (dissoc pq wprev)
            (assoc pq wprev nodes))
          (conj-node (transform n) wnew)))))

(defn- get-map [dir]
  (if (= dir :min) (sorted-map) (sorted-map-by >)))

;;A priority queue type to wrap all the previous operations.
(deftype pqueue [dir basemap entry-count _meta]
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ISeq
  (first [this] (next-val basemap))
  (next  [this]
    (if (empty? basemap) nil
        (pop this)))
  (more [this] (if (empty? basemap)
                 (.empty this)
                 (pop this)))
  clojure.lang.IPersistentCollection
  (empty [this]  (pqueue. dir (get-map dir) 0 {}))
  (equiv [this that]
    (and (instance? pqueue that)
         (= dir (:dir that))
         (identical? basemap (:basemap that))))
  clojure.lang.Seqable
  (seq [this]  ; returns a LazySeq
    (priority-vals basemap))
  clojure.lang.Counted
  (count [this] entry-count)
  clojure.lang.IPersistentVector
  (cons [this a]
    ; called by conj
    (loop [k (first a)
           v (second a)]
      (pqueue. dir (conj-node basemap k v) (inc entry-count) _meta)))
  (length [this]  (.count this))
  (assocN [this index value]
    (if (and (not (zero? entry-count))
             (>= index entry-count)) (throw (Exception. "Index Out of Range"))
      (let [k (nth (keys basemap))]
        (pqueue. dir (assoc basemap k value) entry-count _meta))))
  clojure.lang.IPersistentStack
  (pop  [this] (if (empty? basemap) this
                   (pqueue. dir (drop-first basemap) (dec entry-count) _meta)))
  (peek [this] (next-val basemap))
  clojure.lang.Indexed
  (nth [this i] (if (and (>= i 0) 
                         (< i entry-count)) 
                    (nth (priority-vals basemap) i)
                    (throw (Exception. (str "Index out of range " i )))))
  (nth [this i not-found] 
    (if (and (< i entry-count) (>= i 0))
        (nth (priority-vals basemap) i)       
         not-found))
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))      
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (pqueue. dir basemap entry-count m))      
  clojure.lang.Reversible
  (rseq [this]  (reverse (priority-vals  basemap)))
  java.io.Serializable ;Serialization comes for free with the other stuff.
  )

(def minq (pqueue. :min (get-map :min) 0 {}))
(def maxq (pqueue. :max (get-map :max) 0 {}))
(def emptyq minq)

;;testing

(comment 
  (def the-q emptyq)
)
 
