;;Optimization note: Similar to orderedmap, the basemap is trigger reflection 
;;warnings for equiv.  Might type hint in typdef.

(ns spork.data.priorityq
  "The priorityq implementation is designed for use as an auxillary data
   structure.  It relies on the sorted-map from clojure.core, although I might 
   shift the implementation to use priority-map later.  The current 
   implementation maps a set of weights (keys in the sorted-map), to sequences
   of values.  This rises from the fact that many values may share the same 
   priority or weight.  Weight is assumed to be, although not enforced, a 
   numeric value, typically a floating point value.")


;;note, this persistent PQ is no longer in favor, because most of our 
;;usage for pq's are motivated by search algorithms.  I made the
;;mistake of defaulting to java.util.PriorityQueue for a search 
;;fringe, and while initially useful, I realized I was generating 
;;a lot of garbage by not updating/re-weighing the value of entries 
;;in the queue.  Note that the j.u.pq doesn't provide operations for 
;;re-weighing either.


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
      (if (= x (first xs))
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
    (entry (next-val pq) (first (keys pq)))))

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
  (for [[k q] (seq pq)
        v     q]
    (entry k v)))   
   
(defn priority-vals
  "Return a sequence of popped values from priorityq q."
  [pq]
  (map second (priority-entries pq)))
  
(defn- get-map [dir] (if (= dir :min) (sorted-map) (sorted-map-by >)))

;;A priority queue type to wrap all the previous operations.
(deftype pqueue [dir basemap entry-count _meta]
  Object
  (toString [this] (str (.seq this)))

  clojure.lang.ISeq
  (first [this] (next-val basemap))
  (next  [this]
    (if (empty? basemap) nil
        (pop this)))
  (more [this] (if (empty? basemap) nil
                   (pop this)))
  clojure.lang.IPersistentCollection
  (empty [this]  (pqueue. dir (get-map dir) 0 {}))
  (equiv [this that]
     (cond (= (type this) (type that)) (identical? basemap (.basemap that)))
           (sequential? that) 
           (loop [xs  (.seq this)
                  ys  (seq that)]
             (cond (and (empty? xs) (empty? ys)) true 
                   (=   (first xs) (first ys)) (recur (rest xs)
                                                      (rest ys))
                   :else nil)))   
  ;;Note -> if we don't implement this, vector equality doesn't work both ways!
  java.util.Collection  
  (iterator [self]    (clojure.lang.SeqIterator. (seq  self)))  
  (size     [self]     entry-count)  
  (toArray  [self]    (.toArray (seq self)))
  clojure.lang.Seqable
  (seq [this]  ; returns a LazySeq
    (if (zero? entry-count) 
      (seq basemap)
      (priority-vals basemap)))
  clojure.lang.Counted
  (count [this] entry-count)
  clojure.lang.IPersistentVector
  (cons [this a]
    ; called by conj
    (let [k (first a)
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
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (pqueue. dir basemap entry-count m))      
  clojure.lang.Reversible
  (rseq [this]  (concat (for [q (reverse basemap)]
                          (into '() q))))
  java.io.Serializable ;Serialization comes for free with the other stuff.
  )



(def minq (pqueue. :min (get-map :min) 0 {}))
(def maxq (pqueue. :max (get-map :max) 0 {}))
(def emptyq minq)


;;__TODO__ I think using the set as a filter is actually slow, from some forum
;;posts.  Look at optimizing that call in alter-weight to something else. 


;;Operations that act on priority queues.

(defn get-basemap [^pqueue pq] (.basemap pq))
(defn ^pqueue conj-basemap [new-basemap ^pqueue pq]
  (pqueue. (.dir pq) new-basemap (.entry-count pq) (._meta pq)))

(defn find-entry [^pqueue pq n] 
  (some (fn [x] (when (= (val x) n) x)) (priority-entries (get-basemap pq))))
  
(defn ^pqueue alter-value
  "Returns the result of disjoining node n, with weight wprev, and conjoining 
   node n with weight wnew, relative to the initial priorityq q.  Caller may 
   also supply an optional transformation to apply to the altered node, allowing
   for efficient changes to both weight and the values stored in the priority 
   queue."  
  ([^pqueue pq n wnew] (if-let [e (find-entry pq n)]
                 (alter-value pq n (key e) wnew)
                 (throw (Exception. 
                          (str "Attempting to alter an entry that doesn't exist."
                               n)))))
  ([^pqueue pq n wprev wnew]  
    (if (= wprev wnew) pq        
	      (let [basemap (get-basemap pq)
              nodes (drop-entry (get basemap wprev) n)]
	         (-> (if (empty? nodes) (dissoc basemap wprev)
	                                (assoc basemap wprev nodes))
	              (conj-node n wnew)
                (conj-basemap pq)))))
  ([^pqueue pq n wprev wnew transform]
    (let [basemap (get-basemap pq)]
      (if (= wprev wnew) (conj-basemap 
                           (assoc basemap wprev 
                                  (swap-entry (get basemap wprev) n 
                                              (transform n)))  
                           pq)
        (let [nodes (drop-entry (get basemap wprev) n)]
          (-> (if (empty? nodes) (dissoc basemap wprev)
                                 (assoc  basemap wprev nodes))
              (conj-node (transform n) wnew)
              (conj-basemap pq)))))))

;;a hack to avoid intermediate array-creation.
(defn ^pqueue push-node [^pqueue pq n v]
   (pqueue. (.dir pq) (conj-node (get-basemap pq) n v) (inc (.entry-count pq)) (._meta pq)))
;;__TODO__ Maybe implement a reader literal for the priority queue.

;;testing

(comment  
  (def samples [[:a 0.884] [:b 0.899] [:c 0.589] [:d 0.761]])
  (def ordered-samples (map first (sort-by second samples)))
  (def ordered-vec     (vec ordered-samples))
  (def regular-queue   (into empty-entries ordered-samples))
  (def the-q (into emptyq samples))
  (assert (= the-q ordered-samples))
  (assert (= the-q ordered-vec)) 
  (assert (= ordered-vec the-q)) 
  
)
 
