;;Optimization note: Similar to orderedmap, the basemap is trigger reflection 
;;warnings for equiv.  Might type hint in typdef.

(ns spork.data.priorityq
  "The priorityq implementation is designed for use as an auxillary data
   structure.  It relies on the sorted-map from clojure.core, although I might 
   shift the implementation to use priority-map later.  The current 
   implementation maps a set of weights (keys in the sorted-map), to sequences
   of values.  This rises from the fact that many values may share the same 
   priority or weight.  Weight is assumed to be, although not enforced, a 
   numeric value, typically a floating point value."
  (:require [clojure.data.avl :as avl]
            [clojure.core.reducers :as r]
            [spork.data.protocols]))


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
  ([^clojure.lang.IPersistentMap q n w]
   (.assoc ^clojure.lang.Associative q w
           (if-let [^clojure.lang.PersistentQueue coll (.valAt q w)] 
             (.cons coll n)
             (.cons ^clojure.lang.PersistentQueue empty-entries n)))))
 ; ([q [n w]] (conj-node q n w)
   ;)

(defn conj-many
  "Conjoin many [node weight] pairs onto priorityq q."
  [q entries]
  (reduce conj-node q entries))

(definline next-val
  "Return the highest-priority value." 
  [pq]
  (let [entries (with-meta (gensym "entries") {:tag 'clojure.lang.ISeq})]
    `(let [~entries (vals ~pq); (.first (.first (vals pq))))
           ~entries (.first ~entries)]
       (.first ~entries))))
           
(defn next-entry 
  "Return [priority x] for the highest-priority node." 
  [pq]
  (when (seq pq)
    (entry (next-val pq) (first (keys pq)))))

(defn drop-first
  "Return the priorityq resulting from disjoining the 
   highest priority node."
  [^clojure.lang.IPersistentMap pq]
  (if (zero? (.count  pq)) pq
    (let [[w q] (first pq)]
      (cond
        (= 1  (count q))  (dissoc pq w)
        :else             (assoc pq w (pop q))))))

;;we can probably do better than this.. :(
(defn priority-entries [pq]
  (for [[k q] (seq pq)
        v     q]
    (entry k v)))   
   
(defn priority-vals
  "Return a sequence of popped values from priorityq q."
  [pq]
  (map second (priority-entries pq)))
  
(defn- get-map [dir]
  (if (= dir :min)
    (sorted-map)
    (sorted-map-by >)))

;;an alternative implementation...
;;use triples. [t o entry]
;;where o is a monotonically increasing number.
;;so, we sort based on t, then o, and store the entry.
;;should be pretty fast with a primitive.
(deftype pkey [^double k ^long o
               ^:unsynchronized-mutable ^int _hasheq
               ^:unsynchronized-mutable ^int _hash]
  clojure.lang.IPersistentVector
  (count [obj] 2)
  (seq [obj] (list k o))
  (assocN [this k x]
    (case k
      0   (pkey. (double x) o -1 -1)
      1   (pkey. k (long x)  -1 -1)
      (throw (Exception. (str [:index-out-of-bounds k])))))
  (empty [this] (pkey. 0.0 0  -1 -1))
  ;;cons defines conj behavior
   clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (.nth this k))
  (valAt [this k not-found] (.nth this k not-found))
  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq (int -1))
      (let [h (hash-ordered-coll [k o])]
        (do (set! _hasheq (int h))
            h))
      _hasheq))
  (hashCode [this]
    (if (== _hash (int -1))
      (let [h (hash-ordered-coll [k o])]
        (do (set! _hash (int h))
            h))
      _hash))
  (equals [this o] (identical? this o))
  (equiv [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
              :else nil))  
  clojure.lang.Indexed
  (nth [obj idx]
    (case (int idx)
      0  k
      1  o
      (throw (Exception. (str [:idx idx :out-of-bounds])))))
  (nth [obj idx not-found]
    (if (and (not (neg? idx)) (> idx 2)) not-found
      (case (int idx)
        0  k
        1  o
        (throw (Exception. (str [:idx idx :out-of-bounds]))))))
  )

;;basis for priority entries, using a simply sorted-map.
(defn ->pentry [^double k ^long idx v] (clojure.lang.MapEntry.  (pkey. k idx -1 -1) v))
(definline min-key-compare [lentry rentry]
  (let [l (with-meta (gensym "l") {:tag 'spork.data.priorityq.pkey})
        r (with-meta (gensym "r") {:tag 'spork.data.priorityq.pkey})]
    `(let [~l ~lentry
           ~r ~rentry
           kl# (.k ~l)
           kr# (.k ~r)]
       (cond (== kl# kr#)
             (let [il# (.o ~l)
                   ir# (.o ~r)]
               (cond (== il# ir#) 0
                     (< il# ir#)  -1
                     :else 1))
             ;;compare.
             (< (.k ~l) (.k ~r)) -1
             :else 1))))
(definline max-key-compare [rentry lentry]
  (let [l (with-meta (gensym "l") {:tag 'spork.data.priorityq.pkey})
        r (with-meta (gensym "r") {:tag 'spork.data.priorityq.pkey})]
    `(let [~l ~lentry
           ~r ~rentry
           kl# (.k ~l)
           kr# (.k ~r)]
       (cond (== kl# kr#)
             (let [il# (.o ~l)
                   ir# (.o ~r)]
               (cond (== il# ir#) 0
                     (< il# ir#)  -1
                     :else 1))
             ;;compare.
             (< (.k ~l) (.k ~r)) -1
             :else 1))))

(definline min-entry-compare [lentry rentry]
  (let [l (with-meta (gensym "l") {:tag 'clojure.lang.MapEntry})
        r (with-meta (gensym "r") {:tag 'clojure.lang.MapEntry})]
    `(let [~l ~lentry
           ~r ~rentry]
       (min-key-compare (.key ~l) (.key ~r)))))

(definline max-entry-compare [rentry lentry]
  (let [l (with-meta (gensym "l") {:tag 'clojure.lang.MapEntry})
        r (with-meta (gensym "r") {:tag 'clojure.lang.MapEntry})]
    `(let [~l ~lentry
           ~r ~rentry]
       (min-key-compare (.key ~l) (.key ~r)))))


;;there are two ways to view entries here.
;;We have a seq of [pri val]
;;we also want to have map-like access to the entry itself.
;;specifically, if we want to alter an entry, we need to be able
;;to get it.  That means we need to know its order.  For instance,
;;how would we update the val? If we had indexed access to the entries,
;;and we could easily compute the index, we'd just assocN into it.
;;Instead of indexing, we can do range queries using subseq and rsubseq
;;on the tree map.

(def minset (avl/sorted-set-by min-entry-compare))
(def maxset (avl/sorted-set-by max-entry-compare))
(defn get-set [dir]
  (case dir
    :min minset
    maxset))

(declare ->tpri)
(definline head [idx]
  (let [h (with-meta (gensym "head") {:tag 'clojure.lang.Indexed})]
    `(let [~h ~idx]
       (.nth ~h 0 nil))))
       
;;pequeue implementation using - hopefully - optimized entries and a
;;simpler method for adding to the queue.
(deftype pri [dir ^clojure.data.avl.AVLSet basemap
              ^:unsynchronized-mutable ^long n
              _meta
              ^:unsynchronized-mutable ^int _hash
              ^:unsynchronized-mutable ^int _hasheq]
  spork.data.protocols.IFrontBack
  (front [this] (.nth this 0 nil))
  (back  [this] (.nth this (unchecked-dec n) nil))
  spork.data.protocols.IInsertable
  (insert [this a] (.cons this a))
  clojure.data.avl.IAVLTree
  (getTree [this] (.getTree basemap))
  clojure.data.avl.INavigableTree
  (nearest [this test k]  (.nearest basemap test k))
  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq (int -1))
      (let [h (hash-unordered-coll [dir basemap])]
        (do (set! _hasheq (int h))
            h))
      _hasheq))
  (hashCode [this]
    (if (== _hash (int -1))
      (let [h (hash-ordered-coll [dir basemap])]
        (do (set! _hash (int h))
            h))
      _hash))
  (equals [this o] (identical? this o))
  (equiv [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
              :else nil))  
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.Indexed
  (nth [obj idx] (.nth  basemap idx)) 
  (nth [obj idx not-found] (.nth ^clojure.data.avl.AVLSet basemap idx not-found))
  clojure.lang.IObj
  (meta [this] _meta)
  (withMeta [this m] (pri. dir basemap n m _hash _hasheq))
  clojure.lang.ISeq
  (first [this] (head  basemap)) ;way faster than first..
  (next  [this] (.pop this))
  (more [this]  (.pop this))
  clojure.lang.IPersistentCollection
  (empty [this]  (pri. dir (get-set dir) 0 {} -1 -1 ))
  ;;Note -> if we don't implement this, vector equality doesn't work both ways!
  java.util.Collection  
  (iterator [self]    (clojure.lang.SeqIterator. (seq  self)))  
  (size     [self]    (.count self))  
  (toArray  [self]    (.toArray (seq self)))
  clojure.lang.Seqable
  (seq [this]  ; returns a LazySeq
    (seq basemap))
  clojure.lang.Counted
  (count [this] (.count basemap))
 ; clojure.lang.IPersistentVector
  (cons [this  a]
    ;;check for rollover.  In that event, repack existing entries.  It's inconceivable that this would happen though.
    ; called by conj
    (let [k  (.nth  ^clojure.lang.Indexed a 0)
          v  (.nth  ^clojure.lang.Indexed a 1)
          nnxt (unchecked-inc n)]
      (if (== nnxt Long/MIN_VALUE) ;;rollover
        (let [idx     (volatile! 0)
              basemap (persistent!
                       (reduce (fn [acc e]
                                 (let [^pkey k (key e)
                                       e (->pentry (.k k) @idx (val e))
                                       _ (vswap! idx unchecked-inc)]
                                   (conj! acc e)))
                               (transient (get-set dir)) basemap))]
          (pri. dir basemap @idx _meta -1 -1))        
        (pri. dir (.cons basemap (->pentry k n v)) nnxt  _meta -1 -1))))
  clojure.lang.IPersistentSet
  (disjoin  [this k] (pri. dir (.disjoin basemap k) n _meta -1 -1))
  (contains [this k] (.contains basemap k))
  (get      [this k] (.get basemap k))
  clojure.lang.Sorted
  (seq [this ascending?]
    (.seq ^clojure.lang.Sorted basemap ascending?))
  (seqFrom [this k ascending?] (.seqFrom basemap k ascending?))
  (entryKey [this entry]        entry)
  (comparator [this] (.comparator basemap))
  clojure.lang.IPersistentStack
  (pop  [this] (if (zero?    (.count basemap)) this
                   (pri. dir (.disjoin basemap (.nth basemap 0))
                             n _meta -1 -1)))
  (peek [this] (head basemap))
  clojure.lang.Reversible
  (rseq [this]  (.rseq basemap))
  java.io.Serializable ;Serialization comes for free with the other stuff.
  clojure.lang.IEditableCollection
  (asTransient [this] (->tpri dir (transient basemap) n _meta))
  clojure.core.protocols/IKVReduce
  (kv-reduce [this f init]
    (.kv-reduce ^clojure.data.avl.AVLMap (.avl-map basemap)
                (fn [acc ^clojure.lang.MapEntry k _]
                  (f acc (.key k) (.val k)))
                init))
  clojure.core.protocols/CollReduce
  (coll-reduce [coll f]
      (reduce-kv (fn [acc k _] (f acc k))  (head coll) (r/drop 1 (.avl-map basemap))))  
  (coll-reduce [coll f init]
      (reduce-kv (fn [acc k _] (f acc k))  init  (.avl-map basemap)))
    
  
  )

(deftype tpri [dir 
              ^:unsynchronized-mutable ^clojure.data.avl.AVLTransientSet basemap
              ^:unsynchronized-mutable ^long n
               ^:unsynchronized-mutable _meta]
  spork.data.protocols.IFrontBack
  (front [this] (.nth this 0 nil))
  (back  [this] (.nth this (unchecked-dec n) nil))
  spork.data.protocols.IInsertable
  (insert [this a] (.conj this a))
  clojure.lang.IObj
  (meta [this] _meta)
  (withMeta [this m] (do (set! _meta m) this))
  clojure.lang.Counted
  (count [this] (.count basemap))
  clojure.lang.ITransientSet  
  (get [this k] (.get basemap k))
  (contains [this k] (.contains basemap k))
  (disjoin [this k] (do (set! basemap (.disjoin basemap k)) this))
  (conj [this  a] 
    ;;check for rollover.  In that event, repack existing entries.  It's inconceivable that this would happen though.
    ; called by conj
    (let [k  (.nth ^clojure.lang.Indexed a 0)
          v  (.nth ^clojure.lang.Indexed a 1)
          nnxt (unchecked-inc n)]
      (if (== nnxt Long/MIN_VALUE) ;;rollover
        (let [idx (volatile! 0)
              bm (reduce (fn [acc e]
                                (let [^pkey k (key e)
                                      e (->pentry (.k k) @idx (val e))
                                      _ (vreset! idx (unchecked-inc @idx)) 
                                      ]                                  
                                  (conj! acc e)))
                              (transient (get-set dir)) (persistent! basemap))]
          (do (set! basemap bm)
              (set! n (long @idx))
              this))
        (do (set! basemap (.conj basemap (->pentry k n v)))
            (set! n nnxt)
            this))))
  (persistent [this] (pri. dir (persistent! basemap) n _meta -1 -1)))

(def minpri (pri. :min minset 0 {} -1 -1))
(def maxpri (pri. :max maxset 0 {} -1 -1))

;;operations on pris
(defn at [^pri m t]
  (subseq m >= (->pentry t 0 nil) <=  (->pentry t Long/MAX_VALUE nil)))
(defn earliest [^pri m t]
  (avl/nearest m = (->pentry t 0 nil)))
(defn latest [^pri m t]
  (avl/nearest m = (->pentry t Long/MAX_VALUE nil)))
(defn closest [^pri m t]
  (avl/nearest m <= (->pentry t Long/MAX_VALUE nil)))
(defn tnext [^pri m]
  (when-let [^clojure.lang.MapEntry e (.peek m)]
    (.nth ^clojure.lang.Indexed (.key e) 0)))

(definline marker [t] `(->pentry ~t Long/MAX_VALUE nil))
;; (defn chunk-width [^pri m]
;;   (let [t (tnext m)
;;         hd (first m)
;;         tl (latest m t)]
    

(defprotocol IChunkQueue
  (chunk-peek- [obj])
  (chunk-pop- [obj]))

;;remove
(defn chunk-peek [^pri m]   (subseq m <= (marker (tnext m))))
;;This is waaaay faster than going the seq/doall route in subseq.
(defn chunk-peek! [^pri m]
  (let [t (tnext m)]
    (r/take-while (fn  [^clojure.lang.MapEntry e]
                    (== (.k ^pkey (.key e))  t)) m)))

;;this isn't as lazy as I'd like.
;; (defn chunks! [^pri m]
;;   (let [t (tnext m)]
;;     (sequence (partition-by (fn  [^clojure.lang.MapEntry e]
;;                               (== (.k ^pkey (.key e))  t)))
;;             m)))

;;may be faster if we can use split-at.
(defn chunk-pop!
  ([^pri m xs]
   (persistent!
    (reduce (fn [^tpri acc x]
              (.disjoin acc x))
            (transient m) xs)))
  ([^pri m] (chunk-pop! m (chunk-peek! m))))

(defn chunk-pop
  ([^pri m xs]
    (reduce (fn [^tpri acc x]
              (pop acc)) m
            xs))
  ([^pri m] (chunk-pop m (chunk-peek! m))))

(defn head [coll] (reduce (fn [acc x] (reduced x)) nil coll))
(comment ;testing
  ;;this isn't terrible.
  ;;we can 
  (def xs (range 1000000))
  (def m  (into minpri (map (fn [n] [(quot n 100) n])) xs))

  (time
   (let [bound (.count ^pri m)]
     (loop [idx 0
            acc m
            ]
       (if (== idx bound) nil
           (recur
            (unchecked-inc idx)
            (.pop ^pri acc))))))
  ;;much faster,although we're still slow.  We should be able to
  ;;drain the queue quickly.  Could look into setting up a mutable
  ;;set.  Chunk-pop won't work because we don't know which items are
  ;;in the transient set.  We have to coerce back and forth.
  (time (loop [acc m]
          (when (pos? (.count ^pri acc))
            (recur (chunk-pop ^pri acc)))))
  ;;faster still.

  )


;; (defn foldq [q n combinef reducef]
;;   (cond
;;     (empty? q) (combinef)
;;     (<= (count a) n) (reduce reducef (combinef) q)
;;     :else
;;     (let [split (quot (count q) 2)
;;           v1    (avl/split-at q 

                              
;;A priority queue type to wrap all the previous operations.
(deftype pqueue [dir ^clojure.lang.IPersistentMap basemap entry-count _meta]
  Object
  (toString [this] (str (.seq this)))

  clojure.lang.ISeq
  (first [this] (next-val basemap))
  (next  [this]
    (if (zero? (.count basemap)) nil
        (.pop this)))
  (more [this] (if (zero? (.count basemap)) nil
                   (.pop this)))
  clojure.lang.IPersistentCollection
  (empty [this]  (pqueue. dir (get-map dir) 0 {}))
  (equiv [this that]
    (cond (= (type this) (type that))
            (and  (identical? dir (.dir ^pqueue that))
                  (identical? basemap (.basemap ^pqueue that)))
          (sequential? that) 
            (loop [xs  (.seq this)
                   ys  (seq that)]
              (cond (and (empty? xs) (empty? ys)) true 
                    (=   (first xs) (first ys)) (recur (rest xs)
                                                       (rest ys))
                    :else nil))))   
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
    (let [k  (first a)
          v  (second a)]
      (pqueue. dir (conj-node basemap k v) (unchecked-inc entry-count) _meta)))
  (length [this]  (.count this))
  (assocN [this index value]
    (if (and (not (zero? entry-count))
             (>= index entry-count)) (throw (Exception. "Index Out of Range"))
      (let [k (nth (keys basemap) index)]
        (pqueue. dir (assoc basemap k value) entry-count _meta))))
  clojure.lang.IPersistentStack
  (pop  [this] (if (zero? (.count basemap)) this
                   (pqueue. dir (drop-first basemap) (unchecked-dec entry-count) _meta)))
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
 
