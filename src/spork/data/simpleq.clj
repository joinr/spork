;;A trivial queue implementation
;;intended for efficient operations
;;on message queues of small(ish)
;;sizes.

;;EXPERIMENTAL
(ns spork.data.simpleq
  (:require
   [spork.util.general :as gen]
   ;[clojure.core.rrb-vector :as rrb]
   ;[clojure.data.finger-tree :as ft]
   [spork.data.priorityq :as pq]
   [spork.data.protocols]
   [clj-tuple :as tpl]
            ))


;;we can use counted double lists as deques.  in theory, they should
;;be better than the avl tree, we'll see.
(defn insert-by [keyf v coll]
  (let [lk        (keyf v)
        inserted? (volatile! false)]
    (reduce (fn [acc r]
              (if @inserted?
                (conj acc r) 
                (if (< lk (keyf r))
                  (do (vreset! inserted? true)
                      (-> acc
                          (conj v)
                          (conj r)))
                  (conj acc r))))
            (empty coll) coll)))





;;We maintain a persistent
;;queue in sorted order.

;;Insertion of one or
;;more items leaves us with
;;a copied array, sorted according
;;to the supplied comparator.

;;The intended use case is for a message
;;queue for entities in a persistent
;;simulation context.  We will typically
;;have a very small number of items in the
;;context.  So, we can maintain insertion order
;;pretty easily;  the vast majority of messages
;;will be from an entity to itself at some point
;;in the future; typically this means the new
;;messages will end up at the end of the current
;;set of messages.

;;The goal is to have a small, fast structure that
;;we can easily traverse and copy new structures from.
;;

;;We'd like to use insertion-sort for our operations.
;;Specifically, keep things in sorted order, maintain
;;o(1) access to the lowest element, and o(1) access to
;;the key of the last element.  We don't need to support
;;pulling from the end of the queue.

;;Ordered traversal will shake out by default.
;;Hopefully, this ends up being much more efficient
;;than the seq-based queue implementation for
;;clojure.lang.PersistentQueue.

;;It'll act as a priority queue as well;
;;Don't really care about supporting swapping of existing
;;items in the sequence or resetting priorities.
;;Basically a dumb message queue.

;;One option is to base it off of rrb-vectors.
;;Get persistence for free.  Insertion, if
;;necessary, is efficient with log-ordered
;;construction.  However if we have a small array,
;;we may just want to insert+copy.

;;one strategy is to use a window into the array to
;; (deftype immq [^clojure.core.rrb_vector.rrbt.Vector basevec
;;                ^java.util.Comparator comparator
;;                keyf]
;;   ;; clojure.lang.IHashEq
;;   (hasheq   [this]   (.hasheq basevec))
;;   (hashCode [this]   (.hashCode basevec))
;;   (equals   [this o] (identical? this o))
;;   (equiv    [this o]
;;     (cond (identical? this o) true
;;           (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
;;           (or (instance? clojure.lang.Sequential o)
;;               (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
;;               :else nil))  
;;   Object
;;   (toString [this] (str (.seq this)))
;;   clojure.lang.Indexed
;;   (nth [obj idx] (.nth  basevec idx)) 
;;   (nth [obj idx not-found] (.nth  basevec idx not-found))
;;   clojure.lang.IObj
;;   (meta [this] (.meta basevec))
;;   (withMeta [this m] (immq. (.withMeta basevec m ) comparator))
;;   clojure.lang.ISeq
;;   (first [this] (.nth  basevec 0)) ;way faster than first..
;;   (next  [this] (.pop this))
;;   (more [this]  (.pop this))
;;   clojure.lang.IPersistentCollection
;;   (empty [this]  (immq. (rrb/vec []) comparator))
;;   ;;Note -> if we don't implement this, vector equality doesn't work both ways!
;;   java.util.Collection  
;;   (iterator [self]    (clojure.lang.SeqIterator. (seq  self)))  
;;   (size     [self]    (.count self))  
;;   (toArray  [self]    (.toArray (seq self)))
;;   clojure.lang.Seqable
;;   (seq [this]  ; returns a LazySeq
;;     (seq basevec))
;;   clojure.lang.Counted
;;   (count [this] (.count basevec))
;;  ; clojure.lang.IPersistentVector
;;   (cons [this  a]
;;     )
;;   clojure.lang.IPersistentSet
;;   (disjoin  [this k] (pri. dir (.disjoin basevec k) n _meta -1 -1))
;;   (contains [this k] (.contains basevec k))
;;   (get      [this k] (.get basevec k))
;;   clojure.lang.Sorted
;;   (seq [this ascending?]
;;     (if ascending?
;;       (.seq  basevec)
;;       (.rseq basevec)))
;;   (seqFrom [this k ascending?]
;;                                         ;(.seqFrom basevec k ascending?)
;;     (throw (Exception. "not implemnted"))
;;     )
;;   (entryKey [this entry]        entry)
;;   (comparator [this] (.comparator basevec))
;;   clojure.lang.IPersistentStack
;;   (pop  [this] )
;;   (peek [this] )
;;  ; clojure.lang.Reversible
;;  ; (rseq [this]  (.rseq basevec))
;;   java.io.Serializable ;Serialization comes for free with the other stuff.
;; ;  clojure.lang.IEditableCollection
;; ;  (asTransient [this] (->tpri dir (transient basevec) n _meta))
;;   clojure.core.protocols/IKVReduce
;;   (kv-reduce [this f init]
;;     (.kv-reduce ^clojure.data.avl.AVLMap (.avl-map basevec)
;;                 (fn [acc ^clojure.lang.MapEntry k _]
;;                   (f acc (.key k) (.val k)))
;;                 init))
;;   clojure.core.protocols/CollReduce
;;   (coll-reduce [coll f]
;;       (reduce-kv (fn [acc k _] (f acc k))  (head coll) (r/drop 1 (.avl-map basevec))))  
;;   (coll-reduce [coll f init]
;;       (reduce-kv (fn [acc k _] (f acc k))  init  (.avl-map basevec)))
    

;;   (
  
;;   )

;; (defn insert-value
;;   ([^clojure.lang.PersistentQueue q v max-val]
;;    (if (>= v max-val) (.cons q v)
;;                                         ;insert in sorted order.
;;        (loop [^clojure.lang.PersistentQueue acc clojure.lang.PersistentQueue/EMPTY                 
;;               ^clojure.lang.ISeq xs (.seq q)]         
;;          (if xs
;;            (let [pred (.first xs)]
;;              (if (< pred v)
;;                (recur (.cons acc pred)
;;                       (.next xs))
;;                                         ;insert value here.
;;                (into (-> acc (.cons pred)
;;                          (.cons v))
;;                      (.next xs))))
;;            (.cons acc v)))))
;;   ([^clojure.lang.PersistentQueue q v]
;;    (loop [^clojure.lang.PersistentQueue acc clojure.lang.PersistentQueue/EMPTY                 
;;           ^clojure.lang.ISeq xs (.seq q)]    
;;      (if xs 
;;          (let [pred (.first xs)]
;;            (if (< pred v)
;;              (recur (.cons acc pred)
;;                     (.next xs))
;;                                         ;insert value here.
;;              (into (-> acc (.cons pred)
;;                        (.cons v))
;;                    (.next xs))))
;;            (.cons acc v)))))

                      
      
;; ;;this is almost verbatim from clojure.lang.PersistentQueue                                           
;; (deftype immq [^clojure.lang.ISeq             f
;;                ^clojure.lang.PersistentVector r
;;                ^long                          cnt
;;            ;    ^int                           fcount ;;keeps track of how many items are on the stack.
;;                ^java.util.comparer comparator
;;                ^int ^unsynchronized-mutable _hash
;;                ^int ^unsynchronized-mutable _hasheq
;;                _meta]
;;   clojure.lang.IHashEq
;;   (hasheq [this]
;;     (if (== _hasheq (int -1))
;;       (let [h (hash-ordered-coll [f r])]
;;         (do (set! _hasheq (int h))
;;             h))
;;       _hasheq))
;;   (hashCode [this]
;;     (if (== _hash (int -1))
;;       (let [h (hash-ordered-coll [f r])]
;;         (do (set! _hash (int h))
;;             h))
;;       _hash))
;;   (equals [this o] (identical? this o))
;;   (equiv  [this o]
;;     (cond (identical? this o) true
;;           (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
;;           (or (instance? clojure.lang.Sequential o)
;;               (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
;;               :else nil))  
;;   Object
;;   (toString [this] (str (.seq this)))
;; ;  clojure.lang.Indexed
;; ;  (nth [obj idx] (if (>= idx fcount) (.nth  r idx)) 
;; ;  (nth [obj idx not-found] (.nth ^clojure.data.avl.AVLSet basemap idx not-found))
;;   clojure.lang.IObj
;;   (meta [this] _meta)
;;   (withMeta [this m] (immq. f r  cnt  _hash _hasheq m))
;;   clojure.lang.ISeq
;;   (first [this] (.first f))
;;   (next  [this] (.pop this))
;;   (more [this]  (.pop this))
;;   clojure.lang.IPersistentCollection
;;   (empty [this]  (immq. '() [] 
;;   ;;Note -> if we don't implement this, vector equality doesn't work both ways!
;;   java.util.Collection  
;;   (iterator [self]    (clojure.lang.SeqIterator. (seq  self)))  
;;   (size     [self]    (.count self))  
;;   (toArray  [self]    (.toArray (seq self)))
;;   clojure.lang.Seqable
;;   (seq [this]  ; returns a LazySeq
;;     (seq basemap))
;;   clojure.lang.Counted
;;   (count [this] (.count basemap))
;;  ; clojure.lang.IPersistentVector
;;   (cons [this  a]
;;     ;;check for rollover.  In that event, repack existing entries.  It's inconceivable that this would happen though.
;;     ; called by conj
;;     (let [k  (.nth  ^clojure.lang.Indexed a 0)
;;           v  (.nth  ^clojure.lang.Indexed a 1)
;;           nnxt (unchecked-inc n)]
;;       (if (== nnxt Long/MIN_VALUE) ;;rollover
;;         (let [idx     (volatile! 0)
;;               basemap (persistent!
;;                        (reduce (fn [acc e]
;;                                  (let [^pkey k (key e)
;;                                        e (->pentry (.k k) @idx (val e))
;;                                        _ (vswap! idx unchecked-inc)]
;;                                    (conj! acc e)))
;;                                (transient (get-set dir)) basemap))]
;;           (pri. dir basemap @idx _meta -1 -1))        
;;         (pri. dir (.cons basemap (->pentry k n v)) nnxt  _meta -1 -1))))
;;   clojure.lang.IPersistentSet
;;   (disjoin  [this k] (pri. dir (.disjoin basemap k) n _meta -1 -1))
;;   (contains [this k] (.contains basemap k))
;;   (get      [this k] (.get basemap k))
;;   clojure.lang.Sorted
;;   (seq [this ascending?]
;;     (.seq ^clojure.lang.Sorted basemap ascending?))
;;   (seqFrom [this k ascending?] (.seqFrom basemap k ascending?))
;;   (entryKey [this entry]        entry)
;;   (comparator [this] (.comparator basemap))
;;   clojure.lang.IPersistentStack
;;   (pop  [this] (if (zero?    (.count basemap)) this
;;                    (pri. dir (.disjoin basemap (.nth basemap 0))
;;                              n _meta -1 -1)))
;;   (peek [this] (head basemap))
;;   clojure.lang.Reversible
;;   (rseq [this]  (.rseq basemap))
;;   java.io.Serializable ;Serialization comes for free with the other stuff.
;;   clojure.lang.IEditableCollection
;;   (asTransient [this] (->tpri dir (transient basemap) n _meta))
;;   clojure.core.protocols/IKVReduce
;;   (kv-reduce [this f init]
;;     (.kv-reduce ^clojure.data.avl.AVLMap (.avl-map basemap)
;;                 (fn [acc ^clojure.lang.MapEntry k _]
;;                   (f acc (.key k) (.val k)))
;;                 init))
;;   clojure.core.protocols/CollReduce
;;   (coll-reduce [coll f]
;;       (reduce-kv (fn [acc k _] (f acc k))  (head coll) (r/drop 1 (.avl-map basemap))))  
;;   (coll-reduce [coll f init]
;;       (reduce-kv (fn [acc k _] (f acc k))  init  (.avl-map basemap)))


  ;;do we care about the max?  Yes.  We'd like to be able to query the tail value,
  ;;or at least know the max value in the queue so we can tell if we need to
  ;;sort the queue.  If we're adding a value at the end of the queue, it must
  ;;be >= the previous values.  That maintains the sorted order invariant.
  ;;If it's not, then we have to find out where to insert it.


;; (defprotocol IDeque
;;   (push-front [this a] "insert element at front")
;;   (push-back [this a] "insert element at back")
;;   (pop-front [this] "remove element at front")
;;   (pop-back [this] "remove element at back")
;;   (front [this] "get the first element")
;;   (back [this] "get the last element")
;;   (deque-empty? [this] "is this empty?")
;;   )

;; (comment 
;; (deftype MDeque [^java.util.ArrayDeque dq]
;;   IDeque
;;   (push-front [this a]  (do (.add dq a)))
;;   (push-back  [this a]   (do (.add 
;;   (pop-front [this]
;;     (if (pos? cnt)
;;       (if (zero? (.count fv))
;;         (Deque. [] (subvec rv 1 (.count rv)) (unchecked-dec cnt))
;;         (Deque. (.pop fv) rv (unchecked-dec cnt)))
;;       this))
;;   (pop-back [this]    
;;     (if (pos? cnt)
;;       (if (zero? (.count rv))
;;         (Deque. (subvec fv 1 (.count fv)) [] (unchecked-dec cnt))
;;         (Deque. fv (.pop rv) (unchecked-dec cnt)))
;;       this))
;;   (front [this]  
;;     (when (pos? cnt) 
;;       (if (zero? (.count fv))
;;         (.nth rv 0)
;;         (.peek fv))))
;;   (back [this]
;;     (when (pos? cnt)
;;       (if (zero? (.count rv))
;;         (fv 0)
;;         (.peek rv))))
;;   (deque-empty? [this]
;;     ; (empty? this) = (not (seq this))
;;     ; = (not (concat (rseq fv) (seq rv)))
;;     ; use deque-empty? instead 
;;     (pos? cnt))
;;   clojure.lang.ISeq
;;   (first [this]   
;;     (when (pos? cnt)
;;       (.front this)))
;;   (next [this]
;;     (when (pos? cnt)
;;       (.pop-front this)))
;;   (more [this]
;;     ; called by rest
;;     (when (pos? cnt)      
;;       (.pop-front this)))
;;   clojure.lang.IPersistentCollection
;;   (empty [this]
;;     (Deque. [] [] 0))
;;   (equiv [this that]
;;     ; called by =
;;     (and (instance? Deque that)
;;          (= (.count this) (.count that))
;;          (loop [i (dec (.count this))]
;;            (or (< i 0)
;;                (and (= (this i) (that i))
;;                     (recur (dec i)))))))
;;   clojure.lang.Seqable
;;   (seq [this]
;;     ; returns a LazySeq
;;     (when (pos? cnt) 
;;       (concat (rseq fv) rv)))
;;   clojure.lang.Counted
;;   (count [this]   cnt)
;;   clojure.lang.IFn
;;   (invoke [this index]
;;     (if (< index (.count fv))
;;       (fv (- (.count fv) index 1))
;;       (rv (- index (.count fv)))))
;;   (applyTo [this args]                                                    
;;     (if (not= 1 (count args))
;;       (throw (clojure.lang.ArityException. (count args) "Deque"))
;;       (this (first args))))
;;   clojure.lang.Reversible
;;   (rseq [this]  (Deque. rv fv cnt))
;;   clojure.lang.ILookup
;;   (valAt [this index]
;;     (this index))
;;   (valAt [this index value]
;;     ;;semantically incorrect implementation.
;;     (.valAt this index))
;;   clojure.lang.IPersistentVector
;;   (cons [this a]
;;     (.push-back this a))
;;   (length [this]
;;     (.count this))
;;   (assocN [this index value]
;;     (.assoc this index value))
;;   clojure.lang.Associative
;;   (containsKey [this index]
;;     (and (pos? index) (< index  cnt)))
;;   (entryAt [this index]
;;     (.valAt this index))
;;   (assoc [this index value]
;;     (if (< index (.count fv))
;;       (Deque. (assoc fv (- (.count fv) index 1) value) rv 0)
;;       (Deque. fv (assoc rv (- index (.count fv)) value) 0)))
;;   clojure.lang.Indexed
;;   (nth [this index]        (.valAt this index))
;;   (nth [this index value]  (.assoc this index value))
;;   clojure.lang.IPersistentStack
;;   (pop [this]   (.pop-back this))
;;   (peek [this]  (.back this))
;;   clojure.lang.IEditableCollection
;;   (transient [this] (into (mdeque. fv rv)))
;; )


;;99% of the time, we know that messages are going to
;;come in in-order.
;;One option is to maintain "two" queues....
;;We always check one queue before the other.
;;The "out-of-order" queue vs the "in-order" queue.
;;Out-of-order maintains messages - still in order of arrival,
;;thus monotonically increasing.
;;The in-order queue also maintains messages in order of arrival;
;;although it may be 

;;Courtesy of JayGlascoe, http://www.jayglascoe.com/Deque
;;THe problem with Jay's version, using subvecs, is that
;;we can't ever let go of the underlying vector (it's
;;a view on the vec).  This is fine, so long as we
;;exhaust the front vector eventually.  If we're not
;;keeping track of the view...I guess it's fine?


;nsert in logn...
;; (defn find-right-linear [^clojure.lang.IPersistentVector v ^java.util.Comparator comp x]
;;   (let [init (unchecked-dec (.count v))]
;;     (loop [idx init]
;;       (do
;;      ;   (println idx)
;;         (if (neg? idx) nil
;;             (let [res  (.compare comp  (.nth v idx) x)]  ;gte
;;               (if (or (== res 0) (== res -1))
;;                 (unchecked-inc idx)
;;                 (recur (unchecked-dec idx)))))))))

(definline find-right-linear [v comp x]
  (let [ v (with-meta v {:tag 'clojure.lang.IPersistentVector})
        comp (with-meta comp {:tag 'java.util.Comparator})]
  `(let [init# (unchecked-dec (.count ~v))]
    (loop [idx# init#]
      (do
     ;   (println idx)
        (if (neg? idx#) nil
            (let [res#  (.compare ~comp  (.nth ~v idx#) ~x)]  ;gte
              (if (or (== res# 0) (== res# -1))
                (unchecked-inc idx#)
                (recur (unchecked-dec idx#))))))))))

;; (definline find-bin [v comp x]
;;   (let [ v (with-meta v {:tag 'clojure.lang.IPersistentVector})
;;         comp (with-meta comp {:tag 'java.util.Comparator})]
;;   `(let [init# (unchecked-dec (.count ~v))]
;;      (loop [l# (quot init 2)
;;             r# init#]
;;        (let [distance# (unchecked-subtract r# l#)]
;;          (cond (zero? distance#) ;;converged
;;                l#
;;                (== 1 distance#) ;;either l or r
;;                   (if (
;;              (let [guess#    (unchecked-add l# (quot distance# 2))]
               
        
;;         (let [res#  (.compare ~comp  (.nth ~v l#) ~x)]  ;gte
;;           (if (or (== res# 0) (== res# -1))
;;             (unchecked-inc idx#)
;;             (recur (unchecked-dec idx#)))))))))

(def long-key
  (reify java.util.Comparator
    (compare [this l r]
      (let [lk (long (key l))
            rk (long (key r))]
        (compare lk rk)))))

(defn long-comp [l r]
  (let [lk (long l)
        rk (long r)]
    (if (== lk rk) 0
        (if (pos? (unchecked-subtract lk rk)) 1 -1))))

(defn long-key2  [^clojure.lang.MapEntry l ^clojure.lang.MapEntry  r]
  (let [lk (long (.key  l))
        rk (long (.key r))]
    (if (== lk rk) 0
        (if (pos? (unchecked-subtract lk rk)) 1 -1))))

;;RRBVector makes this pretty easy.
;;we basically just need to find the latest index to insert at.
;;Then, create two subvecs and concat.  Or, if the vec is small enough,
;;just build a new vec directly (how small is small enough?)
(definline insert-back-
  [v comp x]
   (let [v (with-meta v {:tag 'clojure.lang.IPersistentVector})
         comp (with-meta comp {:tag 'java.util.Comparator})]
     `(let [cnt# (.count ~v)]
       (if (zero? cnt#) [~x]
           (if-not (neg? (.compare ~comp ~x (.nth ~v (unchecked-dec cnt#))))
             (.cons ~v ~x)
             (if-let [idx# (find-right-linear ~v ~comp ~x)]               
               (into (conj (subvec ~v 0 idx#) ~x) (subvec ~v idx# (.count ~v)))
               (into [~x] ~v)))))))

(def ^:constant +max-tuple+ 7)

;;there's a pattern here..
;;we have a position at which we insert, bounded by the number of elements in the tuple.


(defn unpair [pairs] (reduce (fn [acc [l r]] (-> acc (conj l) (conj r))) [] pairs)) 
(defmacro positional-combinations  [ctor obj n y]
  (let [nths       (mapv (fn [n] `(.nth ~obj ~n))  (range n))
        combos   (for [i (range (unchecked-inc (count nths)))]
                   (concat (subvec nths 0 i) [y] (subvec nths i (count nths))))]
    `'~(unpair (reverse (map-indexed (fn [idx v] `[~idx (~ctor  ~@v)])  combos)))))

(defmacro positional-insert [ctor from new-idx x]
  (let [coll (with-meta (gensym "coll")  {:tag 'clojure.lang.IPersistentVector})
        idx  (gensym "idx")]
    `(let [~idx ~new-idx
           ~coll ~from
           cnt# (.count ~coll)]
       (case
        (int cnt#)
        ~@(unpair
           (for [cnt (rest (range +max-tuple+))]
             [cnt `(case (int ~idx)
                     ~@(eval `(positional-combinations ~ctor ~coll ~cnt ~x)))]))))))
(defmacro naive-insert [v idx x]
  (let [v (with-meta v {:tag 'clojure.lang.IPersistentVector})]
    `(into (conj (subvec ~v 0 ~idx) ~x) ;build a new vector
           (subvec ~v ~idx (.count ~v)))))

;; (defmacro insert-at- [v idx x]
;;   (let [v (with-meta v {:tag 'clojure.lang.IPersistentVector})]
;;     `(if (< ~idx ~+max-tuple+) 
;;       (positional-insert tpl/tuple ~v ~idx ~x) ;build a new tuple
;;       (naive-insert ~v ~idx ~x)
;;       )))

;;faster....
(defn insert-at [^clojure.lang.IPersistentVector v idx x]
  (if (and (< (.count v) +max-tuple+) (< idx +max-tuple+))
    (positional-insert tpl/tuple v idx x) ;build a new tuple
    (naive-insert v idx x)
  ))
  ;  (catch Exception e (println (str [e v idx x])))))

;; (defn pop-front [^clojure.lang.IPersistentVector v]
;;   (if (instance?  clojure.lang.PersistentUnrolledVector v)
;;     (let [^clojure.lang.PersistentUnrolledVector v v] 
;;       (case (.count ^clojure.lang.PersistentUnrolledVector v)
;;         1  (tpl/tuple) 
;;         2  (tpl/tuple (.nth v 1))
;;         3  (tpl/tuple (.nth v 1) (.nth v 2))
;;         4  (tpl/tuple (.nth v 1) (.nth v 2) (.nth v 3))
;;         5  (tpl/tuple (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 4))
;;         6  (tpl/tuple (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 5))))
;;     (let [c (.count v)]
;;       (if (<= c 7)
;;          (apply tpl/tuple (subvec v 1 c))
;;          (subvec v 1 c))))

;; (defn pop-front [^clojure.lang.IPersistentVector v]
;;   (let [c (.count  v)]
;;     (if (<= c 7)
;;       (case c
;;         1  (tpl/tuple) 
;;         2  (tpl/tuple (.nth v 1))
;;         3  (tpl/tuple (.nth v 1) (.nth v 2))
;;         4  (tpl/tuple (.nth v 1) (.nth v 2) (.nth v 3))
;;         5  (tpl/tuple (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 4))
;;         6  (tpl/tuple (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 4) (.nth v 5))
;;         7  (tpl/tuple (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 4) (.nth v 5) (.nth v 6)))
;;     (subvec v 1 c))))

;;this is faster than subvecing, and no memory leaks.
(defn pop-front [^clojure.lang.IPersistentVector v]
  (let [c (.count  v)]
    (if (<= c 7)
      (case c
        1  (clojure.lang.PersistentUnrolledVector/create ) 
        2  (clojure.lang.PersistentUnrolledVector/create  (.nth v 1))
        3  (clojure.lang.PersistentUnrolledVector/create  (.nth v 1) (.nth v 2))
        4  (clojure.lang.PersistentUnrolledVector/create  (.nth v 1) (.nth v 2) (.nth v 3))
        5  (clojure.lang.PersistentUnrolledVector/create  (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 4))
        6  (clojure.lang.PersistentUnrolledVector/create  (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 4) (.nth v 5))
        7  (clojure.lang.PersistentUnrolledVector/create  (.nth v 1) (.nth v 2) (.nth v 3) (.nth v 4) (.nth v 5) (.nth v 6))
        (throw (Exception. (str [:unknown-case v c]))))
      (subvec v 1 c))))
    
(definline insert-back2-
  [v comp x]
   (let [v (with-meta v {:tag 'clojure.lang.IPersistentVector})
         comp (with-meta comp {:tag 'java.util.Comparator})]
     `(let [cnt# (.count ~v)]
       (if (zero? cnt#) (clojure.lang.PersistentUnrolledVector/create ~x) ;;using tuples...
           (if-not (neg? (.compare ~comp ~x (.nth ~v (unchecked-dec cnt#))))
             (conj ~v ~x)
             ;;if we're using tuples, i.e. insertion size will be < 7, then we
             ;;just compute a new tuple.
             (if-let [idx# (find-right-linear ~v ~comp ~x)]               
               (insert-at ~v idx# ~x)
               (insert-at ~v 0 ~x))))))) ;throw (Exception. (str "could not find" [ ~v ~x])))))))))

(defn insert-back
  ([v comp x] (insert-back2- v comp x))
  ([v x]      (insert-back2- v compare x)))
                                   
;;we can use tuples to do a couple of cool things:

(comment
;;this doesn't save us alot, surprisingly...
(defmacro tuple
  ([]
     `(clj_tuple.Tuple0. nil))
  ([x]
     `(clj_tuple.Tuple1. ~x nil))
  ([x y]
     `(clj_tuple.Tuple2. ~x ~y nil))
  ([x y z]
     `(clj_tuple.Tuple3. ~x ~y ~z nil))
  ([x y z w]
     `(clj_tuple.Tuple4. ~x ~y ~z ~w nil))
  ([x y z w u]
     `(clj_tuple.Tuple5. ~x ~y ~z ~w ~u nil))
  ([x y z w u v]
     `(clj_tuple.Tuple6. ~x ~y ~z ~w ~u ~v nil))
  ([x y z w u v & rst]
     `(let [r# (-> []
                   transient
                   (conj! ~x)
                   (conj! ~y)
                   (conj! ~z)
                   (conj! ~w)
                   (conj! ~u)
                   (conj! ~v))]
        (loop [r# r#, s# ~rst]
          (if (empty? s#)
            (persistent! r#)
            (recur (conj! r# (first s#)) (rest s#)))))))
)

;;efficiency concerns:
;;We're using subvector to pop. so, we're generating garbage over
;;time.  Memory leak occurs if the queue is never fully drained in one
;;operation.

(comment
  
;; (def ^:constant  +empty-root+ (object-array 32))
;; ;;we can also implement small queues in object arrays.
;; ;;we pay the cost of cloning on each operation
;; (deftype OrderedArrayQueue [^objects v
;;                             ^long    cnt                      
;;                             ^java.util.Comparator comparator
;;                             ]
;;   IBackFront
;;   (front [q] (aget v 0))
;;   (back  [q] (aget v (unchecked-dec cnt))
;;   IInsertable
;;   (insert [q x]
;;     (if (pos? cnt)
;;       (OrderedArrayQueue. (insert-back- v comparator x)
;;                           (unchecked-inc cnt)
;;                           comparator)
;;       (OrderedArrayQueue. (aset ^objects (aclone v) 0 x) 1 comparator)))
;;   clojure.lang.IPersistentCollection
;;   (empty [this]
;;     (OrderedQueue. +empty-root+ 0 comparator))
;;   (equiv [this that]
;;     ; called by =
;;     (and (instance? OrderedArrayQueue that)
;;          (= (.count this) (.count ^OrderedArrayQueue that))
;;          (loop [i (dec (.count this))]
;;            (or (< i 0)
;;                (and (= (this i) (that i))
;;                     (recur (dec i)))))))
;;   clojure.lang.Seqable
;;   (seq [this]  (take cnt (seq v)))
;;   clojure.lang.Counted
;;   (count [this]   cnt)
;;   clojure.lang.ILookup
;;   (valAt [this index]            (aget v index))
;;   (valAt [this index not-found]  (aget v index))
;;   clojure.lang.IPersistentVector
;;   (cons [this a]    (.insert this a))
;;   (length [this] cnt)
;;   (assocN [this index value]
;;     (throw (Exception. "cannot assoc into the middle of the queue.  must use insert")))
;;   clojure.lang.Associative
;;   (containsKey [this index]
;;     (and  (< index  cnt) (not (neg? index))))
;;   (entryAt [this index]    (.valAt this index))
;;   (assoc [this index value]
;;         (throw (Exception. "cannot assoc into the middle of the queue.  must use insert")))
;;   clojure.lang.Indexed
;;   (nth [this index]        (.valAt this index))
;;   (nth [this index value]  (.assoc this index value))
;;   clojure.lang.IPersistentStack
;;   (pop [this]   (if (pos? cnt)
;;                   (OrderedQueue. (subvec v 1) (unchecked-dec cnt) comparator)
;;                   (throw (Exception. "Cant pop empty ordered queue..."))))
;;   (peek [this]  (.front this))
                  
;; ;  clojure.lang.IEditableCollection
;; ;  (transient [this] (into (mdeque. fv rv)))
;;   )
)
;;Just realized I didn't need a deque...just a sorted queue/vector.
;;Ugh.  So, the plan is to wrap a vector and use it as a 
(deftype OrderedQueue [^clojure.lang.IPersistentVector v
                       ^long cnt                      
                       ^java.util.Comparator comparator
                       ;^unsynchronized-mutable _min
                       ;^unsynchronized-mutable _max
                      ]
  spork.data.protocols.IFrontBack
  (front [q] (.nth v 0 nil))
  (back [q]  (.nth v (unchecked-dec cnt) nil))
  spork.data.protocols.IInsertable
  (insert [q x]
    (if (pos? cnt)
      (OrderedQueue. (insert-back- v comparator x)
                     (unchecked-inc cnt)
                     comparator)
      (OrderedQueue. [x] 1 comparator)))
  clojure.lang.IPersistentCollection
  (empty [this]
    (OrderedQueue. [] 0 comparator))
  (equiv [this that]
    ; called by =
    (and (instance? OrderedQueue that)
         (= (.count this) (.count ^OrderedQueue that))
         (loop [i (dec (.count this))]
           (or (< i 0)
               (and (= (this i) (that i))
                    (recur (dec i)))))))
  clojure.lang.Seqable
  (seq [this]  (seq v))
  clojure.lang.Counted
  (count [this]   cnt)
  clojure.lang.ILookup
  (valAt [this index]            (.nth v index))
  (valAt [this index not-found]  (.nth v  index not-found))
  clojure.lang.IPersistentVector
  (cons [this a]    (.insert this a))
  (length [this] cnt)
  (assocN [this index value]
    (throw (Exception. "cannot assoc into the middle of the queue.  must use insert")))
  clojure.lang.Associative
  (containsKey [this index]
    (and  (< index  cnt) (not (neg? index))))
  (entryAt [this index]    (.valAt this index))
  (assoc [this index value]
        (throw (Exception. "cannot assoc into the middle of the queue.  must use insert")))
  clojure.lang.Indexed
  (nth [this index]        (.valAt this index))
  (nth [this index value]  (.valAt this index value))
  clojure.lang.IPersistentStack
  (pop [this]   (if (pos? cnt)
                  (OrderedQueue. (subvec v 1) (unchecked-dec cnt) comparator)
                  (throw (Exception. "Cant pop empty ordered queue..."))))
  (peek [this]  (.front this))
                  
;  clojure.lang.IEditableCollection
;  (transient [this] (into (mdeque. fv rv)))
  )

(deftype OrderedQueueSmall [^clojure.lang.IPersistentVector v
                            ^long cnt                      
                            ^java.util.Comparator comparator
                            ]
  spork.data.protocols.IFrontBack
  (front [q] (.nth v 0 nil))
  (back [q]  (.nth v (unchecked-dec cnt) nil))
  spork.data.protocols.IInsertable
  (insert [q x]
    (if (pos? cnt)
      (OrderedQueueSmall. (insert-back2- v comparator x)
                     (unchecked-inc cnt)
                     comparator)
      (OrderedQueueSmall. (tpl/tuple x) 1 comparator)))
  clojure.lang.IPersistentCollection
  (empty [this]
    (OrderedQueueSmall. (tpl/tuple) 0 comparator))
  (equiv [this that]
    ; called by =
    (and (instance? OrderedQueueSmall that)
         (= (.count this) (.count ^OrderedQueueSmall that))
         (loop [i (dec (.count this))]
           (or (< i 0)
               (and (= (this i) (that i))
                    (recur (dec i)))))))
  clojure.lang.Seqable
  (seq [this]  (seq v))
  clojure.lang.Counted
  (count [this]   cnt)
  clojure.lang.ILookup
  (valAt [this index]            (.nth v index))
  (valAt [this index not-found]  (.nth v  index not-found))
  clojure.lang.IPersistentVector
  (cons [this a]    (.insert this a))
  (length [this] cnt)
  (assocN [this index value]
    (throw (Exception. "cannot assoc into the middle of the queue.  must use insert")))
  clojure.lang.Associative
  (containsKey [this index]
    (and  (< index  cnt) (not (neg? index))))
  (entryAt [this index]    (.valAt this index))
  (assoc [this index value]
        (throw (Exception. "cannot assoc into the middle of the queue.  must use insert")))
  clojure.lang.Indexed
  (nth [this index]        (.valAt this index))
  (nth [this index value]  (.valAt this index value))
  clojure.lang.IPersistentStack
  (pop [this]   (if (pos? cnt)
                  (OrderedQueueSmall. (pop-front v) (unchecked-dec cnt) comparator)
                  (throw (Exception. "Cant pop empty ordered queue..."))))
  (peek [this]  (.front this))
                  
;  clojure.lang.IEditableCollection
;  (transient [this] (into (mdeque. fv rv)))
  )

(defn ordered-queue-by [comparator & xs]
  (if (seq xs)
    (let [v (vec (sort-by identity comparator xs))]
      (OrderedQueue. v  (count v) comparator))
      (OrderedQueue. [] 0 comparator)))
(defn ordered-queue [& xs]  (apply ordered-queue-by  compare xs))

(defn small-ordered-queue-by [comparator & xs]
  (if (seq xs)
    (let [v (vec (sort-by identity comparator xs))]
      (OrderedQueueSmall. v  (count v) comparator))
      (OrderedQueueSmall. (tpl/tuple) 0 comparator)))
(defn small-ordered-queue [& xs]  (apply small-ordered-queue-by  compare xs))

(def ^:dynamic *n* 1)
(def ^:dynamic *size* 1000000)
;;this is actually comparable to the persistentqueue implementation....
(defn qtest [ & {:keys [n size] :or {n *n* size *size*}}]
  (let [q (ordered-queue-by (fn [l r] (compare (key l) (key r))))]
    (time (dotimes [i n]
            (loop [idx 0
                   acc q]
              (if (== idx size)
                acc
                (recur (unchecked-inc idx)
                       (conj acc (clojure.lang.MapEntry. idx idx)))))))))
(defn pqtest [& {:keys [n size] :or {n *n* size *size*}}]
  (time (dotimes [i n]
          (loop [idx 0
                 acc clojure.lang.PersistentQueue/EMPTY]
            (if (== idx size)
              acc
              (recur (unchecked-inc idx)
                     (conj acc (clojure.lang.MapEntry. idx idx))))))))
;;roughly 10x slower for simple conjing...
;;avlset-based priority queue implementation.
(defn pritest [& {:keys [n size] :or {n *n* size *size*}}]
    (time (dotimes [i n]
          (loop [idx 0
                 acc pq/minpri]
            (if (== idx size)
              acc
              (recur (unchecked-inc idx)
                     (conj acc (clojure.lang.MapEntry. idx idx))))))))

;;how do we handle random inserts?
;;let's define a dataset with some out-of-order inserts, with the caveat
;;that the inserts will never be earlier than the first item in the queue.

;;a mostly-ordered set of entries to test our data.
(defn random-nums [& {:keys [n thresh] :or {n *size* thresh 0.1}}]
  (let [cutoff (quot n 10)]
    (take n (map (fn [[idx v]] (clojure.lang.MapEntry. v idx))
                 (iterate (fn [[t e]]
                            (let [
                                  tnxt (unchecked-inc t)]
                              [tnxt
                               (if (and (> t cutoff) (<= (rand) thresh))
                                 (let [prior (- t (rand-int  cutoff))]
                                   prior)
                                 tnxt)]))
                          [0 0])))))

(defn iqtest []
  (let [rv (vec (random-nums :n *size*))]
    (time (dotimes [i 1] (reduce conj (ordered-queue-by long-key2) rv)))))
(defn qpoptest []
  (let [q (reduce conj (ordered-queue-by long-key2) (random-nums :n *size*))
        n *size*]
    (time (dotimes [i *n*]
            (loop [idx 0
                   q q]
              (if (== idx n) nil
                  (recur (unchecked-inc idx) (pop q))))))))
;;avlset-based priority queue implementation.
(defn ipritest []
  (let [n *size*
        rv (vec (random-nums :n n))]
    (time (dotimes [i *n*] (reduce conj pq/minpri rv)))))
(defn pripoptest []
  (let [n *size*
        q (reduce conj  pq/minpri (random-nums :n n))]
    (time (dotimes [i *n*]
            (loop [idx 0
                   q q]
              (if (== idx n) nil
                  (recur (unchecked-inc idx) (pop q))))))))
  
;; a closer version of this test is to see how the q responds to
;;the actual workload:
;;messages are popped in bulk.
;;new messages (usually updates) are queued - typically in order.
;;there may be an occurence of new messages - namely altered updates
;;- happen in between this process.

(def l (atom nil))
(defn stress-test [q & {:keys [split getkey]
                        :or {split 0.1 getkey key}}]
  ;;typical rounds of message indicate polling current messages,
  ;;adding 1 or more messages for future updates, and moving on.
  ;;Occasionally, in between the current round and the next, we'll
  ;;out-of-order messages.  These messages do not conform to the
  ;;preponderance of messages that arrive either at the end of the
  ;;queue (horizon) or the front (current).
  ;;Instead, they're somewhere in the future and need to be accounted for.
  (let  [size *size*]
    (loop [idx 0           
           init (conj (empty q) (clojure.lang.MapEntry. -1 0))]
      (if (== idx size) init          
          ;;pop messages.
          (let [;_ (println [:popping init])
                t     (getkey (.peek ^clojure.lang.IPersistentStack init)) ;read the message..
                nxt   (-> (.pop ^clojure.lang.IPersistentStack init)
                          (conj (clojure.lang.MapEntry. (unchecked-add t 100) idx)))    ;pop the message.
                ;_     (println [:t t :remaining nxt])
                nxt   (if (< (rand) split) ;push an out-of-order msg on the queue.
                        (let [e (clojure.lang.MapEntry. (unchecked-add t (rand-int 99)) idx)
                              nnxt (conj nxt e)]
                          (do ;(println [:randing nnxt e])
                              nnxt))
                        nxt)
                ;_   (println [:recurring nxt])
                ]
            (recur (unchecked-inc idx)
                   nxt))))))
                                     

(defn pritest [n]
  (binding [*size* n]
    (time (dotimes [i 1] (stress-test pq/minpri :getkey (fn [e] (.k  ^spork.data.priorityq.pkey (key e))))))))
(defn oqtest [n]
  (binding [*size* n]
    (time (dotimes [i 1] (stress-test (ordered-queue-by long-key2))))))
(defn smalltest [n]
  (binding [*size* n]
    (time (dotimes [i 1] (stress-test (small-ordered-queue-by long-key2))))))

;; (deftype Deque [^clojure.lang.IPersistentVector fv
;;                 ^clojure.lang.IPersistentVector rv
;;                 ^long cnt]
;;   IDeque
;;   (push-front [this a]  (Deque. (.cons fv a) rv (unchecked-inc cnt)))
;;   (push-back [this a]   (Deque. fv (.cons rv a) (unchecked-inc cnt)))
;;   (pop-front [this]
;;     (if (pos? cnt)
;;       (if (zero? (.count fv))
;;         (Deque. [] (subvec rv 1 (.count rv)) (unchecked-dec cnt))
;;         (Deque. (.pop fv) rv (unchecked-dec cnt)))
;;       this))
;;   (pop-back [this]    
;;     (if (pos? cnt)
;;       (if (zero? (.count rv))
;;         (Deque. (subvec fv 1 (.count fv)) [] (unchecked-dec cnt))
;;         (Deque. fv (.pop rv) (unchecked-dec cnt)))
;;       this))
;;   (front [this]  
;;     (when (pos? cnt) 
;;       (if (zero? (.count fv))
;;         (.nth rv 0)
;;         (.peek fv))))
;;   (back [this]
;;     (when (pos? cnt)
;;       (if (zero? (.count rv))
;;         (fv 0)
;;         (.peek rv))))
;;   (deque-empty? [this]
;;     ; (empty? this) = (not (seq this))
;;     ; = (not (concat (rseq fv) (seq rv)))
;;     ; use deque-empty? instead 
;;     (pos? cnt))
;;   clojure.lang.ISeq
;;   (first [this]   
;;     (when (pos? cnt)
;;       (.front this)))
;;   (next [this]
;;     (when (pos? cnt)
;;       (.pop-front this)))
;;   (more [this]
;;     ; called by rest
;;     (when (pos? cnt)      
;;       (.pop-front this)))
;;   clojure.lang.IPersistentCollection
;;   (empty [this]
;;     (Deque. [] [] 0))
;;   (equiv [this that]
;;     ; called by =
;;     (and (instance? Deque that)
;;          (= (.count this) (.count ^Deque that))
;;          (loop [i (dec (.count this))]
;;            (or (< i 0)
;;                (and (= (this i) (that i))
;;                     (recur (dec i)))))))
;;   clojure.lang.Seqable
;;   (seq [this]
;;     ; returns a LazySeq
;;     (when (pos? cnt) 
;;       (concat (rseq fv) rv)))
;;   clojure.lang.Counted
;;   (count [this]   cnt)
;;   clojure.lang.IFn
;;   (invoke [this index]
;;     (if (< index (.count fv))
;;       (fv (- (.count fv) index 1))
;;       (rv (- index (.count fv)))))
;;   (applyTo [this args]                                                    
;;     (if (not= 1 (count args))
;;       (throw (clojure.lang.ArityException. (count args) "Deque"))
;;       (this (first args))))
;;   clojure.lang.Reversible
;;   (rseq [this]  (Deque. rv fv cnt))
;;   clojure.lang.ILookup
;;   (valAt [this index]
;;     (this index))
;;   (valAt [this index value]
;;     ;;semantically incorrect implementation.
;;     (.valAt this index))
;;   clojure.lang.IPersistentVector
;;   (cons [this a]
;;     (.push-back this a))
;;   (length [this]
;;     (.count this))
;;   (assocN [this index value]
;;     (.assoc this index value))
;;   clojure.lang.Associative
;;   (containsKey [this index]
;;     (and (pos? index) (< index  cnt)))
;;   (entryAt [this index]
;;     (.valAt this index))
;;   (assoc [this index value]
;;     (if (< index (.count fv))
;;       (Deque. (assoc fv (- (.count fv) index 1) value) rv 0)
;;       (Deque. fv (assoc rv (- index (.count fv)) value) 0)))
;;   clojure.lang.Indexed
;;   (nth [this index]        (.valAt this index))
;;   (nth [this index value]  (.assoc this index value))
;;   clojure.lang.IPersistentStack
;;   (pop [this]   (.pop-front this))
;;   (peek [this]  (.back this))
;; ;  clojure.lang.IEditableCollection
;; ;  (transient [this] (into (mdeque. fv rv)))
;; )

;; (defmethod clojure.core/print-method spork.data.simpleq.Deque [x writer]
;;   ; extend this multimethod so deques print correctly in repl
;;   (.write writer (.toString x)))

;; (def empty-deque (Deque. [] [] 0))

;; (def empty-sorted-deque (SortedDeque. [] [] 0 compare identity))

;; (defn sorted-deque-by
;;   ([comparator]
;;    (SortedDeque. [] [] 0 compare identity))
;;   ([comparator keyf] 
;;    (SortedDeque. [] [] 0 compare  keyf)))

;; (defn deque [& lst]
;;   (into empty-deque lst))

;; (defn catdeque [& xs]
;;   (into (Deque. (rrb/vec) (rrb/vec)

;(defn sorted-deque [& xs]
;  (into (empty

;; (defn flush-q [coll]
;;   (let [bound (count coll)]
;;     (loop [^Deque  coll coll
;;            idx  0]
;;       (if (== idx bound) nil
;;           (recur (.next coll)
;;                  (unchecked-inc idx))))))

;; (defn flush-q [coll]
;;   (let [bound (count coll)]
;;     (loop [^clojure.lang.IPersistentStack coll coll
;;            idx  0]
;;       (if (== idx bound) nil
;;           (recur (.pop coll)
;;                  (unchecked-inc idx))))))
;; (defn mqtest []
;;   (let [^java.util.ArrayDeque m 
;;         (reduce (fn [^java.util.Collection acc n]
;;                   (doto acc (.add n)))
;;                 (java.util.ArrayDeque.) (range 1000000))]
;;     (time (loop [idx 0]
;;             (if (== idx 1000000) nil
;;                 (do (.pop m)
;;                     (unchecked-inc idx)))))))

;; (defmacro push!! [d v]
;;   (let [d (with-meta d {:tag 'java.util.ArrayDeque})]
;;     `(let [~d (.clone ~d)]
;;        (doto ~d (.push ~v)))))

;; (defmacro pop!! [d]
;;   (let [d (with-meta d {:tag 'java.util.ArrayDeque})]
;;     `(let [~d (.clone ~d)]
;;        (doto ~d (.pop)))))

;; (defmacro pop!!! [d]
;;   (let [d (with-meta d {:tag 'java.util.ArrayDeque})]
;;     `(doto ~d (.pop))))

;; (comment
  
;; (defn pdeq-test []
;;   (let [coll (into (deque) (map (fn [n] [n n]) (range 10)))
;;         bound (count coll)]
;;     (time (dotimes [i 1000000]
;;             (loop [^clojure.lang.IPersistentStack coll coll
;;                    idx  0]
;;               (if (== idx bound) nil
;;                   (recur (.pop coll)
;;                          (unchecked-inc idx))))))))

;; (defn cloned-deq-test []
;;   (let [^java.util.ArrayDeque
;;         d  (reduce (fn [^java.util.Collection acc x]
;;                      (doto acc (.add [x x]))) (java.util.ArrayDeque.) (range 10))
;;         ]
;;     (time (dotimes [i 1000000]
;;             (loop [acc (.clone d)]
;;               (if (zero? (.size acc))
;;                 nil
;;                 (recur (pop!! acc))))))))

;; (defn deq-test []
;;   (let [^java.util.ArrayDeque
;;         d  (reduce (fn [^java.util.Collection acc x]
;;                      (doto acc (.add [x x]))) (java.util.ArrayDeque.) (range 10))
;;         ]
;;     (time (dotimes [i 1000000]
;;             (loop [acc (.clone d)]
;;               (if (zero? (.size acc))
;;                 nil
;;                 (recur (pop!!! acc))))))))
;; )
;;can we get the performance of an array dequeue, particularly a small dequeue, with a persistent
;;vector?

;;yes; if we maintain pointers to the front of the queue, we get that.  also, without the
;;need for subvecing.

;;So, popping is as simple as bumping the long.
;;Making a transient version also is as simple as allowing a mutable variable.
;;if we eventually flush whole queue, we garbage collect the old array;
;;creating a new queue will create a new array.
;;If we can keep things in a flat array, we can quicksort in place, or binsearch it
;;and insertion sort.

;;can we get some of the benefits of the java array deque...
;;for instance, instead of copying the pointer on a dequeue, we merely bump the pointer?
;;We can avoid copying the array this way.

;;f 0 
;;[1 2 3]
;;r 0
;;[4 5 6]

;;so, the only operations we have to worry about are conjs...
;;pops are simply long bumps.
;;we don't get all the benefit of mutation, but it's not ba either.
;;Another option is a copy-on-write array.

;;hd -> 0            tl -> 10
;;[1 2 3 4 5 6 7 8 9 10]

;;pop is simply bump the pointer (don't mutate the array).
;;we can lazily sort messages too....
;;allow arbitary insertion of large numbers of messages.
;;sort on demand?

;;dunno.
