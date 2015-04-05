;:A mutable priorityq implmentation that supports 
;;dynamic re-weighing of keys, as well as operations on 
;;the front and back of the queue (a double-ended queue).
;;Designed for use in general priority queue operations like
;;Dijkstra's algorithm, and for bounded priority queue applications
;;like Beam Search.
(ns spork.data.mpq
  (:require [spork.protocols [core :as core]]))

;;So, an alternative is to provide a mutable priority queue, based 
;;around a treeset.  We can provide some convenience functions to 
;;allow us to cache the min and max elements.  Additionally, 
;;treeset already provides operations for polling the lowest and 
;;highest elements respectively.  For a pure PQ, it'd be nicely
;;implemented  as a leftist heap, or using a 2-3 fingertree. 

;;In our use case for priority fringes, we should be able to 
;;discern the need to re-weigh upon relaxation. 
;;We only ever have one instance of a node on the fringe, and
;;if it's on the fringe, it has a best-known-distance.
;;So if we want to re-weigh the node on the fringe, we 
;;only need to check to see if there's a corresponding 
;;[best-known-weight node] in the treeset. 
;;If so, then we remove the [best-known-weight node] and conj
;;the [new-best-weight node] onto the treeset.
;;That should maintain the invariant that we're looking for, 
;;and allow us to re-weigh nodes on the priority fringe.
;;So, in order to re-weigh the old, you have to know the 
;;old weight.  Another option is to simply maintain a 
;;hashmap of active nodes and the current weight.


(deftype Mpq [^java.util.HashMap node->weightnodes
              ^java.util.TreeSet weightnodes
              _meta]
  Object  (toString [this] (str (.seq this)))
  core/IPQ 
  (priority-seq [pq] (seq weightnodes))
  core/IMinQ
  (get-min [pq] (when-let [^clojure.lang.MapEntry e (.first weightnodes)]
                  (.val e)))
  (pop-min [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollFirst weightnodes)]
                 (.remove node->weightnodes (.val e)))
               pq))                      
  (min-priority [pq]
    (when-let [^clojure.lang.MapEntry e (.first weightnodes)]
      (.key  e)))  
  core/IMaxQ
  (get-max [pq] (when-let [^clojure.lang.MapEntry e (.last weightnodes)]
                  (.val e)))
  (pop-max [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollLast weightnodes)]
                 (.remove node->weightnodes (.val e)))
               pq))                      
  (max-priority [pq]
    (when-let [^clojure.lang.MapEntry e (.last weightnodes)]
      (.key  e)))  
  clojure.lang.IPersistentMap  
  (assoc [this node weight]    
    (if-let [^clojure.lang.MapEntry e (.get node->weightnodes node)]      
      (if (= weight (.key e))    this ;no change.
          (let [enew (clojure.lang.MapEntry. weight node)]
            (do (.put node->weightnodes node enew)
                (.remove weightnodes e)
                (.add weightnodes enew)
                this)))
      (let [enew (clojure.lang.MapEntry. weight node)]
        (do (.put node->weightnodes node enew)
            (.add weightnodes enew)
            this))))
  (assocEx [this k v]  (.assoc this k v))
  (without [this node]   
      (do (when-let [^clojure.lang.MapEntry e (.get node->weightnodes node)] (.remove node->weightnodes node)
                    (.remove weightnodes e))
          this))
  java.lang.Iterable ;weak implementation.
  (iterator [this]  (iterator-seq (.seq this)))
  clojure.lang.Associative
  (containsKey [_ k]  (.containsKey node->weightnodes k))
  (entryAt [_ k]      (.get node->weightnodes k))
  clojure.lang.IPersistentCollection
  (count [_]     (.size node->weightnodes))
  (cons [this a] (.assoc this (first a) (second a)))          
  (empty [_]     (Mpq. (java.util.HashMap.) (java.util.TreeSet.)  {}))
  ;This is terrible...need a better equiv definition, although I'm
  ;"not" using it for equality checks....
  (equiv [this o] (identical? this o))  
  clojure.lang.ILookup
  (valAt [this k] (if-let [^clojure.lang.MapEntry e (.get node->weightnodes k)]
                 (.val e)))
  (valAt [this k not-found] (.valAt this k not-found))
  clojure.lang.Seqable
  (seq [this]   (seq weightnodes))
  clojure.lang.IPersistentStack
  (pop  [this] (.pop-min this))
  (peek [this] (.get-min this))
  clojure.lang.Indexed
  (nth [this i]            (nth (.seq this) i))
  (nth [this i not-found]  (if  (<= i (.count this)) (.nth this i) not-found))
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (Mpq. node->weightnodes weightnodes m))
  clojure.lang.Reversible
  (rseq [this]  (map val (seq (.descendingSet weightnodes))))
;  java.io.Serializable ;Serialization comes for free with the other stuff.
  core/IExposed
  (expose [this]  {:node->weightnodes node->weightnodes
                   :weightnodes weightnodes
                   :meta _meta})
  )


;;bounded queue keeps track of a limit on the total nodes 
;;allowed in the queue.  If we meet that limit, then the 
;;behavior changes.  Future nodes that are added 
;;are checked prior to addition under bound constraints.
;;If their weight is greater than or equal to the cutoff, they are 
;;ignored (fail fast).
;;If their weight is less than the cutoff, we drop the cutoff
;;insert the node, and update the cutoff.

;;The other areas where the cutoff changes is when we have 
;;a re-weighting.  In this case, the bound is okay, we're 
;;just updating the cutoff information.  If the new weight 
;;is greater than the cutoff, we update the cutoff after 
;;re-weighting.

(deftype Boundedpq [^java.util.HashMap node->weightnodes
                    ^java.util.TreeSet weightnodes  
                    ^long bound
                    ^{:unsynchronized-mutable true} cutoff
                    ^{:unsynchronized-mutable true :tag long} distance
                    exceeds?
                    _meta]
  Object  (toString [this] (str (.seq this)))
  core/IPQ 
  (priority-seq [pq] (seq weightnodes))
  core/IMinQ
  (get-min [pq] (when-let [^clojure.lang.MapEntry e (.first weightnodes)]
                  (.val e)))
  (pop-min [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollFirst weightnodes)]
                 (do  (.remove node->weightnodes (.val e))
                      (set! distance (unchecked-inc distance))))
               pq))                      
  (min-priority [pq]
    (when-let [^clojure.lang.MapEntry e (.first weightnodes)]
      (.key  e)))  
  core/IMaxQ
  (get-max [pq] (when-let [^clojure.lang.MapEntry e (.last weightnodes)]
                  (.val e)))
  (pop-max [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollLast weightnodes)]
                 (do (.remove node->weightnodes (.val e))
                     (set! distance (unchecked-inc distance))))
               pq))                      
  (max-priority [pq]
    (when-let [^clojure.lang.MapEntry e (.last weightnodes)]
      (.key  e)))  
  clojure.lang.IPersistentMap  
  (assoc [this node weight]     
    (if-let [^clojure.lang.MapEntry e (.get node->weightnodes node)]  ;reweight    
      (if (= weight (.key e))    this ;no change.          
          (let [enew (clojure.lang.MapEntry. weight node)]
            (do (.put node->weightnodes node enew)
                (.remove weightnodes e)
                (.add weightnodes enew)
                (when (zero? distance)
                  (set! cutoff (.max-priority this))) ;update the bound
                  this)))        
        (if (and (zero? distance) 
                 (exceeds? weight cutoff)) this ;ignore the node.
            (let [enew (clojure.lang.MapEntry. weight node)] ;add new node
              (do (.put node->weightnodes node enew)
                  (.add weightnodes enew)
                  (case distance
                       0  (do (.pop-max this)  ;make room and update
                              (set! distance 0) ;offset popmax
                              (set! cutoff (.max-priority this)))
                       1  (do (set! distance 0) ;update
                              (set! cutoff (.max-priority this)))
                       ;more nodes means less room.
                       (set! distance (unchecked-dec distance)))                    
                  this)))))
  (assocEx [this k v]  (.assoc this k v))
  (without [this node]   
      (do (when-let [^clojure.lang.MapEntry e (.get node->weightnodes node)] (.remove node->weightnodes node)
                    (do (.remove weightnodes e)
                        (set! distance (unchecked-inc distance))))
          this))
  java.lang.Iterable ;weak implementation.
  (iterator [this]  (iterator-seq (.seq this)))
  clojure.lang.Associative
  (containsKey [_ k]  (.containsKey node->weightnodes k))
  (entryAt [_ k]      (.get node->weightnodes k))
  clojure.lang.IPersistentCollection
  (count [_]     (.size node->weightnodes))
  (cons [this a] (.assoc this (first a) (second a)))          
  (empty [_]     (Boundedpq. (java.util.HashMap.) (java.util.TreeSet.)  Long/MAX_VALUE  nil Long/MAX_VALUE compare {}))
  ;This is terrible...need a better equiv definition, although I'm
  ;"not" using it for equality checks....
  (equiv [this o] (identical? this o))  
  clojure.lang.ILookup
  (valAt [this k] (if-let [^clojure.lang.MapEntry e (.get node->weightnodes k)]
                 (.val e)))
  (valAt [this k not-found] (.valAt this k not-found))
  clojure.lang.Seqable
  (seq [this]   (seq weightnodes))
  clojure.lang.IPersistentStack
  (pop  [this] (.pop-min this))
  (peek [this] (.get-min this))
  clojure.lang.Indexed
  (nth [this i]            (nth (.seq this) i))
  (nth [this i not-found]  (if  (<= i (.count this)) (.nth this i) not-found))
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (Boundedpq. node->weightnodes weightnodes  bound cutoff distance exceeds? m))
  clojure.lang.Reversible
  (rseq [this]  (map val (seq (.descendingSet weightnodes))))
;  java.io.Serializable ;Serialization comes for free with the other stuff.
  core/IExposed
  (expose [this]  {:node->weightnodes node->weightnodes
                   :weightnodes weightnodes
                   :bound bound 
                   :cutoff cutoff
                   :distance distance
                   :exceeds? exceeds?
                   :meta _meta})
  )

 
(defn print-mpq [^Mpq p ^java.io.Writer w]   
  (let [bound (.count p)
        _     (.write w "#{")]
    (doseq [entry (seq p)]
        (.write w (str (val entry)))
        (.write w " "))
    (.write w "}")))

(defn print-bpq [^Boundedpq p ^java.io.Writer w]   
  (let [bound (.count p)
        _     (.write w "#{")]
    (doseq [entry (seq p)]
        (.write w (str (val entry)))
        (.write w " "))
    (.write w "}")))
  
;; we can delegate to print-person from the various print hooks in
;; Clojure
(defmethod print-method Mpq [p  writer]  (print-mpq p writer))
(defmethod print-dup Mpq [p  writer] (print-mpq p  writer))

(defmethod print-method Boundedpq [p  writer]  (print-bpq p writer))
(defmethod print-dup Boundedpq [p  writer] (print-bpq p  writer))

(defn min-comparer [^clojure.lang.MapEntry l ^clojure.lang.MapEntry r]
  (let [c (compare (.key l) (.key r))]
    (case c
      0 (compare (.val l) (.val r))
      c)))

(defn max-comparer [^clojure.lang.MapEntry l ^clojure.lang.MapEntry r]
  (let [c (compare (.key r) (.key l))]
    (case c
      0 (compare (.val r) (.val l))
      c)))

(defn ->min-pq []     (Mpq. (java.util.HashMap.) (java.util.TreeSet. ^java.util.Comparator min-comparer) {}))
(defn ->max-pq []     (Mpq. (java.util.HashMap.) (java.util.TreeSet. ^java.util.Comparator max-comparer) {}))
(defn ->custom-pq [^java.util.Comparator f] (Mpq. (java.util.HashMap.) (java.util.TreeSet. f) {}))

(defn ->min-bounded-pq [^long bound]     
  (Boundedpq. (java.util.HashMap.) (java.util.TreeSet. ^java.util.Comparator min-comparer) 
              bound
              nil
              bound
              (fn [l r]  (== (compare l r) 1))              
              {}))

(defn ->max-bounded-pq [^long bound]     
  (Boundedpq. (java.util.HashMap.) (java.util.TreeSet. ^java.util.Comparator max-comparer) 
              bound
              nil
              bound
              (fn [l r]  (== (compare r l) 1))              
              {}))
;;testing 
(comment 

(require '[clojure.test :as test])

(def nodes [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p])
(defn fresh-pq 
  ([] 
     (reduce (fn [acc [node w]]
               (assoc acc node w))
             (->min-pq)
             (map-indexed (fn [i n] [n i]) (reverse nodes))))
  ([bound] 
     (reduce (fn [acc [node w]]
               (assoc acc node w))
             (->min-bounded-pq bound)
             (map-indexed (fn [i n] [n i]) (reverse nodes)))))

(let [pq (fresh-pq)]
  (test/deftest counting   
    (test/is (= (count pq) (count nodes)))
    (test/is (= (core/get-min pq) :p))
    (test/is (= (core/get-max pq) :a))))
  

(let [pq (fresh-pq)
      _  (dotimes [i 10] (core/pop-min pq))]
  (test/deftest popleft
    (test/is (= (count pq) (- (count nodes) 10)))
    (test/is (= (core/get-min pq) :f))
    (test/is (= (core/get-max pq) :a))))


(let [pq (fresh-pq)
      _  (dotimes [i 10] (core/pop-max pq))]
  (test/deftest popright
    (test/is (= (count pq) (- (count nodes) 10)))
    (test/is (= (core/get-min pq) :p))
    (test/is (= (core/get-max pq) :k))))

(let [pq (-> (fresh-pq) (assoc :a 0) (assoc :e 200))]
  (test/deftest reweighting
    (test/is (= (count pq) (count nodes)))
    (test/is (= (core/get-min pq) :a))
    (test/is (= (core/get-max pq) :e))))

(let [pq (fresh-pq 5)]
  (test/deftest countingb   
    (test/is (= (count pq) 5))
    (test/is (= (core/get-min pq) :p))
    (test/is (= (core/get-max pq) :l))))
  

(let [pq (fresh-pq 5)
      _  (dotimes [i 2] (core/pop-min pq))]
  (test/deftest popleftb
    (test/is (= (count pq) 3))
    (test/is (= (core/get-min pq) :n))
    (test/is (= (core/get-max pq) :l))))


(let [pq (fresh-pq 5)
      _  (dotimes [i 2] (core/pop-max pq))]
  (test/deftest poprightb
    (test/is (= (count pq) 3))
    (test/is (= (core/get-min pq) :p))
    (test/is (= (core/get-max pq) :n))))

(let [pq (-> (fresh-pq 5) 
             (assoc :a 0)  ;adds :a, bumps :n
             (assoc :e 200) ;should be ignored
             )]
  (test/deftest addingb
    (test/is (= (count pq) 5))
    (test/is (= (core/get-min pq) :a))
    (test/is (= (core/get-max pq) :m))))
)

