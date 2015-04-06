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
  core/IFringe
  (conj-fringe [fringe n w] (.assoc fringe n w))
  (next-fringe [fringe]     (.get-min fringe))
  (pop-fringe  [fringe]     (.pop-min fringe))
  core/IPQ 
  (priority-seq [pq] (seq weightnodes))
  core/IMinQ
  (get-min [pq] (when (pos? (.count pq)) (.val   ^clojure.lang.MapEntry (.first weightnodes))))
  (pop-min [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollFirst weightnodes)]
                 (.remove node->weightnodes (.val e)))
               pq))                      
  (min-priority [pq]  (when (pos? (.count pq))
                        (.key   ^clojure.lang.MapEntry (.first weightnodes))))  
  core/IMaxQ
  (get-max [pq] (when (pos? (.count pq)) (.val   ^clojure.lang.MapEntry (.last weightnodes))))
  (pop-max [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollLast weightnodes)]
                 (.remove node->weightnodes (.val e)))
               pq))                      
  (max-priority [pq]  (when (pos? (.count pq))
                        (.key   ^clojure.lang.MapEntry (.last weightnodes))))  
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
  core/IFringe
  (conj-fringe [fringe n w] (.assoc fringe n w))
  (next-fringe [fringe]     (.get-min fringe))
  (pop-fringe  [fringe]     (.pop-min fringe))
  core/IMinQ
  (get-min [pq] (when (pos? (.count pq)) (.val   ^clojure.lang.MapEntry (.first weightnodes))))
  (pop-min [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollFirst weightnodes)]
                 (.remove node->weightnodes (.val e)))
               pq))                      
  (min-priority [pq]  (when (pos? (.count pq))
                        (.key   ^clojure.lang.MapEntry (.first weightnodes))))  
  core/IMaxQ
  (get-max [pq] (when (pos? (.count pq)) (.val   ^clojure.lang.MapEntry (.last weightnodes))))
  (pop-max [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollLast weightnodes)]
                 (.remove node->weightnodes (.val e)))
               pq))                      
  (max-priority [pq]  (when (pos? (.count pq))
                        (.key   ^clojure.lang.MapEntry (.last weightnodes)))) 
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

;;Note - using sorted sets for pqs is a pain, because set equality
;;is determined by comparison value.  You forget this is not a
;;"regular" priority queue, i.e. the number of items with the same
;;values is irrelevant, and removal order of items with the same value
;;respects insertion order.  In other words, a regular priority queue
;;acts like a queue; faking a treeset as a queue "can" act like a
;;queue, but we have to finagle the comparer.  Lack of proper
;;finagling means that we can have unintended consequences in things
;;like dijkstra, where we find a path to the destination node and
;;end up re-weighting it, unintentionally visiting it over a
;;pre-existing node with the same cost, the preference due to
;;the fact that the destination node's value compares as less than.
;;To fix this, we force the pq to "break ties" by always decalring
;;greater than.

(defn min-comparer [^clojure.lang.MapEntry l ^clojure.lang.MapEntry r]
  (let [c (compare (.key l) (.key r))]
    (case c
      0  (let [v  (compare (.val l) (.val r))]
             (if (zero? v) 0 1))
      c)))

(defn max-comparer [^clojure.lang.MapEntry r ^clojure.lang.MapEntry l]
  (let [c (compare (.key l) (.key r))]
    (case c
      0  (let [v  (compare (.val l) (.val r))]
               (if (zero? v) 0 1))
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
