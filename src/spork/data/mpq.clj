;:A mutable priorityq implmentation that supports 
;;dynamic re-weighing of keys, as well as operations on 
;;the front and back of the queue (a double-ended queue).
;;Designed for use in general priority queue operations like
;;Dijkstra's algorithm, and for bounded priority queue applications
;;like Beam Search.
(ns spork.data.mpq
  (:require [spork.data.protocls [core :as core]]))

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
  

(deftype mpq [^java.util.HashMap node->weightnodes
              ^java.util.TreeSet weightnodes
              _meta]
  Object  (toString [this] (str (.seq this)))
  core/IPQ 
  (priotity-seq [pq] (seq weightnodes))
  core/IMinQ
  (get-min [pq] (when-let [^clojure.lang.MapEntry e (.first weightnodes)]
                  (.val e)))
  (pop-min [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollFirst weightnodes)]
                 (.remove node->weightnodes (.val e)))
               this))                      
  (min-priority [pq]
    (when-let [^clojure.lang.MapEntry e (.first weightnodes)]
      (.key  e)))  
  core/IMaxQ
  (get-max [pq] (when-let [^clojure.lang.MapEntry e (.last weightnodes)]
                  (.val e)))
  (pop-max [pq] 
           (do (when-let [^clojure.lang.MapEntry e (.pollLast weightnodes)]
                 (.remove node->weightnodes (.val e)))
               this))                      
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
  (containsKey [_ k]  (.containsKey node->weight k))
  (entryAt [_ k]      (.get node->weight k))
  clojure.lang.IPersistentCollection
  (count [_]     (.count node->weight))
  (cons [this a] (.assoc this (first a) (second a)))          
  (empty [_]  (mpq. (java.util.HashMap.) (java.util.TreeSet.)  nil nil {}))
  ;This is terrible...need a better equiv definition, although I'm
  ;"not" using it for equality checks....
  (equiv [this o] (identical? this o))  
  clojure.lang.ILookup
  (valAt [_ k] (if-let [^clojure.lang.MapEntry e (.get node->weightnodes k)]
                 (.val e)))
  (valAt [_ k not-found] (.valAt this k not-found))
  clojure.lang.Seqable
  (seq [this]   (map val (seq weightnodes)))
  clojure.lang.IPersistentStack
  (pop  [this] (if (zero? (.count weightnodes)) 
                 this
                 (do (let [

  (peek [this] (next-val basemap))
  clojure.lang.Indexed
  (nth [this i]            (nth (.seq this) i))
  (nth [this i not-found]  (if  (<= i (.count this)) (.nth this i) not-found))
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (mpq. node->weight weightnodes min max m))
  clojure.lang.Reversible
  (rseq [this]  (map val (seq (.descendingSet weightnodes))))
;  java.io.Serializable ;Serialization comes for free with the other stuff.


  )



