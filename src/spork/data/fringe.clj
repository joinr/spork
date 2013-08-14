(ns spork.data.fringe
  "Stock implementations of the IFringe protocol from spork.protocols.fringe, as 
   inspired by Bob Sedgewick's Graph Algorithms in C."
   (:require [spork.data      [priorityq :as pq] [randq :as rq]]
             [spork.protocols [core :as generic]]))

(def emptyq clojure.lang.PersistentQueue/EMPTY)
(defn ^clojure.lang.MapEntry entry [k v] (clojure.lang.MapEntry. k v))

;;Implementations of basic stack (depth first) and queue (breadth first) 
;;fringes.
(extend-protocol generic/IFringe 
  nil 
  (conj-fringe [fringe n w] (conj '() (entry n w)))
  (next-fringe [fringe]  nil)
  (pop-fringe  [fringe]  nil)
  (re-weigh    [fringe n wold wnew] (conj '() (entry n w)))
  (re-label    [fringe n w newlabel] (throw (Exception. "Empty fringe!")))    
  clojure.lang.PersistentQueue
  (conj-fringe [fringe n w] (conj fringe (entry n w)))
  (next-fringe [fringe]    (first fr))
  (pop-fringe  [fringe]    (pop fr))
  (re-weigh    [fringe n wold wnew] 
    (let [[prior post] (split-with #(not= (entry n wold)) q)]
      (into emptyq (concat prior (entry n wnew) post))))             
  (re-label    [fringe n w newlabel] fr)  
  clojure.lang.PersistentList
  (conj-fringe [fringe n w] (conj fringe (entry n w)))
  (next-fringe [fringe]    (first fr))
  (pop-fringe  [fringe]    (next fr))
  (re-weigh    [fringe n wold wnew] 
    (let [[prior post] (split-with #(not= (entry n wold)) fringe)]
      (concat prior (entry n wnew) post)))             
  (re-label [fringe n w newlabel] fringe) 
  clojure.lang.Cons 
  (conj-fringe [fringe n w] (conj fringe (entry n w)))
  (next-fringe [fringe]    (first fr))
  (pop-fringe  [fringe]    (next fr))
  (re-weigh    [fringe n wold wnew] 
    (let [[prior post] (split-with #(not= (entry n wold)) fringe)]
      (concat prior (entry n wnew) post)))             
  (re-label [fringe n w newlabel] fringe))

;;Extend the IFringe protocol to priority queues, so we get a priority fringe.
(extend-protocol generic/IFringe
  clojure.lang.PersistentTreeMap
    (conj-fringe [fringe n w] (pq/conj-node fringe n w))
    (next-fringe [fringe]     (pq/next-node fringe))
    (pop-fringe  [fringe]     (pq/drop-node fringe))
    (re-weigh    [fringe n wold wnew] (pq/alter-weight fringe n wold wnew))
    (re-label    [fringe n w newlabel] fringe))

(extend-protocol generic/IFringe
  spork.data.randq.randomq
    (conj-fringe [fringe n w] (rq/conj fringe (entry n w)))
    (next-fringe [fringe]     (rq/peek fringe))
    (pop-fringe  [fringe]     (rq/pop fringe))
    (re-weigh    [fringe n wold wnew] ;;really inefficient. 
      (let [[prior post] (split-with #(not= (entry n wold)) q)]
        (into rq/emptyrq (concat prior (entry n wnew) post))))
    (re-label    [fringe n w newlabel] fringe))

;;The four primitive fringes.  Just aliases for provided implementations.
(defn q-fringe 
  "Builds a fringe that stores [node weight] entries in first-in-first-out 
   FIFO order.  Backed by a persistent queue"
   [] (queue))
(defn priority-fringe
  "Builds a fringe that stores [node weight] entries in priority order, 
   according to minimal weight.  Backed by a sorted map."
  [] pq/minq)
(defn random-fringe
  "Builds a fringe that stores [node weight] entries in random order.  Backed 
   by a spork.data.randq.randomq"
  [] rq/emptyrq)
(defn stack-fringe 
  "Builds a fringe that stores [node weight] entries in last-in-first-out 
   LIFO order.  Backed by a persistent list."
  [] (list))