(ns spork.data.fringe
  "Stock implementations of the IFringe protocol from spork.protocols.fringe, as 
   inspired by Bob Sedgewick's Graph Algorithms in C."
   (:require [spork.data      [priorityq :as pq] 
                              [randq :as rq]]
             [spork.protocols [core :as generic]]))

(def emptyq clojure.lang.PersistentQueue/EMPTY)

;;Implementations of basic stack (depth first) and queue (breadth first) 
;;fringes.
(extend-protocol generic/IFringe 
  nil 
  (conj-fringe [fringe n w] (conj '() (generic/entry n w)))
  (next-fringe [fringe]  nil)
  (pop-fringe  [fringe]  nil)
  (re-weigh    [fringe n wold wnew] (conj '() (generic/entry n wnew)))
  (re-label    [fringe n w newlabel] (throw (Exception. "Empty fringe!")))    
  clojure.lang.PersistentQueue
  (conj-fringe [fringe n w] (conj fringe (generic/entry n w)))
  (next-fringe [fringe]    (first fringe))
  (pop-fringe  [fringe]    (pop fringe))
  (re-weigh    [fringe n wold wnew] 
    (let [[prior post] (split-with #(not= (generic/entry n wold)) fringe)]
      (into emptyq (concat prior (generic/entry n wnew) post))))             
  (re-label    [fringe n w newlabel] fringe)  
  clojure.lang.PersistentList
  (conj-fringe [fringe n w] (conj fringe (generic/entry n w)))
  (next-fringe [fringe]    (first fringe))
  (pop-fringe  [fringe]    (next fringe))
  (re-weigh    [fringe n wold wnew] 
    (let [[prior post] (split-with #(not= (generic/entry n wold)) fringe)]
      (concat prior (generic/entry n wnew) post)))             
  (re-label [fringe n w newlabel] fringe)
  clojure.lang.PersistentList$EmptyList
  (conj-fringe [fringe n w] (conj fringe (generic/entry n w)))
  (next-fringe [fringe]    (first fringe))
  (pop-fringe  [fringe]    (next fringe))
  (re-weigh    [fringe n wold wnew] 
    (let [[prior post] (split-with #(not= (generic/entry n wold)) fringe)]
      (concat prior (generic/entry n wnew) post)))             
  (re-label [fringe n w newlabel] fringe) 
  clojure.lang.Cons 
  (conj-fringe [fringe n w] (conj fringe (generic/entry n w)))
  (next-fringe [fringe]     (first fringe))
  (pop-fringe  [fringe]     (next fringe))
  (re-weigh    [fringe n wold wnew] 
    (let [[prior post] (split-with #(not= (generic/entry n wold)) fringe)]
      (concat prior (generic/entry n wnew) post)))             
  (re-label [fringe n w newlabel] fringe))

;;Extend the IFringe protocol to priority queues, so we get a priority fringe.
(extend-protocol generic/IFringe
  spork.data.priorityq.pqueue
    (conj-fringe [fringe n w] (conj  fringe (generic/entry n w)))
    (next-fringe [fringe]     (peek fringe))
    (pop-fringe  [fringe]     (pop fringe))
    (re-weigh    [fringe n wold wnew] (pq/alter-value fringe n wold wnew))
    (re-label    [fringe n w newlabel] (pq/alter-value fringe n w w (fn [_] newlabel))))

(extend-protocol generic/IFringe
  spork.data.randq.randomq
    (conj-fringe [fringe n w] (conj fringe (generic/entry n w)))
    (next-fringe [fringe]     (peek fringe))
    (pop-fringe  [fringe]     (pop fringe))
    (re-weigh    [fringe n wold wnew] ;;really inefficient. 
      (let [[prior post] (split-with #(not= (generic/entry n wold)) fringe)]
        (into rq/emptyrq (concat prior (generic/entry n wnew) post))))
    (re-label    [fringe n w newlabel] fringe))

;;The four primitive fringes.  Just aliases for provided implementations.
(def breadth-fringe 
  "Builds a fringe that stores [node weight] entries in first-in-first-out 
   FIFO order.  Backed by a persistent queue"
    emptyq)
(def priority-fringe
  "Builds a fringe that stores [node weight] entries in priority order, 
   according to minimal weight.  Backed by a sorted map."
   pq/minq)
(def random-fringe
  "Builds a fringe that stores [node weight] entries in random order.  Backed 
   by a spork.data.randq.randomq"
   rq/emptyrq)
(def depth-fringe 
  "Builds a fringe that stores [node weight] entries in last-in-first-out 
   LIFO order.  Backed by a persistent list."
  (list))