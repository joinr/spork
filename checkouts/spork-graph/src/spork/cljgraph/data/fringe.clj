(ns spork.cljgraph.data.fringe
  "A generic set of prorotocols defining operations on auxillary graph 
   data structures.  Specifically, we want a uniform interface for accessing 
   things like search state (fringe nodes, shortest path tree, etc.).
   
   Specifically, we'd like to have a uniform protocol for pushing nodes onto 
   the fringe, getting the next node from the fringe, etc.  Since graph 
   algorithms can be trivially modified by changing the datstructure used to 
   represent the fringe (stack, queue, priority-queue, random-queue, etc.), 
   we can build some useful generic operations around a generic fringe.
   
   This is primarily based off of Bob Sedgewick's graph algorithms book.  
   Bob's primary 4 fringe collections are: stack, queue, priorityq, 
   and random-queue."
   (:require [cljgraph.data [priorityq :as pq] 
                            [random :as rq]]))

;need to add docstrings.  Also, the function names could be clearer...
(defprotocol IFringe 
  (conj-fringe [fringe n w] "Add node n with weight w to the fringe.")
  (next-fringe [fringe] "Get the next node on the fringe")
  (pop-fringe [fringe] "Remove the next node from the fringe")
  (re-weigh [fringe n wold wnew]     
    "Return the result of updating n's weight.  Old weight wold must be 
     provided, since it is a part of n's key." )
  (re-label [fringe n w newlabel]
    "Return the result of updating n's label. Weight w must be 
     provided, since it is a part of n's key."))

(defn conj-fringe-all
  "Add many [node weight] pairs onto the fringe."
  [fringe nws]
  (reduce (partial apply conj-fringe) fringe nws))  
   
(defn fringe-stream
  "Return a sequence of popped {node weight} maps from 
   priorityq q."
  [fringe]
  (if-let [kv (next-fringe fringe)]
    (lazy-seq (cons kv (fringe-stream (pop-fringe fringe))))))
(defn empty-fringe? [fringe] (empty? (fringe-stream fringe)))

(defn- entry [n w] {:node n :weight w})

(defrecord stackfringe [stack]
  IFringe
  (conj-fringe [fringe n w] (stackfringe. (cons (entry n w) stack)))
  (next-fringe [fringe] (first stack))
  (pop-fringe [fringe] (stackfringe. (rest stack)))
  (re-weigh [fringe n wold wnew] 
     (let [[prior, post] (split-with #(not= (entry n wold)) stack)]
       (stackfringe. (concat prior (entry n wnew) post))))             
  (re-label [fringe n w newlabel] fringe))

(defn stack-fringe 
  "Build's an IFringe around list operations."
  [] (stackfringe. (list)))

(defn queue [& items]
  (let [q clojure.lang.PersistentQueue/EMPTY]
    (if items 
      (apply conj q items)
      q)))

(defmethod print-method clojure.lang.PersistentQueue [q,w]
  (print-method (quote <|) w) (print-method (seq q) w) 
  (print-method (quote <|) w))

(defn- enqueue [q itm]
  (conj q itm))

(defn- dequeue [q] 
  (if-not (empty? q)
    [(peek q) (pop q)]))
    
(defn- peek-stream [q]
  (take-while 
    #(not= nil %) 
      (map first (iterate (fn [[v q]] (dequeue q)) (dequeue q)))))

(defrecord qfringe [q]
    IFringe
	    (conj-fringe [fringe n w] (qfringe. (conj q (entry n w))))
	    (next-fringe [fringe] (peek q))
	    (pop-fringe [fringe] (qfringe.  (pop q)))
	    (re-weigh [fringe n wold wnew] 
	       (let [[prior, post] (split-with #(not= (entry n wold)) q)]
	         (qfringe. (into [] (concat prior (entry n wnew) post)))))             
	    (re-label [fringe n w newlabel] (qfringe. q)))

(defn q-fringe 
  "Build's an IFringe around queue operations.
   Note -> the queue is based on a persistent queue."
   [] (qfringe. (queue)))

(extend-protocol IFringe
  clojure.lang.PersistentTreeMap
    (conj-fringe [fringe n w] (pq/conj-node fringe n w))
    (next-fringe [fringe] (pq/next-node fringe))
    (pop-fringe [fringe] (pq/drop-node fringe))
    (re-weigh [fringe n wold wnew] (pq/alter-weight fringe n wold wnew))
    (re-label [fringe n w newlabel] fringe))

(extend-protocol IFringe
  cljgraph.data.random.randomq
    (conj-fringe [fringe n w] (rq/conj-rand fringe [n w]))
    (next-fringe [fringe] (rq/peek-rand fringe))
    (pop-fringe [fringe] (rq/pop-rand fringe))
    (re-weigh [fringe n wold wnew] fringe)
    (re-label [fringe n w newlabel] fringe))

(defn priority-fringe [] pq/minq)
(defn random-fringe [] (rq/random-q))
;(defn queue-fringe 
;  "Build's an IFringe around queue operations.
;   Note -> the queue is based on a vector...should check this against 
;   persistent queue performance."
;  ([data]
;	  (reify 
;	    IFringe
;		    (conj-fringe [fringe n w] (queue-fringe (conj data (entry n w))))
;		    (next-fringe [fringe] (first data))
;		    (pop-fringe [fringe] (queue-fringe  (rest data)))
;		    (re-weigh [fringe n wold wnew] 
;		       (let [[prior, post] (split-with #(not= (entry n wold)) data)]
;		         (queue-fringe (into [] (concat prior (entry n wnew) post)))))             
;		    (re-label [fringe n w newlabel] (queue-fringe data))
;      Object 
;        (toString [fringe] (str data))
;      clojure.lang.Seqable
;		    (seq [fringe]  (seq data)))
;	     )
;  ([] (queue-fringe [])))
