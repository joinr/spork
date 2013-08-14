(ns spork.protocols.fringe)

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