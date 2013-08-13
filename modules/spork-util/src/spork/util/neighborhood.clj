;I think I can implement a simple, pure data structure that 
;approximates a doubly linked list...without mutation and 
;pointer references.  We can use this guy to help us out.
;It's based on the array-based implementation of doubly linked
;lists...
(ns spork.util.neighborhood)

(defn ->dnode [data id from to] {:data data :from from :to to})
(defn ->dlist [head tail nodes] {:head head :tail tail :nodes nodes})
(defn empty-dlist [] (->dlist nil nil []))

;(defn append-node [dlist nd]
;  (let [tail-idx (or (:tail dlist) 0)]
;    (->dnode [
        
{:head nil :tail nil
 :nodes []
 :ins nil
 :outs nil}

{:head 0 :tail 0
 :nodes [:a]
 :ins   [nil]
 :outs  [nil]}

{:head 0 :tail 1
 :nodes [:a :b]
 :ins   [nil 0]
 :outs  [1  nil]}

{:head 0 :tail 2
 :nodes [:a :b :c]
 :ins   [nil 0 1]
 :outs  [1   2 nil]}
  
{:head 0 :tail 3 
 :nodes [:a  :b :c :d]
 :ins   [nil  0  1  2]
 :outs  [0    1  2  nil]}


