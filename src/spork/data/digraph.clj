;;A collection of digraph implementations.
(ns spork.data.digraph
  (:require [spork.protocols.core :refer :all]
            [spork.data [orderedmap :as om]]))

;;The topograph Data Structure, and Implementation of ITopograph
;;==============================================================

;;The topograph is implemented as a directed graph.  Instead of edge lists, we 
;;maintain two edge maps for each node: 
;;sources : {nd #{sink nodes}}
;;sinks   : {nd #{source nodes}}
;;These are analagous to neighborhoods in my cljgraph library.
(defrecord digraph [nodes sources sinks]
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (digraph. m sources sinks))
  (-conj-node [tg k v] 
    (digraph. (assoc nodes k v) 
                (assoc sources k om/empty-ordered-map)  
                (assoc sinks k om/empty-ordered-map)))
  (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sources k)  (-get-sinks tg k))
          new-sinks   (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sinks k)  (-get-sources tg k))]
      (digraph. (dissoc nodes k) new-sources new-sinks)))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (digraph. nodes 
                (update-in sources [sink]   assoc source w)
                (update-in sinks   [source] assoc sink   w))))
  (-disj-arc  [tg source sink]   
    (digraph. nodes 
      (assoc sources sink  (or (dissoc (get sources sink) source) 
                                om/empty-ordered-map)) 
      (assoc sinks   source (or (dissoc (get sinks source) sink) 
                                om/empty-ordered-map))))
  (-has-arc?  [tg source sink] (contains? (get sources sink) source))
  (-get-arc   [tg source sink] [source sink (get-in sinks [source sink])])
  (-get-sources [tg k] (vec (keys (get sources k))))
  (-get-sinks [tg k]   (vec (keys (get sinks k)))))

(def empty-digraph (->digraph {} {} {})) 


  
