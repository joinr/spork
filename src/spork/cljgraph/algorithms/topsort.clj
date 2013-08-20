;;Possibly move this into a load-file dependency.
(ns spork.cljgraph.topsort
  (:require [spork.util [topographic :as top]]))

(defn- get-roots 
  ([g xs] (filter #(top/source-node? g %) xs))
  ([g] (get-roots g (top/get-node-labels g))))
        
(defn- drop-roots [g]
  (reduce #(top/disj-node %1 %2) g (get-roots g)))

(defn topsort
  "Topologically sort the graph.  We use a priority queue to maintain a fringe 
   of nodes to visit, where the priorities are the number of sources to a node.
   We pull arcs off of the "
  [g]
	(loop [the-graph  g
         roots (set (get-roots g)) ;;current roots in g 
         acc []]
     (if (seq roots)
       (let [xs  (mapcat (partial top/sinks the-graph)  roots) ;only possible new roots.
             next-graph  (top/drop-nodes the-graph roots)]
         (recur next-graph (set (get-roots next-graph xs))  (conj acc roots)))
       (if (= (reduce + (map count roots)) 
              (count (top/nodes g)))
           acc
           nil))))

(defn topological-order [g]
  (persistent! 
    (reduce (fn [acc xs] (reduce conj! acc xs)) (transient []) (topsort g)))) 

    