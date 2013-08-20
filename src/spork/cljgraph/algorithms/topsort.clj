;;Possibly move this into a load-file dependency.
(ns spork.cljgraph.topsort
  (:require [spork.util [topographic :as top]]))

(defn- get-roots 
  ([g xs] (filter #(top/source-node? g %) xs))
  ([g] (get-roots g (top/get-node-labels g))))
        
(defn- drop-roots [g]
  (reduce #(top/disj-node %1 %2) g (get-roots g)))

(defn topsort
  "Topologically sort the graph, returning a sequence of sets of topological 
   roots.  Multiple valid topological orderings may be returned from the
   result."
  [g]
	(loop [graph g
         roots (set (get-roots g)) ;;current roots in g 
         acc []]
     (if (seq roots)
       (let [xs  (mapcat (partial top/sinks graph)  roots) ;only possible new roots.
             next-graph  (top/drop-nodes graph roots)]
         (recur next-graph (set (get-roots next-graph xs))  (conj acc roots)))
       (if (= (reduce + (map count acc)) 
              (count (top/nodes g)))
           acc
           nil))))

(defn topological-order [g]
  (persistent! 
    (reduce (fn [acc xs] (reduce conj! acc xs)) (transient []) (topsort g)))) 

;;testing
(comment
  
;a directed graph...
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f/^
 
 (def the-graph 
   (-> top/empty-topograph
       (top/add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:f :c] [:b :d]])))

 (assert (= (topsort the-graph)  [#{:a :e} #{:b :f} #{:c} #{:d}]))
 (assert (= (topological-order the-graph) [:a :e :b :f :c :d]))

 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 
 (def the-graph2 
   (-> top/empty-topograph
       (top/add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])))
 (assert (= (topsort the-graph2) [#{:a :e} #{:b :f} #{:c} #{:d}])) 
 (assert (= (topological-order the-graph2)
            [:a :e :b :f :c :d]))
 
 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 ;    g 
 ;    h
 
 (def the-graph3
   (-> top/empty-topograph
     (top/add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])
     (top/conj-node :g)
     (top/conj-node :h)))
 (assert (= (topsort the-graph3) [#{:a :g :e :h} #{:b :f} #{:c} #{:d}]))
 (assert (= (topological-order the-graph3) [:a :g :e :h :b :f :c :d]))    
)



