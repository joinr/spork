;;Possibly move this into a load-file dependency.
(ns spork.cljgraph.topsort
  (:require [spork.util [topographic :as top]]))

(defn- get-roots 
  [g]
  (->> (top/get-node-labels g)    
       (filter #(= 0 (count (top/sources g %))))))
        
(defn- drop-roots [g]
  (reduce #(top/drop-node %1 %2) g (get-roots g)))

;;##Check your logic, I don't think these do topsort like intended.

(defn topsort
  "Topologically sort the graph.  This is more efficient, in theory, 
   because it only calls get-roots to initialize the sort.  From there, 
   we determine which nodes will be nextroots, and queue them up, until 
   no nodes are left.  We avoid the book-keeping costs from dropping nodes
   in the graph."
  [g]
	(loop [roots (set (get-roots g)) 
	       visited roots
         acc [(seq roots)]]
     (if (seq roots)
       (let [nextroots (->> roots
                            (mapcat (partial top/sinks g))
                            (filter #(not (contains? visited %))))                
             nextacc   (if (seq nextroots) (conj acc nextroots) acc)]            
                                   
         (recur (set nextroots) (clojure.set/union visited roots) nextacc))
       (if (= (count visited) 
              (count (top/nodes g)))
           acc
           nil))))


(defn topsort-seq
  "Return a lazy sequence of topologically-sorted nodes, if there is one."
  [g]
	(if-let [roots (get-roots g)]
     (let [visited (atom (set (get-roots g)))
           valid? (fn [n] 
                     (if-not (contains? @visited n)
                       (do (swap! visited conj n)
                            true)
                        false))
           next-roots (fn [nodes] (->> nodes
                                    (mapcat (partial top/get-sinks g))
                                    (filter valid?)))]
       (take-while #(seq %) (iterate next-roots roots))))) 