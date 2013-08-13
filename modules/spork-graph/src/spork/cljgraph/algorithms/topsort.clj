(in-ns 'cljgraph.graph)
(require  '[clojure.set :as set]) 

(defn- get-roots 
  [g]
  (let [ns (map node-label (get-nodes g))
        root? (fn [n] 
                  (= 0 (count (get-sources g n))))]
    (filter root? ns)))
        
(defn- drop-roots [g]
  (reduce #(drop-node %1 %2) g (get-roots g)))

;(defn topsort1
;  "Topologically sort the graph g.  topsort returns a nested sequence  
;   of topologically-equivalent nodes in the order they enter the root set. 
;   If g is not a directed acyclic graph (DAG), returns nil.  
;   In a DAG, we should have one or more root nodes (nodes with no incoming 
;   arcs).  At each step, we create a subgraph by dropping the root nodes.  The 
;   algorithm will converge on a graph with no roots.  
;   
;   If the graph is DAG, the algorithm will converge on the empty graph 
;   (no nodes), (due to each node getting dropped in turn)."
;  [g]
;	(loop [graph g
;         rootcount 0
;	       acc []]
;	  (let [roots (seq (get-roots graph))]
;       (if roots         
;         (recur (drop-roots graph) 
;                (+ rootcount (count roots)) (conj acc roots))
;         (if (= 0 (count (get-nodes graph)))
;             acc
;             nil)))))

;(defn topsort-seq1
;   "You may only need to consume pieces of the topology, hence the lazy option."
;  [g] 
;  
;  (lazy-seq
;    (if-let [roots (seq (get-roots g))]
;      (concat [roots] (topsort-seq (drop-roots g)))))) 

(defn topsort
  "Topologically sort the graph.  This is more efficient, in theory, 
   because it only calls get-roots to initialize the sort.  From there, 
   we determine which nodes will be nextroots, and queue them up, until 
   no nodes are left.  We avoid the book-keeping costs from dropping nodes
   in the graph."
  [g]
	(loop [roots (set (get-roots g)) 
	       visited #{}
         acc [(seq roots)]]
     (if (seq roots)
       (let [nextroots (->> roots
                            (mapcat (partial get-sinks g))
                            (filter #(not (contains? visited %))))                
             nextacc   (if (seq nextroots) (conj acc nextroots) acc)]            
                                   
         (recur (set nextroots) (set/union visited roots) nextacc))
       (if (= (count visited) 
              (count (get-nodes g)))
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
                                    (mapcat (partial get-sinks g))
                                    (filter valid?)))]
       (take-while #(seq %) (iterate next-roots roots))))) 
         
        
;(defn rand-dag [nodes arcs]
;  (
;(topsort (make-dag))
;[(0) (1) (2) (4 3) (5)]


