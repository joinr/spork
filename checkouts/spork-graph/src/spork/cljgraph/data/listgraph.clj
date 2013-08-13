(ns spork.cljgraph.data.listgraph
    (:use [cljgraph.graph :only [arc-label node-label init-graph]]))
    

;An adjacency-list implementation for IGraph.
;In this case, a graph is a list of source-nodes....
;the set of all nodes in the graph is found by traversing the car of each nested
;list in the adjacency list. 
  

;  "Representation of node data.  A node is just a label (some identifier) and 
;   associated data orthogonal to topology."

(defn make-node
  "Functions to construct list nodes partially."
  ([] (make-node :unlabeled nil))
  ([l] (if (coll? l) 
         (make-node (first l) (fnext l))
         (make-node l nil)))        
  ([l data] (list l data)))

(defn- litelabel [v1 v2] (str v1 "->" v2))
(defn make-arc
  "Fully or partially define arc records."
  ([from to] (make-arc from to 0 nil (litelabel from to)))
  ([from to weight] (make-arc from to weight nil (litelabel from to)))
  ([from to weight data] (make-arc from to weight 0 data (litelabel from to)))
  ([from to weight data label] (list label from to weight data)))
  
(def empty-graph (alist (pair :sources
(defn list-graph []  empty-graph)
(defn empty-graph? [g] (= empty-graph g))

  
(defn- list-sources [g] (fnext g))

(defn- graph-sinks [g]
  (list (fnext listgraph)))

(defn add-node [g nd]
  (list (cons nd (graph-sinks g)) (cons nd (graph-sources g))))



(defn- get-sources*
  "Query the neighborhood to determine nodes acting as sources for this node."
  [g n] (->> g 
          (drop-while #(not= (node-label n) %)))
                                     
(defn- get-sinks*
  "Query the neighborhood to determine nodes as as sinks for this node."
  [g n] (get-in g [:neighbors (get-label n) :sinks]))

(defn- get-neighbors*
  "Return the neighborhood for node n" 
  [g n] (
  

(defn- add-source
  "Add source as a sourcenode for n."
  [g n source]
  (let [nd (get-label n)]
    (assoc-in g [:neighbors nd :sources]
     (conj (get-in g [:neighbors nd :sources]) (get-label source)))))

(defn- add-sink
  "Add sink as a sinknode for n"
  [g n sink]
  (let [nd (get-label n)]
    (assoc-in g [:neighbors n :sinks]
     (conj (get-in g [:neighbors n :sinks]) (get-label sink)))))
               
(defn- add-node* [g n]
  "Record-specific implementation for adding a node to g."
  (let [nd (make-node n)]
  (-> (add-neighborhood g nd)
      (assoc-in [:nodes] (assoc (:nodes g) (get-label nd) nd)))))

(defn- add-nodes* [g ns]
  (reduce add-node* g ns))

(defn- get-nodes* [g]
  "Return lazy-seq of nodes in g"
  (vals (:nodes g)))

(defn- get-node* [g n]
  (get (:nodes g) (get-label n)))

(defn- get-arcs*
  "Return lazy-seq of arcs in g"
  ([g] (vals (:arcs g))))

(defn- get-arc*
  ([g a] (-> g :arcs (get (get-label a)))))

 
(defn- has-node?*
  "Does g contain node n?" 
  ([g n] (contains? (:nodes g) (get-label n))))

(def missing-node?* (comp not has-node?*)) 

(defn- has-arc?*
  "Does g contain arc a?" 
  [g a] (contains? (:arcs g) (get-label a)))

(defn- add-neighbor
  "Register nd1 as a source of nd2, and nd2 as a destination of nd1."
  [g nd1 nd2]
  (-> g
    (add-sink nd1 nd2)
    (add-source nd2 nd1)))

(defn- drop-neighbor
  "Remove nd1 from nd2's sources, and nd2 from nd1's sinks."
  [g nd1 nd2]
  (let [n1hood (get-neighbors* g nd1)
        n2hood (get-neighbors* g nd2)
        n1sinks (disj (:sinks n1hood) nd2)
        n2sources (disj (:sources n2hood) nd1)]
    (-> g
      (assoc-in [:neighbors nd1 :sinks] n1sinks)
      (assoc-in [:neighbors nd2 :sources] n2sources))))
      
(defn- negative-arc? [a]
  (< 0 (:weight a)))

(defn- add-arc* [g a]
   "Return new graph representing addition of arc to g.  If nodes do not exist 
    for arc (implied nodes) they are given empty, default values and added.
    When we add an arc, we must relate the source node (from) and the
    destination node (to).  For now, we assume a digraph.  This forces 
    (from) to be a source for (to), and (to) to be a destination for (from)."
   (let [a (make-arc a)]
	  (if-not (contains? (:arcs g) (arc-label a))
		   (let [impliednodes
		        (map make-node 
	              (filter (partial missing-node?* g) [(:from a) (:to a)]))]
		     (-> (assoc-in g [:arcs] (assoc (:arcs g) (get-label a) a))         
		         (add-nodes* impliednodes)
		         (add-neighbor (:from a) (:to a))))
	      (throw
	        (Exception. 
	          (format "Arc %s already exists in graph!" (get-label a)))))))

(defn drop-arc*
  "Return the result of dropping arc a from g." 
  [g a]
  (if-let [arc (get-arc* g a)]
    (let [[from to] [(:from arc) (:to arc)]]
       
      (-> g
        (drop-neighbor from to) 
        (assoc-in [:arcs] (dissoc (:arcs g) (get-label arc)))))
    
    (throw 
      (Exception. (format "Arc %s does not exist in graph!" (get-label a))))))


(defn incident-arcs*
  "Return all arcs that contain n"
  [g n]
  (let [nseq (repeat n)
        outbound (partition 2 (interleave nseq (get-sinks* g n)))
        inbound (partition 2 (interleave (get-sources* g n) nseq))]
    (concat outbound inbound)))

(defn drop-node*
  "Return the graph the results from dropping node n.  Affected arcs and 
   neighborhood will be removed."
  [g n]
  (let [arcs (map (partial apply make-arc) (incident-arcs* g n))
        sources (get-sources* g n)
        sinks (get-sinks* g n)
        g-minus-arcs (reduce drop-arc* g arcs)]
    (merge g-minus-arcs 
           {:nodes (dissoc (:nodes g-minus-arcs) (get-label n))
            :neighbors (dissoc (:neighbors g-minus-arcs) n)}))) 

(extend-type clojure.lang.PersistentList
  IGraphNode
  (node-label [[nd & more]] nd)
  (node-data [[_ data & more]] data)
  IGraphArc 
  (arc-label [[label & more]] label)
  (arc-weight [[_ _ _ weight & more]] weight)
  (arc-from [[_ from & more]] from)
  (arc-to [[_ _ to & more]]  to)
  (arc-nodes [[_ _ from to & more]] [from to])
  (arc-data [[l f t w d & more]] d)
  IGraph
  (add-node [alist n] )
  (drop-node [g n] )
  (has-node?  [g n] )
  (get-node [g n] )
  (get-nodes [g] )
  (add-arc [g a] )
  (drop-arc [g a] )
  (has-arc? [g a] )
  (get-arc [g a] )
  (get-arcs [g] )
  (get-neighbors [g n] )
  (get-sources [g n] )
  (get-sinks [g n] ))