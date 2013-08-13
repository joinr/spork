(ns spork.cljgraph.data.mapgraph
  "An implementation of a generic IGraph using records (maps)."
  (:use [cljgraph.graph :only [get-label init-graph node-label arc-label 
                               litelabel]])
  (:import [cljgraph.graph IGraph IGraphNode IGraphArc]))
   
;record-based graph implementation
;stubs  for the protocol....might use them.
;(declare add-node* has-node?* get-node* get-nodes* add-arc* has-arc?* get-arc*
;         get-arcs* get-neighbors* get-sources* get-sinks*)

(defrecord node [label data]
  IGraphNode
  (node-label [nd] label)
  (node-data [nd] data)
  Object 
  (toString [this] (str label)))

(defmethod get-label node [a] (:label a))

(defn make-node
  "Functions to construct node records partially."
  ([] (node. :unlabeled nil))
  ([nd] (cond 
         (= (type nd) node) nd
         (not (coll? nd)) (make-node nd nil)
         :else (make-node (first nd) (fnext nd))))
  ([l data & more] (node. l data)))

;"Arc record.  Models relation between two nodes, from and to, 
; including a possible notion of distance or weight, and non-nodal data
; associated with the arc."
(defrecord arc [from to weight data]  
  Object 
  (toString [a] (if-let [label (get (:data a) :label)]
                     label
                     [(:from a) (:to a)]))
  IGraphArc 
  (arc-label [a] (if-let [label (get (:data a) :label)]
                     label
                     [(:from a) (:to a)]))
  (arc-weight [a] weight)
  (arc-from [a] from)
  (arc-to [a] to)
  (arc-nodes [a] [from to])
  (arc-data [a] data))

(defmethod get-label arc [a] (str a)) 

(defn label-arc [a label] (assoc-in a [:data :label] label))

(defn make-arc
  "Fully or partially define arc records."
  ([a] (cond (= (type a) arc) a
             (coll? a) (apply make-arc a)
             :else (throw (Exception. "not a valid arc!"))))
             
  ([from to] (make-arc from to 0 nil (litelabel from to)))
  ([from to weight] (make-arc from to weight nil (litelabel from to)))
  ([from to weight data] (make-arc from to weight data (litelabel from to)))
  ([from to weight data label & more] 
    (label-arc (arc. from to weight data) label)))
  

(defrecord neighborhood [sources sinks])

(defn- get-neighborhood [g] (:neighbors g))
(defn- get-neighbors*
  "Return the neighborhood{:sources x :sinks y} for node n" 
  [g n] (get-in g [:neighbors (node-label n)]))
  
(defn- get-sources*
  "Query the neighborhood to determine nodes acting as sources for this node."
  [g n] (get-in g [:neighbors (node-label n) :sources]))
                                     
(defn- get-sinks*
  "Query the neighborhood to determine nodes as as sinks for this node."
  [g n] (get-in g [:neighbors (node-label n) :sinks]))

(defn- add-neighborhood 
  "Register sources and sinks as the neighbors of node in graph g.
   If sources and sinks are not provided, register empty sets."
  ([g n sources sinks] 
    (assoc-in g [:neighbors (node-label n)] (neighborhood. sources sinks)))
  ([g n] (let [sources (get-sources* g n)
               sinks (get-sinks* g n)]
           (add-neighborhood g n (or sources #{}) (or sinks #{}))))) 

(defn- add-source
  "Add source as a sourcenode for n."
  [g n source]
  (let [nd (node-label n)]
    (assoc-in g [:neighbors nd :sources]
     (conj (get-in g [:neighbors nd :sources]) (node-label source)))))

(defn- add-sink
  "Add sink as a sinknode for n"
  [g n sink]
  (let [nd (node-label n)]
    (assoc-in g [:neighbors n :sinks]
     (conj (get-in g [:neighbors n :sinks]) (node-label sink)))))
               
(defn- add-node* [g n]
  "Record-specific implementation for adding a node to g."
  (let [nd (make-node n)]
  (-> (add-neighborhood g nd)
      (assoc-in [:nodes] (assoc (:nodes g) (node-label nd) nd)))))

(defn- add-nodes* [g ns]
  (reduce add-node* g ns))

(defn- get-nodes* [g]
  "Return lazy-seq of nodes in g"
  (vals (:nodes g)))

(defn- get-node* [g n]
  (get (:nodes g) (node-label n)))

(defn- get-arcs*
  "Return lazy-seq of arcs in g"
  ([g] (vals (:arcs g))))

(defn- get-arc*
  ([g a] (-> g :arcs (get (arc-label a)))))

 
(defn- has-node?*
  "Does g contain node n?" 
  ([g n] (contains? (:nodes g) (node-label n))))

(def missing-node?* (comp not has-node?*)) 

(defn- has-arc?*
  "Does g contain arc a?" 
  [g a] (contains? (:arcs g) (arc-label a)))

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
		     (-> (assoc-in g [:arcs] (assoc (:arcs g) (arc-label a) a))         
		         (add-nodes* impliednodes)
		         (add-neighbor (:from a) (:to a))))
	      (throw
	        (Exception. 
	          (format "Arc %s already exists in graph!" (arc-label a)))))))

(defn drop-arc*
  "Return the result of dropping arc a from g." 
  [g a]
  (if-let [arc (get-arc* g a)]
    (let [[from to] [(:from arc) (:to arc)]]
       
      (-> g
        (drop-neighbor from to) 
        (assoc-in [:arcs] (dissoc (:arcs g) (arc-label arc)))))
    
    (throw 
      (Exception. (format "Arc %s does not exist in graph!" (arc-label a))))))


(defn incident-arcs*
  "Return all arcs that contain n"
  [g n]
  (let [nseq (repeat n)
        outbound (partition 2 (interleave nseq (get-sinks* g n)))
        inbound (partition 2 (interleave (get-sources* g n) nseq))]
    (map (partial get (:arcs g)) 
         (map #(apply litelabel %) (concat outbound inbound)))))

(defn drop-node*
  "Return the graph the results from dropping node n.  Affected arcs and 
   neighborhood will be removed."
  [g n]
  (let [arcs (map (partial apply make-arc) (incident-arcs* g n))
        sources (get-sources* g n)
        sinks (get-sinks* g n)
        g-minus-arcs (reduce drop-arc* g arcs)]
    (merge g-minus-arcs 
           {:nodes (dissoc (:nodes g-minus-arcs) (node-label n))
            :neighbors (dissoc (:neighbors g-minus-arcs) n)}))) 

(defrecord graphdata [nodes arcs neighbors]
  IGraph
  (add-node [g n] (add-node* g n))
  (drop-node [g n] (drop-node* g n))
  (has-node?  [g n] (has-node?* g n))
  (get-node [g n] (get-node* g n))
  (get-nodes [g] (get-nodes* g))
  (add-arc [g a] (add-arc* g a))
  (add-arc [g from to weight] (add-arc* g [from to weight]))
  (drop-arc [g a] (drop-arc* g a))
  (has-arc? [g a] (has-arc?* g a))
  (get-arc [g a] (get-arc* g a))
  (get-arc [g from to] (get-arc* g (litelabel from to)))
  (get-arcs [g] (get-arcs* g))
  (get-neighbors [g n] (get-neighbors* g n))
  (get-sources [g n] (get-sources* g n))
  (get-sinks [g n] (get-sinks* g n))
  (incident-arcs [g n] (incident-arcs* g n)))

(def empty-graph (graphdata. {} {} {}))
(defn make-mapgraph [] (init-graph empty-graph))