(ns spork.cljgraph.graph
  "A protocol and a default implementation for a Graph data type
   first we need a graph data type... 
   ANY graph is a set of edges and vertices...
   G = {{E},{V}}
   Applying a constructor on a set of edges and vertices will return
   a structure that is the representation of both edges and v's.

   Note, we want to store some notion of edge weight within the edges...

   anything that is graphable should have a few functions defined for it...
   It should have a label (or a key) that uniquely identifies it.  
   A vertex/node could be A for instance.  An edge from A to B of weight 2.0 
   could be [A -> B, 2.0]  or [A B 2.0], or {:from A :to B :weight 2.0}.")
  

(defn u-arcbound [nodecount] 
  (/ (* nodecount (dec nodecount)) 2))
(defn d-arcbound [nodecount]
  (* nodecount (dec nodecount)))
  
(defn- flip 
  "Auxillary function.  Returns function that accepts args in reverse order." 
  [f & args] (apply f (reverse args)))

;nodes can contain data
;nodes do NOT contain information about other nodes....they are simply
;labeled pieces of data.  Now....said data "could" include whatever we want,
;including neighbor relations, etc. We don't do that for now....
(defmulti get-label (fn [a] (type a)))
(defmethod get-label java.lang.String [a] a)
(defmethod get-label java.lang.Integer [a] a)
(defmethod get-label java.lang.Long [a] a)
(defmethod get-label clojure.lang.Keyword [a] a)


;Attributes are stored in meta data.
(defrecord graphstats [attributes counts])
(def empty-stats (graphstats. {} {})) 

(defn get-stats [g]
  (:graphstats (meta g)))

(defn alter-stats [g alterf] 
  (with-meta g 
   (assoc (meta g) :graphstats (alterf (get-stats g)))))

(defn add-attrs [g attrs]
  (alter-stats g 
     (fn [stats] 
       (let [atts (:attributes stats)]
         (assoc stats :attributes (reduce conj atts attrs))))))

(defn add-attr [g attr] (add-attrs g [attr]))

;(defn add-count 
;  ([g cnt value]
;	  (alter-stats g 
;	    (fn [stats] 
;	      (let [counts (:counts stats)]
;	         (assoc-in stats [:counts cnt value])))))
;  ([g cnt] (add-count g cnt 0)))
;
;(defn add-counts [g cntrs]
;  (reduce add-count g cntrs))
;
;(defn inc-count 
;  ([g cnt amount] (add-count g cnt amount))
;  ([g cnt] (inc-count g cnt 1)))
;
;(defn dec-count [g cntr]
;  (inc-count g cntr -1)) 
  
(defn drop-attr [g attr]
  (alter-stats g 
     (fn [stats] (merge stats {:attributes (disj (:attributes stats) attr)}))))
    
(defn drop-attrs [g attrs] 
  (reduce drop-attr g attrs))

(defn get-attrs [g] (:attributes (get-stats g)))
;(defn get-count [g cnt] (get-in (get-stats g) [:counts cnt]))

(defn has-attr?
  "Fetch an attribute from the graph.  Attributes describe properities of the 
   graph (i.e. directed, undirected, DAG, etc.) that inform algorithms."
  [g attr] (contains? (get-attrs g) attr))

(def digraph? (partial flip has-attr? ::digraph))
(def dag? (partial flip has-attr? ::dag))
(def ugraph? (partial flip has-attr? ::ugraph))

;(defn negative-arcs? [g] (has-attr g ::negative-arcs))

;(defn- inc-negativity
;  "Increment the graph's negative arc count and associate negativity.
;   Aux for add-arc"
;  ([g amt]
;	  (let [c (+ amt 
;	             (if-not (has-attr? g ::negative-arcs) 0
;	                     (get-count g ::negative-count)))]
;	    (-> (add-attr g ::negative-arcs)
;	        (inc-count ::negative-count c))))
;  ([g] (inc-negativity g 1)))
;
;(defn- dec-negativity
;  "Decrement the graph's negative arc count and possible dissoc negativity.  
;   Aux to drop-arc."
;  ([g amt]
;	  (let [count (if-not (has-attr? g ::negative-arcs) 
;	                0
;	                (::negativecount (meta g)))]
;	    (-> (add-attr g ::negative-arcs)
;	      (add-counter ::negative-count)
;	      (inc-counter ::negative-count))))
;  ([g] (dec-negativity 1)))

;(defn- count-negatives [g] 
;  (if-not (has-attr? g ::negative-arcs) 0 
;          (::negative-count (get-counters g ))))

;record-based graph implementation
(defrecord graphdata [nodes arcs neighbors])           
(def empty-graph (graphdata. {} {} {}))

(defn make-graph
  ([] (add-attrs empty-graph [::digraph]))
  ([nodes arcs neighbors] (make-graph nodes arcs neighbors [::digraph]))
  ([nodes arcs neighbors attrs] 
    (add-attrs (make-graph nodes arcs neighbors) attrs)))

;  "Representation of node data.  A node is just a label (some identifier) and 
;   associated data orthogonal to topology."
(defrecord node [label data]
  Object 
  (toString [this] (str label)))
;  IGraphItem 
;  (getlabel [n] label)
;  (getdata [n] data))

(defmethod get-label node [a] (:label a))

(defn make-node
  "Functions to construct node records partially."
  ([] (make-node :unlabeled nil))
  ([l] (if (not= (type l) node) (make-node l nil) l))
  ([l data] (node. l data)))


(defrecord neighborhood [sources sinks])
(defn get-neighborhood [g] (:neighbors g))
(defn get-neighbors
  "Return the neighborhood{:sources x :sinks y} for node n" 
  [g n] (get-in g [:neighbors (get-label n)]))
  
(defn get-sources
  "Query the neighborhood to determine nodes acting as sources for this node."
  [g n] (get-in g [:neighbors (get-label n) :sources]))
                                     
(defn get-sinks
  "Query the neighborhood to determine nodes as as sinks for this node."
  [g n] (get-in g [:neighbors (get-label n) :sinks]))

(defn add-neighborhood 
  "Register sources and sinks as the neighbors of node in graph g.
   If sources and sinks are not provided, register empty sets."
  ([g n sources sinks] 
    (assoc-in g [:neighbors (get-label n)] (neighborhood. sources sinks)))
  ([g n] (let [sources (get-sources g n)
               sinks (get-sinks g n)]
           (add-neighborhood g n (or sources #{}) (or sinks #{}))))) 

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
               
(defn add-node [g n]
  "Record-specific implementation for adding a node to g."
  (let [nd (make-node n)]
  (-> (add-neighborhood g nd)
      (assoc-in [:nodes] (assoc (:nodes g) (get-label nd) nd)))))

(defn add-nodes
  "Record-specific implementation for adding multiple nodes to g."
  [g nodes]
  (reduce add-node g nodes))

(defn get-nodes [g]
  "Return lazy-seq of nodes in g"
  (vals (:nodes g)))

(defn get-node [g n]
  (get (:nodes g) (get-label n)))

;(defn node-map [g] (:nodes g))

(defn get-arcs
  "Return lazy-seq of arcs in g"
  ([g] (vals (:arcs g))))

(defn get-arc
  ([g a] (-> g :arcs (get (get-label a)))))

;"Arc record.  Models relation between two nodes, from and to, 
; including a possible notion of distance or weight, and non-nodal data
; associated with the arc."
(defrecord arc [from to weight data]
  Object 
  (toString [a] (if-let [label (get (:data a) :label)]
                     label
                     [(:from a) (:to a)])))
(defmethod get-label arc [a] (str a)) 

(defn- litelabel [v1 v2] (str v1 "->" v2))
(defn label-arc [a label] (assoc-in a [:data :label] label))

(defn make-arc
  "Fully or partially define arc records."
  ([from to] (make-arc from to 0 nil (litelabel from to)))
  ([from to weight] (make-arc from to weight nil (litelabel from to)))
  ([from to weight data label] (label-arc (arc. from to weight data) label)))
 
(defn has-node?
  "Does g contain node n?" 
  ([g n] (contains? (:nodes g) (get-label n))))

(def missing-node? (comp not has-node?))

(defn has-arc?
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
  (let [n1hood (get-neighbors g nd1)
        n2hood (get-neighbors g nd2)
        n1sinks (disj (:sinks n1hood) nd2)
        n2sources (disj (:sources n2hood) nd1)]
    (-> g
      (assoc-in [:neighbors nd1 :sinks] n1sinks)
      (assoc-in [:neighbors nd2 :sources] n2sources))))
      
(defn- negative-arc? [a]
  (< 0 (:weight a)))

(defn add-arc [g a]
   "Return new graph representing addition of arc to g.  If nodes do not exist 
    for arc (implied nodes) they are given empty, default values and added.
    When we add an arc, we must relate the source node (from) and the
    destination node (to).  For now, we assume a digraph.  This forces 
    (from) to be a source for (to), and (to) to be a destination for (from)."  
  (if-not (contains? (:arcs g) (get-label a))
	   (let [impliednodes
	        (map make-node 
              (filter (partial missing-node? g) [(:from a) (:to a)]))]
	     (-> (assoc-in g [:arcs] (assoc (:arcs g) (get-label a) a))         
	         (add-nodes impliednodes)
	         (add-neighbor (:from a) (:to a))))
      (throw
        (Exception. 
          (format "Arc %s already exists in graph!" (get-label arc))))))

(defn add-arcs 
  "Return new graph representing addition of arcs to g"
  [g arcs] (reduce add-arc g arcs))

;Protocol-derived functionality.  We define higher order graph operations on 
;top of protocol functions.  This allows us to change the implementation 
;to get performance benefits.

;(defn neighbors
;  "Return a sequence of all nodes related to the target.  In a directed graph, 
;   only sinks are neighbors."  
;  [g n] (conj (get-sources g n) (get-sinks g n)))

(defn terminal-node?
  "Is node n devoid of outbound arcs?"
  [g n] (not (seq (get-sinks g n))))

(defn source-node?
  "Is node n devoid of inbound arcs?"
  [g n] (not (seq (get-sources g n))))

(defn island?
  "Does node n have any neighbors?"
  [g n] (and (terminal-node? g n) (source-node? g n)))

(defn incident-arcs
  "Return all arcs that contain n"
  [g n]
  (let [nseq (repeat n)
        outbound (partition 2 (interleave nseq (get-sinks g n)))
        inbound (partition 2 (interleave (get-sources g n) nseq))]
    (concat outbound inbound)))

(defn drop-arc
  "Return the result of dropping arc a from g." 
  [g a]
  (if-let [arc (get-arc g a)]
    (let [[from to] [(:from arc) (:to arc)]]
       
      (-> g
        (drop-neighbor from to) 
        (assoc-in [:arcs] (dissoc (:arcs g) (get-label arc)))))
    
    (throw 
      (Exception. (format "Arc %s does not exist in graph!" (get-label arc))))))

(defn drop-node
  "Return the graph the results from dropping node n.  Affected arcs and 
   neighborhood will be removed."
  [g n]
  (let [arcs (map (partial apply make-arc) (incident-arcs g n))
        sources (get-sources g n)
        sinks (get-sinks g n)
        g-minus-arcs (reduce drop-arc g arcs)]
    (merge g-minus-arcs 
           {:nodes (dissoc (:nodes g-minus-arcs) (get-label n))
            :neighbors (dissoc (:neighbors g-minus-arcs) n)})))  

(defn drop-nodes [g coll] (reduce drop-node g coll))
        
(defn- get-labels [coll] (map get-label coll))
;uses arclabel
(defn get-arclabels 
  "Return all arc labels in g" 
  [g] (get-labels (get-arcs g)))

;uses nodelabel 
(defn get-nodelabels
  "Return all arc labels in g"
  [g] (get-labels (get-nodes g))) 


;aux functionality
(load "io")
(load "topsort")