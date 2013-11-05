;;##Currently pointing at old cljgraph stuff, also couls be using spork.cljgui.
;;Rewrite this stuff.  
(ns spork.cljgraph.jungapi
  "A simple API to wrap a few useful Java Universal Network Graph classes 
   inside of a functional graph interface."
  (:use [spork.cljgraph.core])
  (:import [javax.swing JFrame JPanel]
           [java.awt BorderLayout Container]
           [edu.uci.ics.jung.graph Graph SparseMultigraph]
           [edu.uci.ics.jung.graph.util Pair EdgeType]
           [edu.uci.ics.jung.algorithms.layout Layout StaticLayout
            FRLayout KKLayout ISOMLayout CircleLayout SpringLayout DAGLayout
            RadialTreeLayout TreeLayout BalloonLayout]
           [edu.uci.ics.jung.visualization VisualizationViewer RenderContext]
           [org.apache.commons.collections15 Transformer Factory]
           [edu.uci.ics.jung.io GraphMLWriter]
           [edu.uci.ics.jung.visualization.renderers Renderer]
           [edu.uci.ics.jung.visualization.control ModalGraphMouse 
                                                   DefaultModalGraphMouse]))


(def smg edu.uci.ics.jung.graph.SparseMultigraph)

;we need some enums from position and mode.
;have to use absolute paths to get at java nested classes, note the $

;This is all just to get over java's nested crap class!!!
(defmacro nestedenum [abs rel]
  `(. ~abs ~rel))

(defmacro modes [expr]
  `(nestedenum edu.uci.ics.jung.visualization.control.ModalGraphMouse$Mode 
               ~expr))

(defmacro positions [expr]
  `(nestedenum 
     edu.uci.ics.jung.visualization.renderers.Renderer$VertexLabel$Position
     ~expr))

(def editing (modes EDITING))
(def picking (modes PICKING))
(def transforming  (modes TRANSFORMING))
(def annotating (modes ANNOTATING))
(def centered (positions CNTR))
(defn make-graphmouse [] 
  (doto (DefaultModalGraphMouse.) (.setMode transforming)))

(defn jgraph [] (SparseMultigraph.)) 
(defn jpair 
  ([v1 v2] (Pair. v1 v2))
  ([vs] (let [[v1 v2] (vec (take 2 vs))]
              (jpair v1 v2))))

(def jdirected (EdgeType/DIRECTED))
(def jundirected (EdgeType/UNDIRECTED))

;;adapted to work with cljgraph.
(defn arc->jedge    [[from to w]]  [w (jpair from to)])
;;adapted to work with cljgraph.
(defn node->jvertex [[n data]] n)

(defn jadd-vertex [g vtx] 
 (doto g (.addVertex vtx)) g)

(defn jadd-edge 
  ([g label ^Pair edge ]
  (doto g (.addEdge label edge jdirected)) g)
  ([g [lbl edgepair]] (jadd-edge g lbl edgepair)))

(defn graph->jgraph [g]
  (let [newg (jgraph)]
    (doseq [node  (get-node-labels g)
            arc   (arcs-from g  node)] 
      (jadd-edge   newg  (arc->jedge arc))
      (jadd-vertex newg  node))
    newg))   

;Handle graph layouts using JUNG classes...
(defn- layoutf [jg f] (f jg))

;;default layouts from JUNG
(def spring #(SpringLayout. %))
(def fr #(FRLayout. %)) 
(def kk #(KKLayout. %)) 
(def isom #(ISOMLayout. %))
(def circle #(CircleLayout. %))
(def dag #(DAGLayout. %))
(def radialtree #(RadialTreeLayout. %))
(def tree #(TreeLayout. %))
(def balloon #(BalloonLayout.  %))


(def layouts {:spring spring
              :fr fr
              :kk kk
              :isometric isom
              :circle circle
              :dag dag
              :radialtree radialtree
              :balloon balloon})

(defn get-context [vis] (.getRenderContext vis))

;JUNG utilizes the Apache Commons Generic Collections Transformer interface
;as a generic, read-only mapping of an input value to an output value.
;  Gee....sounds an awful lot like a function! 
;  All of the slick libraries utilize transformers, so we need a way to 
;  build them on the fly, and/or to extend the Transformer interface to custom 
;  Clojurey data structures (like records...).

;Currently, I chose to use reify, with some pre-defined stuff 
;to get typical graph-related items out of a record-based graph structure. 
(defn make-trans
  "Return a function wrapped as a Transformer, as required by many 
   JUNG operations"
  [f]
  (reify Transformer
            (transform [t x] (f x))))   
(defn map-t
  "Given anything that looks like a map, return a transformer 
   that looks up a key from the supplied map."
  [m] 
  (make-trans #(get m %)))

(def string-labeller (make-trans str)) 

(defn find-layout
  "Grabs a layout from anything in the layout map.  Plan to add functionality 
   for defining new layouts in the future."
  [key]
  (get layouts key fr))

;Maintain the current or default layout to use when rendering graphs.
(def ^:dynamic *current-layout* (atom fr))
(defn- get-layout [] @*current-layout*) 
(defn change-layout [l]
    (swap! *current-layout* (fn [_] l))) 

(defn layout-graph 
  "Layout a graph using a function::jgraph -> jlayout.  
   If no function is provided, it'll use the current-layout."
  ([jg layoutfun] (layoutfun jg))
  ([jg] ((get-layout) jg)))         

(defn optional-layout [& layoutf]
  (if (first layoutf) (first layoutf)  layout-graph))

(defn empty-frame [] (JFrame.))
(defn make-visible [frm]     
  (doto frm 
    (.pack) 
    (.setVisible true)))

;Lame convenience function for rendering floating point edge labels....
(defn- roundn [x n]
  (if-not (or (float? x) 
              (decimal? x))  
              (str x)
    (format (str \% \. n \f) (float x)))) 
    

(defn get-labellers [g]
  {:node-labeller (make-trans (fn [nodelabel] (str nodelabel)))
   :edge-labeller (make-trans 
                    (fn [arclabel] (roundn arclabel 2)))})

(def deflabels  {:node-labeller string-labeller :edge-labeller string-labeller})
(defn make-view [jg layoutf {:keys [node-labeller edge-labeller]}]
  (let [vv (VisualizationViewer. (layout-graph jg layoutf))
        gm (make-graphmouse)]
    (doto (.getRenderContext vv)
      (.setVertexLabelTransformer node-labeller)
      (.setEdgeLabelTransformer edge-labeller))
    (doto (.getVertexLabelRenderer (.getRenderer vv))
      (.setPosition centered))
    (doto vv 
      (.setGraphMouse gm)
      (.addKeyListener (.getModeKeyListener gm)))       
    vv))

(defn add-view [frm view] 
 (do (.add (.getContentPane frm) view) 
         frm))
(defn add-views [frm views] (reduce add-view frm views))


(defmulti view-graph (fn [g layoutf & fs] (type g)))
(defmethod view-graph :default [g layoutf & fs]
  (let [jg (graph->jgraph g)
        labels (get-labellers g)
        visuals (map #(make-view jg % labels) (cons layoutf fs))
        frm (empty-frame)]
    (make-visible (add-views frm visuals))))



;gm.setMode(ModalGraphMouse.Mode.TRANSFORMING);
;vv.setGraphMouse(gm);


(defn get-view [g & layout]
  (let [l (if (first layout) (first layout) @*current-layout*)]
    (future (view-graph g l))))
 

(defn kill [graphframe]
  (.dispose (first graphframe)))

;define a live graph server (an agent with a simple dispatch fn) 
;all the agent does is maintain state...we send it new graphs to view. 
;it's purpose is to provide an IO interface for new graphs we generate.
(defn- switch [atm state]
  (swap! atm (fn [_] state)))

(defn graph-server 
  [& layoutf]
  (let [host (agent [])
        layout  (atom (optional-layout layoutf))
        graph (atom nil)
        trykill (fn [] (if (not= nil @host) (send-off host kill)))
        get-visualizer (fn [] (fnext @host))
        get-frame (fn [] (first @host))
        render-graph (fn [_] (view-graph @graph @layout))]
	   (fn ([msg] (case msg 
	                 ::get-graph @graph
	                 ::get-frame (get-frame)
                   ::clear (trykill)
                   ::render (if (not= nil @graph) 
                              (send-off host 
                                        (fn [_] (view-graph @graph @layout))))                   
                   ::get-host host      
                   ::get-layout @layout
	                 "Unknown, use either ::get-graph or ::get-frame"))
	      ([msg data]
         (case msg
           ::swap (do (switch graph data) 
                      (send-off host (fn [_] (view-graph @graph @layout)))) 
           ::change-layout (switch layout data)
           ::test-layout (send-off host (fn[_] view-graph @graph data)))))))

;setup a default graph server.
(def graphbot (graph-server)) 

(defn render-graph
  ([server graph] (server ::swap graph))
  ([graph] (render-graph graphbot graph)))

(defn clear-graph 
  ([server] (server ::clear))
  ([] (graphbot ::clear)))
      

(defn- check-file [pth]
  (let [f (clojure.java.io/as-file pth)]
    (if-not (.exists f) 
      (do 
        (.mkdirs (.getParentFile f))
        (.createNewFile f)
        f)
      (do
      (.createNewFile f)
      f))))

(defn save-graph [g pth]
  (let [w (GraphMLWriter.)]
    (with-open [writer (clojure.java.io/writer (check-file pth))] 
      (.save w g writer ))))

;;Work in progress....
;how can we state that an IGraph implements the Graph interface?  
;or state that 2 graphs are equivalent? 
;(defn as-jgraph [g]
;  (reify 
;    HyperGraph
;    (^boolean addEdge [g e vs] true)
;		(^boolean addEdge [g e vs ^EdgeType etype] true)
;		(addVertex [g v] true)
;		(containsEdge [g e] (has-arc? g e))
;		(containsVertex [g v] (has-node? g v))
;		(degree [g v] (+ (get-indegree g v) (get-outdegree g v)))
;		(findEdge [g v1 v2] )
;		(findEdgeSet [g v1 v2])
;		(getDefaultEdgeType [g] jdirected)
;		(getDest [g e] (arc-to e))
;		(getEdgeCount [g] (count (get-arcs g)))
;		(getEdgeCount [EdgeType])
;		(getEdges [g] (get-arcs g))
;		(getEdges [g ^EdgeType edgetype] (get-arcs g))
;		(getEdgeType [g e] jdirected)
;		(getIncidentCount [E])
;		(getIncidentEdges [V])
;		(getIncidentVertices [E])
;		(getInEdges [V])
;		(getNeighborCount [V])
;		(getNeighbors [V])
;		(getOutEdges [V])
;		(getPredecessors [V])
;		(getSource [E])
;		(getSuccessors [V])
;		(getVertexCount [])
;		(getVertices [])
;		(inDegree [V])
;		(isIncident [V, E])
;		(isNeighbor [V, V])
;		(outDegree [V])
;		(removeEdge [E])
;		(removeVertex [V])
;    Graph
;	  (^boolean addEdge [g e v1 v2] true)
;	  (^boolean addEdge [g e v1 v2 ^EdgeType etype] true)
;	  (getDest [g e] (arc-to e))
;	  (getEndpoints [g e] (jpair (arc-nodes e)))
;	  (getInEdges [g v] (get-sources g v))
;	  (getOpposite [g v e] (let [opp (arc-from e)]
;                          (if (not= opp (arc-to e))
;                            opp
;                            (arc-from e))))
;	  (getOutEdges [g v] (get-sinks g v))
;	  (getPredecessorCount [g v] (count (get-predecessors g v)))
;	  (getPredecessors [g v] (get-predecessors g v))
;	  (getSource [g e] (arc-from e))
;	  (getSuccessorCount [g v] (count (get-successors g v)))
;	  (getSuccessors [g v] (get-successors g v))
;	  (inDegree [g v] (get-indegree g v))
;	  (isDest [g v e] (= (arc-to e) (node-label v)))
;	  (isPredecessor [g v1 v2] (contains? (get-sinks g v1) v2))                                       
;	  (isSource [g v e] (= (arc-from e) (node-label v)))
;	  (isSuccessor [g v1 v2] (contains? (get-sources g v1) v2))
;	  (outDegree [g v] (get-outdegree g v))))

                   
                   
        
