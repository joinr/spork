;(ns spork.cljgraph.io
;(:use [cljgraph.xmlutils]
;        [cljgraph.graph]
;        [clojure.contrib.json])

(use 'cljgraph.xmlutils 'clojure.contrib.json)
;(require 'cljgraph.xmlutils 'clojure.contrib.json)
(require  'clojure.string )
     
(def xml-head "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
(def gml-head
    "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" 
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
    xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns
     http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">")
(defn- graphml [content]
  (make-xelm "graphml" 
             {:xmlns "http://graphml.graphdrawing.org/xmlns"  
              :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
              :xmlns:y "http://www.yworks.com/xml/graphml" 
              :yed "http://www.yworks.com/xml/yed/3"
              :xsi:schemaLocation 
              (str "http://graphml.graphdrawing.org/xmlns " 
                   "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd "
                   "http://graphml.graphdrawing.org/xmlns " 
                   "http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd")}
             content))
(defn- ylabel [label]
  (str
            "<data key=\"d6\">\n"
            "\t<y:ShapeNode>\n" 
            "\t\t<y:Geometry x=\"170.5\" y=\"-15.0\" width=\"59.0\" "
                 "height=\"30.0\"/>\n" 
            "\t\t<y:Fill color=\"#CCCCFF\" transparent=\"false\"/>\n" 
            "\t\t<y:BorderStyle type=\"line\" width=\"1.0\" "
                               "color=\"#000000\"/>\n" 
            "\t\t<y:NodeLabel>" label "</y:NodeLabel>\n" 
            "\t\t<y:Shape type=\"rectangle\"/>\n" 
            "\t</y:ShapeNode>\n"
            "</data>\n"))

(defn- ynode [n]
  (let [lbl (node-label n)]
	  (make-xelm "node" {:id lbl}
      (make-xelm "data" {:key (str "d" lbl)}
       (make-xelm "y:ShapeNode" nil 
        (make-xelm "y:NodeLabel" nil lbl))))))

(defmulti to-graphml (fn [x] (graph-component? x)))

(defmethod to-graphml :node [n] (ynode n))
(defmethod to-graphml :arc [a]
  (make-xelm "edge" {:source (:from a) :target (:to a)}))

(defmethod to-graphml :graph [g]
  (make-xelm "graph" {:id "G" :edgedefault "undirected"}
   (let [nodes (get-nodes g)
         labels (map node-label nodes)]
	   (concat
	        (map #(make-xelm "key" {:for % :id (str "d" %) 
	                                :yfiles.type "nodegraphics"})
	             labels)
	        (map to-graphml (get-nodes g))
	        (map to-graphml (get-arcs g))))))

(defn- graph->xml [g] (graphml (to-graphml g)))

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

(def localpath 
  (let [home (str (System/getProperty "user.home"))]
    (fn [pth] (str home "\\" pth))))

(defn- graph->file [g pth]  
  (clojure.java.io/copy (with-out-str (xemit (graph->xml g))) (check-file pth)))

(defn- graphext [g]
  (if (satisfies? IGraph g) :IGraph
    (class g)))

(defmulti get-extension graphext)

(defmethod get-extension :IGraph [g] :IGraph)
(defmethod get-extension java.lang.String [x] 
  (keyword (.toUpperCase (last (clojure.string/split x #"\." )))))

(defmethod get-extension java.io.File [x] 
  (get-extension (.getName x)))


(defmulti read-graph get-extension)
(defmethod read-graph :IGraph [g] g)
(defmethod read-graph :CLJ [pth]
  (read-string (slurp pth)))
 
(defmulti write-graph
  "A function for writing graphs to resource paths.  The type of 
   writer is derived from the path given, assuming that the path 
   is valid."
  (fn [g pth] (get-extension pth)))

(defmethod write-graph :default [g pth]
  (spit  (check-file pth) (pr-str g) ))

(defmethod write-graph :IGraph [g pth] nil) ;should never happen.
(defmethod write-graph :CLJ [g pth]
  (spit  (check-file pth) (pr-str g) ))
(defmethod write-graph :XML [g pth]
  (graph->file g pth))
(defmethod write-graph :GRAPHML [g pth]
  (graph->file g pth))


;graph [
;        comment "This is a sample graph"
;        directed 1
;        id 42
;        label "Hello, I am a graph"
;        node [
;                id 1
;                label "Node 1"
;                thisIsASampleAttribute 42
;        ]
;        node [
;                id 2
;                label "node 2"
;                thisIsASampleAttribute 43
;        ]
;        node [
;                id 3
;                label "node 3"
;                thisIsASampleAttribute 44
;        ]
;        edge [
;                source 1
;                target 2
;                label "Edge from node 1 to node 2"
;        ]
;        edge [
;                source 2
;                target 3
;                label "Edge from node 2 to node 3"
;        ]
;        edge [
;                source 3
;                target 1
;                label "Edge from node 3 to node 1"
;        ]
;]


