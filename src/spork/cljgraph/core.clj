(ns spork.cljgraph.core
  (:use [cljgraph.graph])
  (:require [cljgraph.algorithms [generators :as gen]]
            [cljgraph [jungapi :as jung]]
            [cljgraph.data [search :as search]]))

;(println "cljgraph core loaded.  type (help) for help")

(def preamble 
"Welcome to cljgraph.  cljgraph is a graph library for Clojure based on some 
 popular algorithms.  It is designed to be useful, extensible, and [hopefully]
 effecient.  cljgraph is very much in development....feel free to criticize!")
(def menu 
"cljgraph is organized along some simple principles.  This is the core module, 
 or the top-level for the library.  The core provides a simple abstraction for 
 clients who want to load up a generic set of useful graph libraries in a 
 central location.")

;sample graph.
(def tx 
  (-> (make-graph)
	    (add-arcs [["San Antonio" "Austin" 57] ["Houston" "San Antonio" 127]
	               ["Austin" "Houston" 95]])))

(def graphdir (System/getProperty "user.dir"))
(def anongraph (str graphdir "\\anonymousgraph.clj"))

;(defn help []
;  (println preamble)
;  (println menu)
;  (println (str "graph contains: \n"(with-out-str (dir 'cljgraph.graph)))))

(defn random-graph [& {:keys [directed? cyclic? nodecount arccount maxweight]
                       :or {directed? true
                            cyclic? true
                            nodecount 20
                            arccount 15
                            maxweight 100} :as opts}]
  ((case [directed? cyclic?]
                    [true true] gen/rand-digraph
                    [false true] gen/rand-ugraph
                    [false false] gen/rand-dag
                    gen/rand-digraph) nodecount arccount maxweight))

(defn load-graph 
  ([] (read-graph anongraph))
  ([path] (read-graph path)))

(defn view-graph
  "Quickly visualize graph g using JUNG.  Will spawn on another thread."
  ([] (view-graph (load-graph anongraph)))
  ([g] (jung/get-view g))
  ([g layout] (jung/get-view g (jung/find-layout layout))))  

(defn reload [] 
  (require '(cljgraph.algorithms [generators :as gen])
           '(cljgraph [jungapi :as jung]) 
           '(cljgraph.data [search :as search]) :reload))

(defn save-graph 
  ([g] (write-graph g anongraph))
  ([g path] (write-graph g path)))

(comment 
  
(defn graph-walk-seq
  "Generic fn to walk a graph.  The type of walk can vary by changing the 
   fringe of the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a sequence of 
   searchstates of the walk, which contains the shortest path trees, distances, 
   etc. for multiple kinds of walks, depending on the searchstate's fringe 
   structure."
  [g startnode targetnode state & {:keys [halt? weightf neighborf] 
                                   :or {halt? default-halt?
                                        weightf get-weight
                                        neighborf default-neighborf} }]
    (let [walker 
          (fn walker [g targetnode {:keys [fringe] :as searchstate}]
					  (if-let [candidate (generic/next-fringe fringe)]
					    (let [w (:weight candidate) ;perceived weight from start
					          nd (:node candidate)]
						    (if-not (halt? searchstate targetnode nd w)
                  (lazy-seq
                    (let [sinkweights (for [sink (neighborf g nd searchstate)] 
                                        [sink (weightf g nd sink)])
                          relaxation (fn [state [sink w]] 
                                       (relax* nd sink w state))
                          nextstate (reduce relaxation 
                                            (generic/pop-fringe searchstate) 
                                            sinkweights)]
                      (concat (list searchstate) 
                              (walker g targetnode nextstate))))
					       (list searchstate)))
              (list searchstate)))]
        (walker g targetnode (generic/conj-fringe state startnode 0))))
)