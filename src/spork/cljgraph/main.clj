(ns spork.cljgraph.main
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

(def graphdir  (System/getProperty "user.dir"))
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

;;Added some stuff for testing.  Wipe this file out.

(defn find-age  [x] (:age  x))
(defn find-name [x] (:name x))

(let [const 100]
  (def ^:dynamic *specs* {:get-name (fn [p] (:name p))
                          :get-age  (fn [p] (:age p))
                          :alter-name (fn [n] (clojure.string/capitalize n ))
                          :alter-age  (fn [a] (+ a const))}))

(defmacro general-person-printer [p & {:keys [get-name get-age alter-name alter-age]}]
  `(println {:name (~alter-name (~get-name  ~p))
             :age  (~alter-age  (~get-age   ~p))}))          

(defn build-printer 
  [{:keys [get-name get-age alter-name alter-age] :as specs}]
  `(let [gpp# (fn [p#] (general-person-printer p# :get-name ~get-name :get-age ~get-age :alter-name ~alter-name :alter-age ~alter-age))]
     (fn ~(gensym "Printer") [person#] 
       (gpp# person#))))

(defmacro with-all-caps [& expr]
  `(binding [~'*specs* (assoc ~'*specs* :alter-name (fn [n#] (clojure.string/upper-case n#)))]
     ~@expr))

(defmacro build-printer 
  [ specs] 
  `(let [specs# ~specs
         get-name# (:get-name specs#)
         get-age#  (:get-age specs#)
         alter-name# (:alter-name specs#)
         alter-age#  (:alter-age specs#)
         gpp# (fn [p#] (general-person-printer p# :get-name get-name# :get-age get-age# :alter-name alter-name# :alter-age alter-age#))]
     (fn ~(gensym "Printer") [person#] 
       (gpp# person#))))

