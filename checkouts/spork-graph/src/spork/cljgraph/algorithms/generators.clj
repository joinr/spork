;Random graph generating functions.  
(ns spork.cljgraph.algorithms.generators
  (:require [cljgraph [graph :as gr]]))

(defn- rand-key
  "Auxillary.  Grab a random key from a set, map, or flat collection."  
  [coll] 
  (let [ks (if (map? coll) (keys coll) coll)]                    
    (first (shuffle ks))))

(defn- next-key
  "Auxillary.  Grab a random key from a set, map, or flat collection."  
  [coll]
  (let [ks (if (map? coll) (keys coll) coll)]
    (first ks)))

;arc enumeration  
(defn node->arcstream [rootnode sourcenodes]
  "Enumerate the unique directed arc space from rootnode to all sourcenodes,
   where root and source are not equal, in a random fashion."
  (letfn [(arcgen [r dests]
             (if (seq dests)
               (let [selected (rand-key dests)
                     remaining (disj dests selected)]
                   (lazy-seq  
                     (cons (list rootnode selected) 
                           (arcgen rootnode remaining))))))]
         (arcgen rootnode (disj sourcenodes rootnode))))
                                          
(defn nodes->arcstream 
  "Enumerate the unique directed arc space from rootnodes to all possible 
   sourcenodes, where root and source are not equal, in a random fashion."
  ([nodes keygen]
  (let [sources (set (distinct nodes))
        arcstreams (zipmap sources (map #(node->arcstream % sources) sources))]
    (letfn [(draw [streams] 
               (if (seq streams)
	               (let [source (keygen streams)
	                     strm (get streams source )
	                     arc (first strm)
	                     nextstreams
	                     (if (fnext strm) 
	                       (assoc streams source (rest strm))
	                       (dissoc streams source))]
	                 (lazy-seq (cons arc (draw nextstreams))))))]          
        (draw arcstreams))))
  ([nodes] (nodes->arcstream nodes rand-key)))

;Note -> we use constructors here....but make-arc, make-graph, etc.
(defn rand-arcs   
  "Generate a random stream of directed arcs from a collection of nodelabels.
   Duplicates will be pruned from nodelabels."
  ([nodelabels] (rand-arcs nodelabels 0))
  ([nodelabels weight] (rand-arcs nodelabels weight rand-key))
  ([nodelabels weight keygen] 
    (->> (nodes->arcstream nodelabels keygen)
      (map (fn [[n1 n2]] [n1 n2 (rand weight)])))))

;random graph generation
(defn rand-digraph
  "Given an empty IGraph, Generate a random directed graph from a nodecount, 
   an arccount and a bound on arcweights."
  [nodecount arccount weight]
  (let [nodes (range nodecount)
        arcs (take arccount (rand-arcs nodes weight))]
    (-> (gr/make-graph) 
        (gr/add-nodes nodes) 
        (gr/add-arcs arcs))))

(defn randomly-connect [nodelabels arccount & weight]
  (let [w (if (first weight) (first weight) 0)]
    (-> (gr/make-graph) 
        (gr/add-arcs (take arccount (rand-arcs nodelabels w))))))

(defn connected-digraph [nodecount weight]
  (let [nodes (range nodecount)
        arcs (rand-arcs nodes weight next-key)]
    (-> (gr/make-graph) 
        (gr/add-nodes nodes) 
        (gr/add-arcs arcs))))

(defn rand-ugraph [nodecount arccount weight] "not implemented")

(defn rand-dag [nodecount arccount weight] "not implemented")

(defn rand-linear [arccount weight]
  (-> (gr/make-graph)
    (gr/add-arcs (for [x (range arccount)] [x (inc x) (rand weight)]))))

(defn rand-tree [nodecount]
  (let [nodes (vec (range nodecount));'(vec (shuffle (range nodecount)))        
        median #(int (/ (+ %1 %2) 2 ))
        l (fn [n] (get nodes (* 2 n)))
        r (fn [n] (get nodes (inc (* 2 n))))
        bisect (fn bisect [x0 x2]
                 (cond (= x2 x0) [(get nodes x0)]
                       (or (< x2 x0) (> x0 x2)) nil
                       :else
			                   (lazy-seq 
			                     (let [x1 (median x0 x2)]
			                       (concat [(get nodes x1)] 
			                               (bisect x0 (dec x1)) 
			                               (bisect (inc x1) x2))))))
        tree-arcs (bisect 0 (dec (count nodes)))
        arcs       (map vec (take nodecount
                           (partition 3 (flatten 
                                   (map-indexed 
                                    (fn [i nd] [nd (l i) 1
                                            nd (r i) 1]) tree-arcs)))))]
    (gr/add-arcs (gr/make-graph) arcs)))
        
                   
                   
                   
                   

;;Testing.
;(defn make-dag []
;  (-> (make-graph) 
;      (add-arcs (map #(apply make-arc %) 
;                 [[1 2 2] [2 3 1.9] [3 5 0]
;                  [0 1 1] [2 4 2]]))))