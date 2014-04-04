;;implementations of adajacency lists, 
;;matrices, and maps.
(ns spork.data.adjacency
  (:require [spork.cljgraph [core :as graph]
                            [flow :as flow]
                            [jungapi :as viz]]
            [spork.protocols [core :as generic]]
            [spork.data      [searchstate :as searchstate]
                             [mutable :as m]]
            [spork.util      [array :as arr]]))

;;This is an attempt to get back to some simple, performant, data
;;structures that fill a variety of roles.

;;One structure that would be nice is to have a mutable, array-backed
;;table, or a mutable, hash-map-backed table.

;;Mutable, tabular data is vital for some graph algorithms I'm working
;;on, since we often do updates...

;;An adjacency is a structure that supports adjacency queries on a 
;;set of integers.  This is similar to Haskell's data.graph and other
;;adjacency structures ala C, where we have a simplified numerical 
;;index of nodes, or vertices, and supplemental information informing 
;;their adjacency.  We use definterface to support primitive long 
;;args, i.e. unboxed args.
(definterface IAdjacency
  (^long   getSize      [])
  (setSize              [^long n])
  (^longs  getNodes     [])
  (connect              [^long from ^long to])
  (disconnect           [^long from ^long to])
  (^longs  getSources   [^long idx])
  (^longs  getSinks     [^long idx])
  (^long   getIntWeight [^long from ^long to])
  (^double getWeight    [^long from ^long to])) 

  ;; IAdjacency
  ;; (^long   getSize      [this])
  ;; (setSize              [this ^long n])
  ;; (^longs  getNodes     [this ])
  ;; (connect              [this ^long from ^long to])
  ;; (disconnect           [this ^long from ^long to])
  ;; (^longs  getSources   [this ^long idx])
  ;; (^longs  getSinks     [this ^long idx])
  ;; (^long   getIntWeight [this ^long from ^long to])
  ;; (^double getWeight    [this ^long from ^long to]))
  

;;Note -> we're not doing any math here...

(defrecord avector [^clojure.lang.IPersistentVector sources
                    ^clojure.lang.IPersistentVector sinks]
  IAdjacency
  (^long   getSize      [this] (.count sources))
  (setSize              [this ^long n] (throw (Exception. "Operation not supported")))
  (^longs  getNodes     [this ] (throw (Exception. "Operation not supported")))
  (connect              [this ^long from ^long to] 
    (let [res (.valAt sources from )] (.assoc sources from )
  (disconnect           [this ^long from ^long to])
  (^longs  getSources   [this ^long idx])
  (^longs  getSinks     [this ^long idx])
  (^long   getIntWeight [this ^long from ^long to])
  (^double getWeight    [this ^long from ^long to]))
  

;;Another option is to return a vector...
;;That satisfies the int mapping.

;;So a decent structure could be...
;;adj-vectors 
;;[0 [1 2]
;; 1 [3] ] 

;;adj-list 
;;'(0 (1 2))
;;'(1 (1 2))

;;optionally, a weighted-adj-list 
;;'(0 ((1 w01) (2 w02)))

;;Or an adj-table 
;;[[1 nil nil nil]
;; [nil 1 nil nil]
;; [nil nil 1 nil]
;; [nil nil nil 1]]


;;Or an adj-array
;;[[1 nil nil nil]
;; [nil 1 nil nil]
;; [nil nil 1 nil]
;; [nil nil nil 1]]

;;or an edge list
;;[[from to w]
;; [from to w]
;; [from to w]]


;;Pair this with an 0(1) access structure, like a map.
;;Or a vec.

;;We just maintain two adjacencies...

;;Really, all an adjacency needs to do is implement forward-looking
;;adjacencies.
;;We can maintain backward-looking adjacencies using the same
;;structure, with inverted args as an optimization.

;;An array-backed adjacency table.  Connectedness is determined by weight.
(defrecord adjacency-array  [^objects weights]
  IAdjacency
  (^long   getSize      [this])
  (setSize              [this ^long n])
  (^longs  getNodes     [this ])
  (connect              [this ^long from ^long to])
  (disconnect           [this ^long from ^long to])
  (^longs  getSources   [this ^long idx])
  (^longs  getSinks     [this ^long idx])
  (^long   getIntWeight [this ^long from ^long to])
  (^double getWeight    [this ^long from ^long to]))

;;something we want to do for performance reasons is to cut down on
;;the hashing we're relying on for hashmaps.

;;One way to do this is to have an indexed graph representation.
;;We also want to return vectors when we compute neighbors, since
;;we can traverse them much faster than lazy sequences.

;;So, we probably want to build a library of eager vector functions 
;;that mirror the sequence lib.

;;That would comport with the array lib f

;;If we maintain an indexed node set, as we add unique nodes, the
;;nodes get indices.  The indexed set, at a minimum, caches
;;information about the graph's connectedness for fast lookup.
;;Upon insertion, we maintain a 

;;This is more dynamic.  We have numerical indices, and object costs.
;;Coded as a pair of adjacencies: 
;;[1..n] 
(defrecord adjacency-table  [sources sinks]
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (assoc tg :nodes m))
  (-conj-node [tg k v] 
    (-> tg
        (assoc :nodes   (assoc nodes k v))
        (assoc :sources (assoc sources k {}))
        (assoc :sinks   (assoc sinks   k {}))))
  (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sources k)  (-get-sinks tg k))
          new-sinks   (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sinks k)  (-get-sources tg k))]
      (-> tg 
          (assoc :nodes   (dissoc nodes k))
          (assoc :sources new-sources)
          (assoc :sinks   new-sinks))))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (-> tg 
          (assoc :sources (update-in sources [sink]   assoc source w))
          (assoc :sinks   (update-in sinks   [source] assoc sink   w)))))
  (-disj-arc  [tg source sink]   
    (-> tg 
        (assoc :sources
          (assoc sources sink  (or (dissoc (get sources sink) source) {})))
        (assoc :sinks
          (assoc sinks   source (or (dissoc (get sinks source) sink)  {})))))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                   (get snks sink)))
  (-get-arc     [tg source sink] [source sink (get-in sinks [source sink])])
  (-get-sources [tg k]   (keys (get sources k)))
  (-get-sinks   [tg k]   (keys (get sinks   k))))

(defrecord adjacency-map [nodes sources sinks]
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (assoc tg :nodes m))
  (-conj-node [tg k v] 
    (-> tg
        (assoc :nodes   (assoc nodes k v))
        (assoc :sources (assoc sources k {}))
        (assoc :sinks   (assoc sinks   k {}))))
  (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sources k)  (-get-sinks tg k))
          new-sinks   (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sinks k)  (-get-sources tg k))]
      (-> tg 
          (assoc :nodes   (dissoc nodes k))
          (assoc :sources new-sources)
          (assoc :sinks   new-sinks))))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (-> tg 
          (assoc :sources (update-in sources [sink]   assoc source w))
          (assoc :sinks   (update-in sinks   [source] assoc sink   w)))))
  (-disj-arc  [tg source sink]   
    (-> tg 
        (assoc :sources
          (assoc sources sink  (or (dissoc (get sources sink) source) {})))
        (assoc :sinks
          (assoc sinks   source (or (dissoc (get sinks source) sink)  {})))))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                   (get snks sink)))
  (-get-arc     [tg source sink] [source sink (get-in sinks [source sink])])
  (-get-sources [tg k]   (keys (get sources k)))
  (-get-sinks   [tg k]   (keys (get sinks   k))))


;;sedgwick proposes a simpler design.
;;we have a common edgeinfo data structure, ala einfo.
;;Rather than storing two adjacency maps with weights, 
;;we assume the network is undirected, and share 
;;edge-infos between two adjacency lists.
;;He uses a bag (dunno why), but effectively has
;;an adjacency list for each vertex in the network, 
;;ordered by v (froms) and w (tos). 
;;The same edgeinfo datastructure is stored in each list though.
;;So you get the bennies of sparseness and mutation.
;;You also get the ability to see which edges are 
;;adjacent to w or v, and determine direction with a simple
;;check (is v identical to from in the edgeinfo?) 

;;I think that eliminates the edge-info queries.
;;If the structure of the network is not changing drastically, 
;;then we're golden...We also have a single pointer to the 
;;edge info, if we're using mutable edge infos.  That 
;;should be a big win...both sparseness and mutation.

;;if we have a structure that captures adjacencies...
(defprotocol IAdjacency
  (adj-sources [adj v])
  (adj-sinks   [adj w]))

(defrecord adj-map [froms tos])

  
