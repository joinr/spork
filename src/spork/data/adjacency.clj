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



;;An adjacency is a simple mapping of integer indices, or vertices, 
;;to 
(definterface IAdjacency
  (^long getSize        [])
  (setSize              [^long n])
  (^longs getNodes      [])
  (setAdjacent          [^long from ^long to])
  (getNeighbors         [^long idx])
  (^long   getIntWeight [^long from ^long to])
  (^double getWeight    [^long from ^long to]))  

;;An array-backed adjacency table.  We used a re
(defrecord adjacency-array  [^objects costs])

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


