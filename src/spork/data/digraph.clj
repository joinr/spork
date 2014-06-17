;;A collection of digraph implementations.
(ns spork.data.digraph
  (:require [spork.protocols.core :refer :all]
            [spork.data [orderedmap :as om]]
            [spork.util [eager :as eager]
                        [general :as gen]]))


;;The topograph Data Structure, and Implementation of ITopograph
;;==============================================================

;;The topograph is implemented as a directed graph.  Instead of edge lists, we 
;;maintain two edge maps for each node: 
;;sources : {nd {sink nodes}}
;;sinks   : {nd {source nodes}}
(defrecord digraph [nodes sources sinks data]
  IGraphable 
  (-get-graph [g] g)
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (digraph. m sources sinks data))
  (-conj-node [tg k v] 
    (gen/clone-meta tg
      (digraph.
       (assoc nodes   k v)
       (assoc sources k {})
       (assoc sinks   k {})
       data)))
  (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce-kv (fn [acc src _] (update-in acc [src] dissoc k))
                                 (dissoc sources k)  (get sinks k))
          new-sinks   (reduce-kv (fn [acc snk _] (update-in acc [snk] dissoc k))
                                 (dissoc sinks k) (get sources k))]
      (gen/clone-meta tg  
        (digraph. 
         (dissoc nodes k)
         new-sources
         new-sinks
         data))))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (gen/clone-meta tg 
        (digraph. nodes 
           (gen/assoc2 sources sink source w)
           (gen/assoc2 sinks   source sink w)
           data))))
  (-disj-arc  [tg source sink]   
    (gen/clone-meta tg
                (digraph. 
                 nodes
                 (gen/dissoc2 sources sink source)
                 (gen/dissoc2 sinks source sink)
                 data)))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                  (get snks sink)))
  (-get-arc     [tg source sink] [source sink (gen/get2 sinks source sink 0)])
  (-get-sources [tg k]   (eager/keys! (get sources k)))
  (-get-sinks   [tg k]   (eager/keys! (get sinks   k)))
  (-sink-map    [tg k]   (get sinks k))
  (-source-map  [tg k]   (get sources k))
  (-get-graph-data [tg]  data)
  (-set-graph-data [tg d] 
    (gen/clone-meta tg (digraph. nodes sources sinks d))))

(comment
(defrecord sparsedigraph [nodes sources sinks data]
  IGraphable 
  (-get-graph [g] g)
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (digraph. m sources sinks data))
  (-conj-node [tg k v] 
    (gen/clone-meta tg
      (digraph.
       (assoc nodes   k v)
       (assoc sources k {})
       (assoc sinks   k {})
       data)))
  (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce-kv (fn [acc src _] (update-in acc [src] dissoc k))
                                 (dissoc sources k)  (get sinks k))
          new-sinks   (reduce-kv (fn [acc snk _] (update-in acc [snk] dissoc k))
                                 (dissoc sinks k) (get sources k))]
      (gen/clone-meta tg  
        (digraph. 
         (dissoc nodes k)
         new-sources
         new-sinks
         data))))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (gen/clone-meta tg 
        (digraph. nodes 
           (gen/assoc2 sources sink source w)
           (gen/assoc2 sinks   source sink w)
           data))))
  (-disj-arc  [tg source sink]   
    (gen/clone-meta tg
                (digraph. 
                 nodes
                 (gen/dissoc2 sources sink source)
                 (gen/dissoc2 sinks source sink)
                 data)))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                  (get snks sink)))
  (-get-arc     [tg source sink] [source sink (gen/get2 sinks source sink 0)])
  (-get-sources [tg k]   (eager/keys! (get sources k)))
  (-get-sinks   [tg k]   (eager/keys! (get sinks   k)))
  (-sink-map    [tg k] (.valAt sinks k))
  (-source-map  [tg k] (.valAt sources k))
  (-get-graph-data [tg]  data)
  (-set-graph-data [tg d] 
    (gen/clone-meta tg (digraph. nodes sources sinks d))))
)

(definline ordered-assoc2 [m from to v]
  `(assoc ~m ~from (assoc (get ~m ~from om/empty-ordered-map) ~to ~v)))

(definline ordered-update2 [m from to v f]
  `(assoc ~m ~from (assoc (get ~m ~from om/empty-ordered-map) ~to ~v)))

(defrecord ordered-digraph [nodes sources sinks data]
  IGraphable 
  (-get-graph [g] g)
  ITopograph
  (-get-nodes [tg]     nodes)
  (-set-nodes [tg m]  (ordered-digraph.  m sources sinks data))
  (-conj-node [tg k v] 
    (gen/clone-meta tg
      (ordered-digraph.
       (assoc nodes   k v)
       (assoc sources k om/empty-ordered-map)
       (assoc sinks   k om/empty-ordered-map)
       data)))
 (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce-kv (fn [acc src _] 
                                   (let [m (get acc src)]
                                     (assoc acc k
                                            (dissoc m k))))
                                 (dissoc sources k)  (get sinks k))
          new-sinks   (reduce-kv (fn [acc snk _] 
                                   (let [m (get acc snk)]
                                     (assoc acc k 
                                            (dissoc m k))))
                                 (dissoc sinks k) (get sources k))]
      (gen/clone-meta tg  (digraph. 
                           (dissoc nodes k)
                           new-sources
                           new-sinks
                           data))))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (-> tg 
          (assoc :sources (ordered-assoc2 sources sink   source w))
          (assoc :sinks   (ordered-assoc2 sinks   source sink   w)))))
  (-disj-arc  [tg source sink]   
    (gen/clone-meta tg
      (ordered-digraph. 
       nodes
       (gen/dissoc2 sources sink source)
       (gen/dissoc2 sinks source sink)
       data)))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                  (get snks sink)))
  (-get-arc     [tg source sink] [source sink (gen/get2 sinks source sink 0)])
  (-get-sources [tg k]   (eager/keys! (get sources k)))
  (-get-sinks   [tg k]   (eager/keys! (get sinks   k)))
  (-sink-map    [tg k]   (get sinks k))
  (-source-map  [tg k]   (get sources k))
  (-get-graph-data [tg]   data)
  (-set-graph-data [tg d] 
    (gen/clone-meta tg (ordered-digraph. nodes sources sinks d))))

(def  empty-digraph     (->digraph {} {} {} nil))
(def  empty-ordered-digraph (->ordered-digraph {} om/empty-ordered-map om/empty-ordered-map nil))

(defn invert-graph [g]  
  (-> g 
      (assoc :sinks (:sources g))
      (assoc :sources (:sinks g))))


;;Graph Transformations
;;=====================
;; (defrecord masked-graph [g disabled-nodes disabled-arcs]
;;   ITopograph                
;;   (-conj-node [tg k v] (masked-graph. (-conj-node g k v) disabled-nodes disabled-arcs))
;;   (-disj-node [tg k]   (masked-graph. (-disj-node g k v) disabled-nodes disabled-arcs))
;;   (-has-node? [tg k]   (and (-has-node? g k) (not (contains? disabled-nodes k))))
;;   (-conj-arc  [tg source sink w] (masked-graph. (-conj-arc g source sink w) disabled-nodes disabled-arcs))
;;   (-disj-arc  [tg source sink]   (masked-graph. (-disj-arc g source sink) disabled-nodes disabled-arcs))
;;   (-has-arc?    [tg source sink] (when (gen/get2 disabled-arcs source sink nil) 
;;                                    (-has-arc? g source sink)))
;;   (-arc-weight  [tg source sink] (when (gen/get2 disabled-arcs source sink nil)
;;                                    (-arc-weight g source sink)))
;;   (-get-arc     [tg source sink] (when (gen/get2 disabled-arcs source sink nil)
;;                                    (-get-arc g source sink)))
;;   (-get-sources [tg k]   (when )
;;   (-get-sinks   [tg k]   ))

;;Work in progress.

;; (defrecord mutable-digraph [^java.util.HashMap nodes 
;;                             ^java.util.HashMap sources
;;                             ^java.util.HashMap sinks]
;;   ITopograph
;;   (-get-nodes [tg] nodes)
;;   (-set-nodes [tg m] (digraph. m sources sinks))
;;   (-conj-node [tg k v] (do (jassoc nodes   k v)
;;                            (jassoc sources k {})
;;                            (jassoc sinks   k {}) 
;;                             tg))
;;   (-disj-node [tg k]
;;     (assert (contains? nodes k) (str "Node " k " does not exist!")) 
;;     (let [new-sources (reduce-kv (fn [acc src _] (update-in acc [src] dissoc k))
;;                                  (dissoc sources k)  (get sinks k))
;;           new-sinks   (reduce-kv (fn [acc snk _] (update-in acc [snk] dissoc k))
;;                                  (dissoc sinks k) (get sources k))]
;;       (gen/clone-meta tg  
;;         (digraph. 
;;          (dissoc nodes k)
;;          new-sources
;;          new-sinks))))
;;   (-has-node? [tg k]  (contains? nodes k))
;;   (-conj-arc  [tg source sink w]
;;     (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
;;       (gen/clone-meta tg 
;;         (digraph. nodes 
;;            (gen/assoc2 sources sink source w)
;;            (gen/assoc2 sinks   source sink w)))))
;;   (-disj-arc  [tg source sink]   
;;     (gen/clone-meta tg
;;                 (digraph. 
;;                  nodes
;;                  (gen/dissoc2 sources sink source)
;;                  (gen/dissoc2 sinks source sink))))
;;   (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
;;   (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
;;                                   (get snks sink)))
;;   (-get-arc     [tg source sink] [source sink (gen/get2 sinks source sink 0)])
;;   (-get-sources [tg k]   (eager/keys! (get sources k)))
;;   (-get-sinks   [tg k]   (eager/keys! (get sinks   k))))
