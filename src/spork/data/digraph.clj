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
(defrecord digraph [nodes sources sinks]
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (digraph. m sources sinks))
  (-conj-node [tg k v] 
    (gen/clone-meta tg
      (digraph.
       (assoc nodes   k v)
       (assoc sources k {})
       (assoc sinks   k {}))))
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
         new-sinks))))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (gen/clone-meta tg 
        (digraph. nodes 
           (gen/assoc2 sources sink source w)
           (gen/assoc2 sinks   source sink w)))))
  (-disj-arc  [tg source sink]   
    (gen/clone-meta tg
                (digraph. 
                 nodes
                 (gen/dissoc2 sources sink source)
                 (gen/dissoc2 sinks source sink))))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                  (get snks sink)))
  (-get-arc     [tg source sink] [source sink (gen/get2 sinks source sink 0)])
  (-get-sources [tg k]   (eager/keys! (get sources k)))
  (-get-sinks   [tg k]   (eager/keys! (get sinks   k))))

(defrecord ordered-digraph [nodes sources sinks]
  ITopograph
  (-get-nodes [tg]     nodes)
  (-set-nodes [tg m]  (ordered-digraph.  m sources sinks))
  (-conj-node [tg k v] 
    (gen/clone-meta tg
      (digraph.
       (assoc nodes   k v)
       (assoc sources k om/empty-ordered-map)
       (assoc sinks   k om/empty-ordered-map))))
 (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce-kv (fn [acc src _] (update-in acc [src] dissoc k))
                                 (dissoc sources k)  (get sinks k))
          new-sinks   (reduce-kv (fn [acc snk _] (update-in acc [snk] dissoc k))
                                 (dissoc sinks k) (get sources k))]
      (gen/clone-meta tg  (digraph. 
                       (dissoc nodes k)
                       new-sources
                       new-sinks))))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w]
    (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
      (-> tg 
          (assoc :sources (update-in sources [sink]   assoc source w))
          (assoc :sinks   (update-in sinks   [source] assoc sink   w)))))
  (-disj-arc  [tg source sink]   
    (gen/clone-meta tg
      (digraph. 
       nodes
       (gen/dissoc2 sources sink source)
       (gen/dissoc2 sinks source sink))))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                  (get snks sink)))
  (-get-arc     [tg source sink] [source sink (get-in sinks [source sink])])
  (-get-sources [tg k]   (eager/keys! (get sources k)))
  (-get-sinks   [tg k]   (eager/keys! (get sinks   k))))

(def  empty-digraph     (->digraph {} {} {}))
(def  empty-ordered-digraph (->ordered-digraph {} om/empty-ordered-map om/empty-ordered-map))

(defn invert-graph [g]  
  (-> g 
      (assoc :sinks (:sources g))
      (assoc :sources (:sinks g))))

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
