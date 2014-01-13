;;A collection of digraph implementations.
(ns spork.data.digraph
  (:require [spork.protocols.core :refer :all]
            [spork.data [orderedmap :as om]]))

;;The topograph Data Structure, and Implementation of ITopograph
;;==============================================================

;;The topograph is implemented as a directed graph.  Instead of edge lists, we 
;;maintain two edge maps for each node: 
;;sources : {nd #{sink nodes}}
;;sinks   : {nd #{source nodes}}
;;These are analagous to neighborhoods in my cljgraph library.
(defrecord digraph [nodes sources sinks]
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (assoc tg :nodes m))
  (-conj-node [tg k v] 
    (-> tg
        (assoc :nodes   (assoc nodes k v))
        (assoc :sources (assoc sources k om/empty-ordered-map))
        (assoc :sinks   (assoc sinks   k om/empty-ordered-map))))
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
          (assoc sources sink  (or (dissoc (get sources sink) source) 
                                   om/empty-ordered-map)))
        (assoc :sinks
          (assoc sinks   source (or (dissoc (get sinks source) sink) 
                                    om/empty-ordered-map)))))
  (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
  (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
                                  (get snks sink)))
  (-get-arc     [tg source sink] [source sink (get-in sinks [source sink])])
  (-get-sources [tg k]   (keys (get sources k)))
  (-get-sinks   [tg k]   (keys (get sinks   k))))


(defrecord digraph2 [nodes sources sinks]
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

;;The old Digraph
;;===============
;; (defrecord digraph [nodes sources sinks]
;;   ITopograph
;;   (-get-nodes [tg] nodes)
;;   (-set-nodes [tg m] (digraph. m sources sinks))
;;   (-conj-node [tg k v] 
;;     (digraph. (assoc nodes k v) 
;;                 (assoc sources k om/empty-ordered-map)  
;;                 (assoc sinks k om/empty-ordered-map)))
;;   (-disj-node [tg k]
;;     (assert (contains? nodes k) (str "Node " k " does not exist!")) 
;;     (let [new-sources (reduce #(update-in %1 [%2] dissoc k)
;;                               (dissoc sources k)  (-get-sinks tg k))
;;           new-sinks   (reduce #(update-in %1 [%2] dissoc k)
;;                               (dissoc sinks k)  (-get-sources tg k))]
;;       (digraph. (dissoc nodes k) new-sources new-sinks)))
;;   (-has-node? [tg k]  (contains? nodes k))
;;   (-conj-arc  [tg source sink w]
;;     (let [w (or w 0)] ;ensure arcs have numeric weight, not nil
;;       (digraph. nodes 
;;                 (update-in sources [sink]   assoc source w)
;;                 (update-in sinks   [source] assoc sink   w))))
;;   (-disj-arc  [tg source sink]   
;;     (digraph. nodes 
;;       (assoc sources sink  (or (dissoc (get sources sink) source) 
;;                                 om/empty-ordered-map)) 
;;       (assoc sinks   source (or (dissoc (get sinks source) sink) 
;;                                 om/empty-ordered-map))))
;;   (-has-arc?    [tg source sink] (contains?   (get sources sink) source))
;;   (-arc-weight  [tg source sink] (when-let [snks (get sinks source)]
;;                                   (get snks sink)))
;;   (-get-arc     [tg source sink] [source sink (get-in sinks [source sink])])
;;   (-get-sources [tg k]   (vec (keys (get sources k))))
;;   (-get-sinks   [tg k]   (vec (keys (get sinks k)))))

(def  empty-digraph     (->digraph {} {} {}))
;;Testing
(def  empty-digraph2    (->digraph2 {} {} {}))

(defn invert-graph [g]  (->digraph (:nodes g) (:sinks g) (:sources g)))

;;This is from flatland's ordered map implementation.  
;; (delegating-deftype TransientOrderedMap 
;;                     [^{:unsynchronized-mutable true, :tag ITransientMap} backing-map,  
;;                      ^{:unsynchronized-mutable true, :tag ITransientVector} order] 
;;                     {backing-map {ITransientMap [(count [])]}}  
;;                     ITransientMap  
;;                     (valAt [this k]    (.valAt this k nil))  
;;                     (valAt [this k not-found]    
;;                            (if-let [^MapEntry e (.valAt backing-map k)]      
;;                              (.val e)      not-found))  
;;                     (assoc [this k v]    
;;                       (let [^MapEntry e (.valAt backing-map k this)         
;;                             vector-entry (MapEntry. k v)          
;;                             i (if (identical? e this)              
;;                                 (do (change! order .conj vector-entry)                  
;;                                     (dec (.count order)))              
;;                                 (let [idx (.key e)]                
;;                                   (change! order .assoc idx vector-entry)                
;;                                   idx))]      
;;                         (change! backing-map .conj (entry k v i)) this))
;;                     (conj [this e]  
;;                           (let [[k v] e] (.assoc this k v)))  
;;                     (without [this k]   
;;                           (let [^MapEntry e (.valAt backing-map k this)] 
;;                             (when-not (identical? e this)      
;;                               (let [i (.key e)]  
;;                                 (change! backing-map dissoc! k)   
;;                                 (change! order assoc! i nil)))    
;;                             this))
;;                     (persistent [this]    
;;                                 (OrderedMap. (.persistent backing-map)
;;                                              (.persistent order))))
;; (defn transient-ordered-map [^OrderedMap om]  
;;   (TransientOrderedMap. (.asTransient ^IEditableCollection (.backing-map om))     
;;                         (.asTransient ^IEditableCollection (.order om))))
