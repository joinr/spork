;;A set of tree operations on ITopographs.  Useful for simulating bidirectional
;;trees in a functional manner, and for emulating pointer-based structures.
(ns spork.util.topotree
  (:require [spork.util [toplist :as top]]))

;;Tree-like Operations
;;====================

;;Directional walks - either source->sink, or sink->source
(defn ascend  [tg k]  (top/walk tg k :neighbor-func (partial top/sources tg)))
(defn descend [tg k]  (top/ordered-walk tg k))

(defn parent-nodes
  "Returns the parent(s) of k."  
  [tg k] (top/sources  tg k))

(defn child-nodes [tg k] (top/sinks tg k))
(defn sibling-nodes
  "Returns a sequence of the children of the parent of node k."
  [tg k] 
  (disj (apply clojure.set/union 
               (map (comp set (partial child-nodes tg)) 
                    (parent-nodes tg k)))   k))

(defn has-children? [tg k] (not= 0 (count (child-nodes tg k))))
 
(defn subtree
  "Returns the subset of topograph, where root-key is the root 
   of the subtree."
  ([tg node-filter-func root-key] (top/subgraph node-filter-func root-key))
  ([tg root-key] (top/subgraph tg root-key)))     
 
(defn merge-tree
  "Merges two topographs together akin to clojure.core/merge.  Where nodes are
   identically-keyed, the data from other-tg replaces the original data in tg.
   If there are similar arcs, the weights associated with other-tg's arcs take
   precedence.  The choice of nodes to merge is based on a depth walk of 
   other-tg."
  [tg root-key other-tg other-root-key]
  (->> (top/depth-walk other-tg other-root-key)
       (reduce (fn [acc nd] 
                 (let [new-sinks     (top/arcs-from other-tg nd)
                       new-sources   (top/arcs-to other-tg nd)
                       new-data      (top/get-node other-tg nd)
                       new-tg        (top/conj-node acc nd new-data)
                       prior-arcs    (when (top/has-node? tg nd)
                                       (concat (top/arcs-from tg nd) 
                                               (top/arcs-to tg nd)))]
                   (->> (concat prior-arcs new-sinks new-sources)
                        (distinct)
                        (top/add-arcs new-tg)))) tg)))

(defn append-tree
  "Appends two topographs together, where the second topograph is rooted at 
   root-key.  The choice of nodes to append is based on a depth walk of 
   other-tg.  Where nodes are identically-keyed, other-tg is re-labeled with 
   new node names prior to appending.  The end result is an indexed tree that 
   has the relations and data from other-tg as a child of root-key in tg."
  [tg root-key other-tg other-root-key]
  (let [nodes-to-copy  (top/depth-walk other-tg other-root-key)
        existing-nodes (top/nodes tg)
        changed-nodes  (->> (filter #(contains? existing-nodes %) nodes-to-copy)
                            (map (fn [k] [k (keyword (gensym k))]))
                            (into {}))
        new-root-key   (get changed-nodes other-root-key other-root-key)
        conditioned-tg (reduce (fn [acc [k k-new]] (top/relabel-node acc k k-new)) 
                               other-tg changed-nodes)]
    (-> (merge-tree tg root-key conditioned-tg new-root-key)
        (top/add-arcs [[root-key new-root-key nil]]))))

(defn prune-tree
  "Removes all nodes in tg reachable from drop-root-key."
  [tg drop-root-key]        
  (reduce #(top/disj-node %1 %2) tg (top/depth-walk tg drop-root-key)))  