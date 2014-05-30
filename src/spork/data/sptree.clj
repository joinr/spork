;;A lib for working with spanning trees implemented using clojure maps.
(ns spork.data.sptree)

(defn least-common-ancestor
  "Computes the least common ancestor in a predecessor tree.  Note: this 
   will break if one of the nodes does not exist in the predecessor tree. We may 
   want a sentinel on this to guard against that corner case."  
  [preds s t]
  (let  [visited  (doto (java.util.HashSet.) (.add s) (.add t))]
    (loop [u (get preds s)
           v (get preds t)]
      (if (identical? u v) u
          (let [pu       (get preds u u)
                pv       (get preds v v)
                visitedu (.contains visited u)
                visitedv (.contains visited v)]
            (cond (and (not (identical? pu u)) visitedu) u
                  (and (not (identical? pv v)) visitedv) v
                  :else
                  (do (when (not visitedu) (.add visited  u))
                      (when (not visitedv) (.add visited  v))
                      (recur pu pv))))))))

;;Once we have the ability to find the least common ancestor in the
;;spanning tree, we need to be able to swap out edges.  One of the
;;edges on the cycle will be saturated (i.e. fully capacitated or 
;;fully drained), and it will be moved to the L or U set of edges.


;;So, given an LCA, and two nodes, we know we can augment along the 
;;path. that forms their cycle.  This is identical to our augmentation 
;;from netflow, and we can probably use the same algorithm.

;;Once we push flow along the edges, we find that - one or more 
;;edges - is a leaving edge.

;;Given a minimum spanning tree, an edge to add, and a edge to be dropped, can we 
;;alter the tree? 

(defn between? 
  "Given a predecessor tree, determines if target is on a path between start node and 
   stop node."
  [preds start stop target]
  (loop [parent (get preds start)]
    (cond (identical? parent target) true
          (identical? parent stop)  false
          :else      (recur (get preds parent)))))

(defn flip [preds ^clojure.lang.ISeq init-path]
  (loop [p      preds
         path   init-path]
    (if-let [remaining (.next  path)]
      (let [new-child  (.first path)
            new-parent (.first remaining)]
        (recur (assoc p new-child new-parent) 
               remaining))
      p)))  

(definline drop-edge [preds from to]   `(dissoc ~preds ~from))
(definline insert-edge [preds from to] `(assoc ~preds ~from ~to))

;;a substitution is just dropping the edge, adding the new edge, 
;;then flipping the nodes between the to of the new edge and the 
;;to of the old edge

(defn substitute-edges 
  "Traverses the nodes in preds between from and to"
  [preds dropped added]           
  (let [drop-from (flow/edge-from dropped)
        drop-to   (flow/edge-to dropped)
        add-from  (flow/edge-from added)
        add-to    (flow/edge-to   added)
        p (-> preds (drop-edge drop-from drop-to))
        target drop-from]
    (loop [child add-from
           acc   empty-list]
      (if (identical? child target)
        (insert-edge (flip p (cons child acc)) add-from add-to) 
        (if (identical? child (first acc)) (throw (Exception. "No path to target"))
            (recur (get p child) 
                     (cons child acc)))))))

(defn simple-path [preds from to]
  (let [p (java.util.ArrayList.)]
        (loop [child from]
          (do (.add p child)
              (if (identical? child to) p
                  (recur (get preds child)))))))                 

(defn reversed-path [preds from to]
  (loop [child from
         p     empty-list]
    (if (identical? child to) (cons child p)
        (recur (get preds child)
               (cons child p)))))

(defn path-to-root [preds from]
  (loop [child from
         p     empty-list]
    (let [parent (get preds child)]
      (if (identical? child parent) (cons child p)
          (recur parent
                 (cons child p))))))

(defn path-from-root [preds to]
  (let [p     (java.util.ArrayList.)]
    (loop [child to]
      (let [parent (get preds child)]
        (if (identical? child parent) (doto p (.add  child) (java.util.Collections/reverse))
            (do (doto p (.add child))
                (recur parent)))))))

(defn path-from-valid [preds to valid?]
  (let [p     (java.util.ArrayList.)]
    (loop [child to]
      (let [parent (get preds child)]
        (if (or (valid? parent) (identical? child parent)) (doto p (.add  child) (java.util.Collections/reverse))
            (do (doto p (.add child))
                (recur parent)))))))

(definline append [lst elem]
    `(doto ~(with-meta lst {:tag 'java.util.ArrayList}) (.add ~elem)))
