;;An implementation of the network simplex algorithm, 
;;as defined in Sedgwick's C Programming Algorithms.
(ns spork.cljgraph.networksimplex
  (:require [spork.cljgraph [flow :as flow]
                            [core :as graph]]
            [spork.protocols [core :as generic]]))

;;Applying the simplex method to the mincost flow 
;;problem requires a few extra bits of plumbing.

;;Our goal is identical to the primal simplex method 
;;using matrics in linear programming, except we have 
;;the ability to take advantage of special structures in 
;;the network representation for extra efficiency.


;;We need to compute node potentials. 
;;Node potentials form a heuristic for defining 
;;reduced cost.  Reduced cost is the actual (i.e. 
;;static) cost of 1 unit of flow across a node, 
;;less the difference between entering and 
;;leaving node potentials.  It's a heurstic to 
;;help us identify arcs we want to flow across.

;;We can interpret RC as the cost of buying at 
;;a node, shipping from the node to another, then 
;;selling (i.e. recouping cost) at the other node.

;;Thus, reduced cost informs how much we may improve 
;;the objective, based on potentials.
(defrecord simplex-net [net source sink tree potentials lower upper valids])

;;Our basis-tree is a minimum spanning tree, with the property 
;;that edges in the tree have a reduced cost of 0 (they are basic 
;;in simplex parlance). 

;;The general idea is to find a basic solution, which implies 
;;arcs outside of the basis.  We choose an arc outside of the 
;;basis and add it to the tree.  Adding it to the tree forms 
;;a cycle (since it's an MSP), which means we push flow 
;;along the cycle - ala cycle cancelling.  Some arc in the 
;;cycle is guaranteed to drop since flow is push.  So...we 
;;drop the arc, and recompute node potentials.  Then choose 
;;another arc outside of the tree.

;;When choosing an arc outside the tree, we can only choose arcs 
;;that will induce a negative-cost cycle in the basis.  What 
;;would do that?  Since the RC of every edge in the basis-tree 
;;is zero, we want to pull in an arc that, when flow is pushed 
;;along it, will result in a negative cost (.i.e improving the 
;;objective) in the residual network.

;;Since arcs may flow forward or backward, we are looking for 
;;forward, fully capacitated arcs that have positive reduced cost
;;(implying a backward residual arc with NEGATIVE cost), 
;;or empty arcs with negative reduced cost (implying a forward 
;;residual arc with NEGATIVE reduced cost).

;;We have an optimal flow if we cannot find an eligible arc.

;;The consequence here is that we are flipping through simplex 
;;bases, where the bases correspond to MSP trees for the flow network, 
;;transitions between trees force an arc out of the network and into 
;;either the Full or Empty arc set, while swapping in an arc from the 
;;Full or Empty arc set.

;;Each MSP has a unique set of vertex potentials.  So when we 
;;build an MSP (or mutate one), we need to recompute the vertex
;;potentials.

;;Actually, we need to recompute vertex potentials iff the old
;;potentials were affected.  There are cases where we will modify 
;;the MSP, but dependent potentials are unaffected (i.e. they are 
;;still good), so we can avoid recomputing all vertex potentials 
;;every time.

(def ^:constant empty-list (list))
(def ^:constant neginf Long/MIN_VALUE)
;;All we have to do to compute potentials is start at any node, 
;;traverse up through its parents, then when we hit the root (a node 
;;with no parent), we assign a potential to the parent (typically
;;-inf) and traverse back through the children assigning potentials.

;;Given rc(u,v) = cost(u,v) - (phi(u) - phi(v)) 
;;If we assume rc(u,v) = 0, for edges along the tree, 
;;then 0 = cost(u,v) - (phi(u) - phi(v))
;;     0 = cost(u,v) - phi(u) + phi(v)
;;     phi(u) - cost(u,v) = phi(v)

;;So, computing potentials for a child v relative to a parent u, for
;;any edeg (u,v) within the basis tree is:
;;     phi(v) = phi(u) - cost(u,v) 

;;There is a chance to optimize this bad-boy too; We can recompute 
;;potentials lazily.
(comment 

(defn compute-potentials [v preds costfunc potentials valid]
  (loop [child v
         acc empty-list]
    (if-let [parent (get preds child)]
      (recur parent (cons [parent child] acc))
      (reduce (fn [ps edge] 
                (let [parent (nth edge 0)
                      child  (nth edge 1)
                      parent-potential (get potentials parent)
                      cost   (costfunc parent child)
                      child-potential (- parent-potential cost)]
                  (assoc ps child child-potential)))
              (assoc potentials parent neginf)
              acc))))        
)
;;Since we are working with trees, when we add an edge to the basis
;;tree, we create a cycle between the two vertices on the edge (by 
;;virtue of the fact that the tree is in fact a MSP).  Since we 
;;intend to push flow across this cycle, we need to know what the 
;;cycle is. So, we exploit the tree structure to find the least common 
;;ancestor of both nodes.  The paths from each node to the least 
;;common ancestor (plus the edge u v) define the minimum cycle.

;;Determine the least common ancestor of two nodes in a tree, or the 
;;root of the smallest subtree that contains both nodes. Basically 
;;just trace up the parents until we find a common node, i.e. until 
;;we run into the first node that's already been visited.
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

;;More portable version.  We'll see if this is acceptable in a bit.
(comment 
(defn least-common-ancestor 
  "Computes the least common ancestor in a predecessor tree.  Note: this 
   will break if one of the nodes does not exist in the predecessor tree. We may 
   want a sentinel on this to guard against that corner case."  
  [preds s t]
  (loop [u (get preds s)
         v (get preds t)
         visited  (-> (transient {}) (assoc! s true) (assoc! t true))]
    (if (identical? u v) u
        (let [pu (get preds u u)
              pv (get preds v v)
              visitedu (get visited u)
              visitedv (get visited v)]
          (cond (and (not (identical? pu u)) visitedu) u
                (and (not (identical? pv v)) visitedv) v
                :else
                (recur pu pv (as-> visited vnext 
                                   (if (not visitedu) (assoc! vnext  u true) vnext)
                                   (if (not visitedv) (assoc! vnext  v true) vnext))))))))  
)

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
      (let [new-child (.first path)
            new-parent  (.first remaining)]
        (recur (assoc p new-child new-parent) 
               remaining))
      p)))  

(definline drop-edge [preds from to]   `(dissoc ~preds ~from))
(definline insert-edge [preds from to] `(assoc ~preds ~from ~to))

;;a substitution is just dropping the edge, adding the new edge, 
;;then flipping the nodes between the to of the new edge and the 
;;to of the old edge

(def ^:dynamic *debug* false)
(def ^:dynamic *ubound* 10)

(defn substitute-edges 
  "Traverses the nodes in preds between from and to"
  [preds drop-from drop-to add-from add-to]           
  (let [p (-> preds (drop-edge drop-from drop-to))
        target drop-from
        count (atom 0)]
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
;;currently a bottleneck, although it may not matter since 
;;we "should" not be doing tons of augmentations.
(defn cycle-path [preds from to]
  (let [lca (least-common-ancestor preds from to)
        ^java.util.ArrayList res 
             (spork.protocols.core/loop-reduce 
              (fn [^java.util.ArrayList acc x] 
                (doto acc (.add x)))
              (simple-path preds to lca)
              (.next ^clojure.lang.ISeq (reversed-path preds from lca)))]    
    (doto res (.add to))))
         

(defrecord flow-cycle [flow from to ancestor edge-flows]) 

;;Computes a structure that contains the minimum flow across a forward
;;flow path and a backward flow path.
(defn ^flow-cycle derive-cycle [preds from to get-edge-flows]
  (let [^spork.cljgraph.flow.edge-flows fs  (get-edge-flows rp)]
    (flow-cycle. (min (.flow fwd) (.flow bwd))
                 from to lca fwd bwd)))
;;wip
(defn augment-flow-tree [the-net preds get-edge-flows from to]
  (let [^spork.cljgraph.flow.edge-flows ef (get-edge-flows the-net (cycle-path preds from to))
         flow (.flow ef)
         ^java.util.ArrayList xs   (.edges ef)
         n     (.size xs)]
    (loop [idx 0
           acc the-net]
      (if (== idx n) acc
          (recur (unchecked-inc idx)
                 (let [^spork.cljgraph.flow.directed-flow info (.get xs idx)]                  
                   (-push-flow acc (.edge info) (unchecked-multiply (.flowmult info) flow))))))))

(defn spanning-neighbor [net] 
  (fn [_ nd st] 
    (let [spt (:shortest st)]
      (reduce (fn [^clojure.lang.ISeq acc k] 
                   (if (contains? spt k) acc (.cons acc k)))
                 empty-list (flow/flow-neighbors net nd)))))

;;compute a spanning tree in the residual network.
;;basically, we do a flow-flow, 
;; (defn residual-spanning-tree 
;;   [net from to]
;;   (let [init-state (-> (spork.data.searchstate/empty-DFS from)
;;                        (generic/relax 0 to from 0)
;;                        (generic/visit-node to))]                     
;;     (:shortest 
;;      (spork.cljgraph.search/traverse (generic/-get-graph net) from ::no-node init-state
;;        {:neighborf  (spanning-neighbor net)}))))

(defn residual-spanning-tree 
  [net from to]
  (let [res (spork.cljgraph.search/depth-walk 
             (generic/-get-graph net) from ::nonode
              {:neighborf  (spanning-neighbor net)})]
    (-> (:shortest res)
        (assoc from to)
        (assoc to to))))

(defn add-dummy-edge [net from to]
  (let [dummy-cap (flow/max-outflow net from)
        init-flow (min (flow/max-inflow net to) dummy-cap)]
   (-> net 
       (flow/-conj-cap-arc from to flow/posinf dummy-cap)
       (flow/-update-edge  from to #(flow/inc-flow % init-flow)))))

;;The potential of a node is equivalent to the 
;;potential of its predecessor - the cost of traversing predecessor
;;to node.
(definline phi [costf pots u v]
  `(- (get ~pots ~u) 
      (~costf ~u ~v)))

;;we can replicate lazy potentials by only computing potentials on
;;the path we need.  Sedgewick does it recursively.  We just build 
;;a path of nodes, then compute the potentials in order from 
;;root to v, so we have potentials computed.
(defn update-path-potentials [costf preds pots ^java.util.ArrayList p]
  (let [bound (dec (.size p))
        root  (.get p 0)]
    (loop [idx 1
           ps (assoc pots root 0)]
      (if (== idx bound) ps
          (assoc ps (.get p idx) 
                 (phi costf ps (.get p idx) 
                               (.get p (unchecked-inc idx))))))))

;;We can compute potentials for an entire spanning tree (although we
;;may only need to compute potentials for a portion of the tree, i.e. 
;;do it lazily...
;; (defn compute-potentials [preds costf])
(comment 

(defn init-simplex [net from to]
  (let [dummy-cap (flow/max-outflow net from)
        init-flow (min (flow/max-inflow net to) dummy-cap)
        augnet    (-> net 
                      (flow/-conj-cap-arc from to flow/posinf dummy-cap)
                      (flow/-update-edge  from to #(inc-flow % init-flow)))
        spanning  (residual-spanning-tree augnet from to)
        pots      (compute-potentials spanning (fn [from to] (flow/-flow-weight augnet from to))) ]
    (->simplex-net augnet from to spanning {} #{})))
)



(def preds 
  {0 3 1 13 2 14 3 11 4 2 5 5 6 3 7 5 8 0 9 2 10 15 11 5 12 13 13 11 14 0 15 1})



;;From sedgwick, pg 450
(def the-arcs
  [[0 1 3 3 0]
   [0 2 3 1 0]
   [1 3 2 1 0]
   [1 4 2 1 0]
   [2 3 1 4 0]
   [2 4 2 2 0]
   [3 5 2 2 0]
   [4 5 2 1 0]])

(def the-net (reduce (fn [acc [from to cap cost flow]]
                          (let [n (flow/-conj-cap-arc acc from to cost cap)]
                            (if (zero? flow)  n
                                (flow/-push-flow n (flow/-edge-info n from to) flow))))
                     flow/empty-network the-arcs))

(def the-preds (residual-spanning-tree the-net 0 5))
;;makes it consistent with sedgewick, for testing purposes.
(def seg-preds  {0 5 
                 1 0 
                 2 0 
                 3 1 
                 4 1 
                 5 5})

(def predmap (reduce (fn [^java.util.HashMap acc [k v]] (doto acc (.put k v)))
                     (java.util.HashMap.)
                     (seq preds)))
