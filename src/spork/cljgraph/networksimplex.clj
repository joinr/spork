;;An implementation of the network simplex algorithm, 
;;as defined in Sedgwick's C Programming Algorithms.
(ns spork.cljgraph.networksimplex
  (:require [spork.cljgraph [flow :as flow]]))

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
(defrecord simplex-net [net potentials lower basis-tree upper])

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

(defn compute-potentials [v preds cost pots]
  (loop [child v
         ^clojure.lang.ISeq acc empty-list]
    (if-let [parent (get preds child)]
      (recur parent (.cons acc [child parent]))
      (let [ps (assoc pots parent flow/neginf)]
        (reduce (fn [ps [child parent]] (let [phi (get pots child
    
         
  
  








