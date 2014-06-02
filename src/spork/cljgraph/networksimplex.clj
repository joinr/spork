;;An implementation of the network simplex algorithm, 
;;as defined in Sedgwick's C Programming Algorithms.
(ns spork.cljgraph.networksimplex
  (:require [spork.cljgraph [flow :as flow]
                            [core :as graph]]
            [spork.protocols [core :as generic]]
            [spork.data.mutable]
            [spork.data [sptree :as sptree]]
            [clojure.core [reducers :as r]]))

;;The foundation of a network simplex algo is the fact that 
;;we have a partitioning of all edges.

;;This may be too much....we'll rethink, but the API is nice.
(defprotocol IEdgePartition 
  (basic-edge [p n])
  (lower-edge [p n])
  (upper-edge [p n])
  (get-basic-edges [p])
  (get-lower-edges [p])
  (get-upper-edges [p])
  (get-nonbasic-edges [p])
  (basic? [p e]))

(defprotocol IPricing
  (edge-cost      [smplx  e])
  (get-node-potential [smplx nd])
  (set-node-potential [smplex nd pot]))

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

;;Edges exist in the net...
;;If we need random access to the edges, we can always add a 
;;list or vector of edges...
;;If we don't care about adjacency (with network simplex, we don't)
;;we can have a flat network representation....we lose the ability to 
;;modify the underlying network using the same api though.

(defmacro select-edges [edges xs]
  `(for [idx# ~xs]
     (nth ~edges idx#)))
  
;;Thus, reduced cost informs how much we may improve 
;;the objective, based on potentials.
(defrecord simplex-net [net nodes edges source sink tree potentials parts costs]
  generic/IGraphable 
  (-get-graph      [s] (generic/-get-graph net))
  flow/IFlowNet
  (-edge-info      [s from to] (flow/-edge-info net from to))
  (einfos [s]                  (flow/einfos net))
  (-get-direction  [s from to] (flow/-get-direction net from to))
  (-flow-weight    [s from to] (flow/-flow-weight net from to))
  (-set-edge       [s edge]    
      (simplex-net. (flow/-set-edge net edge) nodes edges
                    source sink tree potentials parts costs))
  (-flow-sinks     [s x] (flow/-flow-sinks net x))
  (-flow-sources   [s x] (flow/-flow-sinks net x))
  (-push-flow      [s edge flow] 
      (simplex-net. (flow/-push-flow net edge flow) nodes edges
                    source sink tree potentials parts costs))
  flow/IDynamicFlow 
  (-update-edge  [s from to f]                  
      (simplex-net. (flow/-update-edge net from to f) nodes edges
                    source sink tree potentials parts costs))

  (-disj-cap-arc   [s from to]  
      (simplex-net. (flow/-disj-cap-arc net from to) nodes edges
                    source sink tree potentials parts costs))
  (-conj-cap-arc [s from to w cap]                 
      (simplex-net. (flow/-conj-cap-arc net from to w cap) nodes edges
                    source sink tree potentials parts costs))
  (-active-flows [s] (flow/-active-flows net))
  IEdgePartition 
  (basic-edge [p n] (simplex-net. net  nodes edges
                                 source sink tree potentials (basic-edge parts n) costs))
  (lower-edge [p n] (simplex-net. net  nodes edges
                                  source sink tree potentials (lower-edge parts n) costs))
  (upper-edge [p n] (simplex-net. net  nodes edges
                                  source sink tree potentials (upper-edge parts n) costs))
  ;;This is not necessary...and leads to slowdown.  While nice for
  ;;debugging, we really don't want to query these guys in practice.
  (get-basic-edges [p]    (map #(nth p %) (get-basic-edges parts)))
  (get-lower-edges [p]    (map #(nth p %) (get-lower-edges parts)))
  (get-upper-edges [p]    (map #(nth p %) (get-upper-edges parts)))
  (get-nonbasic-edges [p] (map #(nth p %) (get-nonbasic-edges parts)))
  (basic? [p e]           (basic? parts  e))
  IPricing
  (edge-cost          [smplx e]     (nth costs (flow/edge-data e)))
  (get-node-potential [smplx nd]    (get potentials nd))
  (set-node-potential [smplex nd p] (assoc potentials nd p))
  clojure.lang.Indexed
  (nth [coll n] (nth edges n))
  (nth [coll n not-found] (if-let [res  (nth edges n)] res not-found)))

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

;;Pivoting and Searching the Simplex
;;==================================

(comment 
(defprotocol ISimplexState
  (simplex-pivot [smplx])
  (entering-arc  [smplx])
  (leaving-arc   [smplx])
  (find-entering [smplx]))
)

;; (defprotocol ISimplexEdge
;;   (edge-cost [e])
;;   (edge-index [e]))
  
(comment 
  (defrecord sdata [idx rcost])
  
  (defn edge-index [e]
    (.idx ^sdata s (flow/edge-data e)))
  (defn edge-cost  [e]
    (.idx ^sdata s (flow/edge-data e))))

;;I think we want some data in here...
;(defrecord simplex-state [simplex leaving-arc n find-arc]) 

(def ^:constant empty-list (list))

;;These are weak hacks to avoid some overflow problems for now....
;;need to handle better.
(def ^:constant neginf -9999999999)
(def ^:constant posinf 9999999999)

;;A weak hack, should unify hashmaps and persistent maps behind a protocol.
(defn assoc!! [^java.util.HashMap m k v]
  (doto m (.put k v)))

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

;;a substitution is just dropping the edge, adding the new edge, 
;;then flipping the nodes between the to of the new edge and the 
;;to of the old edge

;;currently a bottleneck, although it may not matter since 
;;we "should" not be doing tons of augmentations.
(defn cycle-path [preds from to]
  (let [lca (sptree/least-common-ancestor preds from to)
        ^java.util.ArrayList res 
        (spork.protocols.core/loop-reduce 
         (fn [^java.util.ArrayList acc x] 
           (doto acc (.add x)))
         (sptree/simple-path preds to lca)
         (.next ^clojure.lang.ISeq (sptree/reversed-path preds from lca)))]    
    (doto res (.add to))))        

(defn substitute-edges 
  [preds dropped added]
  (sptree/substitute-edges (flow/edge-from dropped)
                           (flow/edge-to dropped)
                           (flow/edge-from added)
                           (flow/edge-to   added)))
  
;;Augments the flow between from and to, returning the transformed
;;tree and an edge that was capacitated.
(defn augment-flow-tree [the-net preds get-edge-flows from to]
  (let [^spork.cljgraph.flow.edge-flows ef (get-edge-flows the-net (cycle-path preds from to))
         flow                      (.flow ef)
         ^java.util.ArrayList xs   (.edges ef)
         n                         (long (.size xs))
         dropped                   (atom nil)]
    (loop [idx 0
           acc the-net]
      (if (== idx n) [acc @dropped]
          (recur (unchecked-inc idx)
                 (let [^spork.cljgraph.flow.directed-flow info (.get xs idx)
                       new-flow (unchecked-multiply (.flowmult info) flow)
                       e (flow/inc-flow (.edge info) flow)
                       _ (when (or (zero? (flow/edge-capacity e))
                                   (zero? (flow/edge-flow e)))
                           (reset! dropped e))]                  
                   (flow/-set-edge acc e)))))))

(defn spanning-neighbor [net] 
  (fn [_ nd st] 
    (let [spt (:shortest st)]
      (reduce (fn [^clojure.lang.ISeq acc k] 
                (if (contains? spt k) acc (.cons acc k)))
              empty-list (flow/flow-neighbors net nd)))))

(defn residual-spanning-tree 
  [net from to]
  (let [res (spork.cljgraph.search/depth-walk 
             (generic/-get-graph net) from ::nonode
              {:neighborf  (spanning-neighbor net)})]
    (persistent! (reduce-kv (fn [m k v] (assoc! m k v)) (transient {from to to to}) (:shortest res)))))

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
  `(unchecked-subtract (get ~pots ~u) 
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

(defn update-path-potentials! [costf preds pots ^java.util.ArrayList p]
  (let [bound (dec (.size p))]
    (loop [idx 1
           ps  pots]
      (if (== idx bound) ps
          (assoc! ps (.get p idx) 
                  (phi costf ps (.get p idx) 
                                (.get p (unchecked-inc idx))))))))

;;if we have a potential, we are known.
;;so, if we want to update the potentials for a node, we just 
;;race up the parents until we find a known potential, then race 
;;back down to update.    
(defn update-potentials-from! [preds costf  child known-pots]
  (let [parent     (get preds child)]
    (let [ppot (or (get known-pots parent)
                   (let [_ (update-potentials-from! preds costf parent known-pots)]
                     (get known-pots parent)))]
      (assoc! known-pots child (unchecked-subtract ppot (costf parent child))))))

(defn update-all-potentials! [preds costf pots]
  (reduce-kv (fn [ps child parent]
               (if (identical? parent child) 
                   (assoc! ps parent (get ps parent neginf))
                   (let [ps       (if (get ps parent) ps
                                      (update-potentials-from! preds costf parent ps))]
                     (let [res (update-potentials-from! preds costf child ps)]
                       (do ;(println [:potentiating child parent])
                           ;(print-pots! preds ps)
                           res)))))
               pots
               preds))

;;By implication, all basic arcs WILL have forward arcs, so we only 
;;have to look them up via from-to, we can walk the tree and ignore 
;;arcs.

;;We can compute potentials for an entire spanning tree (although we
;;may only need to compute potentials for a portion of the tree, i.e. 
;;do it lazily...

;;we have to be able to find eligible arcs.
;;Really, find the next eligible arc.  This is pretty huge.
;;Naive implementations just scan the arcs that are NOT on 
;;the basis tree, looking for arcs that have negative reduced 
;;cost.  This is sedgewicks' method. Fortunately, we can 
;;alter pivot strategies to allow for less naive implementations.

;;An edge's reduced cost is equivalent to the cost of the 
;;edge, minus the difference between potentials for each directed 
;;node.
(defn reduced-cost 
  ([from to pots costf]
     (unchecked-subtract (costf from to)
                         (unchecked-subtract
                          (get pots from)
                          (get pots to))))
  ([edge pots costf]
     (let [from (flow/edge-from edge)
           to   (flow/edge-to edge)]
       (unchecked-subtract (costf from to)
                           (unchecked-subtract 
                            (get pots from)
                            (get pots to)))))
  ([edge smplx] 
     (let [from (flow/edge-from edge)
           to   (flow/edge-to edge)]
       (unchecked-subtract (edge-cost smplx edge)
                           (unchecked-subtract 
                            (get-node-potential smplx from)
                            (get-node-potential smplx  to))))))

;;Violating-arcs are arcs in L with negative reduced cost (we can 
;;push flow along them to reduce objective), ;and arcs in U with
;;positive reduced cost (we can retract flow from them to 
;;reduce objective). 

;;One option is to traverse all the arcs, taking the first eligible
;;Another option to traverse all the arcs, taking the best eligible
;;Another option is to traverse some of the arcs, sometimes taking 
;;the most eligible, 
(defn eligible-arc [simplex] 
  (let [l (get simplex :lower) 
        u (get simplex :upper)] 
    (
    :pending)))

;;We can detect if an edge is on the basis if the following 
;;conditions hold true: 

;;Both its from and to nodes are on the basis.
;;From is the parent of to, or to is the parent of from 

(defn in-basis? [spt edge]
  (let [from (flow/edge-from edge)
        to   (flow/edge-to edge)]
    (when-let [parent (get spt to)]
      (or (identical? from parent)
          (when-let [rparent (get spt from)]
            (identical? to rparent))))))

;;THis is weak, creates intermediate colls.
(defn nonbasic-edges 
  ([net spt]
     (filter #(not (in-basis? spt %)) (flow/einfos net)))
  ([smplx]
     (get-nonbasic-edges smplx)))

;;Might be able to move this inline to simplex definition.
(defn reduced-costs [smplx]
  (for [e (get-nonbasic-edges smplx)]
    (reduced-cost e  smplx)))


;;Edge Queries / Pricing
;;======================

;;edges outside the basis are either full or empty;
;;Empty edges are eligible if their reduced cost is negative - pushing
;;flow will reduce the cost of flow; 
;;Full edges are eligible if their reduced cost is positive - removing
;;flow with reduce the cost of flow;
(definline eligible-edge? [non-basic-edge costf]
  `(let [c# (~costf ~non-basic-edge)]
     (if (zero? (flow/edge-flow ~non-basic-edge))
       (neg? c#)
       (pos? c#))))

;; (definline eligible-edge-cost? [non-basic-edge costf]
;;   `(let [c# (~costf ~non-basic-edge)]
;;      (if (zero? (flow/edge-flow ~non-basic-edge))
;;        (neg? c#)
;;        (pos? c#))))

;;Eligible edges are edges in L that have Negative reduced cost, 
;;and edges in U that have Positive reduced cost.
(defn eligible-edges [smplx]
  (let [ps  (get smplx :potentials)
        net (get smplx :net)
        costf (fn [e] (reduced-cost e ps #(flow/-flow-weight net %1 %2)))]
    (filter (fn [e] (eligible-edge? e costf)) (get-nonbasic-edges smplx))))  

(defn eligible-edges! [smplx]
  (let [ps  (get smplx :potentials)
        net (get smplx :net)
        costf (fn [e] (reduced-cost e ps #(flow/-flow-weight net %1 %2)))]
    (reduce (fn [^java.util.ArrayList acc e]
              (if (eligible-edge? e costf)
                  (doto acc (.add e)) 
                  acc)) 
            (java.util.ArrayList.) (get-nonbasic-edges smplx))))

;;Performance Issues: 
;;We want edge selection (.i.e Pivoting) to be efficient.  Really
;;efficient.
;;Some implementations (ala Sedgewick) just blast through the 
;;edges in an array and find the first eligible edge, or they 
;;implement Dantzig's rule and find the best.  Either way, they
;;ignore (do not cache) the edge topology and use O(n) searches 
;;to find the next eligible edge.

;;In my desire to implement a simple, yet efficient pivot rule, 
;;I would like to find eligible edges fast.  I'd also like to 
;;inject some randomness into the pivot selection.
;;We can go about this a couple of ways....

;;1) Select edges randomly
;;2) Block-search edges according 
;;3) Retain candidate list of edges 
;;4) Mix and match.

;;Most schemes will typically revolve around the block scanning
;;mechanism.
;;Let's create a data structure for performing block scans.
;;A block scan maintains a block of edges to look at for eligibility.
;;The block is a fixed-width.  At each iteration, we start from the 
;;previous entering arc.  We examine a "block" of arcs, where the 
;;block are the arcs indexed-adjacent to the entering index in 
;;a random-access edge collection.  We scan out the block, 
;;looking to find an eligible arc.  If we do, we pick the best one.
;;It's like applying Dantzig's rule to a smaller portion of the 
;;arcs at each iteration.  Once we update, the block is extended 
;;to start at the last entering arc.



;;This an implementation of Grigoriadis' block search.  We just 
;;search a block at a time, looking for the "best" arc in the block.
;;If we find an arc, great.  Otherwise, we continue searching.  
;;If we roll over to the original index, we have exhausted all 
;;blocks, and can find no eligible arcs.
(defn search-by-blocks 
  [init-arc block-width indexed-edges costf eligible?]
  (let [bound (count indexed-edges)]
    (loop [idx     (unchecked-inc init-arc)
           width   block-width
           mincost posinf
           best     nil]
      (cond (or (== idx init-arc)
                (and (zero? width) min))    best ;either found an arc
                                        ;or ran out
                (== idx bound)        (recur 0  width mincost best) ;rollover
                (not (eligible? idx)) (recur (unchecked-inc idx) (unchecked-dec width) 
                                             mincost best) ;skip 
                :else ;price the arc
                (let [e     (nth indexed-edges idx)
                      ecost (costf e)]              
                  (if (< ecost mincost)
                    (recur (unchecked-inc idx)
                           (unchecked-dec width)                       
                           ecost 
                           e)
                    (recur (unchecked-inc idx)
                           (unchecked-dec width)                       
                           mincost
                           best)))))))

(defn search-simplex-blocks [smplx init-arc block-width]
  (search-by-blocks init-arc block-width (get smplx :edges) 
                    #(reduced-cost % smplx) #(not (basic? smplx %))))

(defn compute-block-width [smplx]
  (let [count (count (get smplx :edges))]
    (long (Math/sqrt count))))

(comment

(search-by-blocks 0 100 es 
                  #(reduced-cost % (:potentials seg-simplex) 
                                 (flow/-flow-weight seg-simplex 
                                                    (flow/edge-from %) 
                                                    (flow/edge-to %) )) identity)
)

;;We can simplify initializing our simplex algo by creating 
;;a dummy arc from source to sink, with a sufficiently large capacity 
;;and cost.
(defn augmented-network 
  ([net from to init-cost] 
     (let [dummy-cap (flow/max-outflow net from)
           init-flow (min (flow/max-inflow net to) dummy-cap)]
       (-> net 
           (flow/-conj-cap-arc from to init-cost dummy-cap)
           (flow/-update-edge  from to #(flow/inc-flow % init-flow)))))
  ([net from to] (augmented-network net from to posinf)))

;;Edge Partitioning
;;=================
;;Edge partitions are simple data structures that support queries on 
;;edges based on an edge's membership in either the basis tree set,
;;the zero-flow or lower set, and the capacitated or upper set. 
;;We will store indices here.  The edge partition handles the 
;;lower level drudgery of moving edges between partitions as we pivot.

(defrecord transient-edge-partition 
    [^java.util.HashSet basic 
     ^java.util.HashSet lower 
     ^java.util.HashSet upper]
  IEdgePartition 
  (basic-edge [p n] (do (.add basic n)
                        (if (.contains lower n) 
                            (.remove lower n) 
                            (.remove upper n))
                        p))
  (lower-edge [p n] (do (.remove basic n)
                        (.add lower n)
                        p))
  (upper-edge [p n]  (do (.remove basic n)
                         (.add    upper n)
                         p))
  (get-basic-edges [p] basic)
  (get-lower-edges [p] lower)
  (get-upper-edges [p] upper)
  (get-nonbasic-edges [p] (concat lower upper))
  (basic? [p e]           (.contains basic e)))

;;WIP
(comment 

(defrecord linear-edge-partition
    [^java.util.ArrayList items 
     ^long last-basic 
     ^long last-lower 
     ^long last-upper]
  IEdgePartition 
  (basic-edge [p n] (do (.add basic n)
                        (if (.contains lower n) 
                            (.remove lower n) 
                            (.remove upper n))
                        p))
  (lower-edge [p n] (do (.remove basic n)
                        (.add lower n)
                        p))
  (upper-edge [p n]  (do (.remove basic n)
                         (.add    upper n)
                         p))
  (get-basic-edges [p] basic)
  (get-lower-edges [p] lower)
  (get-upper-edges [p] upper)
  (get-nonbasic-edges [p] (concat lower upper))
  (basic? [p e]           (.contains basic e)))
)

;;Exchanging an edge in a partition is simply swapping it out from 
;;the basis and pushing it either into the upper or lower set
;;depending on whether the edge is empty.
(defn swap-edge [edge-part entering leaving empty-edge?]
  (-> (if empty-edge?
        (lower-edge edge-part leaving)
        (upper-edge edge-part leaving))
      (basic-edge entering)))

;;Split a seq of edges into a partitioning based on presence in
;;the basic spanning tree spt.
(defn partition-edges 
  ([es spt part]
     (let [next-count (let [counter (atom 0)]
                        (fn [] (let [c @counter]
                                 (do (swap! counter inc)
                                     c))))]
       (reduce (fn [acc e]
                 (let [idx (next-count)]
                   (cond  (in-basis? spt e)              (basic-edge acc idx)
                          (zero? (flow/edge-flow e))     (lower-edge acc idx)
                          :else                          (upper-edge acc idx))))            
               part
               es)))
  ([es spt] (partition-edges es spt 
              (->transient-edge-partition (java.util.HashSet.) 
                                          (java.util.HashSet.) 
                                          (java.util.HashSet.)))))

;;Simplex Initialization
;;======================
(defn init-potentials 
  ([root preds costf init-cost]
     (persistent! (update-all-potentials! preds costf (transient {root init-cost}))))
  ([root preds costf] 
     (init-potentials root preds costf neginf)))

(defn init-simplex 
  ([net from to dummycost]
     (let [idx       (atom 0)
           augnet    (flow/edge-map (fn [e] 
                                      (let [i @idx
                                            _ (swap! idx inc)]
                                        (flow/set-data e i))) 
                                    (augmented-network net from to dummycost))
           [augnet nodes!] 
                     (reduce-kv (fn [[g idxs] nd data] 
                                  [(graph/relabel-node g nd (count idxs))
                                   (conj! idxs nd)])
                                [augnet (transient [])] (graph/nodes augnet))
           node-map  (persistent! nodes!)
           spanning  (residual-spanning-tree augnet from to  )
           pots      (init-potentials to spanning (fn [from to] (flow/-flow-weight augnet from to)) (- dummycost))
           edges     (into [] (flow/einfos augnet))
           costs     (mapv #(flow/-flow-weight augnet (flow/edge-from %) (flow/edge-to %)) edges)
           part      (partition-edges spanning edges)]
       (->simplex-net augnet node-map edges from to spanning  pots part costs)))
  ([net from to] (init-simplex net from to posinf)))

;;There's probably a nice way to abstract this out, pivot rules and
;;whatnot, but we'll do that later.  We're selecting a random eligible
;;arc here.
(defn find-entering-edge! [smplx]  
  (let [^java.util.ArrayList es (eligible-edges! smplx)]    
    (.get es (rand-int (.size es)))))

(defn find-entering-edge [smplx] (rand-nth (eligible-edges smplx)))


;;The result of a pivot should be the updated simplex, and a leaving arc.
(defn pivot [smplx entering leaving]
  
)


;;Can we traverse lower and upper arcs? 
;;Lower arcs are arcs not on the basis that have no flow.
;;Upper arcs are arcs not on the basis that have no capacity.

;;If implement Danztig's rule, we scan all the arcs for the greatest 
;;reduced price.  Note -> we have to do that anyway to determine
;;optimality.

;;Another option is to maintain two arc sets.  
;;Since the residual network is stored implicitly, we can also 
;;store our bounded arcs implicitly.  Either that, or we 
;;can directly link to them.

;;Since we only maintain forward arcs for the undirected network
;;explicitly, backward (or residual) arcs are implied.  We store
;;keys that assoc to them when they exist, but the pointer goes to the 
;;forward arc. 

;;So all arc modifications are done on forward arcs.  The API takes 
;;care of understanding what it means to push a directional flow 
;;along an arc (i.e. it will infer that the arc is backward based on 
;;the direction we're going).

;;So we can push arcs between L and U. 
;;We can maintain three sets of forward arcs, T, L, U.
;;Arcs in T are on the basis, (forward and residual)
;;Arcs in L are zero-flow, off the basis. (not residual)
;;Arcs in U are full-flow, off the basis. (not residual)

;;Prior to pivoting, we need to identify the entering arc.
;;Assume we can do this...(get-entering-arc s) 
;;Then we identify the cycle.... 
;;(find-cycle (get-entering-arc s) (get-spt s))
;;Then we augment along the cycle, returning an augmentation:
;;(augment-cycle s (find-cycle (get-entering-arc s) (get-spt s)))
;;=> {:augmented-flow n :entering some-arc :leaving some-arc :net altered-network}


;;So, the first cut is to collect arcs that are off the basis.
;;We do this when we initially build the simplex.
;;traverse the SPT and collect arcs that are off the basis.
;;Initially, all non-basic arcs will be in L (zero flow). 

;;The only changes to membership between T, L, and U will happen
;;after augmentation, when an entering and leaving arcs are 
;;identified. 

;;One idea is to uniquely number all arcs, and only shift the 
;;ints around.  We would need to store the int->arc mapping.
;;This would allow us to have a nice hash-map though...


;; (defn pivot [tr entering-arc]
;;   (let [new-tree 
;;         [new-tree leaving-arc] ]
;;   ))

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



(def aug-net (augmented-network the-net 0 5))

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

;;a simple simplex initializer for data from Sedgewick's book:
(def seg-simplex 
  (let [idx       (atom 0)
        augnet    (flow/edge-map (fn [e] 
                                   (let [i @idx
                                         _ (swap! idx inc)]
                                     (flow/set-data e i))) 
                                 (augmented-network the-net 0 5 9))
        [augnet nodes] 
                  (reduce-kv (fn [[g idxs] nd data] 
                               [(graph/relabel-node g nd (count idxs))
                                (conj! idxs nd)])
                             [augnet (transient [])] (graph/nodes augnet))
        node-map  (persistent! nodes)
        spanning  seg-preds
        pots      (init-potentials 5 spanning (fn [from to] (flow/-flow-weight augnet from to)) -9)
        edges     (into [] (flow/einfos augnet))
        costs     (mapv #(flow/-flow-weight augnet (flow/edge-from %) (flow/edge-to %)) edges)
        part      (partition-edges  edges spanning)]
    (->simplex-net augnet node-map edges 0 5 spanning pots part  costs)))


;;OBE
;;=======

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


;;Pending performance implementation.  May not need it.
(comment 

(defrecord edge-partition [basic lower upper]
  IEdgePartition 
  (basic-edge [p n] 
    (if (contains? lower n) 
      (edge-partition. (conj basic n)
                       (disj lower n) 
                       upper)
      (edge-partition. (conj basic n)
                       lower
                       (disj upper n))))
  (lower-edge [p n] 
    (edge-partition. (disj basic n) (conj lower n) upper))
  (upper-edge [p n]  
    (edge-partition. (disj basic n) lower (disj upper  n)))
  (get-basic-edges [p] basic)
  (get-lower-edges [p] lower)
  (get-upper-edges [p] upper)
  (get-nonbasic-edges [p] (clojure.set/union lower upper)))

(def ^:constant +basic+ 0)
(def ^:constant +lower+ -1)
(def ^:constant +upper+ 1)

;;this is ass, but fastish.
(defn filter-longs [pred ^longs a]
  (let [res  (java.util.ArrayList.)]
    (areduce a idx acc res (let [v (aget a idx)] (if (pred v) (doto acc (.add v)) acc)))))

(defn where-flag [flag xs]
  (filter-longs #(== flag %) xs))

(defn except-flag [flag xs]
  (filter-longs #(not (== flag %)) xs))

(defrecord array-edge-partition  [^longs edges]
  IEdgePartition 
  (basic-edge [p n] (do (aset  flags n ^long +basic+) p))
  (lower-edge [p n] (do (aset  flags n ^long +lower+) p))
  (upper-edge [p n] (do (aset  flags n ^long +upper+) p))
  (get-basic-edges [p] (where-flag  +basic+  flags))
  (get-lower-edges [p] (where-flag  +lower+ flags))
  (get-upper-edges [p] (where-flag  +upper+ flags))
  (get-nonbasic-edges [p] (except-flag +basic+ edges)))
)


(comment 


(def the-arcs2
  [[:zero  :one    3 3 0]
   [:zero  :two    3 1 0]
   [:one   :three  2 1 0]
   [:one   :four   2 1 0]
   [:two   :three  1 4 0]
   [:two   :four   2 2 0]
   [:three :five   2 2 0]
   [:four  :five   2 1 0]])

(def the-net2 (reduce (fn [acc [from to cap cost flow]]
                          (let [n (flow/-conj-cap-arc acc from to cost cap)]
                            (if (zero? flow)  n
                                (flow/-push-flow n (flow/-edge-info n from to) flow))))
                     flow/empty-network the-arcs2))

(def seg-preds2  {:zero :five 
                 :one :zero 
                 :two :zero 
                 :three :one 
                 :four :one 
                 :five :five})
(def seg-simplex2 
  (let [idx       (atom 0)
        augnet    (flow/edge-map (fn [e] 
                                   (let [i @idx
                                         _ (swap! idx inc)]
                                     (flow/set-data e i))) 
                                 (augmented-network the-net2 :zero :five 9))
        spanning  seg-preds2
        pots      (init-potentials :five spanning (fn [from to] (flow/-flow-weight augnet from to)) -9)
        edges     (into [] (flow/einfos augnet))
        costs     (mapv #(flow/-flow-weight augnet (flow/edge-from %) (flow/edge-to %)) edges)
        part      (partition-edges  edges spanning)]
    (->simplex-net augnet edges :zero :five spanning pots part  costs)))
)
