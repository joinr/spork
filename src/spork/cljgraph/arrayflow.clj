(ns spork.cljgraph.arrayflow
  (:require [spork.cljgraph [core :as graph]
                            [flow :as flow]
                            [jungapi :as viz]]
            [spork.protocols [core :as generic]]
            [spork.data      [searchstate :as searchstate]
                             [mutable :as m]]
            [spork.util      [array :as arr]]))

;;we can cheat here and use a really small array as our flow
;;network.  This should allow us cheap updates for dynamic arcs..
;;The majority of our structure is actually a static array.
;;The supply network is effectively static.
;;Our network will have the same topological ordering of nodes.
;;Nodes [0..k] will always be the same supply nodes.
;;Node k + 1 will be the SRC node
;;and node k + 2 will always be the fill node.
;;Thus, we can define a dynamic graph and use mutation to get a huge
;;speed boost.  Plus, neighbor searches should be friggin' fast.

(def ^:const posinf Long/MAX_VALUE)
(def ^:const neginf Long/MIN_VALUE)

(defn index-nodes [supply-net] 
  (object-array  (-> (graph/topsort-nodes supply-net))))

(defrecord array-net  [g ^objects nodes nodenum nodemap ^long n 
                       ^objects flows ^objects capacities ^objects costs])

(defn ^array-net net->array-net
  "Create a mutable, array-backed network that can be efficiently searched and 
   mutated.  Highly useful for small dynamic networks.  Creates a mapping of nodes 
   to indices.  Internal graph operations are performed on primitive arrays."
  [supply-net]
  (let [^objects node-template (index-nodes supply-net)
        nodemap (into {} (map-indexed vector node-template))
        node->num (into {} (map-indexed (fn [i r] [r i]) node-template))
        nodes     (alength node-template)
        fill-node  (unchecked-dec nodes)
        demand-node  (unchecked-dec fill-node)
        flows        (arr/longs-2d nodes nodes)
        caps         (arr/longs-2d nodes nodes)
        costs        (arr/longs-2d nodes nodes)]
    (do (doseq [spork.cljgraph.flow.einfo e (flow/get-edge-infos supply-net)]
          (let [from (get node->num (.from e))
                to   (get node->num (.to e))
                cost (generic/-arc-weight supply-net (.from e) (.to e))]
            (do (arr/deep-aset longs flows from to (.flow e))
                (arr/deep-aset longs caps  from to (.capacity e))
                (arr/deep-aset longs costs from to cost))))
        (->array-net supply-net
                     node-template
                     node->num
                     nodemap
                     nodes
                     flows
                     caps
                     costs))))

(defn ^long array-flow-weight [^array-net a ^long from ^long to]
  (if (< from to)
      (arr/deep-aget longs (.coststs a) from to)
      (unchecked-negate    (arr/deep-aget longs (.costs a) to from))))

;;returns neighbors which have flow.
;;since node indices are top sorted, we only have to 
;;check if (< from to)
(defn ^java.util.ArrayList array-flow-neighbors [^array-net a ^long v]
  (let [flows (.flows a)
        capacities (.capacities a)
        bound (.n a)
        acc   (java.util.ArrayOist.)]
    (do 
      (loop [to 0] ;forward arcs
        (when (not (== to bound))
          (if (== to v) (recur (unchecked-inc to))
              (do (when (> (arr/deep-aget longs capacities v to) 0)
                    (.add acc to))
                  (recur (unchecked-inc to))))))
      (loop [from 0]
        (when (not (== from bound))
          (if (== from v) (recur (unchecked-inc from))
              (do (when (and (not (zero? (arr/deep-aget longs flows from v)))
                             (pos? (arr/deep-aget longs capacities from v)))
                    (.add acc from))
                  (recur (unchecked-inc from))))))
      acc)))

(definterface IFlowSearch
  (newPath   [^long source ^long sink ^long w])
  (shorterPath [^long source ^long sink ^long wnew])
  (bestKnownDistance [^long source]))

(m/defmutable arraysearch [^long startnode ^long targetnode ^longs shortest ^longs distance 
                           ^booleans known fringe nodemap]
  generic/IGraphSearch
  (new-path [state source sink w]
            (let [^long source source
                  ^long sink sink
                  ^long w w]
              (do (aset known sink true)
                  (aset shortest sink source)
                  (aset distance sink w)
                  (set! fringe (generic/conj-fringe fringe sink w))
                  state)))
  (shorter-path [state source sink wnew wpast]
                (let [^long source source
                      ^long sink sink
                      ^long wnew wnew]
                  (do (aset shortest sink source)
                      (aset distance sink wnew)
                      (set! fringe (generic/conj-fringe fringe sink wnew))
                      state)))
  (equal-path [state source sink] state)
  (conj-visited [state source] state)
  (best-known-distance [state x] (if (aget known x) (aget distance x) nil))
  IFlowSearch
  (newpath [state ^long sink ^long w]
           (do (aset known sink true)
               (aset shortest sink source)
               (aset distance sink w)
               (set! fringe (generic/conj-fringe fringe sink w))
               state))
  (shorterPath [state ^long source ^long sink ^long wnew]
               (do (aset shortest sink source)
                   (aset distance sink wnew)
                   (set! fringe (generic/conj-fringe fringe sink wnew))
                   state))
  (bestKnownDistance [state ^long x] (if (aget known x) (aget distance x) nil))
  generic/IFringe
  (conj-fringe [this n w] (do (set! fringe (generic/conj-fringe fringe n w)) this))
  (next-fringe [this]     (generic/next-fringe fringe))
  (pop-fringe [this]      (do (set! fringe (generic/pop-fringe fringe)) this)))
                    
                             
                
        
                      
              
             
