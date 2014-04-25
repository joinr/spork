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

(defprotocol IArrayNet
  (_flows      [n])
  (_capacities [n])
  (_costs      [n])
  (_n          [n])
  (_nodemap    [n]))


(defn ^long get-flow [a ^long from ^long to]
  (arr/deep-aget longs (_flows a) from to))

(defn set-flow! [a ^long from ^long to ^long flow]
  (do (arr/deep-aset longs (_flows a) from to flow)
      a))

(defn ^long get-capacity [a ^long from ^long to]
  (arr/deep-aget longs (_capacities a) from to))

(defn  set-capacity! [a ^long from ^long to ^long cap]
  (do (arr/deep-aset longs (_capacities a) from to cap)
      a))

(defn  array-inc-flow! [a ^long from ^long to ^long flow]
  (let [flows      (_flows a)
        capacities (_capacities a)]
    (do (arr/deep-aset longs flows from to (unchecked-add (arr/deep-aget longs flows from to) flow))
        (arr/deep-aset longs capacities from to (unchecked-subtract (arr/deep-aget longs capacities from to) flow))
        a)))

(defn  array-dec-flow! [a ^long from ^long to ^long flow]
  (let [flows (_flows a)
        capacities (_capacities a)]
    (do (arr/deep-aset longs flows from to (unchecked-subtract (arr/deep-aget longs flows from to) flow))
        (arr/deep-aset longs capacities from to (unchecked-add (arr/deep-aget longs capacities from to) flow))
        a)))

;  (let [a (with-meta axp {:tag 'spork.cljgraph.arrayflow.IArrayNet})]
(defmacro do-edges [arr a i j expr]
  `(let [~a ~arr
         bound# (_n ~a)]
     (loop [~i 0]
       (if (== ~i bound#) ~a
           (do (loop [~j 0]
                 (if (== ~i ~j)        (recur (unchecked-inc ~j))
                     (if (== ~j bound#) nil
                         (do ~expr 
                             (recur (unchecked-inc ~j))))))
               (recur (unchecked-inc ~i)))))))

(defn reset-flow! [a]
  (let [flows (_flows a)
        capacities (_capacities a)]
    (do-edges a arr i j
              (let [flow (arr/deep-aget longs flows i j)]
                (when (pos?  flow)
                  (do (arr/deep-aset longs flows i j 0)
                      (arr/deep-aset longs capacities i j (unchecked-add (arr/deep-aget longs capacities i j) flow))))))))

(defn zero-flow! [a]
  (do-edges a arr i j (do (set-flow! arr i j 0))))


(defn disable-edge! [a ^long from ^long to]  
  (set-capacity! a from to neginf))

(defn enable-edge! [a ^long from ^long to]  
  (set-capacity! a from to posinf))

(defn enabled? [a ^long from ^long to]
   (not (neg?  (arr/deep-aget longs (_capacities a) from to))))  

(defn array-edge-info! [the-net i j]
  (spork.cljgraph.flow.einfo. i j 
       (get-capacity the-net i j) (get-flow the-net i j) (if (< i j) :increment :decrement)))

(defn get-edge-infos! [the-net]
  (for [i (range (_n the-net))
        j (range (_n the-net))
        :when (and (not= i j)) ]
    (spork.cljgraph.flow.einfo. i j 
       (get-capacity the-net i j) (get-flow the-net i j) :increment)))

(defn get-labeled-edge-infos! [the-net]
  (let [nm (_nodemap the-net)]
    (for [i (range (_n the-net))
          j (range (_n the-net))
          :when (and (not= i j) (> (get-flow the-net i j) 0))]
      (let [from (nm i)
            to   (nm j)]
        (spork.cljgraph.flow.einfo. from to 
           (get-capacity the-net i j) (get-flow the-net i j) :increment))))) 

(defn index-nodes [supply-net] 
  (object-array  (-> (graph/topsort-nodes supply-net))))

(declare array-sinks array-sources)

(defrecord array-net  [g ^objects nodes nodenum nodemap ^long n 
                         ^objects flows ^objects capacities ^objects costs scaling]
  IArrayNet
  (_flows      [net] flows)
  (_capacities [net] capacities)
  (_costs      [net] costs)
  (_n          [net] n)
  (_nodemap    [net] nodemap)
  flow/IFlowNet
  (-edge-info    [net from to] )
  (einfos [n]     (get-edge-infos! n)  )
  (-get-direction [net from to] (< from to))
  (-flow-weight   [net from to] (if (< from to) (arr/deep-aget longs costs from to)
                                    (- (arr/deep-aget longs costs from to))))
  (-set-edge [net edge]         
    (let [^spork.cljgraph.flow.IEdgeInfo e edge
          from (.from e)
          to   (.to   e)
          capacity (.capacity e)
          flow     (.flow e)]
      (do (arr/deep-aset longs capacities from to capacity)
          (arr/deep-aset longs flows from to flow)
          net)))                         
  (-flow-sinks     [net x] (array-sinks  net x))
  (-flow-sources   [net x] (array-sources net x))
  (-push-flow      [net edge flow] 
    (let [^spork.cljgraph.flow.IEdgeInfo e edge]
      (array-inc-flow! net (.from e) (.to e) flow)))
  flow/IDynamicFlow 
  (-conj-cap-arc [net from to w cap]
    (assert (and (contains? nodemap from)
                 (contains? nodemap to))
            (str "Nodes " [from to] " are unknown"))
    (let [^long i (get nodemap from)
          ^long j (get nodemap to)]
      (do (arr/deep-aset longs capacities i j cap)
          (arr/deep-aset longs costs      i j w)
          net)))         
  (-active_flows [net]
    (let [xs (java.util.ArrayList.)]
       (do-edges net arr i j 
                 (do (let [flow  (arr/deep-aget longs flows i j)]
                       (when (pos? flow)                           
                         (.add xs (spork.cljgraph.flow.einfo. i j 
                                  (arr/deep-aget longs capacities i j) 
                                  flow  :increment) )))))
       xs)))

(defn ^array-net clone-network [^array-net an]
  (array-net.  (.g an)
               (.nodes an)
               (.nodenum an)
               (.nodemap an)
               (.n an)
               (arr/clone-table longs (.flows an))
               (arr/clone-table longs (.capacities an))
               (arr/clone-table longs (.costs an))
               (.scaling an)))
   

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
    (do (doseq [^spork.cljgraph.flow.einfo e (flow/get-edge-infos supply-net)]
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
                     costs
                     nil))))

(defn ^long array-flow-weight [^array-net a ^long from ^long to]
  (if (< from to)
      (arr/deep-aget longs (.costs a) from to)
      (unchecked-negate    (arr/deep-aget longs (.costs a) to from))))

;;returns neighbors which have flow.
;;since node indices are top sorted, we only have to 
;;check if (< from to)
(defn ^java.util.ArrayList array-flow-neighbors [^array-net a ^long v]
  (let [flows (_flows a)
        capacities (_capacities a)
        bound (.n a)
        acc   (java.util.ArrayList.)]
    (if-let [scaling (.scaling a)]
      ;;scaled
      (do 
        (loop [to 0] ;forward arcs
          (when (not (== to bound))
            (if (== to v) (recur (unchecked-inc to))
                (do (when (> (flow/scale scaling (arr/deep-aget longs capacities v to)) 0)
                      (.add acc to))
                    (recur (unchecked-inc to))))))
        (loop [from 0]
          (when (not (== from bound))
            (if (== from v) (recur (unchecked-inc from))
                (do (when (and (not (zero? (flow/scale scaling (arr/deep-aget longs flows from v))))
                               (not (neg?  (flow/scale scaling (arr/deep-aget longs capacities from v)))))
                      (.add acc from))
                    (recur (unchecked-inc from))))))
        acc)
      ;;unscaled
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
                               (not (neg?  (arr/deep-aget longs capacities from v))))
                      (.add acc from))
                    (recur (unchecked-inc from))))))
        acc))))

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
  (newPath [state ^long source ^long sink ^long w]
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

(defn ^arraysearch empty-search [^array-net net ^long startnode ^long endnode fringe]
  (let [knowns (boolean-array (.n net))]
    (do (aset knowns 0 true)
        (arraysearch. startnode
                      endnode
                      (long-array (.n net))
                      (long-array (.n net))
                      knowns
                      fringe
                      (.nodemap net)))))

(defn ^arraysearch flow-relax
  "Given a shortest path map, a distance map, a source node, sink node, 
   and weight(source,sink) = w, update the search state.
   
   Upon visitation, sources are conjoined to the discovered vector.

   The implication of a relaxation on a sink, relative to the source, is that 
   source no longer exists in the fringe (it's permanently labeled).

   So a relaxation can mean one of three things:
   1:  sink is a newly discovered-node (as a consequence of visiting source);
   2:  sink was visited earlier (from a different source), but this visit exposes 
       a shorter path to sink, so it should be elevated in consideration in 
       the search fringe.
   3:  sink is a node of equal length to the currently shortest-known path from
       an unnamed startnode.  We want to record this equivalence, which means
       that we may ultimately end up with multiple shortest paths."
  [^arraysearch state ^long w ^long source ^long sink]
  (let [relaxed (unchecked-add (.bestKnownDistance state source) w)]
    (if-let [known (.bestKnownDistance state sink)]
      (if (< relaxed known) (.shorterPath state source sink relaxed)
          state)
      (.newPath state source sink relaxed))))
                     
                                                                        
(defn array-flow-traverse 
    "Custom function to walk an array-backed flow network.  Using a custom search state."
    [^array-net g ^long startnode ^long targetnode fringe]
    (let [initsearch (empty-search g startnode targetnode fringe)]
      (loop [^arraysearch state (.conj-fringe ^arraysearch initsearch startnode 0)]
      (if-let [source (.next-fringe state)] ;next node to visit
        (let [visited (.pop-fringe state)] ;record visit
          (if (== targetnode source) visited 
              (recur (let [^java.util.ArrayList xs (array-flow-neighbors g source)
                           n (.size xs)]
                       (loop [acc visited
                              idx 0]
                         (if (== idx n) acc
                             (recur (flow-relax state (array-flow-weight g source (.get xs idx))
                                                source (.get xs idx))
                                    (unchecked-inc idx))))))))
        state))))

(defn first-path [^arraysearch a]
  (let [^long target (:targetnode a)]
    (when (generic/best-known-distance a (:targetnode a))
      (let [^long source (:startnode a)
            ^longs spt   (:shortest a)]
        (loop [idx target
               acc '()]
          (if (== idx source) (cons source acc)
              (recur (aget spt idx)
                     (cons idx acc))))))))

(definline array-mincost-aug-path [g from to]
  `(first-path (array-flow-traverse ~g ~from ~to (java.util.PriorityQueue.))))

(defn ^long array-max-flow 
 ( [^objects flows ^objects capacities ^longs p]
     (let [bound (alength p)]
       (loop [to 1
              flow flow/posinf]
         (if (== to bound) flow
             (let [l (aget p (unchecked-dec to))
                   r (aget p to)]
               (recur (unchecked-inc to)
                      (min (if (< l r) (arr/deep-aget longs capacities l r)
                               (arr/deep-aget longs flows r l))
                           flow)))))))
  ( [^objects flows ^objects capacities ^longs p scaling]
     (let [bound (alength p)]
       (loop [to 1
              flow flow/posinf]
         (if (== to bound) (flow/unscale scaling flow)
             (let [l (aget p (unchecked-dec to))
                   r (aget p to)]
               (recur (unchecked-inc to)
                      (min  (if (< l r) (long (flow/scale scaling (arr/deep-aget longs capacities l r)))
                                        (long (flow/scale scaling (arr/deep-aget longs flows r l))))
                            flow))))))))

;;There's an optimization here.  WE can avoid the intermediate
;;long-array conversion cost if we use the list methods.
(defn array-augment-flow [^array-net the-net p]
  (let [^longs xs (long-array p)
        ^objects flows (.flows the-net)
        ^objects capacities (.capacities the-net)
         flow  (if-not  (.scaling the-net)
                 ^long (array-max-flow flows capacities xs)
                 ^long (array-max-flow flows capacities xs (.scaling the-net)))                 
        n (unchecked-dec (alength xs))]
    (do (loop [idx 0]
          (if (== idx n) the-net
              (let [from (aget xs idx)
                    to   (aget xs (unchecked-inc idx))]
                (if (< from to)
                  (do (array-inc-flow! the-net from to ^long flow)
                      (recur (unchecked-inc idx)))
                  (do (array-dec-flow! the-net to from ^long flow)
                      (recur (unchecked-inc idx)))))))
        the-net)))       

(defn array-mincost-flow! [^array-net the-net ^long from ^long to]
  (loop [acc the-net]
    (if-let [p (array-mincost-aug-path acc from to)]
      (recur (array-augment-flow acc p))
      acc)))

(defn array-mincost-flow [^array-net the-net ^long from ^long to]
  (loop [acc (clone-network the-net)]
    (if-let [p (array-mincost-aug-path acc from to)]
      (recur (array-augment-flow acc p))
      acc)))

(defn augmentations [n ^array-net the-net ^long from ^long to]
  (let [res (java.util.ArrayList.)
        nm  (:nodemap the-net)]
    (loop [acc the-net]
      (if-let [p (array-mincost-aug-path acc from to)]
        (let [f (array-max-flow (:flows acc) (:capacities acc) (long-array p))
              _ (.add res [f (map (fn [n] [n (nm n)]) p)])]
          (if (> (count res) n)
              [res acc]
              (recur (array-augment-flow acc p))))
        [res acc]))))

(defmacro with-scaled-arrayflow [scalar binding & body]
  (let [k (first binding) 
        v (second binding)]
    `(let [~k (assoc ~v :scaling (flow/as-scale ~scalar))]
       ~@body)))

(defmacro with-mutable-copy [binding & body]
  (let [k (first binding)
        v (second binding)]
    `(let [~k (clone-network ~v)]
       (let [res# ~@body]
         (do (reset-flow! ~k)
             res#)))))

      

