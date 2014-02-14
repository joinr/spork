;;A library for apply various types of network flow 
;;algorithms, as well as defining networks.  Canonical 
;;implementations of augmenting path algorithms, as well
;;as mincost flow algorithms are included.
(ns spork.cljgraph.flow
  (:require [spork.cljgraph [core :as graph]
                            [search :as search]]
            [spork.data [searchstate :as searchstate]]
            [spork.protocols [core :as generic]]
            [spork.util.general :refer [assoc2 get2]]))

(def posinf Long/MAX_VALUE)

;; (defprotocol IFlowNetwork
;;   (-inc-flow [net from to amt])
;;   (-dec-flow [net from to amt]))

;;Flows and Augmenting Paths
;;==========================

;;Network Flow data is stored as extra data in a 
;;digraph.  The API will be generalized behind a 
;;protocol, but for now this suffices, and it 
;;works on the existing digraph structure.

;A simple set of helper functions that let us embed flow data
;;in a spork.data.digraph record.

(def empty-network (assoc graph/empty-graph :flow-info {}))
;(defrecord edge-info [from to capacity flow])

(defrecord einfo [from to capacity flow dir])

;;FYI, the varargs here are killing us.
;; (defn ->edge-info 
;;   [from to & {:keys [capacity flow] :or {capacity posinf flow 0}}]
;;   (einfo. from  to capacity  flow))


;;These are each 10x faster.  
(definline ->edge-info2 
  [from to]
  `(einfo. ~from  ~to posinf 0 :increment))

(definline ->edge-info4 
  [from to capacity flow]
  `(einfo. ~from ~to ~capacity ~flow :increment))

;;Another performance killer; Using vector keys is cool and all, 
;;but it kills our performance on lookups since we have to go for 
;;a vector equiv.  It's more performant, for hashing, to have nested 
;;vectors of single keys.
;; (definline edge-info [g from to]
;;   `(get (:flow-info ~g) [~from ~to] (->edge-info2 ~from ~to)))

(definline edge-info [g from to]
  `(get2 (:flow-info ~g) ~from ~to (->edge-info2 ~from ~to)))

(definline edge-info2 [g from to]
  `(get2 (:flow-info ~g) ~from ~to (->edge-info2 ~from ~to)))

;;OPTIMIZE
;; (defn edge-info [g from to]
;;   (get-in g [:flow-info [from to]] (->edge-info from to)))

;; (defn edge-info [g from to]
;;   (get-in g [:flow-info [from to]] (->edge-info from to)))

;; (definline update-edge*  
;;   [g from to cap flow]
;;   `(assoc ~g :flow-info                  
;;      (assoc 
;;          (get ~g :flow-info {})
;;        [~from ~to] (einfo. ~from ~to ~cap ~flow :increment))))

;;This should be a bit faster.  We can get even faster if we 
;;insert some kind of mutable record container.
(definline update-edge2*  
  [g from to cap flow]
  `(assoc ~g :flow-info                  
     (assoc2 (get ~g :flow-info {})
       ~from ~to 
       (einfo. ~from ~to ~cap ~flow :increment))))

(defmacro alter-edge [sym g from to & expr]
  `(let [~sym (edge-info2 ~g ~from ~to)]
     (assoc ~g :flow-info
            (assoc2 (get ~g :flow-info {})
                    ~from ~to 
                    ~@expr))))    

;;->edge-info is called a lot here.
;; (defn update-edge 
;;   ([g from to cap flow ]
;;      (assoc g :flow-info                  
;;        (assoc 
;;          (get g :flow-info {})
;;            [from to] (einfo. from to cap flow :increment))))
;;   ([g from to m] 
;;     (assoc-in g [:flow-info [from to]] (merge (edge-info g from to) m))))

;; (defn update-edge 
;;   ([g from to flow cap]
;;     (assoc-in g [:flow-info [from to]] 
;;               (->edge-info from to :capacity cap :flow flow)))
;;   ([g from to m] 
;;     (assoc-in g [:flow-info [from to]] (merge (edge-info g from to) m))))

(defn current-capacity 
  ([info] (- (:capacity info) (:flow info)))
  ([g from to] (current-capacity (edge-info g from to))))

(defn set-capacity [g from to cap] 
  (alter-edge the-edge g from to 
    (assoc the-edge :capacity cap)))

(defn set-flow [g from to flow]    
  (alter-edge the-edge g from to 
     (assoc the-edge :flow flow)))

;;Hold off on this...implementation may be faulty.
(defn swap-capacities [net l c r] net)
(definline forward?   [g from to] `(contains? (get (:sinks ~g) ~from) ~to))
(definline forward2?  [g info] `(forward? ~g (:from ~info) (:to ~info)))

(defn forwardizer [g]
  (memoize (fn [from to] (contains? (get (:sinks g) from) to))))

;;Possible bottleneck here.
;optimized
(defn inc-flow 
  ([g info flow]     
     (assoc g :flow-info
            (assoc2 (:flow-info g) (:from info) (:to info)  
                    (-> info 
                        (assoc :capacity (- (:capacity info) flow))
                        (assoc :flow (+ (:flow info) flow))))))
  ([g from to flow] 
     (alter-edge the-edge g from to 
         (-> the-edge 
             (assoc :capacity (- (:capacity the-edge) flow))
             (assoc :flow     (+ (:flow the-edge) flow))))))

;optimized
(defn dec-flow 
  ([g info flow]
     (assoc g :flow-info
            (assoc2  (:flow-info g)  (:from info) (:to info) 
                     (-> info
                         (assoc :capacity (+ (:capacity info) flow))
                         (assoc :flow     (- (:flow info) flow))))))
  ([g from to flow] 
     (alter-edge the-edge g from to
         (-> the-edge
             (assoc :capacity (+ (:capacity the-edge) flow))
             (assoc :flow     (- (:flow the-edge) flow))))))

(defn flows [g] 
  (for [[k v] (:flow-info g)] [k (select-keys v [:capacity :flow])]))

(defn active-flows [g] 
  (reduce (fn [acc [k info]] (if (> (:flow info) 0)
                               (assoc acc k (:flow info)) acc)) 
          {} (:flow-info g)))

(defn total-flow 
  ([g active-edges] 
    (->> active-edges 
      (filter (fn [[k v]] (graph/terminal-node? g (second k))))
      (map second)
      (reduce + 0)))
  ([g] (total-flow g (active-flows g))))

(defn flow-provider-type [g nd]
  (if (not (graph/island? g nd))
      (cond (graph/terminal-node? g nd) :sinks
            (empty? (graph/sinks g nd))  :source
            :else :trans)
      :island))

(defn flow-topology [g start-node]
  (group-by (partial flow-provider-type g)
            (graph/succs g start-node)))

;;refactored to eliminate reduce and destructuring.
(defn total-cost 
  ([g active-edges]
    (generic/loop-reduce 
     (fn [acc info]
       (let [flow (second info)
             from (first (first info))
             to   (second (first info))]
         (+ acc (* flow (graph/arc-weight g from to)))))
            0 active-edges))
  ([g] (total-cost g (active-flows g))))

;; (defn total-cost 
;;   ([g active-edges]
;;     (generic/loop-reduce 
;;      (fn [acc [[from to] flow]]       
;;        (+ acc (* flow (graph/arc-weight g from to))))
;;             0 active-edges))
;;   ([g] (total-cost g (active-flows g))))

;add a capacitated arc to the graph
(defn conj-cap-arc [g from to w cap]
  (let [finfo (:flow-info g)]
    (-> (graph/conj-arc g from to w)
        (assoc :flow-info finfo)
        (update-edge2* from to cap 0))))

;;Probable hotspot in at least one use case.  We add arcs to the
;;network repeatedly...calls to merge and reduce and destructuring 
;;will slow us down.


;;THIS IS BROKE!  
;;this is a hacked way to go
;;add multiple capacitated arcs to the network.
;; (defn conj-cap-arcs [g arcs]
;;   (let [finfo (:flow-info g)]
;;     (->> arcs 
;;         (reduce 
;;           (fn [[gr flows] [from to w cap]]  [(graph/conj-arc gr from to w) 
;;                                              (update-edge flows from to 0 cap)])
;;              [g {:flow-info finfo}])
;;         (apply merge))))

(defn conj-cap-arcs [g arcs]
  (reduce (fn [gr [from to w cap]]  (conj-cap-arc  gr from to w cap)) g arcs))
       

;;Original version of flow neighbors.  Trying to trim costs for
;;neighbor lookup.
;; (defn flow-neighbors 
;;   [g v & args]
;;   (let [info (partial edge-info g)
;;         capacity (fn [to]   (:capacity (info v to)))
;;         flow     (fn [from] (:flow (info from v)))]
;;     (concat 
;;       (filter (fn [to]   (> (capacity to) 0)) (graph/sinks g v))
;;       (filter (fn [from] (> (flow from)   0)) (graph/sources g v)))))

;;bi-directional flow neighbors.  We allow all forward neighbors with untapped 
;;capacity, and allow any backward neighbors with flow.
(defn flow-neighbors 
  [g v _]
  (let [xs (atom (transient []))]
    (do (doseq [to (graph/sinks g v)]
          (when (> (:capacity (edge-info g v to)) 0) 
            (reset! xs (conj! @xs to) )))
        (doseq [from (graph/sources g v)]
          (when (> (:flow (edge-info g from v)) 0)
            (reset! xs (conj! @xs from))))
        (persistent! @xs))))

(defmacro flow-neighbors! 
  [g v & rest]
  `(let [xs# (atom (transient []))]
     (do (doseq [to# (graph/sinks ~g ~v)]
           (when (> (:capacity (edge-info ~g ~v to#)) 0) 
             (reset! xs# (conj! @xs# to#) )))
         (doseq [from# (graph/sources ~g ~v)]
           (when (> (:flow (edge-info ~g from# ~v)) 0)
             (reset! xs# (conj! @xs# from#))))
         (persistent! @xs#))))

;;Eliminated the varargs
(definline flow-neighbors!! 
  [g v]
  `(concat 
    (filter (fn [to#]   (> (:capacity (edge-info ~g ~v to#)) 0)) (graph/sinks ~g ~v))
    (filter (fn [from#] (> (:flow (edge-info ~g from# ~v))   0)) (graph/sources ~g ~v))))



(defn flow-neighbors!!! 
  ^java.util.ArrayList [g v]     
   (let [^java.util.ArrayList acc (java.util.ArrayList.)]
     (loop [xs (generic/-get-sinks g v)]
       (if-let [to (first xs)]
         (do (when (> (.capacity ^einfo (edge-info g v to)) 0) (.add acc to))
             (recur (rest xs)))))
     (loop [xs (generic/-get-sources g v)]
       (if-let [from (first xs)]
         (do (when (> (.flow ^einfo (edge-info g from v))   0)  (.add acc from))
             (recur (rest xs)))))
     acc))


;; (defn flow-neighbors!!! 
;;   ^java.util.ArrayList [g v]     
;;    (let [^java.util.ArrayList acc (java.util.ArrayList.)]
;;      (loop [xs (generic/-get-sinks g v)]
;;        (if-let [to (first xs)]
;;          (do (when (> (:capacity (edge-info g v to)) 0) (.add acc to))
;;              (recur (rest xs)))))
;;      (loop [xs (generic/-get-sources g v)]
;;        (if-let [from (first xs)]
;;          (do (when (> (:flow (edge-info g from v))   0)  (.add acc from))
;;              (recur (rest xs)))))
;;      acc))

;;slow...
(definline flow-neighbors!!!!
  ^java.util.ArrayList [g v] 
  (let [acc (with-meta (gensym "acc") {:tag 'java.util.ArrayList})]
    `(let [~acc (java.util.ArrayList.)]
       (loop [xs# (graph/sinks ~g ~v)]
         (if-let [to# (first xs#)]
           (do (when (> (:capacity (edge-info ~g ~v to#)) 0) (.add ~acc to#))
               (recur (rest xs#)))))
       (loop [xs# (graph/sources ~g ~v)]
         (if-let [from# (first xs#)]
           (do (when (> (:flow (edge-info ~g from# ~v))   0)  (.add ~acc from#))
               (recur (rest xs#)))))
       ~acc)))

(defn flow-neighbors!!!!! 
  ^java.util.ArrayList [g v]     
   (let [^java.util.ArrayList acc (java.util.ArrayList.)]
     (do (let [^objects xs  (to-array (generic/-get-sinks g v))
               n (alength xs)]
           (loop [idx 0]
             (if (== idx n) nil                    
                 (let [to (aget xs idx)]
                   (do (when (> (:capacity (edge-info g v to) 0)) (.add acc to))
                       (recur (unchecked-inc idx)))))))
         (let [^objects xs  (to-array (generic/-get-sinks g v))
               n (alength xs)]
           (loop [idx 0]
             (if (== idx n) nil
                 (let [from (aget xs idx)]
                   (do (when (> (:flow (edge-info g from v))   0)  (.add acc from))
                       (recur (unchecked-inc idx)))))))
         acc)))
            
;; (definline flow-neighbors!! 
;;   [g v & xs]
;;   `(concat 
;;     (filter (fn [to#]   (> (:capacity (edge-info ~g ~v to#)) 0)) (graph/sinks ~g ~v))
;;     (filter (fn [from#] (> (:flow (edge-info ~g from# ~v))   0)) (graph/sources ~g ~v))))

;;this is a special walk for helping us with greedy flows, where we don't 
;;try to find an augmenting flow.
(defn forward-only-flow-neighbors 
  [g v _]
  (let [info (partial edge-info g)
        capacity (fn [to] (:capacity (info v to)))]
    (filter (fn [to]   (> (capacity to) 0)) (graph/sinks g v))))

;;the flow-cost for g from to.  Forward arcs are positive cost.
;;Backward arcs are negative-cost.
(definline flow-weight [g from to]
  `(if (forward? ~g ~from ~to) (graph/arc-weight ~g ~from ~to) ;forward arc
      (- (graph/arc-weight ~g ~to ~from))))

(definline flow-weight2 [g from to]
  `(if (forward? ~g ~from ~to) (generic/-arc-weight ~g ~from ~to) ;forward arc
      (- (generic/-arc-weight ~g ~to ~from))))

;;A function for caching flow weight calls.
(defn flow-weighter [g forward-pred]
  (memoize (fn [g from to] 
             (if (forward-pred g from to) (graph/arc-weight g from to) ;forward arc
                 (- (graph/arc-weight g to from))))))


;;A function for caching flow weight calls.
(defn flow-weighter2 [g forward-pred]
  (memoize (fn [g from to] 
             (if (forward-pred g from to) (generic/-arc-weight g from to) ;forward arc
                 (- (generic/-arc-weight g to from))))))
             

(defn flow-walk [g startnode endnode]
  (search/priority-walk g startnode :endnode endnode 
                        :weightf flow-weight :neighborf flow-neighbors))

(defn ford-fulkerson-walk [g startnode endnode]
  (search/depth-walk g startnode :endnode endnode :neighborf flow-neighbors))

(defn edmonds-karp-walk [g startnode endnode]
  (search/breadth-walk g startnode :endnode endnode :neighborf flow-neighbors))
(defn pushflow-walk [g startnode endnode]
  (search/priority-walk g startnode 
        :endnode endnode :neighborf forward-only-flow-neighbors))
(defn pushflow-aug-path [g from to]
  (first (graph/get-paths (pushflow-walk g from to))))
(defn maxflow-aug-path [g from to]
  (first (graph/get-paths (edmonds-karp-walk g from to))))

;;Changed from using flow-walk, due to overhead from function
;;invocation.  This guy gets called a lot.  Function overhead adds up.
;;Also, defwalk forms use merge internally...so runtime costs are
;;incurred in tight loops (i.e. lots of flow calcs).
(definline mincost-aug-path [g from to]
  `(first (graph/get-paths 
           (search/traverse ~g ~from ~to (searchstate/empty-PFS ~from)
                            :weightf flow-weight :neighborf flow-neighbors))))

;; (defn mincost-aug-path [g from to]
;;   (first (graph/get-paths 
;;           (search/traverse g from to (searchstate/empty-PFS from)
;;                            :weightf flow-weight :neighborf flow-neighbors))))

;convert a path into a list of edge-info 
;; (defn path->edge-info [g p]
;;   (map (fn [fromto]
;;          (let [from (first fromto)
;;                to   (second fromto)]
;;            (if (forward? g from to)
;;              (assoc (edge-info g from to) :dir :increment)
;;              (assoc (edge-info g to from) :dir :decrement))))
;;        (partition 2 1 p)))

;;optimized?
(defn path->edge-info [g p]
  (loop [acc []
         xs  p]
    (if (nil? (next xs)) acc
        (let [from (first  xs)
              to   (second xs)]
          (recur (conj acc (if (forward? g from to)
                             (assoc (edge-info g from to) :dir :increment)
                             (assoc (edge-info g to from) :dir :decrement)))
                 (next xs))))))
      
;; ;;slow, unoptimized
;; (defn path->edge-info [g p]
;;   (map (fn [fromto]
;;          (let [from (first fromto)
;;                to   (second fromto)]
;;            (if (forward? g from to)
;;              (assoc (edge-info g from to) :dir :increment)
;;              (assoc (edge-info g to from) :dir :decrement))))
;;        (partition 2 1 p)))

;;find the maximum flow that the path can support 
(defn maximum-flow [g infos]
  (loop [info (first infos)
         xs   (rest infos)
         flow posinf]
    (let [new-flow (if (= :increment (:dir info))
                       (:capacity info)
                       (:flow info))
          next-flow (min flow new-flow)]
      (if (empty? xs) next-flow
          (recur (first xs) (rest xs) next-flow)))))

;;Eliminate reduce.
;;apply an amount of flow along the path, dropping any nodes that 
;;become incapacitated.
(defn apply-flow [g edges flow]
  (generic/loop-reduce 
      (fn [gr info]
        (if (= :increment (:dir info))
          (inc-flow gr info flow)
          (dec-flow gr info flow)))
      g edges))

;;helper function to apply flow.
(defn augment-flow [g p]
  (let [edges (path->edge-info g p)]
    (apply-flow g edges (maximum-flow g edges))))

(defn flow-seq [g from to]
  (take-while :gr 
    (iterate (fn [{:keys [path gr]}] 
               (when-let [p (mincost-aug-path gr from to)] 
                 (let [res   {:path p :gr (augment-flow gr p)}]
                   res)))
             {:path nil :gr g})))

;;find the mincost flow, in graph, from -> to, where graph is a directed graph 
;;and contains a key :flow-info with compatible network flow information.
(defn mincost-flow 
  ([graph from to]
    (loop [g graph]
      (if-let [p (mincost-aug-path g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow (assoc graph :flow-info flow-info) from to)))

(defn maxflow 
  ([graph from to]
    (loop [g graph]
      (if-let [p (maxflow-aug-path g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (maxflow (assoc graph :flow-info flow-info) from to)))

(defn max-pushflow 
  ([graph from to]
    (loop [g graph]
      (if-let [p (pushflow-aug-path g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (max-pushflow (assoc graph :flow-info flow-info) from to)))

;;testing 
(comment 
(def net-data 
 [[:s   :chi  0 300]
  [:s   :dc   0 300]
  [:dc  :hou  4 280]
  [:dc  :bos  6 350]
  [:chi :bos  6 200]
  [:chi :hou  7 200]
  [:hou :t    0 300]
  [:bos :t    0 300]])

;;Neighbor Caching
(def the-net2 
  (-> (assoc (spork.data.digraph/->cached-graph) :flow-info {})
      (conj-cap-arcs net-data)))

(def the-net
  (-> empty-network 
      (conj-cap-arcs net-data)))


(definline mincost-aug-path2 [g from to]
  `(first (graph/get-paths 
           (search/traverse ~g ~from ~to (searchstate/empty-PFS2 ~from)
                            :weightf flow-weight :neighborf flow-neighbors))))


(defn traverse2b
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode startstate]
  (loop [state   (-> (assoc! startstate :targetnode targetnode)
                     (generic/conj-fringe startnode 0))]
    (if-let [source    (generic/next-fringe state)] ;next node to visit
      (let  [visited   (generic/visit-node state source)] ;record visit.
        (if (= targetnode source) visited                     
            (recur (loop [acc visited
                          xs (flow-neighbors g source state)]
                     (if (empty? xs) acc                        
                           (recur (generic/relax acc (flow-weight g source (first xs)) source (first xs))
                                  (rest xs)))))))
      state)))

;;Empty-pfs3 is actually pretty good...
(definline mincost-aug-path3 [g from to]
  `(first (graph/get-paths 
           (search/traverse2a ~g ~from ~to (searchstate/empty-PFS3 ~from)
                              :weightf flow-weight :neighborf flow-neighbors))))

(definline mincost-aug-path3a [g from to]
  `(search/traverse2a ~g ~from ~to (searchstate/empty-PFS3 ~from)
                              :weightf flow-weight :neighborf flow-neighbors))

(definline mincost-aug-path3b [g from to]
  `(traverse2b ~g ~from ~to (searchstate/empty-PFS3 ~from)))

(definline mincost-aug-pathm [g from to]
  `(first (graph/get-paths 
           (search/traverse2a ~g ~from ~to (searchstate/mempty-PFS ~from)
                            :weightf flow-weight :neighborf flow-neighbors))))

(definline mincost-aug-pathm2 [g from to]
  `(first (graph/get-paths 
           (search/traverse2a ~g ~from ~to (searchstate/mempty-PFS2 ~from)
                            :weightf flow-weight :neighborf flow-neighbors))))

(definline mincost-aug-pathm3 [g from to state]
  `(first (graph/get-paths 
           (search/traverse2a ~g ~from ~to ~state
                            :weightf flow-weight :neighborf flow-neighbors))))




(defn mincost-flow2
  ([graph from to]
    (loop [g graph]
      (if-let [p (mincost-aug-path3 g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow (assoc graph :flow-info flow-info) from to)))

(defn mincost-flowm
  ([graph from to]
    (loop [g graph]
      (if-let [p (mincost-aug-pathm g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow (assoc graph :flow-info flow-info) from to)))

(defn mincost-flowm2
  [graph from to]
     (let [*the-state* (searchstate/mempty-PFS2 from)]
       (loop [g graph]
         (if-let [p (mincost-aug-pathm3 g from to *the-state*)]
           (recur (do (generic/clear! *the-state*) 
                      (augment-flow g p)))
           (let [active (active-flows g)]
             {
                                        ;:cost (total-cost graph active)
                                        ;:flow (total-flow g active)
              :active active
              :net g})))))
 

(def sample 
  '{["55530LJ00" :filled]
    {:from "55530LJ00", :to :filled, :capacity 31.0, :flow 0},
    [RCAD "55530LJ00"]
    {:from RCAD, :to "55530LJ00", :capacity 9223372036854775807, :flow 0},
    [AC "5530LJ00"]
    {:from AC, :to "55530LJ00", :capacity 9223372036854775807, :flow 0},
    [RC RCAD-BIG] {:from RC, :to RCAD-BIG, :capacity 25000, :flow 0},
    [RC RCAD] {:from RC, :to RCAD, :capacity 25000, :flow 0},
    [Supply RC] {:from Supply, :to RC, :capacity 530000, :flow 0},
    [Supply AC] {:from Supply, :to AC, :capacity 450000, :flow 0},
    [Total Supply] {:from Total, :to Supply, :capacity 980000, :flow 0}})

(def sample-net (->> (for [{:keys [from to capacity flow]} (vals sample)]
                      [from to 0 capacity])
                    (conj-cap-arcs empty-network)))

;;Graph Caching speeds things up a bit.
;;After this, however, I started blowing the heap somehow....
;;Actually, after recovering paths I started blowing the heap...
(def empty-network2 (assoc (spork.data.digraph/->cached-graph) :flow-info {}))
(def sample-net2 
  (->> (for [{:keys [from to capacity flow]} (vals sample)]
         [from to 0 capacity])
       (conj-cap-arcs empty-network2)))
                    
)

