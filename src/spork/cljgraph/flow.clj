;;A library for apply various types of network flow 
;;algorithms, as well as defining networks.  Canonical 
;;implementations of augmenting path algorithms, as well
;;as mincost flow algorithms are included.
(ns spork.cljgraph.flow
  (:require [spork.cljgraph [core :as graph]
                            [search :as search]]
            [spork.data [searchstate :as searchstate]
                        [mutable :as m]]
            [spork.protocols [core :as generic]]
            [spork.util.general :refer [assoc2 assoc2! get2 transient2 persistent2! hinted-get2]]))

(def ^:const posinf Long/MAX_VALUE)


;;Network Protocols
;;=================

;;Abstract container for networks.
(defprotocol IFlowNet
  (-edge-info      [net from to])
  (einfos          [net])
  (-get-direction  [net from to])
  (-flow-weight    [net from to]))

(defprotocol IDynamicFlow
  (-conj-cap-arc   [net from to w cap])
;  (-disj-cap-arc   [net from to])
  (-active-flows   [net]))


;;abstract container for capacitated arcs.
(defprotocol IEdgeInfo
  (-set-flow      [net from to flow])
  (-set-capacity  [net from to cap])
  (-inc-flow      [net from to amt])
  (-get-flow      [net from to])
  (-get-capacity  [net from to]))

(defprotocol IDirected
  (-get-from      [d])
  (-get-to        [d])
  (-directed-pair [d]))

;;Edge Data Types
;;===============

;;We define a persistent record to capture capacitated flow
;;information across edges.
(defrecord einfo [from to capacity flow dir]
  IEdgeInfo
  (-set-flow     [net from to new-flow] (einfo. from to capacity new-flow dir))
  (-set-capacity [net from to cap] (einfo. from to cap flow dir))
  (-inc-flow     [net from to amt]
    (einfo.  from to  (- capacity amt)  (+ flow amt) dir))
  (-get-flow     [net from to] flow)
  (-get-capacity [net from to] capacity)
  IDirected
  (-get-from      [d] from)
  (-get-to        [d] to)
  (-directed-pair [d] (clojure.lang.MapEntry. from to)))

;;A mutable edge list.  For mutable stuff.  Mutation.  Mutants.
;;This ought to be good for small graphs.
(m/defmutable meinfo [from to capacity flow dir]
  IEdgeInfo
  (-set-flow     [net from to new-flow] (do (set! flow new-flow) net))
  (-set-capacity [net from to cap]      (do (set! capacity cap)  net))
  (-inc-flow     [net from to amt]      (do (set! capacity (- capacity amt))
                                            (set! flow     (+ flow amt))))
  (-get-flow     [net from to] flow)
  (-get-capacity [net from to] capacity)
  IDirected
  (-get-from      [d] from)
  (-get-to        [d] to)
  (-directed-pair [d] (clojure.lang.MapEntry. from to)))

;;Inline functions for constructing edges.  We adopt the convention 
;;of delineating the arity of these functions in the numerical suffix.

;;These are each 10x faster then the original varargs implementation.
(definline ->edge-info2 
  [from to]
  `(einfo. ~from  ~to posinf 0 :increment))
(definline ->edge-info4 
  [from to capacity flow]
  `(einfo. ~from ~to ~capacity ~flow :increment))

;;Constructors for mutable edges.
(definline ->medge-info2 
  [from to]
  `(meinfo. ~from  ~to posinf 0 :increment))
(definline ->medge-info4 
  [from to capacity flow]
  `(meinfo. ~from ~to ~capacity ~flow :increment))

;;Shared inline definitions for network topology.

;;This is currently a bit slow due to some overhead.
(definline forward? [g from to] 
  `(contains? (get (:sinks ~g)  ~from) ~to))

(defn forwardizer [g]
  (memoize (fn [from to] (contains? (get (:sinks g) from) to))))

;;Current function, should be replaced by the commented one below.
(definline get-edge-infos [n]
  `(for [[from# vs#] (:flow-info ~n)
         [to# info#] vs#]
     info#))

;;should be faster.
;; (definline get-edge-infos [n]
;;   `(spork.data.general/kv-flatten2 ~n))

(definline get-edge-infos! [n]
  `(let [infos# (:flow-info ~n)
         finfo# (:flow-info (meta ~n))]     
     (reduce-kv (fn [acc# from# tomap#]
                  (reduce-kv (fn [acc2# to# v#]
                               (m/push-arraylist acc# (get2 infos# from# to# nil)))
                             acc#
                             tomap#))
                (java.util.ArrayList.)
                finfo#)))

;;Operations on Map-backed Networks.
;;=================================

;;These are pretty generic operations, and were the original 
;;implementation, where everything was based on a either 
;;persistent or transient maps.  The downside is, we payed 
;;a constant cost unpacking the map.  It's mo-betta to 
;;Have access to the actual structures.  That would have 
;;confounded the interface to the functions, so I am choosing 
;;to wrap the API in a protocol, and provide access to it 
;;via the different data types.  Note that the data types are 
;;actually records, or defmutables, which can act like records 
;;and provide key-based access.

;;A generic function to fetch existing edge information from a
;;network, or to create a new, uncapacitated edge.
(definline edge-info [g from to]
  (let [res (with-meta (gensym "result") {:tag 'einfo})]
    `(if-let [~res (get2 (get ~g :flow-info) ~from ~to nil)]
       ~res
       (->edge-info2 ~from ~to))))

;;This should be a bit faster.  We can get even faster if we 
;;insert some kind of mutable record container.
(definline update-edge2*  
  [g from to cap flow]
  `(assoc ~g :flow-info                  
     (assoc2 (get ~g :flow-info {})
       ~from ~to 
       (einfo. ~from ~to ~cap ~flow :increment))))

(defmacro alter-edge [sym g from to & expr]
  `(let [~sym (edge-info ~g ~from ~to)]
     (assoc ~g :flow-info
            (assoc2 (get ~g :flow-info {})
                    ~from ~to 
                    ~@expr))))    

;;transient op 
(defmacro alter-edge! [sym g from to & expr]
  `(let [~sym (edge-info ~g ~from ~to)]
     (assoc! ~g :flow-info
            (assoc2! (get ~g :flow-info {})
                    ~from ~to 
                    ~@expr))))

(defn current-capacity 
  ([^einfo info] (- (.capacity info) (.flow info)))
  ([g from to] (current-capacity (edge-info g from to))))

(defn set-capacity [g from to cap] 
  (alter-edge the-edge g from to 
    (assoc the-edge :capacity cap)))

(defn set-flow [g from to flow]    
  (alter-edge the-edge g from to 
     (assoc the-edge :flow flow)))

;;transient ops
(defn set-capacity! [g from to cap] 
  (alter-edge! the-edge g from to 
    (assoc the-edge :capacity cap)))

(defn set-flow! [g from to flow]    
  (alter-edge! the-edge g from to 
     (assoc the-edge :flow flow)))

;add a capacitated arc to the graph
(defn conj-cap-arc [g from to w cap]
  (let [finfo (:flow-info g)]
    (-> (graph/conj-arc g from to w)
        (assoc :flow-info finfo)
        (update-edge2* from to cap 0))))

;;Probable hotspot in at least one use case.  We add arcs to the
;;network repeatedly...calls to merge and reduce and destructuring 
;;will slow us down.

;;add multiple capacitated arcs to the network.
(defn conj-cap-arcs [g arcs]
  (reduce (fn [gr [from to w cap]]  (conj-cap-arc  gr from to w cap)) g arcs))


;;Needed forward declarations.
(declare transient-network transient-network2 persistent-network!)

;;Network Data Types
;;==================

;;Due to the overhead associated with modifying networks, 
;;we end up desiring mutation for efficiency.  A transient 
;;net is just a wrapper around the original persistent network, 
;;with a set of mutable flow-info and meta data.
(m/defmutable transient-net [g flow-info metadata]
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] metadata)
  (withMeta [this m] (do (set! metadata m) this))
  ;; clojure.lang.IEditableCollection ;transient
  ;; (asTransient [net] (transient-network net))
  clojure.lang.ITransientCollection ;conj!, persistent!
  (conj       [net v] (throw (Exception. "conj! currently not supported on networks")))
  (persistent [net]   (persistent-network! net)) 
  IEdgeInfo
  (-set-flow     [net from to flow]
        (do (set! flow-info
                  (assoc2! flow-info from to 
                           (.-set-flow ^spork.cljgraph.flow.IEdgeInfo 
                              (.-edge-info net from to) from to flow)))
            net))                 
  (-set-capacity [net from to cap] 
         (do (set! flow-info
                   (assoc2! flow-info from to 
                            (.-set-capacity  ^spork.cljgraph.flow.IEdgeInfo 
                               (.-edge-info net from to)from to cap)))
             net))                 
  (-inc-flow     [net from to amt] 
         (do (set! flow-info
                   (assoc2! flow-info from to 
                            (.-inc-flow ^spork.cljgraph.flow.IEdgeInfo  
                               (.-edge-info net from to) from to amt)))
             net))
  (-get-flow [net from to]     (.-get-flow     (.-edge-info net from to)))
  (-get-capacity [net from to] (.-get-capacity (.-edge-info net from to)))
  IFlowNet
  (-edge-info      [net from to] 
         (if-let [^einfo  res (get2 flow-info from to nil)]
           res
           (->edge-info2 from to)))
  (einfos [net] (get-edge-infos! net))
  (-get-direction [net from to]  (forward? g from to))
  (-flow-weight    [net from to] (if (forward? g from to) (generic/-arc-weight g from to)
                                                         (- (generic/-arc-weight g from to)))))

;;A transient network that uses mutable edges.
;;This will knock off costs to assoc.  We just 
;;read the data and mutate the edge.  If this is much 
;;faster, it will become the default.
(m/defmutable transient-net2 [g flow-info metadata]
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] metadata)
  (withMeta [this m] (do (set! metadata m) this))
  ;; clojure.lang.IEditableCollection ;transient
  ;; (asTransient [net] (transient-network2 net))
  clojure.lang.ITransientCollection ;conj!, persistent!
  (conj       [net v] (throw (Exception. "conj! currently not supported on networks")))
  (persistent [net]   (persistent-network! net)) 
  IEdgeInfo
  (-set-flow     [net from to flow]
      (do (.-set-flow ^meinfo (.-edge-info flow-info from to) from to flow)
          net))                 
  (-set-capacity [net from to cap] 
      (do (.-set-capacity ^meinfo (.-edge-info flow-info from to) from to cap)
          net))                 
  (-inc-flow     [net from to amt] 
      (do (.-inc-flow ^meinfo (.-edge-info flow-info from to) from to amt)
          net))
  (-get-flow [net from to]     (.-get-flow     (.-edge-info net from to)))
  (-get-capacity [net from to] (.-get-capacity (.-edge-info net from to)))
  IFlowNet
  (-edge-info    [net from to] 
      (if-let [^meinfo  res (get2 flow-info from to nil)]
        res
        (let [edge (->medge-info2 from to)]
          (do (assoc2! flow-info from to edge)
              edge))))
  (einfos [n]     (get-edge-infos! n))
  (-get-direction [net from to] (forward? g from to))
  (-flow-weight   [net from to] (if (forward? g from to) (generic/-arc-weight g from to)
                                     (- (generic/-arc-weight g from to))))
  IDynamicFlow
  (-conj-cap-arc [net from to w cap]                 
         (do (set! g  (graph/conj-arc g from to w))
             (set! flow-info
                   (assoc2! flow-info
                            from to  
                            (->medge-info4 net from to cap 0 :increment)))
             net))
  (-active-flows [net] 
    (let [^java.util.ArrayList xs  (get-edge-infos! net)
          n (count xs)]    
      (loop [idx 0
             acc '()]
        (if (== idx n) acc
            (let [^meinfo info (m/get-arraylist xs idx)
                  ^long f (.-get-flow info)]
               (recur (unchecked-inc idx)
                     (if (pos? f)
                         (cons (clojure.lang.MapEntry. (.-directed-pair info)  f) acc)
                         acc))))))))

;;our persistent network is actually a digraph.  Since our digraph is
;;implemented as a record, we can store information in it like a map.
;;Our flow information happens to live in this map.  Note, this has
;;performance implications.
(extend-type spork.data.digraph
  IEdgeInfo
  (-set-flow      [net from to flow] (set-flow     net from to flow))
  (-set-capacity  [net from to cap]  (set-capacity net from to cap))
  (-inc-flow      [net from to amt]  (inc-flow     net from to amt))
  (-get-flow      [net from to]          (get-flow net from to))
  (-get-capacity  [net from to]      (get-capacity net from to))
  IFlowNet
  (-edge-info     [net from to]      (edge-info net from to))
  (einfos         [n]                (get-edge-infos n))
  (-get-direction [net from to]      (forward? g from to))
  (-flow-weight   [net from to]      (if (forward? g from to) 
                                       (generic/-arc-weight g from to)
                                       (- (generic/-arc-weight g from to))))
  IDynamicFlow
  (-conj-cap-arc [net from to w cap]  (conj-cap-arc net from to w cap))
  (-active-flows [net]                (active-flows net)))  


;;Constructors for various networks.
;;=================================


;;Network Flow data is stored as extra data in a 
;;digraph.  The API will be generalized behind a 
;;protocol, but for now this suffices, and it 
;;works on the existing digraph structure. 

;A simple set of helper functions let us embed flow data
;;in a spork.data.digraph record.
(def empty-network (assoc graph/empty-graph :flow-info {}))

;;We can create transient networks from existing networks to 
;;speed up ex. mincost flow computations, or even a series of 
;;mincost flows sharing the same transient.
(defn ^transient-net transient-network [g]
  (transient-net.  g (transient2 (:flow-info g)) {:flow-info (:flow-info g)}))

;;We actually need to push each of these guys into a mutable edge.

(defn ^transient-net2 transient-network2 [g]
  (transient-net2.  g (transient2 (:flow-info g)) {:flow-info (:flow-info g)}))

;;As with clojure transients, we define a function to realize the 
;;transient state as a persistent network.
(defn persistent-network! [^transient-net the-net]
  (let [g (:g the-net)]
    (assoc g
           :flow-info (persistent2! (:flow-info the-net)))))

;;Possible bottleneck here.
;optimized
(defn inc-flow 
  ([g ^einfo info flow]     
     (assoc g :flow-info
            (assoc2 (:flow-info g) (:from info) (:to info)  
                    (-> info 
                        (.assoc :capacity (- (.capacity info) flow))
                        (.assoc :flow (+ (.flow info) flow))))))
  ([g from to flow] 
     (alter-edge the-edge g from to 
         (let [^einfo the-edge the-edge]
           (-> the-edge 
               (.assoc :capacity (- (.capacity the-edge) flow))
               (.assoc :flow     (+ (.flow the-edge) flow)))))))

(defn inc-flow! 
  ([^transient-net g ^einfo info flow]     
     (assoc! g :flow-info
            (assoc2! (:flow-info g) (.from info) (.to info)  
                     (-> info 
                         (.assoc :capacity (- (.capacity info) flow))
                         (.assoc :flow     (+ (.flow info) flow))))))
  ([g from to flow] 
     (alter-edge! the-edge g from to 
                   (let [^einfo e the-edge]
                     (->  e
                          (.assoc :capacity (- (.capacity ^einfo the-edge) flow))
                          (.assoc :flow     (+ (.flow ^einfo the-edge) flow)))))))

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

(defn dec-flow!
  ([^transient-net g ^einfo info flow]
     (assoc! g :flow-info
            (assoc2!  (:flow-info g)  (.from info) (.to info) 
                      (-> info
                          (.assoc :capacity (+ (.capacity info) flow))
                          (.assoc :flow     (- (.flow info) flow))))))
  ([^transient-net g from to flow] 
     (alter-edge! the-edge g from to
       (let [^einfo e the-edge]
         (-> e
             (.assoc :capacity (+ (.capacity ^einfo the-edge) flow))
             (.assoc :flow     (- (.flow ^einfo the-edge) flow)))))))

(defn flows [g] 
  (for [[from vs] (:flow-info g)
        [to info] vs]
    [[from to] (select-keys info [:capacity :flow])]))

(defn enumerate-edges [g]
  (let [infos (:flow-info g)
        ks    (keys infos)]
    (for [k ks]
      (let [m (get infos k)
            sinks (keys m)]
        (for [sink sinks]
          (get m sink))))))          


;;optimization spot.
(defn active-flows [g] 
  (generic/loop-reduce 
   (fn [acc ^einfo info] 
     (let [^long f (.flow info)]
       (if (> f 0)
         (cons  [[(.from info) (.to info)]  (.flow info)] acc) 
                acc))) 
   '()
   (get-edge-infos g)))

;;transient op
(defn active-flows! [g] 
  (let [^java.util.ArrayList xs  (get-edge-infos! g)
        n (count xs)]    
    (loop [idx 0
           acc '()]
      (if (== idx n) acc
          (let [^einfo info (m/get-arraylist xs idx)
                ^long f (.flow info)]
            (recur (unchecked-inc idx)
                   (if (pos? f)
                     (cons (clojure.lang.MapEntry. (clojure.lang.MapEntry. (.from info) (.to info))  f) acc)
                     acc)))))))

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


;;Flows and Augmenting Paths
;;==========================

(defn ^java.util.ArrayList flow-neighbors 
  [g flow-info v]     
  (let [^java.util.ArrayList res (java.util.ArrayList.)]    
    (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
                     (do (when  (pos? (.capacity ^einfo (edge-info flow-info v to))) (.add acc to))
                      acc))
                res
                (get2 g :sinks v nil))
        (reduce-kv (fn [^java.util.ArrayList acc from w]
                     (do (when (pos? (.flow ^einfo (edge-info flow-info from v)))  (.add acc from))
                       acc))
                res 
                (get2 g :sources v nil)))))

(defn ^java.util.ArrayList -flow-neighbors 
  [g flow-info v]     
  (let [^java.util.ArrayList res (java.util.ArrayList.)]    
    (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
                     (do (when  (pos? (.capacity ^einfo (-edge-info flow-info v to))) (.add acc to))
                      acc))
                res
                (get2 g :sinks v nil))
        (reduce-kv (fn [^java.util.ArrayList acc from w]
                     (do (when (pos? (.flow ^einfo (-edge-info flow-info from v)))  (.add acc from))
                       acc))
                res 
                (get2 g :sources v nil)))))

;;Might be a faster way to do this, possibly cache via protocol.
(defn ^java.util.ArrayList -flow-neighbors2 
  [g flow-info v]     
  (let [^java.util.ArrayList res (java.util.ArrayList.)]    
    (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
                     (do (when  (pos? (-get-capacity (-edge-info flow-info v to) nil nil)) (.add acc to))
                      acc))
                res
                (get2 g :sinks v nil))
        (reduce-kv (fn [^java.util.ArrayList acc from w]
                     (do (when (pos? (-get-flow (-edge-info flow-info from v) nil nil))  (.add acc from))
                       acc))
                res 
                (get2 g :sources v nil)))))




;;the flow-cost for g from to.  Forward arcs are positive cost.
;;Backward arcs are negative-cost.

(definline flow-weight2 [g from to]
  `(if (forward? ~g ~from ~to) (generic/-arc-weight ~g ~from ~to) ;forward arc
      (- (generic/-arc-weight ~g ~to ~from))))



;;A function for caching flow weight calls.
(defn flow-weighter2 [g forward-pred]
  (memoize (fn [g from to] 
             (if (forward-pred g from to) (generic/-arc-weight g from to) ;forward arc
                 (- (generic/-arc-weight g to from))))))

;;based off of traverse2e
(defn flow-traverse
  "Custom function to walk a flow network."
  [g startnode targetnode startstate]
  (loop [state   (-> (assoc! startstate :targetnode targetnode)
                     (generic/conj-fringe startnode 0))]
    (if-let [source    (generic/next-fringe state)] ;next node to visit
      (let  [visited   (generic/visit-node state source)] ;record visit.
        (if (identical? targetnode source) visited                     
            (recur (let [xs (flow-neighbors g g source)
                         n (count xs)]
                     (loop [acc visited
                            idx 0]
                       (if (== idx n) acc                        
                           (recur (generic/relax acc (flow-weight2 g source 
                                                        (m/get-arraylist  xs idx)) source 
                                                        (m/get-arraylist  xs idx)
                                                        (generic/best-known-distance visited source))
                                  (unchecked-inc idx))))))))
      state)))

;;Changed from using flow-walk, due to overhead from function
;;invocation.  This guy gets called a lot.  Function overhead adds up.
;;Also, defwalk forms use merge internally...so runtime costs are
;;incurred in tight loops (i.e. lots of flow calcs).

;;formerly mincost-aug-pathme
(definline mincost-aug-path [g from to]
  `(searchstate/first-path (flow-traverse ~g ~from ~to (searchstate/mempty-PFS ~from))))

;;optimized?
(defn path->edge-info [g flow-info p]
  (loop [acc []
         xs  p]
    (if (nil? (next xs)) acc
        (let [from (first  xs)
              to   (second xs)]
          (recur (conj acc (if (forward? g from to)
                             (.assoc ^einfo (edge-info flow-info from to) :dir :increment)
                             (.assoc ^einfo (edge-info flow-info to from) :dir :decrement)))
                 (next xs))))))     

;;find the maximum flow that the path can support 
(defn maximum-flow [g infos]
  (loop [^einfo info (first infos)
         xs   (rest infos)
         flow posinf]
    (let [new-flow (if (= :increment (.dir info))
                       (.capacity info)
                       (.flow info))
          next-flow (long (min flow new-flow))]
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
  (let [edges (path->edge-info g g p)]
    (apply-flow g edges (maximum-flow g edges))))



;;find the mincost flow, in graph, from -> to, where graph is a directed graph 
;;and contains a key :flow-info with compatible network flow information.
(defn mincost-flow* 
  ([graph from to]
    (loop [g graph]
      (if-let [p (mincost-aug-path g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {:active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow* (assoc graph :flow-info flow-info) from to)))


(defn transient-flow-traverse
  "Custom function to walk a transient flow network."
  [net startnode targetnode startstate]
  (let [g (:g net)]
    (loop [state   (-> (assoc! startstate :targetnode targetnode)
                       (generic/conj-fringe startnode 0))]
      (if-let [source    (generic/next-fringe state)] ;next node to visit
        (let  [visited   (generic/visit-node state source)] ;record visit.
          (if (identical? targetnode source) visited                     
              (recur (let [xs (flow-neighbors g net source)
                           n  (count xs)]
                       (loop [acc visited
                              idx 0]
                         (if (== idx n) acc                        
                             (recur (generic/relax acc (flow-weight2 g source (m/get-arraylist xs idx)) source 
                                                                              (m/get-arraylist xs idx)
                                                   (generic/best-known-distance visited source))
                                    (unchecked-inc idx))))))))
        state))))

(defn -transient-flow-traverse
  "Custom function to walk a transient flow network."
  [^transient-net net startnode targetnode startstate]
  (let [g (:g net)]
    (loop [state   (-> (assoc! startstate :targetnode targetnode)
                       (generic/conj-fringe startnode 0))]
      (if-let [source    (generic/next-fringe state)] ;next node to visit
        (let  [visited   (generic/visit-node state source)] ;record visit.
          (if (identical? targetnode source) visited                     
              (recur (let [xs (-flow-neighbors g net source)
                           n  (count xs)]
                       (loop [acc visited
                              idx 0]
                         (if (== idx n) acc                        
                             (recur (generic/relax acc (flow-weight2 g source (m/get-arraylist xs idx)) source 
                                                                              (m/get-arraylist xs idx)
                                                   (generic/best-known-distance visited source))
                                    (unchecked-inc idx))))))))
        state))))

(defn -transient-flow-traverse2
  "Custom function to walk a transient flow network."
  [^transient-net2 net startnode targetnode startstate]
  (let [g (:g net)]
    (loop [state   (-> (assoc! startstate :targetnode targetnode)
                       (generic/conj-fringe startnode 0))]
      (if-let [source    (generic/next-fringe state)] ;next node to visit
        (let  [visited   (generic/visit-node state source)] ;record visit.
          (if (identical? targetnode source) visited                     
              (recur (let [xs (-flow-neighbors g net source)
                           n  (count xs)]
                       (loop [acc visited
                              idx 0]
                         (if (== idx n) acc                        
                             (recur (generic/relax acc (.-flow-weight g source (m/get-arraylist xs idx)) source 
                                                                              (m/get-arraylist xs idx)
                                                   (generic/best-known-distance visited source))
                                    (unchecked-inc idx))))))))
        state))))

(definline mincost-aug-path! [g from to]
  `(searchstate/first-path! (transient-flow-traverse ~g ~from ~to (searchstate/mempty-PFS ~from))))

(definline -mincost-aug-path! [g from to]
  `(searchstate/first-path! (-transient-flow-traverse ~g ~from ~to (searchstate/mempty-PFS ~from))))

(definline -mincost-aug-path2! [g from to]
  `(searchstate/first-path! (-transient-flow-traverse2 ~g ~from ~to (searchstate/mempty-PFS ~from))))

(defrecord edge-flows [^long flow ^java.util.ArrayList edges])

(defn ^edge-flows path->edge-flows! [flow-info ^clojure.lang.ISeq p]
   (let [g     (:g flow-info)
         edges  (java.util.ArrayList. )]
    (loop [xs   (.next p)
           from (.first p)
           flow posinf]
      (if (empty? xs) (edge-flows. flow edges)
          (let [to     (.first xs)
                dir   (if (forward? g from to) :increment :decrement)
                ^einfo info (if (identical? dir :increment) 
                              (edge-info flow-info from to)
                              (edge-info flow-info to from))]
            (do (.add edges (.assoc info :dir dir))
                (recur (.next xs) to
                       (let [^long new-flow (if (identical? :increment dir)
                                                (.capacity info)
                                                (.flow info))] 
                         (min flow  new-flow)))))))))    

(defn ^edge-flows -path->edge-flows! [^transient-net flow-info ^clojure.lang.ISeq p]
   (let [g     (:g flow-info)
         edges  (java.util.ArrayList. )]
    (loop [xs   (.next p)
           from (.first p)
           flow posinf]
      (if (empty? xs) (edge-flows. flow edges)
          (let [to     (.first xs)
                dir   (if (forward? g from to) :increment :decrement)
                ^einfo info (if (identical? dir :increment) 
                              (.-edge-info flow-info from to)
                              (.-edge-info flow-info to from))]
            (do (.add edges (.assoc info :dir dir))
                (recur (.next xs) to
                       (let [^long new-flow (if (identical? :increment dir)
                                                (.capacity info)
                                                (.flow info))] 
                         (min flow  new-flow)))))))))        

(defn ^edge-flows -path->edge-flows2! [^transient-net2 flow-info ^clojure.lang.ISeq p]
   (let [g     (:g flow-info)
         edges  (java.util.ArrayList. )]
    (loop [xs   (.next p)
           from (.first p)
           flow posinf]
      (if (empty? xs) (edge-flows. flow edges)
          (let [to     (.first xs)
                dir   (if (forward? g from to) :increment :decrement)
                ^meinfo info (if (identical? dir :increment) 
                              (.-edge-info flow-info from to)
                              (.-edge-info flow-info to from))]
            (do (.add edges (.assoc info :dir dir))
                (recur (.next xs) to
                       (let [^long new-flow (if (identical? :increment dir)
                                                (.-get-capacity info)
                                                (.-get-flow info))] 
                         (min flow  new-flow)))))))))  

(defn augment-flow! [^transient-net the-net p]
  (let [^edge-flows ef (path->edge-flows! the-net p)
         flow (.flow ef)
         ^java.util.ArrayList xs   (.edges ef)
         n     (count xs)]
    (loop [idx 0
           ^transient-net acc the-net]
      (if (== idx n) acc
          (recur (unchecked-inc idx)
                 (let [^einfo info (.get xs idx)]                  
                   (if (identical? :increment (.dir info))
                     (inc-flow! the-net info flow)
                     (dec-flow! the-net info flow)))))))) 

(defn -augment-flow! [^transient-net the-net p]
  (let [^edge-flows ef (-path->edge-flows! the-net p)
         flow (.flow ef)
         ^java.util.ArrayList xs   (.edges ef)
         n     (count xs)]
    (loop [idx 0
           ^transient-net acc the-net]
      (if (== idx n) acc
          (recur (unchecked-inc idx)
                 (let [^einfo info (.get xs idx)]                  
                   (if (identical? :increment (.dir info))
                     (-inc-flow the-net (.from info) (.to info) flow)
                     (-inc-flow the-net (.from info) (.to info) (- flow)))))))))

;;Since we're augmenting via mutation, we have mutable edges.  We just 
;;increment the flow directly instead of indirectly.
(defn -augment-flow2! [^transient-net2 the-net p]
  (let [^edge-flows ef (-path->edge-flows! the-net p)
         flow (.flow ef)
         ^java.util.ArrayList xs   (.edges ef)
         n     (count xs)]
    (loop [idx 0
           ^transient-net2 acc the-net]
      (if (== idx n) acc
          (recur (unchecked-inc idx)
                 (let [^meinfo info (.get xs idx)]                  
                   (if (identical? :increment (.dir info))
                     (.-inc-flow info nil nil flow)
                     (.-inc-flow info nil nil (- flow)))))))))

(defn mincost-flow!
  ([graph from to]
    (loop [g (transient-network graph)]
      (if-let [p (mincost-aug-path! g from to)]
        (recur (augment-flow! g p))
        (let [active (active-flows! g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow! (assoc graph :flow-info flow-info) from to)))

(defn -mincost-flow!
  ([graph from to]
    (loop [g (transient-network graph)]
      (if-let [p (-mincost-aug-path! g from to)]
        (recur (-augment-flow! g p))
        (let [active (active-flows! g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow! (assoc graph :flow-info flow-info) from to)))

(defn -mincost-flow2!
  ([graph from to]
    (loop [g (transient-network2 graph)]
      (if-let [p (-mincost-aug-path2! g from to)]
        (recur (-augment-flow2! g p))
        (let [active (-active-flows g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow! (assoc graph :flow-info flow-info) from to)))

(defn augmentations!
  ([graph from to]
     (let [augs (java.util.ArrayList.)]
       (loop [g (transient-network graph)]
         (if-let [p (mincost-aug-path! g from to)]
           (do (let [f (maximum-flow (:g g) (path->edge-info (:g g) g p))]
                 (.add augs [f p]))
               (recur (augment-flow! g p)))
           (let [active (active-flows! g)]
             {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)             
           :active active
           :augmentations augs
           :net g})))))
  ([flow-info graph from to]
    (augmentations! (assoc graph :flow-info flow-info) from to)))

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

(defn augmentations
  ([graph from to]
     (let [augs (java.util.ArrayList.)]
       (loop [g graph]
         (if-let [p (mincost-aug-path g from to)]
           (do (let [f (maximum-flow g (path->edge-info g g p))]
                 (.add augs [f p]))
               (recur (augment-flow g p)))
           (let [active (active-flows g)]
             {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)             
           :active active
           :augmentations augs
           :net g})))))
  ([flow-info graph from to]
    (augmentations (assoc graph :flow-info flow-info) from to)))

;;testing has been moved to flowtests.


;;Vestigial, or pending.  Not currently relevant, but would be nice to
;;get backported.

;;this is a special walk for helping us with greedy flows, where we don't 
;;try to find an augmenting flow.
;; (defn forward-only-flow-neighbors 
;;   [g v _]
;;   (let [info (partial edge-info g)
;;         capacity (fn [to] (:capacity (info v to)))]
;;     (filter (fn [to]   (> (capacity to) 0)) (graph/sinks g v))))

            
;; (defn flow-walk [g startnode endnode]
;;   (search/priority-walk g startnode :endnode endnode 
;;                         :weightf flow-weight :neighborf flow-neighbors))

;; (defn ford-fulkerson-walk [g startnode endnode]
;;   (search/depth-walk g startnode :endnode endnode :neighborf flow-neighbors))

;; (defn edmonds-karp-walk [g startnode endnode]
;;   (search/breadth-walk g startnode :endnode endnode :neighborf flow-neighbors))

;; (defn pushflow-walk [g startnode endnode]
;;   (search/priority-walk g startnode 
;;         :endnode endnode :neighborf forward-only-flow-neighbors))

;; (defn pushflow-aug-path [g from to]
;;   (first (graph/get-paths (pushflow-walk g from to))))

;; (defn maxflow-aug-path [g from to]
;;   (first (graph/get-paths (edmonds-karp-walk g from to))))


;;working on transient stuff.
;; (definline update-edge2*! 
;;   [^transient-net g from to cap flow]
;;   `(assoc! ~g :flow-info                  
;;      (assoc2! (get ~g :flow-info {})
;;        ~from ~to 
;;        (einfo. ~from ~to ~cap ~flow :increment))))

;; (defn flow-seq [g from to]
;;   (take-while :gr 
;;     (iterate (fn [{:keys [path gr]}] 
;;                (when-let [p (mincost-aug-path gr from to)] 
;;                  (let [res   {:path p :gr (augment-flow gr p)}]
;;                    res)))
;;              {:path nil :gr g})))

;; (defn max-pushflow 
;;   ([graph from to]
;;     (loop [g graph]
;;       (if-let [p (pushflow-aug-path g from to)]
;;         (recur (augment-flow g p))
;;         (let [active (active-flows g)]
;;           {:flow (total-flow g active)
;;            :active active
;;            :net g}))))
;;   ([flow-info graph from to]
;;     (max-pushflow (assoc graph :flow-info flow-info) from to)))


;; (defn maxflow 
;;   ([graph from to]
;;     (loop [g graph]
;;       (if-let [p (maxflow-aug-path g from to)]
;;         (recur (augment-flow g p))
;;         (let [active (active-flows g)]
;;           {:flow (total-flow g active)
;;            :active active
;;            :net g}))))
;;   ([flow-info graph from to]
;;     (maxflow (assoc graph :flow-info flow-info) from to)))

;; (comment
;; (defn maximum-flow! [g ^objects infos]
;;   (let [n (alength infos)]
;;     (loop [idx 0
;;            flow posinf]     
;;       (if (== idx n) 
;;         (let [^einfo info (aget infos idx)]
;;           (let [new-flow (if (= :increment (.dir info))
;;                            (.capacity info)
;;                            (.flow info))]                
;;                 (recur (unchecked-inc idx) (min flow new-flow))))))))


 
;; (let [xs (object-array [(->einfo2 :s :t) (->einfo2 :dc :q) (->einfo2 :blah :foo) (->einfo2 :a :d)])] 
;; )

;; (defn maximum-flow! [g ^objects infos]
;;   (areduce infos idx ret 0 (min ret ^long (.flow ^spork.cljgraph.flow.einfo (aget infos idx)))))
;; )



;; (defn apply-flow! [g edges flow]  
;;   (loop [xs edges
;;          acc g] 
;;     (if-let [^einfo info (first xs)
;;         (if (= :increment (:dir info))
;;           (inc-flow! gr info flow)
;;           (dec-flow! gr info flow)))
;;       g edges))

;;transient ops.
;; (defn augment-flow! [^transient-net the-net p]
;;   (let [edges (path->edge-info (.valAt the-net :g) the-net p)]
;;     (apply-flow! the-net edges (maximum-flow the-net edges))))

;(defrecord edge-flows [^long flow ^objects edges])

;; (defn augment-flow! [^transient-net the-net p]
;;   (let [^edge-flows ef (path->edge-flows! the-net (object-array p))
;;          flow (.flow ef)
;;         ^objects xs (.edges ef)
;;         n  (alength xs)]
;;     (loop [idx 0
;;            ^transient-net acc the-net]
;;       (if (== idx n) acc
;;           (recur (unchecked-inc idx)
;;                  (let [^einfo info (aget xs idx)]                  
;;                    (if (identical? :increment (.dir info))
;;                      (inc-flow! the-net info flow)
;;                      (dec-flow! the-net info flow)))))))) 


;; (defn ^edge-flows path->edge-flows! [flow-info ^objects p]
;;    (let [g     (:g flow-info)
;;          xs    p
;;          n     (alength xs)
;;          edges (object-array (unchecked-dec n))]
;;     (loop [from 0
;;            to   1
;;            flow posinf]
;;       (if (== to n) (edge-flows. flow edges)
;;           (let [l     (aget xs from)
;;                 r     (aget xs to)
;;                 dir   (if (forward? g l r) :increment :decrement)
;;                 ^einfo info (if (identical? dir :increment) 
;;                               (edge-info flow-info l r)
;;                               (edge-info flow-info r l))]
;;             (do (aset edges from (.assoc info :dir dir))
;;                 (recur (unchecked-inc from) (unchecked-inc to)
;;                        (let [^long new-flow (if (identical? :increment dir)
;;                                                 (.capacity info)
;;                                                 (.flow info))] 
;;                          (min flow  new-flow)))))))))     


;;based off of traverse2e
;; (defn flow-traverse
;;   "Custom function to walk a flow network."
;;   [g startnode targetnode startstate]
;;   (loop [state   (-> (assoc! startstate :targetnode targetnode)
;;                      (generic/conj-fringe startnode 0))]
;;     (if-let [source    (generic/next-fringe state)] ;next node to visit
;;       (let  [visited   (generic/visit-node state source)] ;record visit.
;;         (if (= targetnode source) visited                     
;;             (recur (let [^objects xs (to-array (flow-neighbors g g source))
;;                          n (alength xs)]
;;                      (loop [acc visited
;;                             idx 0]
;;                        (if (== idx n) acc                        
;;                            (recur (generic/relax acc (flow-weight2 g source (aget xs idx)) source (aget xs idx))
;;                                   (unchecked-inc idx))))))))
;;       state)))

;;based off of transient-traverse2e
;;transient ops
;; (defn transient-flow-traverse
;;   "Custom function to walk a transient flow network."
;;   [net startnode targetnode startstate]
;;   (let [g (:g net)]
;;     (loop [state   (-> (assoc! startstate :targetnode targetnode)
;;                        (generic/conj-fringe startnode 0))]
;;       (if-let [source    (generic/next-fringe state)] ;next node to visit
;;         (let  [visited   (generic/visit-node state source)] ;record visit.
;;           (if (identical? targetnode source) visited                     
;;               (recur (let [^objects xs (to-array (flow-neighbors g net source))
;;                            n (alength xs)]
;;                        (loop [acc visited
;;                               idx 0]
;;                          (if (== idx n) acc                        
;;                              (recur (generic/relax acc (flow-weight2 g source (aget xs idx)) source (aget xs idx)
;;                                                    (generic/best-known-distance visited source))
;;                                     (unchecked-inc idx))))))))
;;         state))))

;; (defn ^java.util.ArrayList flow-neighbors 
;;   [g flow-info v]     
;;   (reduce-kv (fn [^java.util.ArrayList acc to w]               
;;                (do (when  (or (pos? (.capacity ^einfo (edge-info flow-info v to))) 
;;                               (pos? (.flow ^einfo     (edge-info flow-info to v))))
;;                      (.add acc to))
;;                    acc))
;;              (java.util.ArrayList.)
;;              (get2 g :sinks v nil)))

;;transient op
;; (definline get-edge-infos! [n]
;;   `(let [infos# (:flow-info ~n)
;;          finfo# (:flow-info (meta ~n))]     
;;      (reduce-kv (fn [acc# from# tomap#]
;;                   (reduce-kv (fn [acc2# to# v#]
;;                                (m/push-arraylist acc# (get2 infos# from# to# nil)))
;;                              acc#
;;                              tomap#))
;;                 (java.util.ArrayList.)
;;                 finfo#)))
         
;; (defn flow-neighbors 
;;   ^java.util.ArrayList [g flow-info v]     
;;    (let [^java.util.ArrayList acc (java.util.ArrayList.)]     
;;      (loop [xs (generic/-get-sinks g v)]
;;        (if-let [to (first xs)]
;;          (do (when (> (.capacity ^einfo (edge-info flow-info v to)) 0) (.add acc to))
;;              (recur (rest xs)))))
;;      (loop [xs (generic/-get-sources g v)]
;;        (if-let [from (first xs)]
;;          (do (when (> (.flow ^einfo (edge-info flow-info from v))   0)  (.add acc from))
;;              (recur (rest xs)))))
;;      acc))


;;reduce is much faster.
;; (defn ^java.util.ArrayList flow-neighbors 
;;   [g flow-info v]     
;;   (let [^java.util.ArrayList res (java.util.ArrayList.)]    
;;     (do (reduce (fn [^java.util.ArrayList acc to]               
;;                   (do (when  (> (.capacity ^einfo (edge-info flow-info v to)) 0) (.add acc to))
;;                       acc))
;;                 res
;;                 (generic/-get-sinks g v))
;;         (reduce (fn [^java.util.ArrayList acc from]
;;                   (do (when (> (.flow ^einfo (edge-info flow-info from v))   0)  (.add acc from))
;;                        acc))
;;                 res 
;;                  (generic/-get-sources g v)))))

;;reduce-kv is much faster.  uses no intermediate keys! call...
;;We can also probably ignore the second call to reduce-kv, and move
;;the edge-info 
;; (defn ^java.util.ArrayList flow-neighbors 
;;   [g flow-info v]     
;;   (let [^java.util.ArrayList res (java.util.ArrayList.)]    
;;     (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
;;                      (do (when (or  (pos? (.capacity ^einfo (edge-info flow-info v to)))
;;                                     (pos? (.flow     ^einfo (edge-info flow-info to v))))  
;;                            (.add acc to))
;;                          acc))
;;                    res
;;                    (get2 g :sinks v nil)))))
