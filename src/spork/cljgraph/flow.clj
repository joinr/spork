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
            [spork.util.general :refer [assoc2 assoc2! get2 transient2 persistent2! hinted-get2 kv-reduce2 kv-map2]]))

(def ^:const posinf Long/MAX_VALUE)

;;Notes on performance implications of using protocols and such...
;;

;;Network Protocols
;;=================

;;Abstract container for networks.
(defprotocol IFlowNet
  (-edge-info      [net from to])
  (einfos          [net])
  (-get-direction  [net from to])
  (-flow-weight    [net from to])
  (-set-edge       [net edge])
  (-flow-sinks     [net x])
  (-flow-sources   [net x])
  (-push-flow      [net edge flow]))

(defprotocol IDynamicFlow
  (-conj-cap-arc   [net from to w cap])
  (-active-flows   [net]))

(defprotocol IScaling
  (scale    [n x])
  (unscale  [n x]))

;;Another idea here...
;;Have a container with the function fields that we need.
;;Let the user provide over-rides...


(defprotocol IFlowOptions 
  (_flow-function  [x])
  (_flow-neighbors [x from])
  (_flow-scale     [x])
  (_path->edges    [x]))

(defrecord scaled-int-flow [^long factor]
  IScaling
  (scale [n x] (quot x factor))
  (unscale [n x] (* ^long x factor)))

(defrecord general-scaled-flow [scale-func unscale-func]
  IScaling
  (scale [n x] (scale-func x))
  (unscale [n x] (unscale-func x)))
 

;;We use an interface here for a reason....
;;An interface allows direct method invokation on 
;;anything that's cast to the interface; so we can 
;;uniformly access containers with field access 
;;akin to .field 
;;If we use protocols, we incur about a 2-4x slowdown.
;;With the interface, we actually get close to 
;;array access speeds, so it's preferable.  The only 
;;downside is that the API gets a bit wonkier.

;;I only recommend doing this for limited use cases, 
;;i.e. where we are trying to unify a protocol over 
;;multiple implementations (due to mutation efficiencies).
;;For the most part, protocols are pretty damn good, and 
;;the run-time type dispatch cost, particularly for inlined 
;;protocols, is unlikely to be a bottleneck.  For performance 
;;and uniform field-like access, interfaces are lower level 
;;and more performant.   [insert pithy observation about 
;;premature optimization here]
(definterface IEdgeInfo
  (setFlow      [flow])
  (setCapacity  [cap])
  (setDirection [d])
  (incFlow      [amt])
  (flow         [])
  (capacity     [])
  (from         [])
  (to           [])
  (dir          [])
  (directedPair []))

;;Still performance testing protocol implementation.
;;We'll see if the method invocation afforded by the 
;;interface is worth it....it "looks" to be 3x slower 
;;over a million calls....Inlined protocols may be just dandy, 
;;however...Certainly more straightforward.

;; (defprotocol IEdgeInfo
;;   (setFlow      [net  flow])
;;   (setCapacity  [net  cap])
;;   (incFlow      [net  amt])
;;   (flow         [net ])
;;   (capacity     [net ])
;;   (from         [e])
;;   (to           [e])
;;   (dir          [e])
;;   (directedPair [e]))

;;Edge Data Types
;;===============

;;We define a persistent record to capture capacitated flow
;;information across edges.
(defrecord einfo [from to capacity flow dir]
  IEdgeInfo
  (setFlow      [edge new-flow] (einfo. from to  capacity new-flow dir))
  (setCapacity  [edge cap]      (einfo. from to cap flow dir))
  (setDirection [edge d]   (einfo. from to capacity flow d))
  (incFlow      [edge amt] (einfo. from to   (- capacity amt)  (+ flow amt) dir))
  (flow         [edge] flow)
  (capacity     [edge] capacity)  
  (from         [e] from)
  (to           [e] to)
  (dir          [e] dir)
  (directedPair [e] (clojure.lang.MapEntry. from to)))

;;A mutable edge list.  For mutable stuff.  Mutation.  Mutants.
;;This ought to be good for small graphs.
(m/defmutable meinfo [from to capacity flow dir]
  IEdgeInfo
  (setFlow      [edge new-flow] (do (set! flow new-flow) edge))
  (setCapacity  [edge cap]      (do (set! capacity cap)  edge))
  (setDirection [edge d]        (do (set! dir d) edge))
  (incFlow      [edge amt]      (do (set! capacity (- capacity amt))
                                             (set! flow     (+ flow amt))))
  (flow         [edge] flow)
  (capacity     [edge] capacity)
  (from         [d]    from)
  (to           [d]    to)
  (dir          [e]    dir)
  (directedPair [d]    (clojure.lang.MapEntry. from to)))


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

(defmacro edge-hint [e] 
  `(vary-meta ~e assoc :tag 'spork.cljgraph.flow.IEdgeInfo))

;;Might just turn these into macros...i mean, we've got the 
;;equivalent of field access on these guys...
;;The inline function calls are costing us a slight bit of 
;;speed.

;;Hinted operations on edges...sorry for the wierdness.
(definline edge-capacity [e]
  `(.capacity ~(edge-hint e)))

(definline edge-flow [e]
  `(.flow ~(edge-hint e)))

;;Need to test the efficacy of using protocol calls instead of funcs.
(definline edge-from [e]
  `(.from ~(edge-hint e)))

(definline edge-to [e]
  `(.to ~(edge-hint e)))

(definline inc-flow [e  amt]
  `(.incFlow ~(edge-hint e) ~amt))

(definline set-capacity [e amt]
  `(.setCapacity ~(edge-hint e) ~amt))

(definline set-flow [e  amt]
  `(.setFlow ~(edge-hint e) ~amt))

;; (definline inc-flow [e from to amt]
;;   `(.incFlow ~(edge-hint e) ~from ~to ~amt))

(definline edge-dir [e]
  `(.dir ~(edge-hint e)))

(definline edge-pair [e]
  `(.directedPair ~(edge-hint e)))

;;Note:
;;Might need to move the direction component into a protocol, or lift
;;it out entirely.

(defn  edge->medge [^einfo edge]
    (meinfo. (.from edge) (.to edge) (.capacity edge) (.flow edge) (.dir edge)))

(defn ^einfo medge->edge [^meinfo edge]
  (einfo. (.from edge) (.to edge) (.capacity edge) (.flow edge) (.dir edge)))

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
;;   `(kv-flatten2 ~n))

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
  `(let [~(vary-meta sym assoc :tag 'spork.cljgraph.IEdgeInfo) (edge-info ~g ~from ~to)]
     (assoc ~g :flow-info
            (assoc2 (get ~g :flow-info {})
                    ~from ~to 
                    ~@expr))))    
;;transient op 
(defmacro alter-edge! [sym g from to & expr]
  `(let [~(vary-meta sym assoc :tag 'spork.cljgraph.IEdgeInfo) (edge-info ~g ~from ~to)]
     (assoc! ~g :flow-info
            (assoc2! (get ~g :flow-info {})
                    ~from ~to 
                    ~@expr))))

(defn current-capacity 
  ([^einfo info] (- (.capacity info) (.flow info)))
  ([g from to] (current-capacity (edge-info g from to))))

;;build on top of a protocol function here to unify 
;;the inc-flow arities, there's a case where we pass in 
;;an edge info.

;;Macros to help with edge updates, relying on protocols.
;;Type hints everything for us.
(defmacro update-edge [net info edge-sym & expr]
  `(let [~(vary-meta edge-sym assoc :tag 'spork.cljgraph.flow.IEdgeInfo) ~info]
     (.-set-edge ~(vary-meta net assoc :tag 'spork.cljgraph.flow.IFlowNet)
                 ~@expr)))               

(definline -inc-edge-flow [net info amt]
  `(update-edge ~net ~info the-edge# 
     (inc-flow the-edge#  ~amt)))
 
(definline -dec-edge-flow [net info amt]
  `(-inc-edge-flow ~net ~info (- ~amt)))
               
(definline -inc-edge-capacity [net info amt]
  `(update-edge ~net ~info the-edge# 
     (.setCapacity the-edge# (+ ~amt (edge-capacity the-edge#)))))

(definline -dec-edge-capacity [net info amt]
  `(-inc-edge-capacity ~net ~info (- ~amt)))


;;Probable hotspot in at least one use case.  We add arcs to the
;;network repeatedly...calls to merge and reduce and destructuring 
;;will slow us down.

;;add multiple capacitated arcs to the network.
(defn conj-cap-arcs [g arcs]
  (reduce (fn [gr [from to w cap]]  (-conj-cap-arc  gr from to w cap)) g arcs))

(defmacro forward-flow [g from to]
  `(if (forward? ~g ~from ~to) 
     (generic/-arc-weight ~g ~from ~to)
     (- (generic/-arc-weight ~g ~to ~from))))

;;Network Data Types
;;==================

;;our persistent network is actually a digraph.  Since our digraph is
;;implemented as a record, we can store information in it like a map.
;;Our flow information happens to live in this map.  Note, this has
;;performance implications.
(extend-type spork.data.digraph.digraph  
  IFlowNet
  (-edge-info     [net from to]   (edge-info net from to))
  (einfos         [n]             (get-edge-infos n))
  (-get-direction [net from to]   (forward? net from to))
  (-flow-weight   [net from to]   (forward-flow net from to))
  (-set-edge      [net edge]         
    (assoc net :flow-info                  
           (assoc2 (get net :flow-info {})
                   (.from ^einfo edge) (.to ^einfo edge) 
                   edge)))
  (-flow-sinks     [net x] (get2 net :sinks x nil))
  (-flow-sources   [net x] (get2 net :sources x nil))
  (-push-flow      [net edge flow] (-set-edge net (inc-flow edge flow)))
  IDynamicFlow
  (-conj-cap-arc [net from to w cap]  
         (let [finfo (:flow-info net)]
           (-> (graph/conj-arc net from to w)
               (assoc :flow-info finfo)
               (update-edge2* from to cap 0))))
  (-active-flows [net]  (generic/loop-reduce 
                         (fn [acc ^einfo info] 
                           (let [^long f (.flow info)]
                             (if (> f 0)
                               (cons  [[(.from info) (.to info)]  (.flow info)] acc) 
                               acc))) 
                         '()
                         (get-edge-infos net))))  
 
;;A transient network that uses mutable edges.
;;This will knock off costs to assoc.  We just 
;;read the data and mutate the edge.  If this is much 
;;faster, it will become the default.
(m/defmutable transient-net [g flow-info metadata]
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] metadata)
  (withMeta [this m] (do (set! metadata m) this))
  IFlowNet
  (-edge-info    [net from to] 
      (if-let [^meinfo  res (get2 flow-info from to nil)]
        res
        (let [edge (->medge-info2 from to)]
          (do (assoc2! flow-info from to edge)
              edge))))
  (einfos [n]     (get-edge-infos! n))
  (-get-direction [net from to] (forward? g from to))
  (-flow-weight   [net from to] (forward-flow g from to))
  (-set-edge [net edge]         
     (let [^meinfo e edge
           from (.from e)
           to   (.to   e)]
       (do (assoc2! flow-info from to edge)
           net)))
  (-flow-sinks     [net x] (get2 g :sinks x nil))
  (-flow-sources   [net x] (get2 g :sources x nil))
  (-push-flow      [net edge flow] (do (inc-flow edge flow) net))
  IDynamicFlow 
  (-conj-cap-arc [net from to w cap]                 
         (do (set! g  (graph/conj-arc g from to w))
             (set! flow-info
                   (assoc2! flow-info
                            from to  
                            (meinfo. from to cap 0 :increment)))
             net))
  (-active-flows [net] 
    (let [^java.util.ArrayList xs  (get-edge-infos! net)
          n (count xs)]    
      (loop [idx 0
             acc '()]
        (if (== idx n) acc
            (let [^meinfo info (m/get-arraylist xs idx)
                  ^long f (edge-flow info)]
               (recur (unchecked-inc idx)
                     (if (pos? f)
                         (cons (clojure.lang.MapEntry. (edge-pair info)  f) acc)
                         acc))))))))

;;Similar idea based off sedgewick.
;; (m/defmutable transient-net-sedge [g flow-info metadata]
;;   clojure.lang.IObj
;;   ;adds metadata support
;;   (meta [this] metadata)
;;   (withMeta [this m] (do (set! metadata m) this))
;;   IFlowNet
;;   (-edge-info    [net from to] 
;;       (if-let [^meinfo  res (get2 flow-info from to nil)]
;;         res
;;         (let [edge (->medge-info2 from to)]
;;           (do (assoc2! flow-info from to edge)
;;               edge))))
;;   (einfos [n]     (get-edge-infos! n))
;;   (-get-direction [net from to] (forward? g from to))
;;   (-flow-weight   [net from to] (forward-flow g from to))
;;   (-set-edge [net edge]         
;;      (let [^meinfo e edge
;;            from (.from e)
;;            to   (.to   e)]
;;        (do (assoc2! flow-info from to edge)
;;            net)))
;;   (-flow-sinks     [net x] (get2 g :sinks x nil))
;;   (-flow-sources   [net x] (get2 g :sources x nil))
;;   (-push-flow      [net edge flow] (do (inc-flow edge flow) net))
;;   IDynamicFlow 
;;   (-conj-cap-arc [net from to w cap]                 
;;          (do (set! g  (graph/conj-arc g from to w))
;;              (set! flow-info
;;                    (assoc2! flow-info
;;                             from to  
;;                             (meinfo. from to cap 0 :increment)))
;;              net))
;;   (-active-flows [net] 
;;     (let [^java.util.ArrayList xs  (get-edge-infos! net)
;;           n (count xs)]    
;;       (loop [idx 0
;;              acc '()]
;;         (if (== idx n) acc
;;             (let [^meinfo info (m/get-arraylist xs idx)
;;                   ^long f (edge-flow info)]
;;                (recur (unchecked-inc idx)
;;                      (if (pos? f)
;;                          (cons (clojure.lang.MapEntry. (edge-pair info)  f) acc)
;;                          acc))))))))

;; (defrecord transient-array-net [g ^objects flow-info nodemap nodenum metadata]
;;   clojure.lang.IObj
;;   ;adds metadata support
;;   (meta [this] metadata)
;;   (withMeta [this m] (do (set! metadata m) this))
;;   IFlowNet
;;   (-edge-info    [net from to] 
;;       (if-let [^meinfo  res (get2 flow-info from to nil)]
;;         res
;;         (let [edge (->medge-info2 from to)]
;;           (do (assoc2! flow-info from to edge)
;;               edge))))
;;   (einfos [n]     (get-edge-infos! n))
;;   (-get-direction [net from to] (forward? g from to))
;;   (-flow-weight   [net from to] (forward-flow g from to))
;;   (-set-edge [net edge]         
;;      (let [^meinfo e edge
;;            from (.from e)
;;            to   (.to   e)]
;;        (do (assoc2! flow-info from to edge)
;;            net)))
;;   (-flow-sinks     [net x] (get2 g :sinks x nil))
;;   (-flow-sources   [net x] (get2 g :sources x nil))
;;   (-push-flow      [net edge flow] (do (inc-flow edge flow) net))
;;   IDynamicFlow 
;;   (-conj-cap-arc [net from to w cap]                 
;;          (do (set! g  (graph/conj-arc g from to w))
;;              (set! flow-info
;;                    (assoc2! flow-info
;;                             from to  
;;                             (meinfo. from to cap 0 :increment)))
;;              net))
;;   (-active-flows [net] 
;;     (let [^java.util.ArrayList xs  (get-edge-infos! net)
;;           n (count xs)]    
;;       (loop [idx 0
;;              acc '()]
;;         (if (== idx n) acc
;;             (let [^meinfo info (m/get-arraylist xs idx)
;;                   ^long f (edge-flow info)]
;;                (recur (unchecked-inc idx)
;;                      (if (pos? f)
;;                          (cons (clojure.lang.MapEntry. (edge-pair info)  f) acc)
;;                          acc))))))))

;;we can build a fairly powerful abstraction if we allow the user 
;;to define ways to wrap existing flows.

;;(defmacro with-memo-context [blah expr]) 
  
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

;;We actually need to push each of these guys into a mutable edge.
;;Basically, for each edge in the stored flow-info, which is a kv2, 
;;we can get the the flow-info into a flat sequence via kv2-flatten.

;;As with clojure transients, we define a function to realize the 
;;transient state as a persistent network.
(defn ^transient-net transient-network [g]
  (let [flow-info!   (kv-reduce2 
                        (fn [acc from to v] 
                          (assoc2! acc from to (edge->medge v)))
                        (transient {})
                        (:flow-info g))]        
  (transient-net.  g  flow-info! {:flow-info (:flow-info g)})))

(defn persistent-network! [^transient-net the-net]
  (let [g (:g the-net)]
    (assoc g
           :flow-info (kv-map2 medge->edge (persistent2! (:flow-info the-net))))))


;;THe API prizes edge-update by accessing edge infos 
;;directly.  Consequently, we only want to pay the cost 
;;of looking up an edge once.  

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

(defn total-flow 
  ([g active-edges] 
    (->> active-edges 
      (filter (fn [[k v]] (graph/terminal-node? g (second k))))
      (map second)
      (reduce + 0)))
  ([g] (total-flow g (-active-flows g))))

;;refactored to eliminate reduce and destructuring.
(defn total-cost 
  ([g active-edges]
    (generic/loop-reduce 
     (fn [acc info]
       (let [flow (second info)
             from (first  (first info))
             to   (second (first info))]
         (+ acc (* flow (graph/arc-weight g from to)))))
            0 active-edges))
  ([g] (total-cost g (-active-flows g))))

;;Flows and Augmenting Paths
;;==========================

;;Might be a faster way to do this, possibly cache via protocol.
(defn ^java.util.ArrayList flow-neighbors
  [flow-info v sinks sources]     
  (let [^java.util.ArrayList res (java.util.ArrayList.)]    
    (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
                     (do (when  (pos? (edge-capacity (-edge-info flow-info v to))) (.add acc to))
                      acc))
                res
               sinks)
        (reduce-kv (fn [^java.util.ArrayList acc from w]
                     (do (when (pos? (edge-flow (-edge-info flow-info from v)))  (.add acc from))
                       acc))
                res 
               sources))))

(defn ^java.util.ArrayList flow-neighbors-scaled
  [flow-info v sinks sources ^spork.cljgraph.flow.IScaling scaling]     
  (let [^java.util.ArrayList res (java.util.ArrayList.)]    
    (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
                     (do (when  (pos? (.scale scaling (edge-capacity (-edge-info flow-info v to)))) (.add acc to)))
                      acc)
                res
               sinks)
        (reduce-kv (fn [^java.util.ArrayList acc from w]
                     (do (when (pos? (.scale scaling (edge-flow (-edge-info flow-info from v))))  (.add acc from)))
                       acc)
                res 
               sources))))

;;We have some configuration that's going on here...
;;When computing flows, there are a couple of different pieces of the
;;pipeline..

;;flow-traversal 
;;    flow neighbors
;;       flow-sinks
;;       edge-capacity

;;edge-flow-computation 
;;   path->edge-flows 

;;augmentation 

;;Both flow-traversal and edge-flow computation 
;;are subject to significant alterations, via memoization
;;and scaling.  There may even be arbitrary ways to 
;;do this stuff.


;;We can then describe combinators that help us out.  

(defn default-neighbors [net source] 
  (flow-neighbors net source (-flow-sinks net source) (-flow-sources net source)))

(defn default-weight [net source sink]
  (-flow-weight net source sink))


(defmacro tagged [type name]
  `(with-meta (gensym ~name) {:tag ~type}))

;;Generic, customizable version.
(defmacro general-flow-traverse
  "Custom macro to walk a transient flow network."
  [net startnode targetnode startstate 
   & {:keys [weightf neighborf] 
      :or {weightf 'default-weight neighborf 'default-neighbors}}]
  (let [xs (tagged 'java.util.ArrayList "xs")]
    `(loop [state#   (-> (assoc! ~startstate :targetnode ~targetnode)
                         (generic/conj-fringe ~startnode 0))]
       (if-let [source#    (generic/next-fringe state#)] ;next node to visit
         (let  [visited#   (generic/visit-node state# source#)] ;record visit.
           (if (identical? ~targetnode source#) visited#                     
               (recur (let [~xs (~neighborf ~net source#)
                            n#   (.size ~xs)]
                        (loop [acc# visited#
                               idx# 0]
                          (if (== idx# n#) acc#                        
                              (recur (generic/relax acc# (~weightf ~net source# (m/get-arraylist ~xs idx#)) source# 
                                                    (m/get-arraylist ~xs idx#)
                                                    (generic/best-known-distance visited# source#))
                                     (unchecked-inc idx#))))))))
         state#)))) 

;;This should work for both transient and persistent networks.
(defn flow-traverse
  "Custom function to walk a transient flow network."
  [net startnode targetnode startstate]
  (general-flow-traverse net startnode targetnode startstate))

(defn flow-traverse-scaled 
  [net startnode targetnode startstate scaling]
  (general-flow-traverse net startnode targetnode startstate 
    :neighborf (fn [net source] 
                 (flow-neighbors-scaled net source  
                   (-flow-sinks net source) (-flow-sources net source) scaling))))

;; (defn flow-traverse-scaled
;;   "Custom function to walk a transient flow network."
;;   [net startnode targetnode startstate scaling]
;;   (loop [state   (-> (assoc! startstate :targetnode targetnode)
;;                      (generic/conj-fringe startnode 0))]
;;     (if-let [source    (generic/next-fringe state)] ;next node to visit
;;       (let  [visited   (generic/visit-node state source)] ;record visit.
;;         (if (identical? targetnode source) visited                     
;;             (recur (let [^java.util.ArrayList xs (flow-neighbors-scaled net source  
;;                                                                  (-flow-sinks net source) (-flow-sources net source) scaling)
;;                          n  (.size xs)]
;;                      (loop [acc visited
;;                                idx 0]
;;                        (if (== idx n) acc                        
;;                            (recur (generic/relax acc (-flow-weight net source (m/get-arraylist xs idx)) source 
;;                                                  (m/get-arraylist xs idx)
;;                                                  (generic/best-known-distance visited source))
;;                                   (unchecked-inc idx))))))))
;;          state))) 


  
;;formerly mincost-aug-pathme
(definline mincost-aug-path [g from to]
  `(searchstate/first-path (flow-traverse ~g ~from ~to (searchstate/mempty-PFS ~from))))

(definline mincost-aug-path-scaled [g from to scaling]
  `(searchstate/first-path (flow-traverse-scaled ~g ~from ~to (searchstate/mempty-PFS ~from) ~scaling)))

;;A container for augmenting flows
(defrecord edge-flows [^long flow ^java.util.ArrayList edges])

;;original
(comment

(defn ^edge-flows path->edge-flows [flow-info ^clojure.lang.ISeq p]
   (let [edges  (java.util.ArrayList. )]
    (loop [xs   (.next p)
           from (.first p)
           flow posinf]
      (if (empty? xs) (edge-flows. flow edges)
          (let [to     (.first xs)
                dir   (if (-get-direction flow-info from to) :increment :decrement)
                ^IEdgeInfo info (if (identical? dir :increment) 
                                  (-edge-info flow-info from to)
                                  (-edge-info flow-info to from))]
            (do (.add edges (.setDirection info dir))
                (recur (.next xs) to
                       (let [^long new-flow (if (identical? :increment dir)
                                                (edge-capacity info)
                                                (edge-flow info))] 
                         (min flow  new-flow)))))))))
)

(defmacro path-reduce [flow-info rfunc init p]
  (let [the-edge (with-meta (gensym "the-edge") {:tag 'spork.cljgraph.flow.IEdgeInfo})]
    `(loop [xs#   (.next ~p)
            from# (.first ~p)
            acc#  ~init]
       (if (empty? xs#) acc#
           (let [to#     (.first xs#)
                 dir#   (if (-get-direction ~flow-info from# to#) :increment :decrement)
                 ~the-edge (if (identical? dir# :increment) 
                             (-edge-info ~flow-info from# to#)
                             (-edge-info ~flow-info to# from#))]
                 (recur (.next xs#) to#
                        (~rfunc acc# dir# ~the-edge)))))))

;;Given a path and a network, reduce over the path's edge's, as
;;represented by the flow information in the network.
(defn ^edge-flows path->edge-flows [flow-info ^clojure.lang.ISeq p]
  (let [edges (java.util.ArrayList.)]
    (edge-flows. (path-reduce flow-info 
                   (fn [^long feasible-flow dir ^IEdgeInfo e] 
                     (do (.add edges (.setDirection e dir)) 
                         (min feasible-flow              
                              (if (identical? :increment dir)
                                (edge-capacity e)
                                (edge-flow e))))) posinf p) 
                 edges)))


;;Given a scaling context, reduce over the path's edges using a
;;the provided scaling factor, such that both flow and capacity are
;;scaled, and the resulting minimum flow is scaled.

(defn path->edge-flows-scaled [flow-info ^clojure.lang.ISeq p ^spork.cljgraph.flow.IScaling scaling]
  (let [edges (java.util.ArrayList.)]
    (edge-flows. (.unscale scaling 
                    (path-reduce flow-info 
                                 (fn [^long feasible-flow dir ^IEdgeInfo e] 
                                   (do (.add edges (.setDirection e dir)) 
                                       (min feasible-flow              
                                            (if (identical? :increment dir)
                                              (.scale scaling (edge-capacity e))
                                              (.scale scaling (edge-flow e))))))
                                 posinf p)) 
                    edges)))

;;Persistent augmentation actually sets the edge to the result 
;;of increasing flow.
(defn augment-flow [^spork.cljgraph.flow.IFlowNet the-net p get-edge-flows]
  (let [^edge-flows ef (get-edge-flows the-net p)
         flow (.flow ef)
         ^java.util.ArrayList xs   (.edges ef)
         n     (.size xs)]
    (loop [idx 0
           acc the-net]
      (if (== idx n) acc
          (recur (unchecked-inc idx)
                 (let [info (.get xs idx)]                  
                       (if (identical? :increment (edge-dir info))
                         (-push-flow acc info flow)
                         (-push-flow acc info (- flow)))))))))

;;High level API
;;==============



(defn mincost-flow
  ([graph from to]
     (loop [g graph]
       (if-let [p (mincost-aug-path g from to)]
         (recur (augment-flow g p path->edge-flows))
         (let [active (-active-flows g)]
           {:active active
            :net g}))))
  ([graph from to scaling]
     (loop [g graph]
       (if-let [p (mincost-aug-path-scaled g from to scaling)]
         (recur (augment-flow g p #(path->edge-flows-scaled %1 %2 scaling)))
         (let [active (-active-flows g)]
           {:active active
            :net g})))))



;;scaling affects our traversal

;;scaling also affects our flow

(defn augmentations
  ([graph from to]
     (let [augs (java.util.ArrayList.)]
       (loop [g graph]
         (if-let [p (mincost-aug-path g from to)]
           (do (let [f (.flow ^edge-flows (path->edge-flows g p))]
                 (.add augs [f p]))
               (recur (augment-flow g p)))
           (let [active (-active-flows g)]
             {:active active
              :augmentations augs
              :net g})))))
  ([graph from to scaling]
     (let [augs (java.util.ArrayList.)]
       (loop [g graph]
         (if-let [p (mincost-aug-path-scaled g from to scaling)]
           (do (let [f (.flow ^edge-flows (path->edge-flows-scaled g p scaling))]
                 (.add augs [f p]))
               (recur (augment-flow g p #(path->edge-flows-scaled %1 %2 scaling))))
           (let [active (-active-flows g)]
             {:active active
              :augmentations augs
              :net g}))))))





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


;;old accessors...
;;Possible bottleneck here.
;optimized
;; (defn inc-flow 
;;   ([g ^einfo info flow]     
;;      (assoc g :flow-info
;;             (assoc2 (:flow-info g) (:from info) (:to info)  
;;                     (-> info 
;;                         (.assoc :capacity (- (.capacity info) flow))
;;                         (.assoc :flow (+ (.flow info) flow))))))
;;   ([g from to flow] 
;;      (alter-edge the-edge g from to 
;;          (let [^einfo the-edge the-edge]
;;            (-> the-edge 
;;                (.assoc :capacity (- (.capacity the-edge) flow))
;;                (.assoc :flow     (+ (.flow the-edge) flow)))))))

;; (defn inc-flow! 
;;   ([^transient-net g ^einfo info flow]     
;;      (assoc! g :flow-info
;;             (assoc2! (:flow-info g) (.from info) (.to info)  
;;                      (-> info 
;;                          (.assoc :capacity (- (.capacity info) flow))
;;                          (.assoc :flow     (+ (.flow info) flow))))))
;;   ([g from to flow] 
;;      (alter-edge! the-edge g from to 
;;                    (let [^einfo e the-edge]
;;                      (->  e
;;                           (.assoc :capacity (- (.capacity ^einfo the-edge) flow))
;;                           (.assoc :flow     (+ (.flow ^einfo the-edge) flow)))))))

;; ;optimized
;; (defn dec-flow 
;;   ([g info flow]
;;      (assoc g :flow-info
;;             (assoc2  (:flow-info g)  (:from info) (:to info) 
;;                      (-> info
;;                          (assoc :capacity (+ (:capacity info) flow))
;;                          (assoc :flow     (- (:flow info) flow))))))
;;   ([g from to flow] 
;;      (alter-edge the-edge g from to
;;          (-> the-edge
;;              (assoc :capacity (+ (:capacity the-edge) flow))
;;              (assoc :flow     (- (:flow the-edge) flow))))))

;; (defn dec-flow!
;;   ([^transient-net g ^einfo info flow]
;;      (assoc! g :flow-info
;;             (assoc2!  (:flow-info g)  (.from info) (.to info) 
;;                       (-> info
;;                           (.assoc :capacity (+ (.capacity info) flow))
;;                           (.assoc :flow     (- (.flow info) flow))))))
;;   ([^transient-net g from to flow] 
;;      (alter-edge! the-edge g from to
;;        (let [^einfo e the-edge]
;;          (-> e
;;              (.assoc :capacity (+ (.capacity ^einfo the-edge) flow))
;;              (.assoc :flow     (- (.flow ^einfo the-edge) flow)))))))



;; (defn set-capacity [g from to cap] 
;;   (alter-edge the-edge g from to 
;;     (assoc the-edge :capacity cap)))

;; (defn set-flow [g from to flow]    
;;   (alter-edge the-edge g from to 
;;      (assoc the-edge :flow flow)))

;; ;;transient ops
;; (defn set-capacity! [g from to cap] 
;;   (alter-edge! the-edge g from to 
;;     (assoc the-edge :capacity cap)))

;; (defn set-flow! [g from to flow]    
;;   (alter-edge! the-edge g from to 
;;      (assoc the-edge :flow flow)))

;add a capacitated arc to the graph
;; (defn conj-cap-arc [g from to w cap]
;;   (let [finfo (:flow-info g)]
;;     (-> (graph/conj-arc g from to w)
;;         (assoc :flow-info finfo)
;;         (update-edge2* from to cap 0))))


;; ;;optimization spot.
;; (defn active-flows [g] 
;;   (generic/loop-reduce 
;;    (fn [acc ^einfo info] 
;;      (let [^long f (.flow info)]
;;        (if (> f 0)
;;          (cons  [[(.from info) (.to info)]  (.flow info)] acc) 
;;                 acc))) 
;;    '()
;;    (get-edge-infos g)))

;; ;;transient op
;; (defn active-flows! [g] 
;;   (let [^java.util.ArrayList xs  (get-edge-infos! g)
;;         n (count xs)]    
;;     (loop [idx 0
;;            acc '()]
;;       (if (== idx n) acc
;;           (let [^einfo info (m/get-arraylist xs idx)
;;                 ^long f (.flow info)]
;;             (recur (unchecked-inc idx)
;;                    (if (pos? f)
;;                      (cons (clojure.lang.MapEntry. (clojure.lang.MapEntry. (.from info) (.to info))  f) acc)
;;                      acc)))))))


;;Maybe useful? 

;; (defn flow-provider-type [g nd]
;;   (if (not (graph/island? g nd))
;;       (cond (graph/terminal-node? g nd) :sinks
;;             (empty? (graph/sinks g nd))  :source
;;             :else :trans)
;;       :island))

;; (defn flow-topology [g start-node]
;;   (group-by (partial flow-provider-type g)
;;             (graph/succs g start-node)))

;; (defn ^java.util.ArrayList -flow-neighbors 
;;   [g flow-info v]     
;;   (let [^java.util.ArrayList res (java.util.ArrayList.)]    
;;     (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
;;                      (do (when  (pos? (.capacity ^einfo (-edge-info flow-info v to))) (.add acc to))
;;                       acc))
;;                 res
;;                 (get2 g :sinks v nil))
;;         (reduce-kv (fn [^java.util.ArrayList acc from w]
;;                      (do (when (pos? (.flow ^einfo (-edge-info flow-info from v)))  (.add acc from))
;;                        acc))
;;                 res 
;;                 (get2 g :sources v nil)))))


;; ;;A function for caching flow weight calls.
;; (defn flow-weighter2 [g forward-pred]
;;   (memoize (fn [g from to] 
;;              (if (forward-pred g from to) (generic/-arc-weight g from to) ;forward arc
;;                  (- (generic/-arc-weight g to from))))))


;; (defn transient-flow-traverse
;;   "Custom function to walk a transient flow network."
;;   [net startnode targetnode startstate]
;;   (let [g (:g net)]
;;     (loop [state   (-> (assoc! startstate :targetnode targetnode)
;;                        (generic/conj-fringe startnode 0))]
;;       (if-let [source    (generic/next-fringe state)] ;next node to visit
;;         (let  [visited   (generic/visit-node state source)] ;record visit.
;;           (if (identical? targetnode source) visited                     
;;               (recur (let [xs (flow-neighbors g net source)
;;                            n  (count xs)]
;;                        (loop [acc visited
;;                               idx 0]
;;                          (if (== idx n) acc                        
;;                              (recur (generic/relax acc (flow-weight2 g source (m/get-arraylist xs idx)) source 
;;                                                                               (m/get-arraylist xs idx)
;;                                                    (generic/best-known-distance visited source))
;;                                     (unchecked-inc idx))))))))
;;         state))))


;; (defn -transient-flow-traverse
;;   "Custom function to walk a transient flow network."
;;   [^transient-net net startnode targetnode startstate]
;;   (let [g (:g net)]
;;     (loop [state   (-> (assoc! startstate :targetnode targetnode)
;;                        (generic/conj-fringe startnode 0))]
;;       (if-let [source    (generic/next-fringe state)] ;next node to visit
;;         (let  [visited   (generic/visit-node state source)] ;record visit.
;;           (if (identical? targetnode source) visited                     
;;               (recur (let [xs (-flow-neighbors g net source)
;;                            n  (count xs)]
;;                        (loop [acc visited
;;                               idx 0]
;;                          (if (== idx n) acc                        
;;                              (recur (generic/relax acc (flow-weight2 g source (m/get-arraylist xs idx)) source 
;;                                                                               (m/get-arraylist xs idx)
;;                                                    (generic/best-known-distance visited source))
;;                                     (unchecked-inc idx))))))))
;;         state))))


;;based off of traverse2e
;; (defn flow-traverse
;;   "Custom function to walk a flow network."
;;   [g startnode targetnode startstate]
;;   (loop [state   (-> (assoc! startstate :targetnode targetnode)
;;                      (generic/conj-fringe startnode 0))]
;;     (if-let [source    (generic/next-fringe state)] ;next node to visit
;;       (let  [visited   (generic/visit-node state source)] ;record visit.
;;         (if (identical? targetnode source) visited                     
;;             (recur (let [xs (flow-neighbors g g source)
;;                          n  (count xs)]
;;                      (loop [acc visited
;;                             idx 0]
;;                        (if (== idx n) acc                        
;;                            (recur (generic/relax acc (flow-weight2 g source 
;;                                                         (m/get-arraylist  xs idx)) source 
;;                                                         (m/get-arraylist  xs idx)
;;                                                         (generic/best-known-distance visited source))
;;                                   (unchecked-inc idx))))))))
;;       state)))


;; (defn ^java.util.ArrayList flow-neighbors 
;;   [g flow-info v]     
;;   (let [^java.util.ArrayList res (java.util.ArrayList.)]    
;;     (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
;;                      (do (when  (pos? (.capacity ^einfo (edge-info flow-info v to))) (.add acc to))
;;                       acc))
;;                 res
;;                 (get2 g :sinks v nil))
;;         (reduce-kv (fn [^java.util.ArrayList acc from w]
;;                      (do (when (pos? (.flow ^einfo (edge-info flow-info from v)))  (.add acc from))
;;                        acc))
;;                 res 
;;                 (get2 g :sources v nil)))))

;;the flow-cost for g from to.  Forward arcs are positive cost.
;;Backward arcs are negative-cost.
;; (definline flow-weight2 [g from to]
;;   `(if (forward? ~g ~from ~to) (generic/-arc-weight ~g ~from ~to) ;forward arc
;;       (- (generic/-arc-weight ~g ~to ~from))))


;; (definline mincost-aug-path! [g from to]
;;   `(searchstate/first-path! (transient-flow-traverse ~g ~from ~to (searchstate/mempty-PFS ~from))))

;; (definline -mincost-aug-path! [g from to]
;;   `(searchstate/first-path! (-transient-flow-traverse ~g ~from ~to (searchstate/mempty-PFS ~from))))

;; (definline -mincost-aug-path2! [g from to]
;;   `(searchstate/first-path! (-transient-flow-traverse2 ~g ~from ~to (searchstate/mempty-PFS ~from))))


;; (defn ^edge-flows -path->edge-flows! [^transient-net flow-info ^clojure.lang.ISeq p]
;;    (let [g     (:g flow-info)
;;          edges  (java.util.ArrayList. )]
;;     (loop [xs   (.next p)
;;            from (.first p)
;;            flow posinf]
;;       (if (empty? xs) (edge-flows. flow edges)
;;           (let [to     (.first xs)
;;                 dir   (if (forward? g from to) :increment :decrement)
;;                 ^einfo info (if (identical? dir :increment) 
;;                               (.-edge-info flow-info from to)
;;                               (.-edge-info flow-info to from))]
;;             (do (.add edges (.assoc info :dir dir))
;;                 (recur (.next xs) to
;;                        (let [^long new-flow (if (identical? :increment dir)
;;                                                 (.capacity info)
;;                                                 (.flow info))] 
;;                          (min flow  new-flow))))))))) 


;; ;;optimized?
;; (defn path->edge-info [g flow-info p]
;;   (loop [acc []
;;          xs  p]
;;     (if (nil? (next xs)) acc
;;         (let [from (first  xs)
;;               to   (second xs)]
;;           (recur (conj acc (if (forward? g from to)
;;                              (.assoc ^einfo (edge-info flow-info from to) :dir :increment)
;;                              (.assoc ^einfo (edge-info flow-info to from) :dir :decrement)))
;;                  (next xs))))))     

;; ;;find the maximum flow that the path can support 
;; (defn maximum-flow [g infos]
;;   (loop [^einfo info (first infos)
;;          xs   (rest infos)
;;          flow posinf]
;;     (let [new-flow (if (= :increment (.dir info))
;;                        (.capacity info)
;;                        (.flow info))
;;           next-flow (long (min flow new-flow))]
;;       (if (empty? xs) next-flow
;;           (recur (first xs) (rest xs) next-flow)))))

;; ;;Eliminate reduce.
;; ;;apply an amount of flow along the path, dropping any nodes that 
;; ;;become incapacitated.
;; (defn apply-flow [g edges flow]
;;   (generic/loop-reduce 
;;       (fn [gr info]
;;         (if (identical? :increment (:dir info))
;;           (-inc-edge-flow gr info flow)
;;           (-dec-edge-flow gr info flow)))
;;       g edges))

;; ;;helper function to apply flow.
;; (defn augment-flow [g p]
;;   (let [edges (path->edge-info g g p)]
;;     (apply-flow g edges (maximum-flow g edges))))

;; ;;find the mincost flow, in graph, from -> to, where graph is a directed graph 
;; ;;and contains a key :flow-info with compatible network flow information.
;; (defn mincost-flow* 
;;   ([graph from to]
;;     (loop [g graph]
;;       (if-let [p (mincost-aug-path g from to)]
;;         (recur (augment-flow g p))
;;         (let [active (active-flows g)]
;;           {:active active
;;            :net g}))))
;;   ([flow-info graph from to]
;;     (mincost-flow* (assoc graph :flow-info flow-info) from to)))

;; (defn ^edge-flows path->edge-flows! [flow-info ^clojure.lang.ISeq p]
;;    (let [g     (:g flow-info)
;;          edges  (java.util.ArrayList. )]
;;     (loop [xs   (.next p)
;;            from (.first p)
;;            flow posinf]
;;       (if (empty? xs) (edge-flows. flow edges)
;;           (let [to     (.first xs)
;;                 dir   (if (forward? g from to) :increment :decrement)
;;                 ^einfo info (if (identical? dir :increment) 
;;                               (edge-info flow-info from to)
;;                               (edge-info flow-info to from))]
;;             (do (.add edges (.assoc info :dir dir))
;;                 (recur (.next xs) to
;;                        (let [^long new-flow (if (identical? :increment dir)
;;                                                 (.capacity info)
;;                                                 (.flow info))] 
;;                          (min flow  new-flow)))))))))    
 
;; (defn augment-flow! [^transient-net the-net p]
;;   (let [^edge-flows ef (path->edge-flows! the-net p)
;;          flow (.flow ef)
;;          ^java.util.ArrayList xs   (.edges ef)
;;          n     (count xs)]
;;     (loop [idx 0
;;            ^transient-net acc the-net]
;;       (if (== idx n) acc
;;           (recur (unchecked-inc idx)
;;                  (let [^einfo info (.get xs idx)]                  
;;                    (if (identical? :increment (.dir info))
;;                      (inc-flow! the-net info flow)
;;                      (dec-flow! the-net info flow)))))))) 

;; (defn -augment-flow! [^transient-net the-net p]
;;   (let [^edge-flows ef  (-path->edge-flows! the-net p)
;;          flow (.flow ef)
;;          ^java.util.ArrayList xs   (.edges ef)
;;          n     (count xs)]
;;     (loop [idx 0
;;            ^transient-net acc the-net]
;;       (if (== idx n) acc
;;           (recur (unchecked-inc idx)
;;                  (let [^einfo info (.get xs idx)]                  
;;                    (if (identical? :increment (.dir info))
;;                      (-inc-edge-flow the-net info  flow)
;;                      (-inc-edge-flow the-net info (- flow)))))))))


;; (defn -mincost-flow!
;;   ([graph from to]
;;     (loop [g (transient-network graph)]
;;       (if-let [p (-mincost-aug-path! g from to)]
;;         (recur (-augment-flow! g p))
;;         (let [active (active-flows! g)]
;;           {
;;            ;:cost (total-cost graph active)
;;            ;:flow (total-flow g active)
;;            :active active
;;            :net g}))))
;;   ([flow-info graph from to]
;;     (mincost-flow! (assoc graph :flow-info flow-info) from to)))

;; (defn -mincost-flow2!
;;   ([graph from to]
;;     (loop [g (transient-network2 graph)]
;;       (if-let [p (-mincost-aug-path2! g from to)]
;;         (recur (-augment-flow2! g p))
;;         (let [active (-active-flows g)]
;;           {
;;            ;:cost (total-cost graph active)
;;            ;:flow (total-flow g active)
;;            :active active
;;            :net g}))))
;;   ([flow-info graph from to]
;;     (mincost-flow! (assoc graph :flow-info flow-info) from to)))

;;protocol for performing updates based on 
;;compound edge datastructures, rather than 
;;from to pairs.  This is more efficicent 
;;for augmenting flows.

;  (-set-edge-flow      [net edge flow])
;  (-set-edge-capacity  [net edge cap])


;; (defprotocol IDirected
;;   (-get-from      [d])
;;   (-get-to        [d])
;;   (-directed-pair [d]))


;;;;trans
  ;; clojure.lang.IEditableCollection ;transient
  ;; (asTransient [net] (transient-network net))
  ;; clojure.lang.ITransientCollection ;conj!, persistent!
  ;; (conj       [net v] (throw (Exception. "conj! currently not supported on networks")))
  ;; (persistent [net]   (persistent-network! net)) 
  ;; IEdgeInfo
  ;; (setFlow     [net from to flow]
  ;;       (do (set! flow-info
  ;;                 (assoc2! flow-info from to 
  ;;                          (.-set-flow ^spork.cljgraph.flow.IEdgeInfo 
  ;;                             (.-edge-info net from to) from to flow)))
  ;;           net))                 
  ;; (setCapacity [net from to cap] 
  ;;        (do (set! flow-info
  ;;                  (assoc2! flow-info from to 
  ;;                           (.-set-capacity  ^spork.cljgraph.flow.IEdgeInfo 
  ;;                              (.-edge-info net from to) from to cap)))
  ;;            net))                 
  ;; (incFlow     [net from to amt] 
  ;;        (do (set! flow-info
  ;;                  (assoc2! flow-info from to 
  ;;                           (.-inc-flow ^spork.cljgraph.flow.IEdgeInfo  
  ;;                              (.-edge-info net from to) from to amt)))
  ;;            net))
  ;; (flow     [net from to] (edge-flow     (.-edge-info net from to)))
  ;; (capacity [net from to] (edge-capacity (.-edge-info net from to)))            
  ;; (from         [e] (unsupported 'from))
  ;; (to           [e] (unsupported 'to))
  ;; (dir          [e] (unsupported 'dir))
  ;; (directedPair [e] (unsupported 'directed-pair ))

;;;;trans2
  ;; clojure.lang.IEditableCollection ;transient
  ;; (asTransient [net] (transient-network2 net))
  ;; clojure.lang.ITransientCollection ;conj!, persistent!
  ;; (conj       [net v] (throw (Exception. "conj! currently not supported on networks")))
  ;; (persistent [net]   (persistent-network! net)) 
  ;; IEdgeInfo
  ;; (setFlow     [net from to flow]
  ;;     (do (.setFlow ^meinfo (.-edge-info flow-info from to) from to flow)
  ;;         net))                 
  ;; (setCapacity [net from to cap] 
  ;;     (do (.setCapacity ^meinfo (.-edge-info flow-info from to) from to cap)
  ;;         net))                 
  ;; (incFlow     [net from to amt] 
  ;;     (do (.incFlow ^meinfo (.-edge-info flow-info from to) from to amt)
  ;;         net))
  ;; (flow         [net from to] (edge-flow     (.-edge-info net from to)))
  ;; (capacity     [net from to] (edge-capacity (.-edge-info net from to)))            
  ;; (from         [e] (unsupported 'from))
  ;; (to           [e] (unsupported 'to))
  ;; (dir          [e] (unsupported 'dir))
  ;; (directedPair [e] (unsupported 'directed-pair ))

;;;;digraph
;; IEdgeInfo
;;   (-set-flow      [net from to flow] 
;;     (let [^einfo e (-edge-info net from to)]
;;       (-set-edge net e (.setFlow e nil nil flow))))
;;   (-set-capacity  [net from to cap]  
;;     (let [^einfo e (-edge-info net from to)]
;;       (-set-edge net e (.setCapacity e nil nil cap))))
;;   (-inc-flow      [net from to flow]  
;;     (-set-edge net e
;;        (inc-flow (-edge-info net from to) nil nil flow)))
;;   (flow      [net from to]  (edge-flow  (.-edge-info net from to)))
;;   (capacity  [net from to]  (edge-capacity  (.-edge-info net from to)))
;;   (from         [e] (unsupported 'from))
;;   (to           [e] (unsupported 'to))
;;   (dir          [e] (unsupported 'dir))
;;   (directedPair [e] (unsupported 'directed-pair ))


;; (defmacro with-hint [x hint] 
;;   `(vary-meta (quote ~x) assoc :tag (quote ~hint)))

;;we could define a macro that defines different ways to get
;;edge-flows...
;; (defmacro generic-path->edgeflows [flow-info p & {:keys [edge-symb get-edge-info final-flow-expr edges-expr]
;;                                                   :or {edge-symb 'info
;;                                                        get-edge-info '-edge-info
;;                                                        final-flow-expr 'id
;;                                                        edges-expr      'id}}
                                                       
;;   `(let [edges#  (java.util.ArrayList.)]
;;      (loop [xs#   (.next  ~p)
;;             from# (.first ~p)
;;             flow# posinf]
;;        (if (empty? xs#)  (edge-flows. (~final-flow-expr flow#) (~edges-expr edges#))
;;            (let [to#     (.first xs#)
;;                  dir#    (if (-get-direction flow-info# from# to#) :increment :decrement)
;;                  ^IEdgeInfo the-edge# (~edge-expr flow-info# from# to#)]
;;              (do (.add edges# (.setDirection the-edge# dir#))
;;                  (recur (.next xs#) to#
;;                         (let [^long new-flow# (~flow-expr dir# the-edge#)] 
;;                           (min flow# new-flow#))))))))


;; (defedgeflow path->edge-flows-scaled [scaling]
;;   (fn [flow-info p]
;;     (generic-path->edgeflows flow-info p 
;;        :edge-symb (with-hint info IEdge)
;;        :edges the-edges 
;;        :flow  the-flow
;;        :final-flow-expr (edge-flows. (.unscale scaling the-flow) the-edges))


;; (defn ^edge-flows path->edge-flows-scaled [flow-info ^clojure.lang.ISeq p ^spork.cljgraph.flow.IScaling scaling]
;;    (let [edges  (java.util.ArrayList. )]
;;     (loop [xs   (.next p)
;;            from (.first p)
;;            flow posinf]
;;       (if (empty? xs) (edge-flows. (.unscale scaling flow) edges)
;;           (let [to     (.first xs)
;;                 dir   (if (-get-direction flow-info from to) :increment :decrement)
;;                 ^IEdgeInfo info (if (identical? dir :increment) 
;;                                   (-edge-info flow-info from to)
;;                                   (-edge-info flow-info to from))]
;;             (do (.add edges (.setDirection info dir))
;;                 (recur (.next xs) to
;;                        (let [^long new-flow (if (identical? :increment dir)
;;                                                 (.scale scaling (edge-capacity info))
;;                                                 (.scale scaling (edge-flow info)))] 
;;                          (min flow  new-flow)))))))))






;;beta
;;========

;; (defprotocol IFlowContext
;;   (internal-flow-neighbors   [fl net source])
;;   (internal-path->edge-flows [fl net p])
;;   (internal-augment-flow     [f1 p])
;;   (internal-flow-weight      [f1 from to]))

;; (definline mincost-aug-path-generic [g from to ctx]
;;   `(searchstate/first-path (flow-traverse-generic ~g ~from ~to (searchstate/mempty-PFS ~from) ~ctx)))

;; (defn flow-traverse-generic 
;;   "Custom function to walk a transient flow network."
;;   [net startnode targetnode startstate ^IFlowContext ctx]
;;   (loop [state   (-> (assoc! startstate :targetnode targetnode)
;;                      (generic/conj-fringe startnode 0))]
;;     (if-let [source    (generic/next-fringe state)]       ;next node to visit
;;       (let  [visited   (generic/visit-node state source)] ;record visit.
;;         (if (identical? targetnode source) visited                     
;;             (recur (let [^java.util.ArrayList xs (.internal-flow-neighbors ctx net source)
;;                          n  (.size xs)]
;;                      (loop [acc visited
;;                                idx 0]
;;                        (if (== idx n) acc                        
;;                            (recur (generic/relax acc (.internal-flow-weight ctx net source (m/get-arraylist xs idx)) source 
;;                                                  (m/get-arraylist xs idx)
;;                                                  (generic/best-known-distance visited source))
;;                                   (unchecked-inc idx))))))))
;;          state)))


;;(defn memoized-mincost-flow
  ;; [graph from to memo-context]     
  ;; (loop [g graph]
  ;;   (if-let [p (mincost-aug-path-memoized g from to memo-context)]
  ;;     (recur (augment-flow g p))
  ;;     (let [active (-active-flows g)]
  ;;       {:active active
  ;;        :net g}))))



;;Due to the overhead associated with modifying networks, 
;;we end up desiring mutation for efficiency.  A transient 
;;net is just a wrapper around the original persistent network, 
;;with a set of mutable flow-info and meta data.
;; (m/defmutable transient-net [g flow-info metadata]
;;   clojure.lang.IObj
;;   ;adds metadata support
;;   (meta [this] metadata)
;;   (withMeta [this m] (do (set! metadata m) this))
;;   IFlowNet
;;   (-edge-info      [net from to] 
;;          (if-let [^einfo  res (get2 flow-info from to nil)]
;;            res
;;            (->edge-info2 from to)))
;;   (einfos [net] (get-edge-infos! net))
;;   (-get-direction [net from to]  (forward? g from to))
;;   (-flow-weight    [net from to] (forward-flow g from to))
;;   (-set-edge [net edge]         
;;              (let [^einfo e edge
;;                    from (.from e)
;;                    to   (.to   e)]
;;                (do (set! flow-info
;;                          (assoc2! flow-info from to 
;;                                   edge))
;;                    net)))
;;   (-flow-sinks     [net x] (get2 g :sinks x nil))
;;   (-flow-sources   [net x] (get2 g :sources x nil))
;;   (-push-flow      [net edge flow] (-set-edge net (inc-flow edge flow)))
;;   IDynamicFlow
;;   (-conj-cap-arc [net from to w cap]  (throw (Exception. "unsupported op -conj-cap-arc"))) 
;;   (-active-flows [net] 
;;                  (let [^java.util.ArrayList xs  (get-edge-infos! net)
;;                        n (count xs)]    
;;                    (loop [idx 0
;;                           acc '()]
;;                      (if (== idx n) acc
;;                          (let [^einfo info (m/get-arraylist xs idx)
;;                                ^long f (edge-flow info)]
;;                            (recur (unchecked-inc idx)
;;                                   (if (pos? f)
;;                                     (cons (clojure.lang.MapEntry. (edge-pair info)  f) acc)
;;                                     acc)))))))) 
