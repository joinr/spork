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
            [spork.util.general :refer [assoc2 assoc2! dissoc2 dissoc2! get2 transient2 
                                        persistent2! hinted-get2 kv-reduce2 kv-map2 memo-fn]]
            [spork.util.metaprogramming :refer [id tagged binding-keys key->symb]]))

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
  (-update-edge    [net from to f])
  (-disj-cap-arc   [net from to])
  (-conj-cap-arc   [net from to w cap])
  (-active-flows   [net]))

;;Many flow networks will be dynamically scaled.  Rather than 
;;modifying the network, we formalize the notion of a scaling.
(defprotocol IScaling
  (scale     [n x])
  (unscale   [n x]))

;;Another idea here...
;;Have a container with the function fields that we need.
;;Let the user provide over-rides...

(defrecord scaled-int-flow [^long factor]
  IScaling
  (scale [n x] (quot x factor))
  (unscale [n x] (* ^long x factor)))

(defrecord general-scaled-flow [scale-func unscale-func]
  IScaling
  (scale [n x] (scale-func x))
  (unscale [n x] (unscale-func x)))

(defrecord variable-scaled-flow [scalar]
  IScaling
  (scale [n x]   (quot x @scalar))
  (unscale [n x] (* x @scalar)))

(defn scaling? [x] (satisfies? IScaling x))
(defn as-scale [x] 
  (cond (scaling? x) scale            
        (instance? clojure.lang.IDeref x) (->variable-scaled-flow x)
        (number? x) (->scaled-int-flow x)
        :else (throw (Exception. "Cannot make a flow scaler out of " x))))

;;Note -> protocols are as efficient as interfaces if the dispatch is 
;;performed via a hinted field call.  If dispatch is performed via the 
;;protocol function, it is still within 1.75x as fast as a member 
;;dispatch, which is tolerable.
(defprotocol IEdgeInfo
  (set-flow      [e flow])
  (set-capacity  [e cap])
  (set-direction [e d])
  (inc-flow      [e amt])
  (edge-flow     [e])
  (edge-capacity [e])
  (capacity-to   [e v])
  (edge-from     [e])
  (edge-to       [e])
  (edge-dir      [e])
  (edge-pair [e]))

;;Edge Data Types
;;===============

;;We define a persistent record to capture capacitated flow
;;information across edges.
(defrecord einfo [from to capacity flow dir]
  IEdgeInfo
  (set-flow      [edge new-flow] (einfo. from to  capacity new-flow dir))
  (set-capacity  [edge cap]      (einfo. from to cap flow dir))
  (set-direction [edge d]   (einfo. from to capacity flow d))
  (inc-flow      [edge amt] (einfo. from to   (unchecked-subtract capacity amt)  
                                              (unchecked-add flow amt) dir))
  (edge-flow     [edge] flow)
  (edge-capacity [edge] capacity)  
  (capacity-to   [edge v]   (if (identical? v to) capacity flow))
  (edge-from     [e] from)
  (edge-to       [e] to)
  (edge-dir      [e] dir)
  (edge-pair [e] (clojure.lang.MapEntry. from to)))

;;A mutable edge list.  For mutable stuff.  Mutation.  Mutants.
;;This ought to be good for small graphs.
(m/defmutable meinfo [from to capacity flow dir]
  IEdgeInfo
  (set-flow      [edge new-flow] (do (set! flow new-flow) edge))
  (set-capacity  [edge cap]      (do (set! capacity cap)  edge))
  (set-direction [edge d]        (do (set! dir d) edge))
  (inc-flow      [edge amt]      (do (set! capacity (unchecked-subtract capacity amt))
                                     (set! flow     (unchecked-add flow amt))))
  (edge-flow     [edge] flow)
  (edge-capacity [edge] capacity)
  (capacity-to   [edge v]   (if (identical? v to) capacity flow))
  (edge-from     [d]    from)
  (edge-to       [d]    to)
  (edge-dir      [e]    dir)
  (edge-pair [d]    (clojure.lang.MapEntry. from to)))

;;Inline functions for constructing edges.  We adopt the convention 
;;of delineating the arity of these functions in the numerical suffix.

;;These are each 10x faster then the original varargs implementation.
(definline ->edge-info2 
  [from to]
  `(einfo. ~from  ~to posinf 0 :forward))

(definline ->edge-info3
  [from to dir]
  `(einfo. ~from  ~to posinf 0 ~dir))

(definline ->edge-info4 
  [from to capacity flow]
  `(einfo. ~from ~to ~capacity ~flow :forward))

;;Constructors for mutable edges.
(definline ->medge-info2 
  [from to]
  `(meinfo. ~from  ~to posinf 0 :forward))

(definline ->medge-info3 
  [from to dir]
  `(meinfo. ~from  ~to posinf 0 ~dir))

(definline ->medge-info4 
  [from to capacity flow]
  `(meinfo. ~from ~to ~capacity ~flow :forward))    

(defmacro edge-hint [e] 
  `(vary-meta ~e assoc :tag 'spork.cljgraph.flow.IEdgeInfo))

(definline has-capacity-to? [e v]
  `(pos? (capacity-to ~e ~v)))
 
;;Note:
;;Might need to move the direction component into a protocol, or lift
;;it out entirely.

(defn  edge->medge [^einfo edge]
  (meinfo. (.from edge) (.to edge) (.capacity edge) (.flow edge) (.dir edge)))

(defn ^einfo medge->edge [^meinfo edge]
  (einfo. (.edge-from edge) (.edge-to edge) (.edge-capacity edge) (.edge-flow edge) (.edge-dir edge)))

;;Shared inline definitions for network topology.
;;This is currently a bit slow due to some overhead.
(definline forward? [g from to] 
  `(contains? (get (:sinks ~g)  ~from) ~to))

;;Current function, should be replaced by the commented one below.
(definline get-edge-infos [n]
  `(for [[from# vs#] (:flow-info ~n)
         [to# info#] vs#]
     info#))

;;should be faster.
;;================
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

;;This should be a bit faster.  We can get even faster if we 
;;insert some kind of mutable record container.
(definline update-edge2*  
  [g from to cap flow]
  `(assoc ~g :flow-info                  
     (assoc2 (get ~g :flow-info {})
       ~from ~to 
       (einfo. ~from ~to ~cap ~flow :forward))))

(defmacro alter-edge [sym g from to & expr]
  `(let [~(vary-meta sym assoc :tag 'spork.cljgraph.IEdgeInfo) (-edge-info ~g ~from ~to)]
     (assoc ~g :flow-info
            (assoc2 (get ~g :flow-info {})
                    ~from ~to 
                    ~@expr))))    
;;transient op 
(defmacro alter-edge! [sym g from to & expr]
  `(let [~(vary-meta sym assoc :tag 'spork.cljgraph.IEdgeInfo) (-edge-info ~g ~from ~to)]
     (assoc! ~g :flow-info
            (assoc2! (get ~g :flow-info {})
                    ~from ~to 
                    ~@expr))))

(defn current-capacity 
  ([^einfo info] (- (.capacity info) (.flow info)))
  ([g from to] (current-capacity (-edge-info g from to))))

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
     (set-capacity the-edge# (+ ~amt (edge-capacity the-edge#)))))

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

(definline direction [g from to]
  `(if (forward? ~g ~from ~to)
     :forward
     :backward))

;;Network Data Types
;;==================

;;our persistent network is actually a digraph.  Since our digraph is
;;implemented as a record, we can store information in it like a map.
;;Our flow information happens to live in this map.  Note, this has
;;performance implications.
(extend-type spork.data.digraph.digraph  
  IFlowNet
  (-edge-info     [net from to]  
    (let [finfo (get net :flow-info)]
      (if-let [fwd (get2 finfo from to nil)]
        fwd
        (if-let [bwd (get2 finfo to from nil)]
          bwd
          (->edge-info3 from to (direction net from to))))))
  (einfos         [n]             (get-edge-infos n))
  (-get-direction [net from to]   (direction net from to))
  (-flow-weight   [net from to]   (forward-flow net from to))
  (-set-edge      [net edge]         
    (assoc net :flow-info                  
           (assoc2 (get net :flow-info {})
                   (.from ^einfo edge) (.to ^einfo edge) 
                   edge)))
  (-flow-sinks     [net x] (get2 net :sinks   x nil))
  (-flow-sources   [net x] (get2 net :sources x nil))
  (-push-flow      [net edge flow] (-set-edge net 
                                     (inc-flow edge  flow)))
  IDynamicFlow
  (-update-edge  [net from to f] 
    (let [edge (-edge-info net from to)]
      (assoc net :flow-info                  
             (assoc2 (get net :flow-info {})
                     from to  (f edge)))))
  (-disj-cap-arc   [net from to]  
    (let [finfo (dissoc2 (get net :flow-info) from to)]
           (-> (graph/disj-arc net from to)
               (assoc :flow-info finfo))))               
  (-conj-cap-arc [net from to w cap]  
         (let [finfo (get net :flow-info)]
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
    (if-let [^meinfo  fwd (get2 flow-info from to nil)]
      fwd
      (if-let [^meinfo bwd (get2 flow-info to from nil)]
        bwd
        (let [edge (->medge-info3 from to (direction g from to))]
          (do (assoc2! flow-info from to edge)
              edge)))))
  (einfos [n]     (get-edge-infos! n))
  (-get-direction [net from to] (direction g from to))
  (-flow-weight   [net from to] (forward-flow g from to))
  (-set-edge [net edge]         
     (let [^meinfo e edge
           from (.edge-from e)
           to   (.edge-to   e)]
       (do (assoc2! flow-info from to edge)
           net)))
  (-flow-sinks     [net x] (get2 g :sinks x nil))
  (-flow-sources   [net x] (get2 g :sources x nil))
  (-push-flow      [net edge flow]                   
       (do (inc-flow edge  flow)
           net))
  IDynamicFlow 
  (-update-edge  [net from to f] 
    (let [edge (-edge-info net from to)]
      (do (when-let [res (f edge)]
            (assoc2! flow-info from to res))
          net)))
  (-disj-cap-arc   [net from to]  
    (do (dissoc2! flow-info from to)
        (set! g  (graph/disj-arc net from to))
        net))
  (-conj-cap-arc [net from to w cap]                 
         (do (set! g  (graph/conj-arc g from to w))
             (set! flow-info
                   (assoc2! flow-info
                            from to  
                            (meinfo. from to cap 0 :forward)))
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
                        (get g :flow-info))]        
  (transient-net.  g  flow-info! {:flow-info (:flow-info g)})))

(defn persistent-network! [^transient-net the-net]
  (let [g (:g the-net)]
    (assoc g
           :flow-info (kv-map2 (fn [_ _ v] (medge->edge v)) (persistent2! (get the-net :flow-info))))))

;;THe API prizes edge-update by accessing edge infos 
;;directly.  Consequently, we only want to pay the cost 
;;of looking up an edge once.  
(defn flows [g] 
  (for [[from vs] (:flow-info g)
        [to info] vs]
    [[from to] (select-keys info [:capacity :flow])]))

(defn enumerate-edges [g]
  (let [infos (get g :flow-info)
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
         (+  acc (* flow (graph/arc-weight g from to)))))
            0 active-edges))
  ([g] (total-cost g (-active-flows g))))

(defmacro edge-reduce [net node f & {:keys [get-children] :or {get-children 'spork.cljgraph.flow/flow-sinks}}]
  `(let [dfunc#    ~get-children
         visited# (atom (transient {}))
         visited?# (fn [from# to#]
                     (spork.util.general/get2 @visited# from# to# nil))
         visit!#   (fn [from# to#]
                     (reset! visited# (spork.util.general/assoc2! @visited# from# to# 1)))]
     (loop [fringe# #{~node}
            acc#    ~net](if (zero? (.count fringe#)) acc#
                             (let [from# (first fringe#)
                                   parents# (reduce-kv
                                             (fn [acc# k# v#] (conj acc# k#)) #{} (dfunc# ~net from#))]
                               (reduce (fn [inner-acc# to#]
                                         (if (visited?# from# to#)
                                           inner-acc#
                                           (let [~'_ (visit!# from# to#)
                                                 e#  (spork.cljgraph.flow/-edge-info
                                                      inner-acc# to# from#)]
                                             (~f inner-acc# e#))))
                                       acc# parents#))))))

(defmacro edge-map [net node f & {:keys [get-children] :or {get-children 'spork.cljgraph.flow/flow-sinks}}]
  `(edge-reduce ~net ~node
                (fn [acc# edge#]
                  (let [res# (~f edge#)]
                    (if (identical? res# edge#) acc#
                        (spork.cljgraph.flow/-set-edge acc# res#))))
                :get-children ~get-children))
                  
  

;;Flows and Augmenting Paths
;;==========================
(definline empty-search [from] `(searchstate/mempty-PFS ~from))

;;another option is to compute incident edges.
;;From that, return a pair of ins and outs.
;;rather than cram it all into the neighbors fn.
;;Flow scaling operates on the edge capacities, which are 
;;stored in actual edge objects.
;;So we need to 

;;Might be a faster way to do this, possibly cache via protocol.
(defn ^java.util.ArrayList flow-neighbors
  [flow-info v]     
  (let [^java.util.ArrayList res (java.util.ArrayList.)]    
    (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
                     (do (when  (pos? (edge-capacity (-edge-info flow-info v to))) (.add acc to))
                      acc))
                res
               (-flow-sinks flow-info v))
        (reduce-kv (fn [^java.util.ArrayList acc from w]
                     (do (when (pos? (edge-flow (-edge-info flow-info from v)))  (.add acc from))
                       acc))
                res 
               (-flow-sources flow-info v)))))

;;we can revision this guy...
(defn ^java.util.ArrayList flow-neighbors-by
  "Generically parameterize how to compute flow-neighbors.  Given a function that gets the 
   sink node labels, the source node labels, and filters IEdgeInfo edges, we can compute 
   the neighbors in a general manner."
  [flow-info v get-sinks get-sources forward-filter backward-filter get-edge]     
  (let [^java.util.ArrayList res (java.util.ArrayList.)]    
    (do (reduce-kv (fn [^java.util.ArrayList acc to w]               
                     (do (when  (forward-filter (get-edge flow-info v to) to) (.add acc to))
                         acc))
                   res
                   (get-sinks flow-info v))
        (reduce-kv (fn [^java.util.ArrayList acc from w]
                     (do (when (backward-filter (get-edge flow-info from v) from)  (.add acc from))
                         acc))
                   res 
                   (get-sources flow-info v)))))

(defmacro general-flow-neighbors 
  [flow-info v & {:keys [get-sinks get-sources forward-filter backward-filter get-edge]}]     
  (let [res (tagged 'java.util.ArrayList "res")
        acc (tagged 'java.util.ArrayList "acc")]
    `(let [~res (java.util.ArrayList.)]    
       (do (reduce-kv (fn [~acc to# w#]               
                        (do (when  (~forward-filter (~get-edge ~flow-info ~v to#) to#) (.add ~acc to#))
                            ~acc))
                      ~res
                      (~get-sinks ~flow-info ~v))
           (reduce-kv (fn [~acc from# w#]
                        (do (when (~backward-filter (~get-edge ~flow-info from# ~v) from#)  (.add ~acc from#))
                            ~acc))
                      ~res 
                      (~get-sources ~flow-info ~v))))))

;;Generic, customizable version.
(defmacro general-flow-traverse
  "Custom macro to walk a transient flow network."
  [net startnode targetnode startstate & {:keys [weightf neighborf]}]
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

(defmacro aug-path [g from to traversal state]
  `(searchstate/first-path (~traversal ~g ~from ~to (~state ~from))))  

;;Allows us to define flow multiplieres to indicate flow directionality.
(definline flow-mult [dir] `(if (identical? ~dir :forward) 1 -1))
;;A simple container for directed flows along edges.
(defrecord  directed-flow [^long flowmult edge])
;;A container for augmenting flows
(defrecord edge-flows [^long flow ^java.util.ArrayList edges])

;;Relook this guy, I think we can replace the direction lookup 
;;with a single edge lookup.  -get-direction is also a poor 
;;idiom, we need to change that guy.

;;Traverse a path, relative to a flow provider, as per reduce.
;;Takes a function, func, which takes as args [acc direction edge].
(defmacro path-reduce [flow-info rfunc init p & {:keys [get-edge get-direction coercion]
                                                 :or   {coercion 'id}}]
  (let [the-edge (with-meta (gensym "the-edge") {:tag 'spork.cljgraph.flow.IEdgeInfo})
        p (vary-meta p assoc :tag 'clojure.lang.ISeq) ]
    `(loop [xs#   (.next ~p)
            from# (.first ~p)
            acc#  ~init]
       (if (nil? xs#) acc#
           (let [to#       (.first xs#)
                 dir#      (~get-direction ~flow-info from# to#)                 
                 ~the-edge (~get-edge ~flow-info from# to#)]
                 (recur (.next xs#) to#
                        (~coercion (~rfunc acc# dir# ~the-edge))))))))

(defmacro path-walk [flow-info p & 
                     {:keys [alter-flow unalter-flow get-edge get-direction]}]
  (let [e             (tagged 'spork.cljgraph.flow.IEdgeInfo "edge")
        feasible-flow (gensym "flow")];(vary-meta (gensym "flow") assoc :tag 'long)];(tagged 'long "flow")]
    `(let [edges# (java.util.ArrayList.)]
        (edge-flows. 
         (~unalter-flow 
          (path-reduce ~flow-info 
                       (~(with-meta 'fn {:tag 'long}) [~feasible-flow dir# ~e] 
                        (do (.add edges#  (directed-flow. (flow-mult dir#) ~e))
                            (min ~feasible-flow              
                                 (if (identical? :forward dir#)
                                   (~alter-flow (edge-capacity ~e))
                                   (~alter-flow (edge-flow ~e))))))
                       posinf ~p
                       :get-edge      ~get-edge
                       :get-direction ~get-direction
                       :coercion      ~'long)) 
         edges#))))

;;Persistent augmentation actually sets the edge to the result 
;;of increasing flow.
(defn augment-flow [the-net p get-edge-flows]
  (let [^edge-flows ef (get-edge-flows the-net p)
         flow (.flow ef)
         ^java.util.ArrayList xs   (.edges ef)
         n     (.size xs)]
    (loop [idx 0
           acc the-net]
      (if (== idx n) acc
          (recur (unchecked-inc idx)
                 (let [^directed-flow info (.get xs idx)]                  
                   (-push-flow acc (.edge info) (unchecked-multiply (.flowmult info) flow))))))))

;;These could probably be moved to macrolets, but eh.
;;Supplemental functions for building flow computations, namely 
;;the actual flow itself.  Determines whether the agumenting 
;;paths are captured and returned.
(defmacro flowbody [net from to aug-path- path->edge-flows]
  `(loop [acc# ~net]
     (if-let [p# (~aug-path- acc# ~from ~to)]
       (recur (augment-flow acc# p# ~path->edge-flows))
       (let [active# (-active-flows acc#)]
         {:active active#
          :net acc#}))))

(defmacro augbody [net from to aug-path- path->edge-flows]
  `(let [augs# (java.util.ArrayList.)]
     (loop [acc# ~net]
       (if-let [p# (~aug-path- acc# ~from ~to)]
         (let [~(with-meta 'flow-res {:tag 'spork.cljgraph.flow.edge-flows}) (~path->edge-flows acc# p#)
               f# (.flow ~'flow-res)
               ~'_  (.add augs# [f# p#])]
           (recur (augment-flow acc# p# ~path->edge-flows)))
         (let [active# (-active-flows acc#)]
           {:active active#
            :net acc#
            :augmentations augs#})))))

(defmacro augdebug [net from to aug-path- path->edge-flows]
  `(let [augs# (java.util.ArrayList.)]
     (loop [acc# ~net]
       (if-let [p# (~aug-path- acc# ~from ~to)]
         (let [~(with-meta 'flow-res {:tag 'spork.cljgraph.flow.edge-flows}) (~path->edge-flows acc# p#)
               f# (.flow ~'flow-res)
               ~'_  (println [f# p#])
               continue# (clojure.string/upper-case (read))]
           (when ({"Y" "YES"} continue#)
             (recur (augment-flow acc# p# ~path->edge-flows))))             
         (let [active# (-active-flows acc#)]
           {:active active#
            :net acc#
            :augmentations augs#})))))


(defmacro flow-fn
  "Defines a flow computation across net, originating at from and ending at to.  
   Caller may supply flow options explicitly, or defer to the explicit 
   *flow-options* dynamic binding, using  supporting macros ala with-flow-options 
   or manual modification."
  [opts]
   `(let [opts# (reduce-kv (fn [acc# k# v#] (assoc acc# k# (eval v#))) {} ~opts)
          ~'_   (doseq [[k# v#] opts#] (when (not= k# :neighborf) (assert (not (nil? v#)) (println [k# :is :nil!]))))
          {:keys [~'get-sinks ~'get-sources ~'get-edge ~'get-direction ~'neighborf ~'weightf ~'augmentations
                  ~'forward-filter ~'backward-filter ~'state ~'alter-flow ~'unalter-flow]} opts#
          neighborf# (fn neighbors# [flow-info# v#]
                       (general-flow-neighbors flow-info# v#
                                               :get-sinks       ~'get-sinks
                                               :get-sources     ~'get-sources
                                               :forward-filter  ~'forward-filter
                                               :backward-filter ~'backward-filter
                                               :get-edge        ~'get-edge))
          traverse# (fn traversal# [net# startnode# targetnode# startstate#] 
                      (general-flow-traverse net# startnode# targetnode# startstate#
                                             :weightf ~'weightf   :neighborf neighborf#))
          aug#     (fn aug-path# [g# from# to#] (aug-path g# from# to# traverse# ~'state))
          flows#   (fn path->edge-flows# [flow-info# p#]
                     (path-walk flow-info# p#
                                :alter-flow    ~'alter-flow
                                :unalter-flow  ~'unalter-flow
                                :get-edge      ~'get-edge
                                :get-direction ~'get-direction))]
      (with-meta         
        (case ~'augmentations 
          nil (fn flow# [net# from# to#] 
                (spork.cljgraph.flow/flowbody  net# from# to# aug# flows#))
          (true :recording)  (fn flow# [net# from# to#] 
                              (spork.cljgraph.flow/augbody  net# from# to# aug# flows#))
          :debug (fn flow# [net# from# to#] 
                   (spork.cljgraph.flow/augdebug  net# from# to# aug# flows#))
          (throw (Exception. (str "unknown aug type" 
                                  (:augmentations opts#)))))
        {:options opts#
         :neighborf neighborf#
         :traverse  traverse#
         :aug-path  aug#
         :path->edge-flows flows#})))

;;High level API
;;==============
;;we define a set of options for building flow computations.
;;NOTE -> protocol functions, unlike anonymous functions, do NOT like
;;being passed around in macros as a first class functions.  They toss
;;an error when invoked, complaining about "unmatched ctor" despite
;;being given the right args.  Consequently, we use the quoted symbol name of
;;the protocol function.  Every other function appears to be fine.
(def default-flow-opts
  {:alter-flow    id
   :unalter-flow  id
   ;filters for neighborhoods
   :forward-filter  (fn [e _] (pos? (edge-capacity e)))
   :backward-filter (fn [e _] (pos? (edge-flow e)))
   ;edge accessors
   :get-edge      -edge-info
   :get-direction -get-direction
   ;used to construct a new neighborf
   :get-sinks     -flow-sinks
   :get-sources   -flow-sources
   ;custom weight function
   :weightf       -flow-weight
   ;custom neighborhood function.
   :neighborf     flow-neighbors
   :doc           (str "A custom flow computation, taking three args [net from to] " 
                       "returning a map of active flow and the resulting network.")
   ;flow searchstate constructor.
   :state         empty-search})

(def ^:dynamic *flow-options* default-flow-opts)
(defmacro  with-flow-options
  "Merges the supplied options with the current options.  Useful for defining 
   complex flow computations."
  [opts & body]
  `(binding [*flow-options* (merge *flow-options* ~opts)]
      ~@body))

;;Flow alterations and such...

;;This guy only works on flow....we probably want something much more
;;general, that works on edges writ-large.
(defmacro with-altered-flow [alter-func unalter-func & body]
  `(let [alter# ~alter-func
         unalter# ~unalter-func]
     (binding [*flow-options* 
               (merge *flow-options* {:alter-flow   alter#
                                      :unalter-flow unalter#
                                      :forward-filter  (fn [e# ~'_] (pos? (alter# (edge-capacity e#))))
                                      :backward-filter (fn [e# ~'_] (pos? (unalter# (edge-flow e#))))})]
       ~@body)))

(defmacro with-scaled-flow [scale & body]
  `(let [x# (as-scale ~scale)]
     (with-altered-flow 
       (fn [flow#] (.scale  x# flow#))
       (fn [flow#] (.unscale x# flow#))
       ~@body)))

(defmacro with-augmentations [& body]
  `(with-flow-options 
     {:augmentations true}
     ~@body))

(defmacro with-flow-context [ctx & body]
  `(let [~'traverse (:traverse ~ctx)
         ~'aug-path (:aug-path ~ctx)
         ~'path->edge-flows (:path->edge-flows ~ctx)
         ~'neighborf (:neighborf ~ctx)]
     ~@body))

(def default-flow  (flow-fn default-flow-opts))
(def aug-flow      (flow-fn (assoc default-flow-opts :augmentations true)))

(defn mincost-flow 
  [net from to]  (default-flow net from to))
(defn augmentations 
  [net from to]     (aug-flow net from to))

(defn ->scaled-mincost-flow [scalar]
  (with-scaled-flow scalar
     (flow-fn *flow-options*)))

;;Extraneous - CounterIntuitive

;;These guys are relatively useless....
;;I think maybe we'd use them if there was a really costly 
;;weight function or something....but they are less than awesome :(

;;slow...
(defmacro with-memoized-topology [net & body]
 `(let [net# ~net]
    (with-flow-options 
      {:get-sinks   (memo-fn [~'_ source#] (-flow-sinks   net# source#))
       :get-sources (memo-fn [~'_ sink#]   (-flow-sources net# sink#))}
      ~@body)))
;;slow...
(defmacro with-memoized-weight [net & body]
  `(let [net#    (gensym "net")
         source# (gensym "source")
         sink#   (gensym "sinks")]
     (with-flow-options 
       {:weightf     `(memo-fn [~~''_ ~source# ~sink#] (-flow-weight ~~net ~source# ~sink#))}
       ~@body)))
;;slow...
(defmacro with-memoized-direction [net & body]
  `(let [net#    (gensym "net")
         source# (gensym "source")
         sink#   (gensym "sinks")]
     (with-flow-options 
       {:get-direction     `(memo-fn [~~''_ ~source# ~sink#] (-get-direction ~~net ~source# ~sink#))}
       ~@body)))

;;slowest...
(defmacro with-memoed-direction [net & body]
  `(let [net#    (gensym "net")
         source# (gensym "source")
         sink#   (gensym "sinks")]
     (with-flow-options 
       {:get-direction     `(memoize (fn [~~''_ ~source# ~sink#] (-get-direction ~~net ~source# ~sink#)))}
       ~@body)))
;;useless
(defmacro with-cached-graph [net & body]
  `(with-memoized-topology ~net 
     (with-memoized-weight ~net
       ~@body)))    



;;Old, misguided code....

;; (defn flow-fn
;;   "Defines a flow computation across net, originating at from and ending at to.  
;;    Caller may supply flow options explicitly, or defer to the explicit 
;;    *flow-options* dynamic binding, using  supporting macros ala with-flow-options 
;;    or manual modification."
;;   [{:keys [get-sinks get-sources get-edge get-direction neighborf weightf augmentations
;;            forward-filter backward-filter state alter-flow unalter-flow] :as opts}]
;;    (doseq [[k v] opts] (when (not= k :neighborf) (assert (not (nil? v)) (println [k :is :nil!]))))
;;    `(let [neighborf# (fn ~(gensym "neighborf") [flow-info# v#]
;;                        (general-flow-neighbors flow-info# v#
;;                                                :get-sinks       ~get-sinks
;;                                                :get-sources     ~get-sources
;;                                                :forward-filter  ~forward-filter
;;                                                :backward-filter ~backward-filter
;;                                                :get-edge        ~get-edge))
;;           traverse# (fn ~(gensym "traverse") [net# startnode# targetnode# startstate#] 
;;                       (general-flow-traverse net# startnode# targetnode# startstate#
;;                                              :weightf ~weightf   :neighborf neighborf#))
;;           aug#     (fn ~(gensym "aug-path") [g# from# to#] (aug-path g# from# to# traverse# ~state))
;;           flows#   (fn ~(gensym "path->edge-flows") [flow-info# p#]
;;                      (path-walk flow-info# p#
;;                                 :alter-flow    ~alter-flow
;;                                 :unalter-flow  ~unalter-flow
;;                                 :get-edge      ~get-edge
;;                                 :get-direction ~get-direction))]
;;       (with-meta 
;;         (fn ~(gensym "flow") [net# from# to#] 
;;           (~(case augmentations 
;;               nil 'spork.cljgraph.flow/flowbody 
;;               (true :recording) 'spork.cljgraph.flow/augbody 
;;                 :debug 'spork.cljgraph.flow/augdebug 
;;                 (throw (Exception. (str "unknown aug type" augmentations))))
;;            net# from# to# aug# flows#))
;;         {:opts ~opts
;;          :neighborf neighborf#
;;          :traverse  traverse#
;;          :aug-path  aug#
;;          :path->edge-flows flows#}))) 


;;ripped out.

          ;; get-sinks#       (:get-sinks          opts#)
          ;; get-sources#     (:get-sources        opts#)
          ;; get-edge#        (:get-edge           opts#)
          ;; get-direction#   (:get-direction      opts#)
          ;; neighborf#       (:neighborf          opts#)
          ;; weightf#         (:weightf            opts#)
          ;; forward-filter#  (:forward-filter     opts#)
          ;; backward-filter# (:backward-filter    opts#) 
          ;; state#           (:state              opts#)
          ;; alter-flow#      (:alter-flow         opts#)
          ;; unalter-flow#    (:untalter-flow      opts#)
