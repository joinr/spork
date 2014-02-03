;;A library for apply various types of network flow 
;;algorithms, as well as defining networks.  Canonical 
;;implementations of augmenting path algorithms, as well
;;as mincost flow algorithms are included.
;;Using mutable containers to represent the network.  
;;Since path augmentation is slowing us down, let's reduce 
;;the cost of updating flows.
(ns spork.cljgraph.mflow
  (:require [spork.cljgraph [core :as graph]
                            [search :as search]
                            [flow    :as flow]]
            [spork.data [searchstate :as searchstate] [mutable :as mut]]
            [spork.protocols [core :as generic]]
            [spork.util [array :as arr]]
            [clojure.core [reducers :as r]])
  (:import  [clojure.lang ITransientMap MapEntry]))

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

(defn net->node-map [edges]
  (let [add-node (fn [m nd] (if (contains? m nd) m (assoc m nd (count m))))]
    (loop [acc {} 
           es     edges]
      (if (empty? es) acc
          (let [e (first es)]
            (recur (-> acc (add-node (:from e)) (add-node (:to e)))
                   (rest es)))))))


(defmacro defhinted [name fields & specs]
  (let [flds (mapv (fn [sym] (vary-meta sym merge {:unsynchronized-mutable true})) fields)]
    `(~@flds)))


(deftype einfo [^{:unsynchronized-mutable true  :tag long}  from 
                ^{:unsynchronized-mutable true  :tag long}  to
                ^{:unsynchronized-mutable true  :tag long}  dir]
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (case k 
                    :from from 
                    :to to
                    :dir dir 
                    (throw (Error. (str "Invalid field: " k)))))
  (valAt [this k not-found] 
    (case k 
      :from from 
      :to   to
      :dir  dir
      (throw (Error. (str "Invalid field: " k))))))
(mut/defmutable einfo [^long from ^long to ^long dir])
  
     
;;definterface gets us more into java, but unlike defprotocol, allows
;;us to define primitive types and return types on the args.  Note
;;that also unlike defprotocol, the interface spec does NOT have an
;;initial argument representing the argument.  Instead, it looks
;;identical to a java interface.  When we go to implement the
;;interface, as with deftype or defrecord, we have to add the initial
;;argument for the object to the args list of the interface.  Other
;;than that, it's identical to the interface spec.  Also note the
;;restriction on names in the java interface.  We cannot use clojurian
;;names, i.e. no '- in the symbol name.
(definterface IMFlow 
  (^long getFlow      [from to])
  (^long getCapacity  [from to])
  (incFlow            [from to ^long amt])
  (setFlow            [from to ^long x])
  (setCapacity        [from to ^long cap])
  (setEdge            [from to ^long flow ^long cap]))

(definline get-index [indices k]
  `(if (number? ~k)  ~k
       (get ~indices ~k)))
  
(defmacro with-indices [indices binds & exprs]
  (let [isym (gensym "indices")]
    `(let [~isym ~indices
           ~@(reduce (fn [acc [k v]] (-> acc (conj k) (conj v))) []
                     (for [[k v] (partition 2 binds)] [k (if (number? v) (long v) `(get-index ~isym ~v))]))]
       ~@exprs)))

(defrecord netinfo [nodes ^objects flows ^objects capacities]
  IMFlow
  (^long getFlow   [m from to]     
    (let [i (get-index nodes from)
          j (get-index nodes to)]
      (arr/deep-aget longs flows i j)))  
  (setFlow   [m from to ^long x]
    (let [i (get-index nodes from)
          j (get-index nodes to)]
      (do (arr/deep-aset longs flows i j x)
          m)))  
  (incFlow      [m from to ^long amt] 
    (let [i (get-index nodes from)
          j (get-index nodes to)]
    (do (arr/deep-aset longs flows i j (+ amt (arr/deep-aget longs flows i j)))
        (arr/deep-aset longs capacities i j (+ amt (arr/deep-aget longs capacities i j)))
        m)))  
  (^long getCapacity [m from to]
    (let [i (get-index nodes from)
          j (get-index nodes to)]
      (arr/deep-aget longs capacities i j)))  
  (setCapacity [m from to ^long cap]
    (let [i (get-index nodes from)
          j (get-index nodes to)]
      (do (arr/deep-aset longs capacities i j cap)
          m)))
  (setEdge [m from to ^long amt ^long cap]
    (let [i (get-index nodes from)
          j (get-index nodes to)]
      (do (arr/deep-aset longs flows i j amt)
          (arr/deep-aset longs capacities i j cap)
          m))))

(defn ^netinfo edges->netinfo [edges]
  (let [nm (net->node-map edges)
        flows      (arr/longs-2d (count nm) (count nm))
        capacities (arr/longs-2d (count nm) (count nm))]
    (doseq [e edges]
      (let [from (:from e)
            to   (:to   e)
            cap  (:capacity e)
            flow (:flow e)
            i (get nm from)
            j (get nm to)]
        (do (arr/deep-aset longs flows i j      (long flow))
            (arr/deep-aset longs capacities i j (long cap)))))
    (->netinfo nm flows capacities)))
     
(def posinf Long/MAX_VALUE)

;;We define an operation to cast a persistent network into a mutable
;;network.  Basically, we store the network info in our netinfo
;;object, and define operations on that.  Things like apply-flow, and
;;other stateful functions should benefit greatly from smashing on a
;;mutable array, rather than smashing on 

(defn as-mutable-net [n]
  ;append netinfo to n.  So that operations for flows and augmenting
  ;paths use netinfo for computing flows.  Keeps the original
  ;persistent data around.  Performance optimization only.
  (let [edges (vals (:flow-info n))]
    (-> n 
        (assoc :net-info (edges->netinfo edges))
        (assoc :mutable   true))))

;;Flows and Augmenting Paths
;;==========================


(defmacro with-net 
  "Macro for working with embedded net info in a map.
   Pulls out the value associated with :net-info in the map, 
   and binds it to a type-hinted local called *net*"
  [m expr]
  `(let [~(with-meta '*net* {:tag 'netinfo} ) (:net-info ~m)]
     ~expr))

(defmacro doto-net 
  "Evals expr with *net* bound to a type-hinted ^netinfo *net*, 
   pulled from the val associated with :net-info in m"
  [m & exprs]
  `(with-net ~m 
     (do ~@exprs
         ~m)))

;;Reformed to use interface methods.  Should be mutable now. 
(definline update-edge*  
  [g from to flow cap]    
  (let [n (with-meta (gensym "net") {:tag 'netinfo})]
    `(let [~n  (:net-info ~g)
           nodes# (:nodes ~n)
           i#  (get-index nodes# ~from)
           j#  (get-index nodes# ~to)]
       (do (.setFlow ~n i# j# ~flow)
           (.setCapacity ~n  i# j# ~cap)
           ~g))))

(definline inc-edge*  
  [g from to amt]  
  `(with-net ~g 
     (.incFlow ~'*net* ~from ~to ~amt)))

(definline dec-edge*
  [g from to amt]  
  `(inc-edge* ~g ~from ~to (- ~amt)))

;;Doubtful this is in use....
;; (defn current-capacity 
;;   ([info] (- (:capacity info) (:flow info)))
;;   ([g from to] (current-capacity (edge-info g from to))))

;;should remove these guys....
;; (defn set-capacity [g from to cap] (update-edge g from to {:capacity cap}))
;; (defn set-flow [g from to flow]    (update-edge g from to {:flow flow}))

;;Hold off on this...implementation may be faulty.
;;(defn swap-capacities [net l c r] net)


;;There's another definition of forward, given a flownet..
;;We consult the nodemap to see whose index is less...we can memoize
;;this...[CORRECTION] we can do that if an only if the node indices
;;are placed in topological order....as it stands, the lookup cost is
;;probably outweighed by the persistent updating cost.   This stays.
(defn forward? 
   ([g from to] (contains? (get (:sinks g) from) to))
   ([g info]    (forward? g (:from info) (:to info))))

(definline direction? 
  [g from to]
  `(if (contains? (get (:sinks ~g) ~from) ~to) 1 -1))

;;Removed the edge info case for now...
;; (defn inc-flow 
;;   ([g info flow]
;;     (update-edge* g (:from info) (:to info) 
;;                  (+ (:flow info) flow) (- (:capacity info) flow)))
;;   ([g from to flow] 
;;     (update-edge* g from to (+ (:flow info) flow) (- (:capacity info) flow))))

;; (defn dec-flow 
;;   ([g info flow]
;;     (update-edge* g  (:from info) (:to info) 
;;                  (- (:flow info) flow) (+ (:capacity info) flow)))
;;   ([g from to flow] (dec-flow g (edge-info g from to) flow)))

;;Doesn't work.
;;(defn flows [g] 
;;  (for [[k v] (:flow-info g)] [k (select-keys v [:capacity :flow])]))

;; (defn active-flows [g] 
;;   (reduce (fn [acc [k info]] (if (> (:flow info) 0)
;;                                (assoc acc k (:flow info)) acc)) 
;;           {} (:flow-info g)))

;;Rewrite, may be faster using netinfo.
;;bi-directional flow neighbors.  We allow all forward neighbors with untapped 
;;capacity, and allow any backward neighbors with flow.
;; (defn flow-neighbors 
;;   [g v _]
;;   (let [xs (atom (transient []))]
;;     (do (doseq [to (graph/sinks g v)]
;;           (when (> (:capacity (edge-info g v to)) 0) 
;;             (reset! xs (conj! @xs to) )))
;;         (doseq [from (graph/sources g v)]
;;           (when (> (:flow (edge-info g from v)) 0)
;;             (reset! xs (conj! @xs from))))
;;         (persistent! @xs))))

(defn flow-neighbors 
  [g v _]
  (with-net g     
    (let [u  (get-index (:nodes *net*) v)
          xs (atom (transient []))]
      (do (doseq [to (graph/sinks g v)]
            (when (> (.getCapacity  *net* u to) 0) 
              (reset! xs (conj! @xs to))))
          (doseq [from (graph/sources g v)]
            (when (> (.getFlow *net* from u) 0)
              (reset! xs (conj! @xs from))))
          (persistent! @xs)))))

(defn flow-neighbors2 
  [g v _]
  (with-net g     
    (let [u  (get-index (:nodes *net*) v)
          xs (atom (transient []))]
      (do (doseq [to (graph/sinks g v)]
            (when (> (.getCapacity  *net* u to) 0) 
              (reset! xs (conj! @xs to))))
          (doseq [from (graph/sources g v)]
            (when (> (.getFlow *net* from u) 0)
              (reset! xs (conj! @xs from))))
          (persistent! @xs)))))

(defn flow-neighbors3 
  [g v _]
  (with-net g     
    (let [u  (get-index (:nodes *net*) v)
          xs (atom (transient []))]
      (do (doseq [to (graph/sinks g v)]
            (when (> (.getCapacity  *net* u to) 0) 
              (reset! xs (conj! @xs to))))
          (doseq [from (graph/sources g v)]
            (when (> (.getFlow *net* from u) 0)
              (reset! xs (conj! @xs from))))
          (persistent! @xs)))))

;;Changed from using flow-walk, due to overhead from function
;;invocation.  This guy gets called a lot.  Function overhead adds up.
;;Also, defwalk forms use merge internally...so runtime costs are
;;incurred in tight loops (i.e. lots of flow calcs).
(definline mincost-aug-path [g from to]
  `(first (graph/get-paths 
          (search/traverse ~g ~from ~to (searchstate/empty-PFS ~from)
                           :weightf flow/flow-weight :neighborf flow/flow-neighbors))))
     
;;Optimized to work on a path of numeric indices...
;;Also needs optimizing.  We now use netinfo instead of infos...
;;find the maximum flow that the path can support.
;;Nodes is intended to be a list of longs, corresponding to a path.
;;We encode flow direction by negating the "from" node...if the 
;;node is negative, the flow is backwards, else forward.
(defn maximum-flow [^netinfo net nodes]
  (loop [^long from  (first nodes)
         xs    (rest  nodes)
         ^long flow   posinf]
    (if-let [to (first xs)]
      (let [new-flow  (if 
                        (.getCapacity net from to)
                        (.getFlow net from to))
            next-flow (min flow new-flow)]
        (recur (first xs) (rest xs) next-flow))
      flow)))

(definline indexed-path [nodes p]
  `(map (fn [n#] (list n# (get-index ~nodes n#)))  ~p))

(definline indexed-path2 [nodes p]
  `(vec (map (fn [n#] (list n# (get-index ~nodes n#)))  ~p)))

(definline indexed-path3 [nodes p]
  `(generic/loop-reduce (fn [acc# n#] (conj acc# (list n# (get-index ~nodes n#)))) []  ~p))

(definline indexed-path4 [nodes p]
  `(persistent! (generic/loop-reduce (fn [acc# n#] (conj! acc# (list n# (get-index ~nodes n#)))) (transient [])  ~p)))

(definline indexed-path5 [nodes p]
  `(into []  (map (fn [n#] (list n# (get-index ~nodes n#)))  ~p)))

(definline indexed-path6 [nodes p]
  `(doall (map (fn [n#] (list n# (get-index ~nodes n#)))  ~p)))



(definline indexed-path7 [nodes p]
  `(into [] (r/map (fn [n#] (list n# (get-index ~nodes n#)))  ~p)))

;; (defn indexed-path8
;;   [nodes p]
;;   (r/fold (fn ([] nil) ([l r] (conj 

;;Apply flow should now be using netinfo.  Instead of edges, we just
;;walk the path in order and inc flow or dec flow depending on whether
;;the edges are forward or backward.  Should reduce allocation a lot.

;;apply an amount of flow along the path, dropping any nodes that 
;;become incapacitated.
(defn apply-flow [g p flow]  
  (generic/loop-reduce 
   (fn [gr to]
     (if (= :increment (:dir info))
       (inc-edge* gr info flow)
       (dec-edge* gr info flow))) g edges))


;;Replace edges with something else.  Given a path, we just traverse
;;the path and mutate the flows as necessary.  Should be simplified
;;now, no edge-info objects, no call to path->edge-info.
;;helper function to apply flow.
(defn augmenting-flow [g p]  
  (with-net g
    (let [p     (object-array (indexed-path (:nodes *net*) p))
          n     (alength p)]
      (loop [from  (first p)
             xs    (rest  p)
             ^long flow   posinf
             acc   '()]
        (if-let [to (first xs)]
          (let [;i (second from)
                ;j (second to)
;                dir        (direction? g (first from) (first to))                
                dir        1
                new-flow   (if (pos? dir)
                             1
                             1
                             )
                             ;(.getCapacity *net* i j)
                             ;(.getFlow     *net* i j))
                ;next-flow (min flow new-flow)
                ]
            (recur (first xs) 
                   (rest xs)
                   0
                   ;next-flow 
                   acc
                  ;(conj acc (->einfo i j dir))
                  ))
          (list flow acc))))))

(defn augmenting-flow [g p]  
  (with-net g
    (let [p     (indexed-path (:nodes *net*) p)]
      (loop [from  (first p)
             xs    (rest  p)
             ^long flow   posinf
             acc   '()]
        (if-let [to (first xs)]
          (let [;i (second from)
                ;j (second to)
;                dir        (direction? g (first from) (first to))                
                dir        1
                new-flow   (if (pos? dir)
                             1
                             1
                             )
                             ;(.getCapacity *net* i j)
                             ;(.getFlow     *net* i j))
                ;next-flow (min flow new-flow)
                ]
            (recur (first xs) 
                   (rest xs)
                   0
                   ;next-flow 
                   acc
                  ;(conj acc (->einfo i j dir))
                  ))
          (list flow acc))))))

;;These guys are all the same.  The difference is, we inject a call to
;;create a mutable netinfo, using as-mutable, and smash on that with
;;our custom mutable operators.  The resulting graph is then converted
;;to a persistent flow structure.  So, wrap the libs from
;;cljgraph.flow.  

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
(def the-net 
  (-> flow/empty-network 
      (flow/conj-cap-arcs net-data)))

(def info       (edges->netinfo (vals sample)))      
(def sample-net 
  (->> (for [{:keys [from to capacity flow]} (vals sample)]
         [from to 0 capacity])
       (flow/conj-cap-arcs flow/empty-network)
       (as-mutable-net)))

)



        
        
        
            

  
(comment 

;;Another option for a representation is to have an array of linfos...
;;and to use unsynched, mutable fields for the flow and cap.
(deftype linfo [^{:unsynchronized-mutable true}  from 
                ^{:unsynchronized-mutable true}  to
                ^{:unsynchronized-mutable true :tag long}  flow 
                ^{:unsynchronized-mutable true :tag long}  capacity]
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (case k 
                    :from from 
                    :to to
                    :flow flow
                    :capacity capacity 
                    (throw (Error. (str "Invalid field: " k)))))
  (valAt [this k not-found] 
    (case k 
      :from from 
      :to to
      :flow flow
      :capacity capacity 
      (throw (Error. (str "Invalid field: " k)))))
  IMFlow
  (^long getFlow      [m from to] flow)
  (^long getCapacity  [m from to] capacity)
  (incFlow            [m from to ^long amt] (do (set! flow (+ flow amt)) m))
  (setFlow            [m from to ^long x]   (do (set! flow x) m))
  (setCapacity        [m from to ^long cap] (do (set! capacity cap) m))
  (setEdge            [m from to ^long amt ^long cap] (do (set! flow amt) (set! capacity cap) m)))

  ;; clojure.lang.ILookup
  ;; ; valAt gives (get pm key) and (get pm key not-found) behavior
  ;; (valAt [this k] (get edges k (throw (Error. (str "Invalid field: " k)))))
  ;; (valAt [this k not-found]  (get edges k not-found))

(deftype flownet [^{:unsynchronized-mutable true, :tag ITransientMap} edges]
  IMFlow
  (^long getFlow      [m from to] (get ^linfo (get edges [from to]) :flow))
  (^long getCapacity  [m from to] (get ^linfo (get edges [from to]) :capacity))
  (incFlow            [m from to ^long amt] (.incFlow ^linfo (get edges [from to]) from to amt))
  (setFlow            [m from to ^long x]   (.setFlow ^linfo (get edges [from to]) from to x))
  (setCapacity        [m from to ^long cap] (.setCapacity ^linfo (get edges [from to]) from to cap))
  (setEdge            [m from to ^long amt ^long cap] (.setEdge ^linfo (get edges [from to]) from to amt cap))
   
  ITransientMap  
  (valAt [this k] (.valAt this k nil))  
  (valAt [this k not-found]    
    (if-let [^clojure.lang.MapEntry e (.valAt edges k)]      
      (.val e)      not-found))  
  (assoc [this k v] (do (assoc! edges k v)      this))  
  (conj  [this e]    (let [[k v] e]      (.assoc this k v)))  
  (without [this k]  (do (dissoc! edges k)  this))
  (persistent [this]    (.persistent edges)))

 ;; clojure.lang.IPersistentMap
 ;;  (count [this] (count edges))
 ;;  (assoc [this k v]     ;;revisit        
 ;;    (flownet. (assoc! edges k v)))
 ;;  (empty [this] (flownet. (transient {})))
 ;;  ;cons defines conj behavior
 ;;  (cons [this e]   (.assoc this (first e) (second e)))
 ;;  (equiv [this o]  (.equiv edges o))  
 ;;  (hashCode [this] (.hashCode edges))
 ;;  (equals [this o] (or (identical? this o) (.equals edges o)))
  
 ;;  ;containsKey implements (contains? pm k) behavior
 ;;  (containsKey [this k] (contains? edges k))
 ;;  (entryAt [this k]
 ;;    (let [v (.valAt this k this)]
 ;;      (when-not (identical? v this) ;might need to yank this guy.
 ;;        (generic/entry k v))))
 ;;  (seq [this] (if (empty? edges) (seq {})
 ;;                (map (fn [k] (generic/entry k (get edges k))) 
 ;;                     (vals idx->key))))  
 ;;  ;without implements (dissoc pm k) behavior
 ;;  (without [this k] 
 ;;    (if (not (contains? edges k)) this
 ;;        (ordered-map. n
 ;;                      (dissoc edges k) 
 ;;                      (dissoc idx->key (get key->idx k))
 ;;                      (dissoc key->idx k)
 ;;                      _meta))))

(defn ^linfo map->linfo [{:keys [from to flow capacity]}]
  (linfo. from to flow capacity))

;;So another option is to have mutable edge lists, store them in a
;;map.
;;One option is to just mutate the linfos in place...


;;This is about as performant as a deftype declaration.
(defn ^longs ->edge [^long from ^long to ^long flow ^long cap]
  (let [arr (long-array 4)]
    (do  (aset arr 0 from)
         (aset arr 1 to)
         (aset arr 2 flow)
         (aset arr 3 cap)
         arr)))
)
  
