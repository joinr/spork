;;An API for defining entity-based, event-driven simulations.
;;TODO: Pull in all of the disparate namespace vars into a
;;convenient central location.  Currently, spork.sim.simcontext
;;serves that purpose, providing a decent API for general purpose
;;simulation stuff.  However, there are cases where we pull in
;;from spork.sim.agenda and other areas.  Currently a placeholder
;;until we reorganize the API.

;;The core abstraction here is a simcontext.
;;Given a simcontext, we have some state, which will be 
;;occupied by an entitystore.
(ns spork.sim.core
  (:require [spork.sim [simcontext :as sim] [data :as simdata]]
            [spork.util [metaprogramming :as util]
                        [general  :as gen]
                        [tags     :as tag]
                        [table    :as tbl]
                        [reducers]
                        ;[cellular :as cells]
                        [inspection :as inspect]
                        [temporal :as temp]]
            ;[spork.cljgraph [jungapi :as jung]]
;            [spork.sketch :as sketch]                        
            [spork.entitysystem.store :refer :all :exclude [entity-name merge-entity] :as store]            
            [spork.ai [core        :as ai]
                      [behaviorcontext :as b]
                      [messaging]]             
            ;[marathon.data.store       :as simstate]
            [clojure.core.reducers     :as r]))

;;We'll build the context by hand, then look at defining 
;;a nice API to build it for us.

;;Since simcontext already implements IEntityStore, we can 
;;use it like an entity container.
(def emptysim (assoc sim/empty-context :state store/emptystore))

;;Note:
;;THe current "API" is in spork.sim.simcontext, more or less.
;;This would just be importing vars from there, honestly.

;;sim.core is a glue library that provides access to a common set of 
;;subsystems upon which a more complicated federated simulation infrastructure
;;is implemented.  

;;Marathon utilizes a generic simulation context, defined in __sim.simcontext__, 
;;which provides primitive access to a generic simulation state - heterogenous 
;;chunks of data relevant to different domains of the simulation - and a basic
;;discrete event simulation framework.  
;;Systems are then defined over this shared architecture, and contribute special 
;;means to affect a simulation in their particular domain.  At the highest 
;;level, a coordinating function, or an engine, orders and composes the systems 
;;into a comprehensive state transition function that maps one simulation 
;;context to the next.  

;;This is a lifesaver...
(def noisy?            (atom true))
(defn toggle-noisy []  (swap! noisy? (fn [n] (not n))))
;;From Stuart Sierra's blog post, for catching otherwise "slient" exceptions
;;Since we're using multithreading and the like, and we don't want
;;exceptions to get silently swallowed
(let [out *out*]
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (when @noisy? 
         (binding [*out* out]
           (println ["Uncaught Exception on" (.getName thread) ex])))))))

;;Providing Common Access to the State in the Simulation Context
;;==============================================================

;;The simulation context contains the simulation state - a large
;;nested map of information that is 'typically' partitioned by a
;;particular domain.  Some systems, like the fill function, need
;;access to multiple domains to fulfill their purpose.  In response,
;;we maintain an open, broad structure for the state portion of the
;;context, but provide a set of common paths or mappings to resources
;;embedded in the state. 

;;As a result, subsystems can query resources by predefined paths - a
;;higher level of abstraction and clarity - OR they may dip down to
;;low level operations on the raw state.

;;This should maintain a balance between flexibility and clarity.

;;Common Paths to Simulation Resources
;;====================================

;;In the previous implementation, the 'state' was implemented as a
;;class, with concrete members to access each piece.  We retain that
;;level of commonality via the paths, but the underlying
;;representation is based on a dynamic map structure, so we can still
;;add new data as needed in a flexible manner.

;;For instance, the following example creates a set of accessors for
;;our simulation state.  This allows us to dissect our nested map of
;;state a bit easier.  Each symbol in the defpath binding returns a
;;function that, given a simulation context, points to the named
;;resource using standard clojure map operations via
;;clojure.core/get-in.

(util/import-vars
 [spork.entitysystem.store
  entity-name
  defpath
  defpaths]
 )

;;For example:
;; (defpaths   [:state] 
;;   {parameters    [:parameters]
;;    supplystore   [:supplystore]
;;    demandstore   [:demandstore]
;;    policystore   [:policystore]
;;    fillstore     [:fillstore]
;;    fill-function [:fillstore :fillfunction]
;;    fillmap       [:fillstore :fillmap]
;;    behaviors     [:behaviormanager]
;;    supply-tags   [:supplystore :tags]
;;    demand-tags   [:demandstore :tags]})

;;Convenience functions for simulation operations
;;===============================================

(util/import-vars
 [spork.sim.simcontext 
  merge-updates
  merge-entity
  get-time
  add-time])

;;possibly deprecate?
(defn location-table [ctx]
  (let [t (sim/get-time ctx)]
    (->> (store/only-entities ctx [:name :locationname :location])
         (map #(assoc % :t t))
         (tbl/records->table))))
;;Functions for dealing with subsets of supply/demand,
;;for defining smaller simulations.
(defn entity? [x] (instance? spork.entitysystem.store.entity x))

;;#Operations for working with mutable references
;;particularly working with pieces of state in a nested associative
;;structure.

;;Imports from spork.util.cellular and simcontext
(util/import-vars
 [spork.util.inspection
  tree-view]
 ;;TODO: revisit the utility of cellular...
 #_[spork.util.cellular
    with-cells
    with-transient-cells
    swap-cell!
    reset-cell!
    ->cell])

;;Messaging convenience functions.
;;===============================
(util/import-vars 
 [spork.ai.messaging  
  ->msg
  handle-message!
  send!!])

(defn set-parameter
  "Sets simulation parameter p in simcontext s to v."
  [s p v] (assoce  s :parameters p v))
(defn merge-parameters
  "Merges existing simulation parameters in simcontext s
   with the map defined by ps"
  [s ps]  (mergee  s :parameters  ps))

;;#Empty Simulation Contexts

(def emptysim
  "A generic simulation context based on an empty entitystore, with
   an initial :time event added at t = 0."
  (sim/add-time 0 (sim/make-context :state store/emptystore)))

;;A useful debugging context for us.  Prints out everything it sees.
(def ^:dynamic *debug*   nil)
(def ^:dynamic *verbose* nil)
(def ^:dynamic *ignored* #{})

(defmacro debugging
  "Creates a lexical body in which the *debug* binding is true.
   This will enable a lot of debug output related to the 
   event system, in particular event messages will be realized
   rather than delayed.  Disabled by default."
  [& expr]
  `(binding [~'spork.sim.core/*debug* true]
     ~@expr))

(defmacro debugging!
  "Creates a lexical body in which the *debug* binding is true,
   as well as the spork.ai.core/*debug* binding.  
   This will enable a lot of debug output related to the 
   event system, in particular event messages will be realized
   rather than delayed.  Additional messages will be printed 
   if there are any debug behaviors triggering on the 
   spork.ai.core behavior system."
  [& expr]
  `(binding [~'spork.sim.core/*debug* true
             ~'spork.ai.core/*debug* true]
     ~@expr))

(defmacro debug-entity
  "Creates a lexical body in which the behavioral debugging 
   and tracing is enabled for a specific entity, as defined
   by name.  When (:name entity) is identical to the name 
   given, behavioral debugging output for the entity 
   will be enabled, iff the global spork.ai.core/*debug* 
   is not already truthy."
  [name & expr]
  `(binding [~'spork.ai.behaviorcontext/*observed* ~name]
    ~@expr))

(defmacro ignoring
  "Creates a lexical body in which the events specified by 
   es are conjoined onto the spork.sim.core/*ignored* var, 
   eliminating them from any logged output."
  [es & expr]
  `(binding [~'spork.sim.core/*ignored*  (into ~'spork.sim.core/*ignored* ~es)]
     ~@expr))

(defmacro noisy
  "Creates a lexical context in which debugging is enabled, and 
   and excluded event types are actively ignored."
  ([es  expr]
     `(debugging 
       (ignoring ~es ~expr)))
  ([expr] `(debugging ~expr)))

(defn visible?
  "Predicate to determine if an event data meets the criteria 
   for inclusion per debug and ignored options."
  [edata] 
  (and *debug*
     (not (*ignored* (spork.sim.data/event-type edata)))))

(def ^:dynamic *event-filter*
  "Dynamic var for setting the visibility criterion for 
   events.  Defaults to spork.sim.core/visible?, which 
   in turn only makes events visible if both spork.sim.core/*debugged*
   is truthy, and the event-type is not in spork.sim.core/*ignored*."
  visible?)

(defmacro with-event-filter
  "Creates a lexical body in which the spork.sim.core/*event-filter* 
   is bound to f, rather than the default."
  [f & expr]
  `(binding [~'spork.sim.core/*event-filter* ~f]
     ~@expr))

;;TODO: flip the branch order in this if expr, use if-not.  Should
;;be faster....
(defn debug-listener
  "Serves as an spork.sim.network.pure/IEventContext compatible event-handling
   function.  Applies the *event-filter* criteria to determine if the event 
   should be printed to the debug log.  The default debug-msg is to print 
   only the packet-message associated with edata.  To see the entire 
   event-data, bind spork.sim.ai/*verbose* to true."
  [ctx edata name] 
  (do  (when (*event-filter* edata)
         (println (if *verbose* 
                    (sim/debug-msg  ":debugger saw " 
                                    {:type (spork.sim.data/event-type  edata) 
                                     :from (spork.sim.data/event-from edata)
                                     :to   (spork.sim.data/event-to edata)
                                     :msg  (sim/packet-message edata)
                                     :data (spork.sim.data/event-data  edata)})
                    (sim/debug-msg (sim/packet-message edata)))))
       ctx))

;;Note:
;;This is the primary simulation context we use from
;;marathon.analysis, particularly when we generate marathon-streams or
;;histories.

(def debugsim
  "The default persistent simulation context, in which we have a
   global event subscriber, :debugger, which uses
   spork.sim.core/debug-listener to log all events, and pre-populates
   the simulation with a time-event at t = 0.  Callers can unsubscribe
   :debug-handler or alternately use spork.sim.core/emptysim .  Given
   the relatively low overhead of using the default debugger, and the
   relatively high utility of having it available on-demand, it makes
   sense to prefer this simulation context."
  (->> (-> (sim/make-debug-context 
            :debug-handler  
            debug-listener)
           (assoc :state store/emptystore))
       (sim/add-time 0)))

(defn debug!
  "Convenience function to add the default spork.sim.core debugger
   event subscriber to a context if it doesn't have one already."
  [ctx] 
  (if (contains?  (spork.sim.pure.network/get-event-clients ctx :all)
                  :debugger)
    ctx
    (sim/add-listener :debugger debug-listener [:all] ctx)))

(defn debug-by!
  "Replaces the :debugger listener with a function f, where 
   f is a valid event-handler :: ctx -> edata -> name -> ctx ."
  [ctx f]
  (sim/add-listener :debugger
     (fn [ctx edata _]
       (do (f edata)
           ctx)) [:all] ctx))

;;#State-wide queries...

;;TODO# port this over into the API in spork.sim
(defn events
  "Returns a sequence of all pending events in the simulation context."
  [ctx]   (spork.sim.data/event-seq ctx))
(defn times
  "Returns a sequence of all pending :times in the simulation context."
  [ctx] (map :time (events ctx)))
(defn segments
  "Returns a sequence of events with an addition :duration 
   field appended, which provides the length of time 
   said event lasted."
  [ctx] 
  (->> (partition 2 1 (events ctx))
       (map (fn [[l r]]
              [(assoc l :duration (- (:time r) (:time l)))
               r]))
       (map first)
       (map (fn [r] (if (:duration r) r (assoc r :duration 1))))))          

(util/import-vars 
 [spork.util.general  
  deep-assoc
  deep-get
  deep-update 
  deep-dissoc
  collect
  atom?
  float-trunc])

;;Ephemeral Data
;;==============
;;Typically used for storing append-only daily logging
;;information.
(defn get-ephemeral
  ([ctx id component default]
   (if-let [state (store/gete ctx id component)]
     (if (atom? state)
       [ctx state] 
       (let [new-state (atom state)]
         [(store/assoce ctx id component new-state) new-state]))
     (let [atm (atom default)
           ctx (store/assoce ctx id component atm)]
       [ctx atm])))
  ([ctx id component] (get-ephemeral ctx id component (transient []))))

(defn conj-ephemeral [ctx id component v]
  (let [[ctx state] (get-ephemeral ctx id component)]
    (swap! state conj! v)
    ctx))

(defn reset-ephemeral [ctx id component v]
  (let [[ctx state] (get-ephemeral ctx id component)]
    (reset! state v)
    ctx))

(defn swap-ephemeral
  [ctx id component f]
   (let [[ctx state] (get-ephemeral ctx id component)]
     (swap! state f)
     ctx))

(defn some-ephemeral [ctx id component]
  (when-let [atm (store/gete ctx id component)]    
    (when (pos? (count @atm)) atm)))


;;Ephemeral Counter
;;=================
;;Typically used for storing append-only daily logging
;;information.

;;Associate an entity with a unique numerical value.
;;Provides an efficient way to get "global" numerical
;;counters while maintaining the ability to persistent
;;at the end of the day.
(defn swap-counter
  [ctx id f]
   (let [[ctx state] (get-ephemeral ctx id :counter 0)]
     (swap! state f)
     ctx))

(defn inc-counter
  ([ctx id]
   (let [[ctx storage] (get-ephemeral ctx id :counter 0)]         
     (swap-counter ctx id #(unchecked-inc %))))
  ([ctx id n] 
   (swap-counter ctx  id #(unchecked-add % n))))

(defn get-counter [ctx id]
  (or (store/gete ctx id :counter) 0))

(defn persist-counters [ctx]
  (store/filter-map-component ctx :counter
      (fn [_ x] (atom? x)) ;filter
      deref))                              

(defn get-count
  [ctx id]
  (if-let [cnt (get-counter ctx id)]
    (if (number? cnt) cnt
        (deref cnt))
    0))

;;common counters
(defn deployment-count [ctx]
  (get-count ctx :deployment-count))
(defn inc-deployment-count [ctx]
  (inc-counter ctx :deployment-count))

;;Acts like juxt, except it returns the results in a map.
;;The map is implied by the args in kvps, where simple keys - numbers,
;;strings, keywords, are assumed to be field names. A map is built 
;;from the fields by getting the corresponding field in an input
;;record.  Vector keys are assumed to imply [key function-to-apply]
(defn juxtmap [& ks]
  (let [fs  (reduce (fn [acc x]
              (cond (or  (keyword? x)  (string? x) (number? x))   (assoc acc x #(get % x))
                    (vector? x)  
                      (let [fld (first x) 
                            getter (second x)]
                        (assoc acc fld 
                          (cond (fn? getter) getter 
                                (or  (keyword? getter)  (string? getter) (number? getter))   
                                 #(get % getter)
                                :else (throw (Exception. (str "unknown juxt-map getter " getter))))))
                      :else (throw (Exception. (str "unknown juxt-map arg " x)))))
                   {} ks)]
    (fn [x] 
      (reduce-kv (fn [m fld f] (assoc m fld (f x))) {} fs))))

(defmacro fields [xs]
  (cond (map? xs)
        `(marathon.sim.core/juxtmap ~@(:fields xs))
        (vector? xs) 
        `(juxt ~@xs)))

;;TODO maybe make this a reducer....dunno yet.
(defn collectr [fs xs]  
  (let [f (if (coll? fs) (apply juxt fs) fs)]
    (reify     
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]   (reduce f1 (f1) (r/map f xs)))        
      (coll-reduce [_ f1 init] (reduce f1 init (r/map f xs)))
      clojure.lang.Seqable 
      (seq [this]  (seq (map f xs))))))

;;legacy api, just using CES now.
;(defmacro entities [ctx] `(entity-seq ~ctx))


(defn demands->track [xs] 
  ;(sketch/->track (map (fn [r] (merge r {:start (:startday r)})) xs) :track-name name)
  )

(defn demand->tracks  
  [xs & {:keys [keyf] :or {keyf (juxt :demandgroup :src)}}]
  (for [[g xs] (group-by keyf xs)]
    [g (map (fn [r] (merge r {:start (:startday r)})) xs)]))


(defn visualize-events [es track-keyf color-keyf]  
;;   (let [coloring (zipmap (map color-keyf es) (take (count es) (sketch/palette)))
;;         tracks   (demand->tracks es :keyf track-keyf)
;;         rendered-tracks (sketch/->tracks tracks)
;;         track-width     (:width (spork.graphics2d.canvas/shape-bounds rendered-tracks))
;; ;        lgnd     (sketch/->legend coloring)
;; ;        lwidth   (:width (spork.graphics2d.canvas/shape-bounds lgnd))
;;         ]
;;     (sketch/with-event->color (fn [e] (get coloring (color-keyf e)))
;;       (sketch/sketch-image
;;        (sketch/scale 1.0 1.5
;;                      (sketch/stack [(sketch/->tracks tracks)
;;                                    ; (sketch/translate 10 5 
;;                                    ;    (sketch/scale (float (/ track-width lwidth)) 2.0
;;                                    ;      lgnd))
;;                                     ])))))
  )

(defn visualize-demands [ctx  & {:keys [track-keyf color-keyf] :or {track-keyf (juxt :demandgroup :src)
                                                                    color-keyf (juxt :vignette)}}]
  (visualize-events (vals (demands ctx)) track-keyf color-keyf))

;;This is going to be a little brittle; should access to updates
;;behind a protocol...
(defn updates [ctx] 
  (for [[ks xs] (-> ctx :updater :updates)
        [ts  us]  xs
        [nm pckt] us]
    pckt))

(defn update-events [ctx] 
  (map (fn [{:keys [update-time request-time] :as r}]
         (assoc r :startday request-time :duration (- update-time request-time)))
       (updates ctx)))

(defn visualize-updates [ctx  & {:keys [track-keyf color-keyf] :or {track-keyf (juxt :requested-by)
                                                                    color-keyf (juxt :update-type)}}]
  (visualize-events (update-events ctx) track-keyf color-keyf))

(defn visualize-supply [ctx]
  (->> (units ctx)
       (map (fn [u] (update-in u [:policy] :name)))
       (spork.util.table/records->table)
       (spork.util.table/visualize)))

(defn visualize-fillmap [ctx]
  (if-let [fm (:fillmap (get-fillstore ctx))]
    (->> (for [[parent children] fm
               [child cost] children]
           {:donor parent :recepient child :cost cost})
         (spork.util.table/records->table)
         (spork.util.table/select :fields [:donor :recepient :cost] :from)
         (spork.util.table/visualize))
    (throw (Exception. "No fill map to visualize!"))))

(defn visualize-graph [g]
  (jung/view-graph g jung/fr))

(defn visualize-fillgraph [ctx]
  (if-let [fg (:fillgraph (get-fillstore ctx))]
    (visualize-graph fg)
    (throw (Exception. "No fillgraph to visualize!"))))

(defn visualize-store [ctx]
  (tree-view  (store/domains ctx)))

(defn visualize-data [ctx]
  (tree-view ctx))

;;Short queries...we should move these away from being a map for
;;entities, and into sets. Set access is actually faster than
;;maps, so bonus.
(defn demand-names    [ctx] (keys (gete ctx :DemandStore :demandmap)))
(defn unit-names      [ctx] (keys (gete ctx :SupplyStore :unitmap)))
(defn unit-entities   [s]   (store/get-domain s :unit-entity))
(defn unit-records    [ctx] (store/get-entities ctx (unit-names ctx)))
(defn demand-entities [s]   (store/get-domain s :DemandType))


(defn current-entity
  "Fetch the entity with it's current time interpolated as a :dt component.
   We use this to perform lightweight updates, particularly for computing   
   accumulated stats, so that we can interpolate statistics for things like 
   fill criteria.  Rather than invoke the machinery to update every entity, 
   we save time doing lightweight updates as a function of the computed 
   :dt component."
  ([ctx nm t]
   (let [e  (store/get-entity ctx nm)
         dt (- t (get e :last-update 0))]
     (assoc e :dt dt)))
  ([ctx nm] (current-entity ctx nm (sim/current-time ctx))))


;;fetch units with appended :dt information for
;;potential synchronization purposes.
(defn current-units
  ([ctx t]
    (for [id  (keys (:unitmap   (get-supplystore ctx)))]
      (current-entity ctx id t)))
  ([ctx] (current-units ctx (sim/current-time ctx))))

;;this is actually using our new positional stuff.
;;we need to map deployed-trend to risk....
;;I think we just want to use demand-trends...
;;demand-changes gets us the units...
;;we also need to compute the peak demand.
(defn all-demands [store]
  (store/select-entities store :from [:startday :duration :quantity :DemandType]))

;;not sure about this, we may want to break out by trends...
;;We'll see what use case pops up for this type of query.
(defn demand-profile [store]
  (->> (temp/activity-profile 
        (all-demands store)
        :start-func :startday :duration-func :duration
        )
       (map (fn [[t {:keys [actives]}]]
              [t (reduce + 0 (map :quantity actives))]))))

(defn peak-demand [store]
  (->> (temp/peak-activities
        (all-demands store)
        :start-func :startday :duration-func :duration
        :peak-function (fn [{:keys [actives]}]
                         (reduce + 0 (map :quantity actives))))
       (first)
       (val)
       (:actives)
       (map :quantity)
       (reduce + 0)))


;;This is fairly close....
(defn visualize-entities [ctx]
  (let [demandnames    (demand-names ctx)
        unitnames      (unit-names   ctx)
        disabled       (keys (store/get-domain ctx :disabled))
        basic-entities (set  (concat demandnames unitnames disabled))
        stores         (clojure.set/difference
                        (set  (keys (store/entities ctx)))
                        basic-entities)
        named-entry (fn [e]
                      (if (instance? clojure.lang.MapEntry e) (inspect/entryvis e)
                          (inspect/entryvis (clojure.lang.MapEntry. (or (:name e)
                                                                        (store/entity-name e)) e))))]         
    (inspect/tree-view
       {:stores  (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx stores)))
        :demands (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx demandnames)))
        :units   (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx unitnames)))
        :disabled (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx disabled))) })))

(defn visualize-subscriptions [ctx] 
  (inspect/tree-view (:subscriptions (get-policystore ctx))))

;;TODO# define a visualization protocol, extend it to core datatypes...
(defn visualize-unit [u]   (inspect/tree-view u))    
(defn visualize-policy [p] (jung/view-graph (:positiongraph p) jung/fr))

       
;;#Shared Functions

;;These functions were extracted due to use across multiple domains.  I may 
;;refactor them into core services, but for now, relocating them in sim.core 
;;allows every system access to them.

(defn now
  "Consult the system clock for the current time.  Used for logging."
  [] 
  (System/currentTimeMillis))

;;Tom hack 26 MAy 2016
;;We discriminate between known or canonical buckets, and
;;ad-hoc buckets (buckets that are created as ephemeral supply
;;for followon-demands.  In contrast, we will likely always have
;;:default and :SRM categories of supply, i.e. they never go away.
(def known-buckets #{:default :SRM "SRM"})

;;PERFORMANCE NOTE: This is on the HotSpot, Apparently....
;;maybe just clojure 1.7, but we're creating a keyseq for clojure.core/keys,
;;which is exploiting this hotspot.  Should be way faster.  we'll use
;;reduce-kv to alleviate it....

;;The name here is a bit generic.  We're really trying to acquire the keys of 
;;a map that contains information about followon-eligible supply.  In this 
;;context, the keys are actually the followon-code of the unit, (typically a
;;demandgroup). 
(defn get-followon-keys
  "Returns a sequence of followon codes that are currently present in the 
   supply.  Used for constraining or preferring supply during the followon 
   fill phase."
  [ctx]
  (let [m  (reduce-kv (fn [acc k _]
                        (if-not (known-buckets k) (conj! acc k) acc)) (transient #{})
                        (store/gete ctx :SupplyStore :deployable-buckets))]
    (when (pos? (count m)) (persistent! m))))

(comment ;old
(defn get-followon-keys
  "Returns a sequence of followon codes that are currently present in the 
   supply.  Used for constraining or preferring supply during the followon 
   fill phase."
  [ctx]
  (let [m (into #{} (filter (complement known-buckets)) (keys (store/gete ctx :SupplyStore :deployable-buckets)))]
    (when (pos? (count m)) m)))
)

;;Check the validity here...
;;Do we need so much redundancy now?
(defn update-unit
  "Associates unit u into the context's supply store, using the unit's name 
   as a key."
  [u ctx]                    
  (set-unit u ctx)  
  )

(defn ghost? [unit] (= (clojure.string/upper-case (:src unit)) "GHOST"))
(defn followon? [u] (:followoncode u))
(defn ghost-followon?  [u] (and (ghost? u) (followon? u)))
(defn default-recovery [ctx]
  (or (:DefaultRecoveryTime (get-parameters ctx))
      0))

;;#TODO get this working like it used to, right now it's not important.
;; (defn interval->date  [t ctx]
;;   (let [ps (get-parameters ctx)
;;         start-date (get ps :StartDate)
;;         time-scale (get ps :time-scale 1)] 
;;     (+ start-date (* time-scale t))))

;;interval->date is a simple stub, maybe unnecessary (although I like
;;having it around for records in the output)
(defn interval->date  [t ctx]  t)

(defn in-scope? [params src]
  (and (not (get-in params [:SRCs-Out-Of-Scope src]))
       (get-in params [:SRCs-In-Scope src])))

(defn scope-info [ctx]
  {:in-scope     (get (get-parameters ctx) :SRCs-In-Scope)
   :out-of-scope (get (get-parameters ctx) :SRCs-Out-Of-Scope)})

;;#Tag Related Functions#
;;Another useful bit of higher order, or meta data, is the notion of simple 
;;tags.  We use tags as a simple mechanism to track all kinds of effects and 
;;information, without having to embed data in lower classes.  This way, when
;;an entity - or even a system - needs to be grouped or categorized we can 
;;use tags.  As a result, we see a lot of shared tags that represent common 
;;effects, like whether an entity is enabled or disabled.

;Generic tag-related....These apply equally to supplystore, demandstore, etc.
;The interface is identical.  Only the interpretation of the tags is different.
(defn enabled? [store item]
  (tag/has-tag? (:tags store) :enabled item))
(defn disabled? [store item] (not (enabled? store item)))
(defn enable [store item]
  (update-in store [:tags] tag/tag-subject :enabled item))
(defn disable [store item]
  (update-in store [:tags] tag/untag-subject :enabled item))
(defn special-src? [tags src] (when tags (tag/has-tag? tags src "Special")))

;;#Fill Related Functions

;find-eligible-demands is implemented as multimethod that dispatches based on 
;type of the category that's passed in.  category-type provides a simple 
;dispatch mechanism.  Note-> could use built-in hierarchy functions to do this.
(defn category-type [x]
  (cond (or (keyword? x) (string? x))  :simple            
        (vector? x) :src-and-group 
        (map? x)    :rule-map
        :else (throw (Exception. "Unknown category type " (type x)))))

;;#Utility Functions and Macros

;;A collection of shared functions, might turn into protocols someday...
;;This should contain only core functionality shared across subsystems defined 
;;in other namespaces, to eliminate redunancy.
;;I started this due to the fact that several functions - primarily accessors, 
;;were bubbling up in each of the domain-specific modules.  As a result, we'll 
;;just shove them in here.  If we don't, we'll get circular dependencies.

;;We alias the more efficient make-string function, rather than 
;;using core/str.  This is commonly used for logging messages 
;;and other things.  Since there are lots of events flying 
;;around with string data, this is a serious bottleneck.
(def msg gen/make-string)

;;Stubs were used during the initial port to toss exceptions if an unimplemented
;;or deferred function was called.  
(defmacro stub [msg & empty-body]  
  `(~'fn ~'[& args] (~'throw (~'Exception. ~msg))))

;This is a general utility function, that allows us to derive predicates based
;on atomic values, sets, or maps.  Used for filtering (currently in demand 
;categories and supply categories).
;Can probably extend this to allow for arbitrary predicate functions (later).
(defn make-member-pred [g]
  (if (or (set? g) (map? g))
    #(contains? g %)
    #(= g %)))

;If function results in an empty map, contained within another map, 
;removes the entry associated with the empty map.
(defn prune-in [m ks f & args]
  (let [updated (apply update-in ks f args)]
    (if (empty? (get-in updated ks))
      (let [path   (butlast ks)
            parent (get-in m path)]            
        (assoc-in m path (dissoc parent (last ks))))
      updated)))

;;Predicate for determining if a map has a key equal to a val.
(defn key= [k v] (fn [m] (= (get m k) v)))

;;#TODO evaluate memoize here and see if we're paying a useless
;penalty.
(defn key-tag-maker
  "Creates a little keyword factory that allows to to define descriptive 
   tags using keywords efficiently and consistently."
  [base] (gen/memo-1
          (fn [tag] (keyword (str base tag)))))

;helper macro for defining key-building functions.
(defmacro defkey [name base] `(def ~name (key-tag-maker ~base)))

;;#Utils
(defn ensure-name
  "We tend to prefer unique names, and often times we accomplish that by concatenating the 
   count of a container onto a non-unique name.  ensure-names generalizes this stuff."
  [named names]                  
  (let [nm (entity-name named)]
    (if (contains? names nm)
      (assoc named :name 
             (msg nm "_" (count names)))
    named)))

(definline empty-string? [x] `(= ~x ""))
(defn debug-print [msg obj] (do (println msg) obj))

(defn as-records [record-source]
  (if (and (seq? record-source) (map? (first record-source))) record-source      
      (tbl/record-seq record-source)))

(let [idx (atom 0)]
  (defn next-idx 
    "Utility function for getting incrementing indices.  Used for 
     naming entities.  Passing an argument resets the index to the arg,
     and returns the arg.  Otherwise, a mutable counter is incremented 
     and the result is returned."
    ([] (let [i @idx]
          (do (swap! idx unchecked-inc)
              i)))
    ([new-idx] (do (reset! idx new-idx)
                   new-idx))))


;;Imported from spork.util.general
(util/import-vars 
 [spork.util.general  
  deep-assoc
  deep-get
  deep-update 
  deep-dissoc])

;;TODO de-duplicate this from marathon.data.protocols
;;look into replacing this with a universal constant, or upperbound
;;for longs
(def ^:constant +inf+ 9999999)

;;#Wrapper around spork.simcontext 
;;As it stands, update requests aren't events...
;;They're disconnected...since the updater is handling the event
;;separately...
;;Maybe we can clear that up at some point....
(defn request-update [tupdate requested-by request-type ctx]
  (->> ctx
       (sim/request-update tupdate requested-by request-type)
       ;(sim/trigger-event request-type requested-by :update-manager 
       ;                   (msg requested-by " requested an " request-type " at " tupdate) nil)
       ))

;;Note/TODO: Probable Performance Enhancement for generating events.
;;We call sim/trigger-event quite a bit from the manager libraries.
;;We basically have a call to build strings when we trigger events.
;;If we aren't debugging or capturing output...we can just as easily
;;skip firing off the event.
;;So, events only matter if we want them to, thus we only pay the
;;cost if we want to...
;;We can make this a semi-dynamic call to sim/trigger-event
;;using a macro.
;;What we'd really like to do is....if there's no listener...
;;we preclude the event from happening...
;;We can probably save a shitload of time going this route.
;;Alternately, we can write a macro to rip out messages...
;;eliminate all the string building.
;;Something like...

;;Before: 6  
(defmacro trigger-event
  "Macro optimization to allow messages to be stripped out 
   if the context is not actively messaging (i.e. debugging).
   This is a nifty cheat, since we avoid allocation for 
   strings entirely. Behaves as a replacement for  
   spork.sim.context/trigger-event , while restricting 
   certain argument combinations, i.e. non-variadic."
  ([id from to msg-form data ctx] ;;changce to strip message.
   `(let [msg# (when ~'spork.sim.core/*debug* ~msg-form)]  
      (sim/trigger-event ~id ~from ~to  msg# ~data ~ctx)))
  ([e ctx] ;event is already baked.
   (sim/trigger-event ~e ~ctx)))

;;##Developer Notes

;;#Transitioning from Effectful Simulation and State Updating#
;;One problem area in the port from VBA->Clojure was the handling of 
;;decentralized state updates.  This primarily happened via event dispatch, or 
;;through side-effects called during management functions.  The solution for our
;;pure simulation is to formalize batch updates to the simulation by using 
;;simcontext/merge-updates.  This allows us to pass maps of updates around, and 
;;the updates are merged with the simulation state in a predefined manner.  
;;We may wish to formalize this as a language feature (i.e. macro) to define 
;;different types of update.

;;#Conventions for Notifications/Events/Messaging Functions
;;Message functions are suffixed by the \! character, by convention, to denote 
;;the possibility of side effects. The function signatures are pure, in that 
;;they return a context.  The naming convention will help identify when 
;;"messaging" is occuring.  While effects may happen, in the form of logging 
;;or display updating, the internal simulation mechanics are still pure.
;;__TODO__ -> implement defmessage macro....

;;#Cries for Better Abstractions
;;Note--> I'm noticing some patterns emerging that might make for nice 
;;abstractions...
;;We seem to be doing a lot of nested updates for particular bits of a system.
;;A lot of updates also appear to be existential in nature, i.e. if the 
;;update is a dissoc, and the resulting collection is empty, we want to 
;;remove the empty entry from the parent container.

;;Also, we have a class of functions that specifically shovels context around.
;;By context, I mean the environment in which the simulation is happening, as 
;;in a simcontext from sim.simcontext.  We can clean up some of the function 
;;signatures by allowing macros to operate in this context, possibly even a 
;;special evaluator.

;;On the topic of special evaluators, in each namespace, we end up defining 
;;pure functions that act to update specific bits of a context, sometimes 
;;acting on isolated structures within the context.  There's typically a 
;;nested structure in the context's :state, which destructure and update in 
;;pieces.  It might be nice to have something like #'doto. 

;;It might be nice to have a macro that lets us define ways to address or query
;;the context's state.  This leads directly to the entity-store stuff I already 
;;built, and facilitates a component-based architecture.

;;Finally, we probably want some kind of language for specifying different 
;;types of updates to the context.  For instance, we end up -inside the local 
;;namespaces - defining functions that operate on a specific area of the context
;;often with deep nesting paths.  I already introduced the notion of updates and
;;merge-updates in the simcontext.  It might be as simple as defining
;;multimethods, or a protocol, that implements the merge protocol.

;;what would a nice abstraction look like? 
;;adjust-max-utilization!, from marathon.sim.supply is a great candidate..
;;there are a couple of bread-n-butter items of interest...
;;1) we're updating a unit functionally, storing the result of the update, 
;;   a new unit
;;2) we're updating the supplystore, to reflect the updated unit from 1).
;;3) we're merging the new supplystore into the updated context, by 
;;   passing it to a generic "update" handler that knows how to interpret 
;;   the key :supplystore, the new supply, and a context, to transition to 
;;   a new context.
;;One improvement is to wrap the operation behind a function to provide a clean
;;API....namely, update-unit 
;;This would fetch the appropriate unit, apply a function to it (like update-in),
;;and return a new supplystore with the unit updated.
;;  In this sense, we're lifting a specific function, updating the policyq of 
;;  a unit, into the context of a container for units. 
;;  In haskell speak, the supplystore is a functor, we're applying an update to 
;;  an element of the container.  It's the container's job to understand how 
;;  to lift the updating function into the proper context, and return the new 
;;  container.  monads/monoids anyone? 

;;#Policy Management Notes
;;Policy changes were encapsulated in the IRotationPolicy implementations.
;;This assumed that units would change policies, regardless of event-context.
;;That's actually a decent assumption.
;;However, when we tell unitdata to change policy, it evokes a change state 
;;evaluation.
;;Under the decoupled architecture, this requires simulation context.

;;I'm going to have the policy ops define a function (really just adapted from 
;;policymanager), that passes the context needed.  This is in-line with other 
;;decoupled, functional representations.
;;I have to rewire the IRotationPolicy implementation.....specifically taking 
;;out the onperiodchange event handling.
;;Rather, we'll let policy ops take care of changing units' composite policies.  
;;The good news is, all the bits are here.  Just need to re-organize the code.

;;Is there a way we can create shallow maps?  For instance, if we're 
;;trying to update a nested map, and we don't want to make the whole 
;;thing transient, can we pass around a temporarily mutable version? 

;;one idea here, is to have something that wraps the map and creates 
;;a map of transient info...
;;When we do reads, we use the transient, when we do writes, we 
;;use the transient.  Then, when we want to reify, 
;;we merge the transient with the original.  Ideally, this 
;;keeps the copy paths nice and small.

;;the simplest way is to have a mutable collection, i.e. 
;;a hashtable, with the values along the path.
;;Then for get-in and assoc-in, we can look at the 
;;path cache and see if the path exists there.  Then, we 
;;can apply the ops to the pathcache. 

;;Operations optimized for speed.  the -in and friends 
;;are not sufficient...


;;<<<<<<<<<<<                                    >>>>>>>>>>>>>>>>>
;;This was an early idea to improve perf, but the side-effects are
;;causing simulation history to end up out-of-order...

;;Aborted Performance Optimization Tests
;;For now, we're putting row-store operations on hold.  Quite a bit of issues here...

;;#_(println [:<<<<<TODO :TEST-ROWSTORE 'spork.sim.core/emptystate :TODO>>>>>])
;;#_(def emptystate (simstate/->store :init-store store/empty-rowstore))

;;Shitty hacks.
;; (defn row-state! []
;;   (do ;;Testing purposes...
;;       (throw (Exception. "Currently disabled due to changes row-store incurs against reproducibility."))
;;       (def emptystate    (simstate/->store :init-store spork.entitysystem.store/empty-rowstore))
;;        ;;temporary testing purposes...
;;        ;;we could make emptysim dynamic...
;;       (def emptysim   (sim/add-time 0 (sim/make-context :state emptystate)))
;;       (def debugsim   
;;         (->> (-> (sim/make-debug-context 
;;                   :debug-handler  
;;                   debug-listener)
;;                  (assoc :state emptystate))
;;              (sim/add-time 0)))))

;; (defn col-state! []
;;     (do ;;Testing purposes...
;;       (def emptystate    (simstate/->store :init-store spork.entitysystem.store/emptystore))
;;        ;;temporary testing purposes...
;;        ;;we could make emptysim dynamic...
;;       (def emptysim   (sim/add-time 0 (sim/make-context :state emptystate)))
;;       (def debugsim   
;;         (->> (-> (sim/make-debug-context 
;;                   :debug-handler  
;;                   debug-listener)
;;                  (assoc :state emptystate))
;;              (sim/add-time 0)))))
