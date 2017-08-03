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
            [spork.entitysystem.store :refer :all :exclude
             [defpath defpaths entity-name merge-entity] :as store]
            [spork.entitysystem.ephemeral]            
            [spork.ai [core        :as ai]
                      [behaviorcontext :as b]
                      [messaging]]))

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
  add-time
  request-update
  request-updates])

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

(util/import-vars 
 [spork.entitysystem.ephemeral  
  get-ephemeral
  conj-ephemeral
  reset-ephemeral
  swap-ephemeral
  some-ephemeral  
  ])

;;Ephemeral Counter
;;=================
;;Typically used for storing append-only daily logging
;;information.

;;Associate an entity with a unique numerical value.
;;Provides an efficient way to get "global" numerical
;;counters while maintaining the ability to persistent
;;at the end of the day.
(util/import-vars 
 [spork.entitysystem.ephemeral  
  swap-counter
  inc-counter
  get-counter
  persist-counters
  get-count 
  ])

;;Acts like juxt, except it returns the results in a map.
;;The map is implied by the args in kvps, where simple keys - numbers,
;;strings, keywords, are assumed to be field names. A map is built 
;;from the fields by getting the corresponding field in an input
;;record.  Vector keys are assumed to imply [key function-to-apply]
;;TODO: Verify then deprecate!
(comment
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
 )


;;This is going to be a little brittle; should access to updates
;;behind a protocol...
(defn updates
  "Returns an unordered sequence of pending update packets in simcontext ctx."
  [ctx] 
  (for [[ks xs] (-> ctx :updater :updates)
        [ts  us]  xs
        [nm pckt] us]
    pckt))

(defn update-events
  "Projects the update packets in simcontext ctx onto a sequence of 
   :startday :duration appended fields."
  [ctx] 
  (map (fn [{:keys [update-time request-time] :as r}]
         (assoc r :startday request-time :duration (- update-time request-time)))
       (updates ctx)))

;;TODO Determine if it's useful to retain this....
#_(defn visualize-updates [ctx  & {:keys [track-keyf color-keyf] :or {track-keyf (juxt :requested-by)
                                                                    color-keyf (juxt :update-type)}}]
  (visualize-events (update-events ctx) track-keyf color-keyf))

(defn visualize-store
  "Returns a visualization of the simcontext ctx as a tree, allowing 
   interactive exploration of the entitystore domains."
  [ctx]
  (tree-view  (store/domains ctx)))

(defn visualize-data
  "Provides a generic tree-view on input x for interactive data exploration."
  [x]
  (tree-view x))

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

;;This is fairly close....
;;TODO: determine how to generalize this (or parameterize it
;;to allow custom views...)
#_(defn visualize-entities [ctx]
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

;;TODO Change to visualize-event-subscriptions, look at the simnetwork.
#_(defn visualize-subscriptions [ctx] 
  (inspect/tree-view (:subscriptions (get-policystore ctx))))
      
;;#Shared Functions

;;These functions were extracted due to use across multiple domains.  I may 
;;refactor them into core services, but for now, relocating them in sim.core 
;;allows every system access to them.

(defn now
  "Consult the system clock for the current time.  Used for logging."
  [] 
  (System/currentTimeMillis))

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
(def msg
  "faster replacement for clojure.core/str"
  gen/make-string)

;;This is a general utility function, that allows us to derive
;;predicates based on atomic values, sets, or maps.  Used for
;;filtering (currently in demand categories and supply categories).
;;Can probably extend this to allow for arbitrary predicate functions
;;(later).
;;TODO: Determine if we want to put thi  in util.general
#_(defn make-member-pred
  [g]
  (if (or (set? g) (map? g))
    #(contains? g %)
    #(= g %)))

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

(defmacro trigger-event
  "Macro optimization to allow messages to be stripped out 
   if the context is not actively messaging (i.e. debugging).
   This is a nifty cheat, since we avoid allocation for 
   strings entirely. Behaves as a replacement for  
   spork.sim.context/trigger-event , while restricting 
   certain argument combinations, i.e. non-variadic.

   Takes a message id, party from, party to, a form to evaluate
   as a message-string, and relevant data, and a simcontext ctx.
   Effeciently triggers the event immediately (i.e. synchronously
   at the current simulation time)."
  ([id from to msg-form data ctx] ;;chance to strip message.
   `(let [msg# (when ~'spork.sim.core/*debug* ~msg-form)]  
      (sim/trigger-event ~id ~from ~to  msg# ~data ~ctx)))
  ([e ctx] ;event is already baked.
   (sim/trigger-event ~e ~ctx)))











