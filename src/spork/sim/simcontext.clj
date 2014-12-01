;A simulation context is a general purpose structure for bearing discrete event
;simulation information, specifically a scheduler, an updater, event-routing/
;event registration (propogation), and the simulation state.

;The simulation context defines the dependencies in the simulation, as well as 
;the pending instantaneous changes (event), and the notion of time or an 
;absolute ordering of the computation in the simulation.

;The scheduler is just an agenda value from sim.agenda.  Its purpose is to 
;provide a focus for ordering events.  In the observer-based, mutation-driven 
;style of old, we would "trigger" events in functions, which would communicate
;the presence of an event along the event stream by broadcasting an
;instantaneous event.  In the newer functional styling, "triggering" an event 
;should amount to pushing an event onto the simulation context.  We can then
;handle instantaneous events by threading an event-handling function in between
;each of the main simulation process functions....thus allowing for an absolute
;ordering of events (even instantaneous events) as they are queued.

;Scheduling is typically used to "wake" entities within the simulation at 
;specified times, or to indicate that a set of simulation processing needs to 
;occur.  

;The updater allows us to encode localized updates or patches to entities, 
;and to keep track of the last "time" an entity was updated.  For temporal 
;simulations, this is valuable as it provides efficient, localized updating 
;of entities, and keeps the system consistent (i.e. prevents the application of
;old time-dependent updates with outdated notions of elapsed time).
;Much of the simulation domain requires time deltas to compute the next step 
;of the simulation, or to define an entity's behavior, so keeping track of 
;this is fairly useful.

;Event routing information encodes a communications network, represented by an 
;event-network from sim.pure.network.  For every simulation, there is a 
;vocabulary used to communicate changes in the simulation or to trigger 
;side-effects (rendering or logging for instance). The propogator describes a 
;network of named events, and for each event, an ordered set of subscribers that 
;are "interested" in the event. We typically handle this with an 
;observable/observer setup, which encodes an event as an enumerated type, 
;mapping every event to a list of oberservers who need to be notified when an 
;event is observed.  We can simulate this setup in a pure manner, by maintaining 
;the same dependency map, and - rather than broadcasting the event, fold the 
;event and the simulation state through the handlers in series.

;The rest of the namespace defines an API for creating, querying, and modifying
;abstract simulation contexts.

(ns spork.sim.simcontext
  (:require [spork.sim [data :as sim] [agenda :as agenda] [updates :as updates]]
            [spork.sim.pure     [network :as simnet]]
            [spork.entitysystem [store :as store]]
            [spork.data [mutable :as mut]]
            [spork.util [metaprogramming :as util]
                        [general :as gen]]))
;probably need to move from marathon.updates to something in the sim namespace.

(defn next-type
  "Gets the type of the next event, based off of its :type field."
  [ec] (-> ec
           (sim/first-event)
           (sim/event-type)))

(defprotocol IUpdateable
  (add-update [ctx tupdate requested-by request-type]   
    "Public API for accounting for update requests, which consist of a time 
     to update a specific entity in the simulation, and a form of request.  No
      additional data is passed (although I may change that in future...)")
  (add-updates [ctx xs]   
    "Allows user to request multiple updates, represented as 
     [update-time request-by request-type] vectors."))

;;Simulation Context
;;==================
;;The simcontext is a container for all the information we need to 
;;perform complex entity-based, discrete event simulations.

;;Right now, I'm just extending the existing simulation protocols to
;this guy.  We could have a base event context here....I suppose that
;would be a good thing to do...
;;Also, we could spin this out into a macro, to handle all the
;;different kinds of simulation definitions, if say, we wanted 
;;more fields than the defaults.  We'll see...
(declare ->msimcontext)

(defrecord simcontext 
  [^spork.sim.agenda.agenda
   scheduler ;supported by agenda.  
   updater ;a weak agenda with some special state, tracks previous updates. 
   propogator  ;event propogation, represented by a propogation network. 
   state ;;typically an entity store...but not necessarily.
   ]
  agenda/IAgenda 
  (previous-time  [a]     (.previous-time scheduler))
  (final-time     [a]     (.final-time scheduler))
  (set-final-time [a tf]  (simcontext. (.set-final-time scheduler tf) updater propogator state))
  (agenda-count   [a]     (.agenda-count scheduler))
  (time-segments  [a]     (.time-segments scheduler))
  (add-times      [a ts]  (simcontext. (.add-times scheduler ts) updater propogator state))
  (get-times      [a]     (.get-times scheduler))
  sim/IEventSeq
  (add-event   [ctx e] (simcontext. (sim/add-event scheduler e) 
                                    updater propogator state))                                 
  (drop-event  [ctx] (simcontext. (sim/drop-event scheduler) updater propogator state))
  (first-event [ctx] (sim/first-event scheduler))
  (nth-event [ctx n] (sim/nth-event scheduler n))
  store/IEntityStore
  (add-entry      [db id domain data] 
    (simcontext. scheduler updater propogator 
       (store/add-entry state id domain data)))
  (drop-entry     [db id domain] 
    (simcontext. scheduler updater propogator 
        (store/drop-entry state id domain))) 
  (get-entry      [db id domain] (store/get-entry state id domain))
  (entities       [db]     (store/entities state))
  (domains        [db]     (store/domains state))
  (domains-of     [db id]  (store/domains-of state id))
  (components-of  [db id]  (store/components-of state id))
  (get-entity     [db id]  (store/get-entity state id))
  (conj-entity    [db id components] 
    (simcontext. scheduler updater propogator 
      (store/conj-entity state id components)))
  simnet/IEventSystem
  (get-events [ctx]  (simnet/get-event    propogator))
  (get-clients [ctx] (simnet/get-clients  propogator))
  (get-event-clients [obs event-type]  (simnet/get-event-clients propogator event-type))
  (get-client-events [obs client-name] (simnet/get-client-events propogator client-name))
  (unsubscribe  [obs client-name event-type] 
    (simcontext. scheduler updater 
       (simnet/unsubscribe propogator client-name event-type) state))
  (subscribe [obs client-name handler event-type] 
    (simcontext. scheduler updater 
       (simnet/subscribe propogator client-name handler event-type) state))
  simnet/IEventContext
  (handle     [ctx e] 
    (let [type               (sim/event-type e)
          client-handler-map (simnet/get-event-clients propogator type)]
      (let [res   (simnet/serial-propogator 
                   (-> simnet/default-context 
                       (simnet/set-event e)
                       (simnet/set-net propogator)
                       (simnet/set-state ctx)) 
                   (simnet/get-event-clients propogator :all)
                   client-handler-map)
            nxtsim   (simnet/get-state res)]
        (assoc nxtsim :propogator (simnet/get-net res)))))
  (set-event  [ctx e]        (throw (Error. "set-event unavailable on simcontext")))
  (set-transition [ctx txn]  (throw (Error. "set-transition unavailable on simcontext")))
  (set-state  [ctx s]   (throw (Error. "set-state unavailable on simcontext")))
  (set-net    [ctx n]   (throw (Error. "set-net unavailable on simcontext")))
  (get-event  [ctx]     (throw (Error. "get-event unavailable on simcontext")))
  (get-transition [ctx] simnet/default-transition)
  (get-state [ctx]      state)
  (get-net   [ctx]      propogator)
  IUpdateable
  (add-update [ctx tupdate requested-by request-type]   
    (let [t    (or  (sim/current-time scheduler) 0)]
      (simcontext.  (agenda/add-time scheduler tupdate)
                    (updates/request-update updater tupdate requested-by request-type t)                    
                    propogator
                    state)))
  (add-updates [ctx xs]   
     (let [t    (or  (sim/current-time scheduler) 0)]
       (simcontext. (agenda/add-times scheduler xs)
                    (reduce (fn [acc [tupdate by type]] 
                              (updates/request-update acc tupdate by type t))
                            (transient updater)  xs)
                    propogator
                    state)))
  clojure.lang.IEditableCollection ;WIP
  (asTransient [coll] (->msimcontext scheduler ;supported by agenda.  
                                     (transient updater) ;a weak agenda with some special state, tracks previous updates. 
                                     propogator  ;event propogation, represented by a propogation network. 
                                     state)))

;;A mutable simulation context.  Currently almost identical to the
;;persistent version.
(mut/defmutable msimcontext 
  [^spork.sim.agenda.agenda
   scheduler ;supported by agenda.  
   updater ;a weak agenda with some special state, tracks previous updates. 
   propogator  ;event propogation, represented by a propogation network. 
   state ;;typically an entity store...but not necessarily.
   ]
  agenda/IAgenda 
  (previous-time  [a]     (.previous-time scheduler))
  (final-time     [a]     (.final-time scheduler))
  (set-final-time [a tf]  (simcontext. (.set-final-time scheduler tf) updater propogator state))
  (agenda-count   [a]     (.agenda-count scheduler))
  (time-segments  [a]     (.time-segments scheduler))
  (add-times      [a ts]  (simcontext. (.add-times scheduler ts) updater propogator state))
  (get-times      [a]     (.get-times scheduler))
  sim/IEventSeq
  (add-event   [ctx e] (do (set! scheduler (sim/add-event scheduler e)) ctx))                                 
  (drop-event  [ctx] (do (set! scheduler (sim/drop-event scheduler)) ctx))
  (first-event [ctx] (sim/first-event scheduler))
  (nth-event [ctx n] (sim/nth-event scheduler n))
  store/IEntityStore
  (add-entry      [db id domain data] 
    (do (set! state  (store/add-entry state id domain data)) db))
  (drop-entry     [db id domain] 
    (do (set! state  (store/drop-entry state id domain)) db))
  (get-entry      [db id domain] (store/get-entry state id domain))
  (entities       [db]     (store/entities state))
  (domains        [db]     (store/domains state))
  (domains-of     [db id]  (store/domains-of state id))
  (components-of  [db id]  (store/components-of state id))
  (get-entity     [db id]  (store/get-entity state id))
  (conj-entity    [db id components]                   
    (do (set! state  (store/conj-entity state id components)) db))
  simnet/IEventSystem
  (get-events [ctx]  (simnet/get-event    propogator))
  (get-clients [ctx] (simnet/get-clients  propogator))
  (get-event-clients [obs event-type]  (simnet/get-event-clients propogator event-type))
  (get-client-events [obs client-name] (simnet/get-client-events propogator client-name))
  (unsubscribe  [obs client-name event-type] 
    (do (set! propogator (simnet/unsubscribe propogator client-name event-type)) obs))
  (subscribe [obs client-name handler event-type] 
    (do (set! propogator (simnet/subscribe propogator client-name handler event-type)) obs))
  simnet/IEventContext
  (handle     [ctx e] 
    (let [type               (sim/event-type e)
          client-handler-map (simnet/get-event-clients propogator type)]
      (let [res   (simnet/serial-propogator 
                   (-> simnet/default-context 
                       (simnet/set-event e)
                       (simnet/set-net propogator)
                       (simnet/set-state ctx)) 
                   (simnet/get-event-clients propogator :all)
                   client-handler-map)
            nxtsim   (simnet/get-state res)]
        (assoc nxtsim :propogator (simnet/get-net res)))))
  (set-event  [ctx e]        (throw (Error. "set-event unavailable on simcontext")))
  (set-transition [ctx txn]  (throw (Error. "set-transition unavailable on simcontext")))
  (set-state  [ctx s]   (throw (Error. "set-state unavailable on simcontext")))
  (set-net    [ctx n]   (throw (Error. "set-net unavailable on simcontext")))
  (get-event  [ctx]     (throw (Error. "get-event unavailable on simcontext")))
  (get-transition [ctx] simnet/default-transition)
  (get-state [ctx]      state)
  (get-net   [ctx]      propogator) 
  (persistent [coll] 
     (simcontext. scheduler ;supported by agenda.  
                  (persistent! updater) ;a weak agenda with some special state, tracks previous updates. 
                  propogator  ;event propogation, represented by a propogation network. 
                  state))
  IUpdateable
  (add-update [ctx tupdate requested-by request-type]   
    (let [t    (or  (sim/current-time scheduler) 0)]
      (do (set! scheduler  (agenda/add-time scheduler tupdate))
          (set! updater    (updates/request-update updater tupdate requested-by request-type t))
          ctx)))
  (add-updates [ctx xs]   
     (let [t    (or  (sim/current-time scheduler) 0)] 
       (do (set! scheduler (agenda/add-times scheduler xs))
           (set! updater  (reduce (fn [acc [tupdate by type]] 
                                    (updates/request-update acc tupdate by type t))
                                   updater  xs))
           ctx))))

(defmacro transiently 
  "Establish a transient binding to the context, returning a persistent call to to the binding after 
   evaluating expr.  This is currently used for allowing transient updates, since that 
   seems to be the most apparent hotspot in the simulation context.  I may extend this in the future 
   to more general transient enroachment, culminating with a call to (transient ) on the ctx."
  [[bind ctx] & expr]
  `(let [~bind (transient ~ctx)]
     (do ~@expr
         (persistent! ~bind))))

(defmacro update-field [ctx field f & args]
  (let [getter (if (keyword? field) (symbol (str "." (subs (str field) 1))))
        k    (if (keyword? field) field (keyword field))]
    `(.assoc ~ctx ~k
        (~f (~getter ~ctx) ~@args))))                                      
  
(defmacro update-scheduler [ctx f & args]
  `(let [sched# (f (.scheduler ctx) ~@args)]
     (.assoc ctx sched#)))                                      

;I should probably just use the protocol functions here....rather than 
;re-implementing them....that's a refactoring/clean-up step.  I'm just 
;trying to get the port working.
(defn current-time
  "Fetches the current time of the context."
  [^simcontext ctx]  (sim/current-time (.scheduler ctx)))

(defn current-quarter
  "Don't really need this in agenda..Fetches the current quarter."
  [^simcontext ctx] (agenda/quarter (.scheduler ctx)))

(defn elapsed
  "Computes the time elapsed since the last event in the context."
  [^simcontext ctx] (agenda/elapsed (.scheduler ctx)))

(defn add-time
  "Ensures that time t exists on the agenda."
  [t ^simcontext ctx] (update-field ctx :scheduler agenda/add-time t))

(defn set-final-time
  "Sets the upper bound on the time horizon."
  [tf ^simcontext ctx] (update-field ctx :scheduler agenda/set-final-time tf)) 

;;Maybe rewrite this guy...Older functions expect add-listener, 
;;should replace them with library calls.
(defn add-listener 
  "Legacy proxy for adding event handlers to the context.  Given a client-name, 
   a handler function, and a sequence of event-types to subscribe to, associates
   the client with each event in the context's propogation network.
  
   Note - The older code assumed that each client would be associated with one, 
   specific handler function (usually with its own internal dispatch).  The 
   clojure version of the event propogation network does not have this 
   restriction, and allows a single client to handle multiple event types 
   differently."
  [client-name handler subscriptions ctx]
  (reduce (fn [context e] (simnet/register context client-name handler e))
          ctx subscriptions))

(def ^:constant +empty-msg-data+ {:msg nil :data nil})

(defrecord packet [t type from to msg data]
  sim/IEvent 
  (event-type [e] type)
  (event-data [e] data)
  (event-id   [e] type)
  (event-time [e] t)
  (event-from [e] from)
  (event-to   [e] to))

;;Extended IEvent to packets, created a like data structure.  Specific 
;;to marathon's stuff.  May want to generalize this.  For now, it works.  
(defn ->packet
  "Creates an event record that - possibly - carries a message and some data.
   This is derived from the initial GenericPacket class from an earlier version
   of Marathon.  In the clojure manifestation, it's a sim.data.event with a 
   special structure for the event-data.  The message field is typically used
   for passing logging information, but the remaining fields are more or less 
   identical.  Also time-stamps the packet with the current-time."
  ([t type from to]          (packet. t type from to nil nil))
  ([t type from to msg data] (packet. t type from to msg data)))

(defn packet-message
  "Fetches the message, if any from an event that carries a packet."
  [^packet p] (.msg p))

(defn packet-data 
  "Fetches associated data, if any, from an event that carries a packet."
  [^packet p] (.data p))

;Functions ported from Marathon.SimLib 
(defn trigger-event
  "Shorthand convenience function for triggering immediate, synchronous events
   inline, according to the simulation context's event propogation scheme.
   Returns the resulting simulation context."
  ([event-type entity-from entity-to msg data ctx]
    (simnet/handle-event (packet. (current-time ctx) event-type entity-from 
                                  entity-to msg data)  ctx))
  ([event ctx]
    (simnet/handle-event event ctx)))

(defn trigger-events [xs ctx] (reduce #(simnet/handle-event  %2 %1) ctx xs))

(defn set-time-horizon
  "Ensures that time events exist for both tstart and tfinal, and sets the 
   final time for the context to tfinal."
  [tstart tfinal ctx]
  (->> (add-time tstart ctx)
       (add-time tfinal)
       (set-final-time tfinal)))

;; Public API for accounting for update requests, which consist of a time 
;; to update a specific entity in the simulation, and a form of request.  No
;; additional data is passed (although I may change that in future...)
(definline request-update
  [tupdate requested-by request-type ctx]
  `(add-update ~ctx ~tupdate ~requested-by ~request-type))
 
;;Allows user to request multiple updates, represented as 
;;[update-time request-by request-type] vectors.
(definline
  request-updates 
  [xs  ctx]
 `(add-updates ~ctx ~xs))

(defn advance-time 
  "Pop the next event off of the simulation context.  If the simulation context
   only contains time events, as in the legacy implementation, this will 
   effectively advance time.  In the case that the schedule contains other 
   events, it will return the result of popping the next event, which may or may
   not result in a change in the current time.  Probably needs re-looking..."
  [^simcontext ctx] 
  (update-field ctx :scheduler agenda/advance-time))

(defn get-final-time
  "Returns the upper bound on the simulation time, if bounded."
  [^simcontext ctx] (agenda/final-time (.scheduler ctx)))

(defn get-next-time
  "Returns the time of the next event in the context."
  [^simcontext ctx] 
  (sim/next-time (.scheduler ctx)))

(defn add-times
  "Add multiple times to the schedule of the simulation context."
  [xs ^simcontext ctx]
  (update-field ctx :scheduler agenda/add-times xs))

(defn get-time
  "Duplicate functionality of current-time.  Deprecate?"
  [ctx] (current-time ctx))

(defn has-time-remaining?
  "Consult the context to determine if there are, in terms of scheduled
   time events and a possible final time, any time events remaining."
  [^simcontext ctx] (agenda/still-time? (.scheduler ctx)))

(defn make-context
  "Creates a default simulation context record from component pieces.  If no 
   keys are supplied, defaults are inserted.  Also initializes default routing 
   for the context - specifically ensures that update requests are routed to 
   the updater.  Returns the simulation context."
  [& {:keys [scheduler updater events state]}]
  (let [ustore (or updater updates/empty-updatestore)
        events (-> (or events (simnet/empty-network :event-propogation))
                   (updates/add-routes ustore))] 
    (->simcontext (or scheduler agenda/empty-agenda) ustore events state)))

(def empty-context (make-context))

(defn last-update
  "Returns the last time the entity was updated in the simulation context."
  [entity-name ^simcontext ctx]
  (updates/last-update (.updater ctx) entity-name))

(defn get-updates
  "Returns a list of updates, by type, scheduled for time t in the simulation
   context."
  [update-type t ^simcontext ctx]
  (updates/get-updates (.updater ctx) update-type t))
  
(defn get-state 
  "Accesses the state of the simulation context."
  [^simcontext ctx] (.state ctx))

(defn debug-msg [& xs] (apply str "<Debug>" xs))

(defn make-debug-context
  "Creates a simulation context that pipes every event through a debug handler, 
   which the caller can specify."
  [& {:keys [debug-handler] 
      :or {debug-handler 
           (fn [ctx edata name] 
             (do (println (debug-msg ":debugger saw " [(sim/event-type edata) 
                                                       (sim/event-data edata)])) ctx))}}]    
  (add-listener :debugger debug-handler [:all] (make-context)))

;new helper functions.
(defn update-state [f   ^simcontext ctx]  (update-field ctx :state  f))
(defn assoc-state  [k v ^simcontext ctx]  (update-field ctx :state  assoc k v))
;should probably allow for a parallel version of this guy.
(defn merge-updates [m  ctx] 
  (if (map? m)
    (reduce-kv (fn [^simcontext c k v]
                 (if (= k :trigger) (v c) 
                     (assoc-state k v c)))
               ctx m)
    (reduce (fn [^simcontext c [k v]]
              (if (= k :trigger) (v c) 
                  (assoc-state  k v c)))
            ctx m)))

;it'd be really nice if we had a simple language for describing updates...
;not unlike Conrad's "patch" 
;there are a few idioms that keep popping up....
;roll the context through some event notifications, 
;update bits of state in the context, sometimes in a very nested fashion..

;We can take advantage of that...just use events...
  ;unit-update can pass new units around...
  ;supply handler would be in charge of integrating update...

;Another idea is to leverage the global-state nature of the simstate..
;treat it as a nested datastructure (which we currently do), 
;define a nice API for updating paths in the structure.

;This is going back to the entitystore route...

;maybe a macro, like a sim monad?
;we'll see about this later.
;(with-simcontext [blah ctx] ;wraps everything in an implicit do...
;  (trigger  ....) 
;  (update   ....))

;should translate into...
;(let [*ctx* ...
;      *time* ...
;      *blah*
;      ]
;)

;need to work some more examples.
;(with-let [context ctx]
;  [the-key the-val
;   [the-key the-other-key] the-val]
;  (body))


(comment ;testing
;  (defn pass-through [msg] (fn [ctx e name] (do (println msg) ctx)))
  (def echo-name 
    (fn [ctx e name] (do (println (debug-msg name " says 'Ping!'")) ctx)))
  (def bare-routes {:supply-manager {:supply-events echo-name}
                    :demand-manager {:demand-events echo-name}    
                    :policy-manager {:policy-events echo-name}
                    :output-manager {:output-events echo-name}})
  (def simple-ctx (let [c (make-debug-context)
                        p (:propogator c)]
                    (assoc c :propogator 
                           (-> (simnet/register-routes  bare-routes p)
                               (updates/add-routes (:updater c))))))

  (defn simple-update [t name ctx]
    (->> ctx 
         (request-update t name :some-update)
         (request-update (+ t 100) name :some-update)))
)

