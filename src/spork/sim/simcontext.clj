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
            [spork.sim.pure [network :as simnet]]
            [spork.util [metaprogramming :as util]]))
;probably need to move from marathon.updates to something in the sim namespace.

(defn next-type
  "Gets the type of the next event, based off of its :type field."
  [ec] (-> ec
         (sim/first-event)
         (sim/event-type)))

;we need to elevate this to a protocol for simulation contexts...
;things like simstate (in another namespace) need to be seen as valid contexts.
(defrecord simcontext 
  [scheduler ;supported by agenda.  
   updater ;a weak agenda with some special state, tracks previous updates. 
   propogator  ;event propogation, represented by a propogation network. 
   state]) ;the state of the simulation.

;I should probably just use the protocol functions here....rather than 
;re-implementing them....that's a refactoring/clean-up step.  I'm just 
;trying to get the port working.
(defn current-time
  "Fetches the current time of the context."
  [ctx]  (sim/current-time (:scheduler ctx)))

(defn current-quarter
  "Don't really need this in agenda..Fetches the current quarter."
  [ctx] (agenda/quarter (:scheduler ctx)))

(defn elapsed
  "Computes the time elapsed since the last event in the context."
  [ctx] (agenda/elapsed (:scheduler ctx)))

(defn add-time
  "Ensures that time t exists on the agenda."
  [t ctx] (update-in ctx [:scheduler] agenda/add-time t))

(defn set-final-time
  "Sets the upper bound on the time horizon."
  [tf ctx] (update-in ctx [:scheduler] agenda/set-final-time tf)) 

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
  (let [net (:propogator ctx)]
    (->> (reduce (fn [context e] (simnet/register context client-name handler e))
                 net subscriptions)
      (assoc ctx :propogator))))

(defn ->packet
  "Creates an event record that - possibly - carries a message and some data.
   This is derived from the initial GenericPacket class from an earlier version
   of Marathon.  In the clojure manifestation, it's a sim.data.event with a 
   special structure for the event-data.  The message field is typically used
   for passing logging information, but the remaining fields are more or less 
   identical.  Also time-stamps the packet with the current-time."
  [t type from to & {:keys [msg data] 
                   :or {msg nil data nil}}] 
  (sim/->event sim/event-type {:msg msg :data data}   nil t from to))

(defn packet-message
  "Fetches the message, if any from an event that carries a packet."
  [p] (get (sim/event-data p) :msg))

(defn packet-data 
  "Fetches associated data, if any, from an event that carries a packet."
  [p] (get (sim/event-data p) :data))

;Data marshalling crap.  We shouldn't have to do this...should be using
;a protocol.
;This is a total hack, we're just packaging the context....
(defn ctx->state [ctx] 
  {:state     (:state ctx)
   :net       (:propogator ctx)
   :updater   (:updater ctx)
   :scheduler (:scheduler ctx)})

;This is a total hack, we're just un-packaging the context....
(defn state->ctx [{:keys [scheduler net updater state]}]
  (->simcontext scheduler updater net state)) 

;Functions ported from Marathon.SimLib 
(defn trigger-event
  "Shorthand convenience function for triggering immediate, synchronous events
   inline, according to the simulation context's event propogation scheme.
   Returns the resulting simulation context."
  ([event-type entity-from entity-to msg data ctx]
    (trigger-event (->packet (current-time ctx) event-type entity-from 
                                 entity-to :msg msg :data data)  ctx))
  ([event ctx]
    (state->ctx (simnet/handle-event event (ctx->state ctx)))))

(defn trigger-events [xs ctx] (reduce #(trigger-event %2 %1) ctx xs))

(defn set-time-horizon
  "Ensures that time events exist for both tstart and tfinal, and sets the 
   final time for the context to tfinal."
  [tstart tfinal ctx]
  (->> (add-time tstart ctx)
       (add-time tfinal)
       (set-final-time tfinal)))

(defn request-update
  "Public API for accounting for update requests, which consist of a time 
   to update a specific entity in the simulation, and a form of request.  No
   additional data is passed (although I may change that in future...)"
  [tupdate requested-by request-type ctx]
  (let [t    (or (current-time ctx) 0)
        req-data  {:update-time tupdate  
                   :requested-by requested-by
                   :update-type request-type 
                   :trequest t}]
    (trigger-event  (sim/->simple-event :update-request t req-data)
                    (add-time tupdate ctx))))

(defn advance-time 
  "Pop the next event off of the simulation context.  If the simulation context
   only contains time events, as in the legacy implementation, this will 
   effectively advance time.  In the case that the schedule contains other 
   events, it will return the result of popping the next event, which may or may
   not result in a change in the current time.  Probably needs re-looking..."
  [ctx] 
  (update-in ctx [:scheduler] agenda/advance-time))

(defn get-final-time
  "Returns the upper bound on the simulation time, if bounded."
  [ctx] (agenda/final-time (:scheduler ctx)))

(defn get-next-time
  "Returns the time of the next event in the context."
  [ctx] 
  (sim/next-time (:scheduler ctx)))

(defn add-times
  "Add multiple times to the schedule of the simulation context."
  [xs ctx]
  (update-in ctx [:scheduler] agenda/add-times xs)) 

(defn get-time
  "Duplicate functionality of current-time.  Deprecate?"
  [ctx] (current-time ctx))

(defn has-time-remaining?
  "Consult the context to determine if there are, in terms of scheduled
   time events and a possible final time, any time events remaining."
  [ctx] (agenda/still-time? (:scheduler ctx)))

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
  [entity-name ctx]
  (updates/last-update (:updater ctx) entity-name))

(defn get-updates
  "Returns a list of updates, by type, scheduled for time t in the simulation
   context."
  [update-type t ctx]
  (updates/get-updates (:updater ctx) update-type t))
  
(defn get-state 
  "Accesses the state of the simulation context."
  [ctx] (:state ctx))

(defn debug-msg [& xs] (apply str "<Debug>" xs))

(defn make-debug-context
  "Creates a simulation context that pipes every event through a debug handler, 
   which the caller can specify."
  [& {:keys [debug-handler] 
      :or {debug-handler 
           (fn [ctx edata name] 
             (do (println (debug-msg ":debugger saw " [(:type ctx) edata])) ctx))}}]    
  (add-listener :debugger debug-handler [:all] (make-context)))

;new helper functions.
(defn update-state [f ctx]   (update-in ctx [:state] f)) 
(defn assoc-state [k v ctx]  (update-state #(assoc % k v) ctx))
;should probably allow for a parallel version of this guy.
(defn merge-updates [m ctx] 
  (reduce (fn [c [k v]]
            (if (= k :trigger) (v c) 
            (assoc-state k v c)))))


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
                           (simnet/register-routes  bare-routes p))))
)

