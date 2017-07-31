;##Overview
;sim.engine contains the higher-level operations that define a discrete event simulation.  It acts 
;as a harness around the simulation context, and guides the flow of control 
;throughout the simulation.  At a high level of abstraction, the engine is a 
;process that coordinates one or more concurrent processes to calculate 
;new simulation contexts from prior simulation contexts.  
;From a functional perspecive, the engine defines an ordered composition of 
;simpler transfer functions - or systems - that, when applied to a initial
;context, returns a new simulation context.  

;When performed repeatedly, feeding preceding simulation contexts into
;the transfer function, a recurrence relation between previous contexts and 
;future contexts emerges.  The result is effectively a snapshot of simulation 
;contexts, which are computed via a simple transfer function, __sim-step__,  
;which simulates multiple domains relevant to the defined simulation.  

;Thus, the engine serves as the causal backbone, and coordinating mechansim, 
;for every bit of logic executed during the course of the simulation. The 
;primary function, __event-step-marathon__, prescribes the order of application 
;;of each logical subsystem. 
(ns spork.sim.engine
  (:require [spork.sim     [observers :as obs]]
            [spork.sim     [simcontext :as sim]]
            [spork.entitysystem [store :as store]]))

;;NOTE: This is Ripped from M4, we'd like to generalize some of the
;;lessons learned into library-specific functions that we can
;;use for easily describing simulations!
;;Right now, this is a just a hack of M4, WIP!.


;#Auxillary functions, and legacy functions

;;Auxillary functions from the old simstate module
;;==================================================
(defn guess-last-day 
  ([ctx lastday]
     (if-let [first-non-zero
              (first (filter #(and (not (nil? %)) (pos? %))
                             [lastday 
                              (-> ctx :state :parameters :last-day)
                              (-> ctx :state :parameters :last-day-default)]))]
       first-non-zero
       0))
  ([ctx] (guess-last-day ctx 0)))

;;Predicate to determine if we continue drawing time from the stream, i . e . simulating.
;;If an endtime is specified, we use that in our conditional logic, else we keep working until
;;no more eventful days are upon us.

;;This is costing us a lot of time in profiling.  Almost 1/3 of the simulation
;;runtime....function invocation is weak.
;;THis is a pretty weak port....
(defn keep-simulating? [ctx]
    (if (store/gete ctx :parameters :truncate-time)
      (let [tlastdemand (or (store/gete ctx :DemandStore :tlastdeactivation)
                            -1)
            t (sim/get-time ctx)]        
        (if (> t tlastdemand)
          (if (neg? tlastdemand) 
            (throw (Error. "No demands scheduled.  Latest deactivation never initialized!"))
            (do (core/trigger-event :Terminate "Simstate" "Simstate"
                   (str "Time " t "is past the last deactivation: "
                        tlastdemand " in a truncated simulation, terminating!") nil ctx)
                false))
          true))
      (sim/has-time-remaining? ctx)))

;##Simulation Initialization
(defn set-time
  "Initialize the start and stop time of the simulation context, based on 
   last-day."
  [last-day ctx]  
 (sim/set-time-horizon 1 (guess-last-day ctx last-day) ctx))

;#Initialization
;Initialization consists of 3 tasks:    
;1. Prep the system for day 0  
;2. Adjust the context so that it defines a finite time horizon for simulation.
;   The last  day of the simulation is determined either by passing in value, 
;   or by allowing the context to derive its own last day dynamically.  
;3. Tell observers to sync themselves to the simulation state.  Some observers 
;   need access to the specific elements of the state.  This makes it easy 
;   (and indirect) to advertise the state, and to allow the observers to link 
;   to it as needed. Observers/watchers will need to be expanded on, since 
;   watches will involve effects.  

;;Temporary Stubs
;;===============
;;Used to be entityfactory.start-state supplymanager
;;Intent is to apply initial conditions to the simulation state,
;;particularly moving unit entities to where they need to be.
(defn start-state [ctx]
  (do (println (str *ns*) "start-state is a stub.  Should be setting entities to their starting states.")
      ctx))

(defn initialize-sim
  "Given an initial - presumably empty state - and an optional upper bound on 
   on the simulation time - lastday - returns a simulation context that is 
   prepared for processing, with default time horizons and any standard 
   preconditions applied."
  [ctx & {:keys [lastday observer-routes]
          :or {observer-routes obs/default-routes}}]
  (->> ctx
       (sim/register-routes observer-routes)
       ;(start-state)
       (set-time lastday)
       (supply/manage-supply   0)
       (policy/schedule-periods)
       (policy/manage-policies 0)))

;##Simulation Termination Logic
;When we exit the simulation, we typically want to perform some final tasks.
;For instance, any resources (for logging, display, etc.) may need to be freed.
;The default mechanism for this is to propogate some events through the context
;and let interested parties handle themselves appropriately.

;;Note-> I bolted on manage-policies before.  I think we can just move
;;that to a handler...manage-policies enters units into the final period.
(defn finalize
  "Shifts the simulation period into a final period.  Forces sampling and any
   other cleanup actions, like computing final statistics, truncating unit 
   lifecycles, etc. Notify any other listeners that the simulation has 
   terminated.  Such notification is particularly important for observers that 
   may be stewarding resources."
  [t ctx]
  (let [final-ctx  (-> (manage-policies t ctx :final) ;;this just captures
                       ;;the final period in a period-driven update.
                       ;;really triggers an update-all-units action,
                       ;;relative to the final period.
                       (assoc-in [:state :parameters :work-state] :terminating) ;useless?
                       (assoc-in [:state :time-finish] (now)))]
    (core/trigger-event :Terminate :Engine :Engine "Simulation OVER!" nil final-ctx)))

;##Begin Day Logic
;Prior to starting a new time inteval (currently a day), we typically want to 
;perform some pre-processing or updating. 

(defn day-msg [msg day]  (str "<-------- " msg " Day " day " ---------->"))
(defn check-pause
  "In an interactive simulation, like the legacy sim, this hook lets us check 
   for user intervention each active day.  DEPRECATED."
  [ctx] 
  (if (-> ctx :state :pause)
     (core/trigger-event :pause-simulation :Engine :Engine 
                  "Simulation Paused" [(sim/get-time ctx) 
                                       (sim/get-next-time ctx)] ctx)
     ctx))

;_Note_: in _begin-day_, check-pause is incidental to the ui, not the repl. 
;The call should be yanked.
(defn begin-day
  "Update Logic for beginning a day.  Broadcasts the beginning of the current 
   day on the simulation context's event stream.  Also, if a GUI is involved, 
   notifies listeners whether a user has paused the simulation."
  [day ctx]
  (->> ctx
    (core/trigger-event :begin-day :Engine :Engine
                       (day-msg "Begin" day) [day (sim/get-next-time ctx)
                                              ])))  

;##End Day Logic
;At the end of each "day" or discrete time step, we typically mark the passage 
;of time with some processing.  This notion could be abstracted into a higher 
;order function to wrap the simulation, along with the aforementioned begin-day
;logic.  The default behavior is to trigger a sampling event, to record daily 
;samples, and to broadcast the end-of-day to interested parties.

(defn end-day
  "Logs the passing of the day, notifies listeners of a need to generate samples
   for the day, and possibly truncates the simulation early if criteria have 
   been met."
  [day ctx]
  (->> ctx 
    (core/trigger-event :log-status :Engine :Engine 
       (str "Processed day " day " of " (sim/get-final-time ctx) " of Simulation") nil)
    (core/trigger-event :sample :Engine :Engine "Sampling" nil)
    (core/trigger-event :end-of-day :Engine :Engine (day-msg "End" day) nil)    
    (core/persist-counters) ;convert atomic counters to numeric components.
    ))

(defn can-simulate? 
  "Simple predicate to ensure we have supply and demand in 
   the simulation state.  If we don't, we currently toss an 
   error."
  [ctx] 
  (let [dem  (core/get-demandstore ctx)
        supp (core/get-supplystore ctx)]
    (and 
     (supply/can-simulate? supp)
     (demand/can-simulate? dem))))

;##Primary Simulation Logic
;We enter the main engine of the Marathon simulation, which implements a 
;single-threaded, discrete event simulation by default.


;The simulation is technically a discrete event simulation, although it's 
;relatively "coarse-grained" in that, rather than having a ton of fine-grained 
;events on the queue, we only really enqueue "eventful" times.  The simulation
;step function is written as a high-level process, that is intended to be 
;invoked on every eventful day (every time on the simulation context's agenda).
;We primarily use the event system to communicate changes, and to notify 
;- potentially effectful - observers that could be logging, updating displays,
;or doing other things.  Internal simulation workings are handled in a fairly 
;bulk or batch manner by the coarse functions that comprise the step function.

;The end result is that control flow is fairly simple: we only really have 
;pending "times" to evaluate the step function against, rather than a slew of 
;smallish events to trace.  In a sense, it is very similar to the "game loop" 
;or simulation loop of time-step simulations, except for the ability to vary the
;time intervals.  Also, we retain the flexibility to push more fine-grained 
;events onto the queue if we need to.  It's the best of both worlds.  

;We want to ensure that there is an enabled supply and /or 
;demand that will actually provide some meaningful simulation output.  If not, 
;we should warn the user about missing data leading to the absence of "stuff"
;to simulate.

;#State Transition Function#
;We simulate each eventful day using a composition of simulation systems in 
;Marathon.  Each system acts in turn, computing updates to pieces of the overall
;simulation state.  Some systems communicate with eachother via events.  
;All systems have access to the entire simulation context, including the event 
;queue, for communication purposes.

;;In one sense, this defines higher-order behavior for a simulation
;;entity....
;;It handles steps by trying to achieve each of the steps in turn,
;;as with a [seq] behavior.  Another, less-structured way of looking
;;at this is that we are trying to get the system into a consistent
;;state by the end of the day.  In order to achieve consistency,
;;we can have no further pending events for the day.  So, distributing
;;the responsibilities by system is perhaps less meaningful if we're using
;;a kind of actor context.  One way we could partition these responsibilities
;;is to have each store implement its own messaging system.  For example, managing
;;the supply means allowing entities under the supply's purvue to process
;;updates for the day.
(defn sim-step
  "Primary state transition function for Marathon.  Threads the next day and
   an initial state through a series of transfer functions that address 
   high-level state transfers for supply, policy, demand, filling, and more. 
   Computes the resulting state - either the final state, or the initial state
   for the next step."
  ([day ctx]
   (->> ctx 
        (begin-day        day)  ;Trigger beginning-of-day logic and notifications.
        (manage-supply    day)  ;Update unit positions and policies.
        (manage-policies  day)  ;Apply policy changes, possibly affecting supply.
        (manage-demands   day)  ;Activate/DeActiveate demands, handle affected units.      
        (fill-demands     day)  ;Try to fill unfilled demands in priority order. 
        (end-day day)           ;End of day logic and notifications.
        ))
  ([ctx] (sim-step (sim/get-time ctx) ctx)))

(defn parametric-sim-step
  "Like marathon.ces.engine/sim-step, except callers can plug in alternate 
   functions using keyword parameters.  Useful for debugging.  Stages with 
   no associated funtion will be excluded, otherwise the step-functions 
   of type:: day->ctx->ctx,  will be called in order of 
   [begin supply policies demands fill end]."
  [ctx & {:keys [begin supply policies demands fill end] :or
               {begin begin-day
                supply manage-supply
                policies manage-policies
                demands manage-demands
                fill fill-demands
                end end-day}}]
  (let [day (sim/get-time ctx)]
    (->> (filter identity [begin
                           supply
                           policies
                           demands
                           fill
                           end])
         (reduce (fn [acc f] (f day acc))
                 ctx))))

;;in ECS parlance, the functions in sim-step are just systems.
;;Each of these systems operates on the ECS, or more specifically,
;;the components within the ECS that are of interest, and
;;then narrowly computes the new components for the system from
;;the inputs.  We have composition of system by applying them
;;in-order to the input ECS; however, it's entirely possible that
;;we could define different means of composition, namely parallel
;;composition and the like, to allow the system execution to
;;run any way we'd like.
;;Since systems are just functions, we can build them up from smaller
;;systems (functions) and layer the behavior.
;;This is right and proper and functional;

;;Thinking of systems as vertical (in the sense of operating on columns of components),
;;we can think of single-purpose systems the operate on broad-swaths of components
;;-namely on an entity-by-entity basis- like messaging and behaviors - as
;;horizontal systems.  Horizontal systems may act at sporadic times, and
;;across many components (for instance at the entity level rather than
;;at the component level);   If we have an abstract entity that is
;;able to receive messages, its behavior is dictated by some system.
;;This system will be called as a function of events;  we'd like the
;;convenience of "triggering" events now or later.  One type of apparent
;;event that gets called quite a bit is the "behavior" or update event,
;;which we currently implement as handling a message.  We can let
;;the entity handle the message in any number of ways, but this means
;;we have an ambient system that can be invoked concurrently with
;;any currently running system (or really, we can queue it for invokation).
;;Do we really need to use messaging, or can we just apply the behavior
;;system on ad-hoc basis?   I think we can go adhoc, and just allow
;;other systems to have access to the behavior system if they need it.
;;For now, we can just do everything synchronously and not worry about
;;scaling it via messaging.  Alternately, we can queue the messages/events
;;and have them handled in-between system calls (part of binding the system)
;;Before and afte running any system, we maybe handle pending messages.
;;Either that, or we deal with messages in-line.  Either way, it works out and
;;can be handled synchronously.

;#Simulation Interface
;sim.engine/event-step-marathon is the entry point for Marathon.
(defn event-step-marathon
  "Higher order simulation handling function.  Given an initial state and an 
   upper bound on simulated time, computes the resulting simulation context via
   a state transfer function, typically sim.engine/sim-step."
  [last-day ctx] 
  (let [init-ctx (initialize-sim
                  ctx ;(initialize-output ctx)
                  last-day)]
    (assert (can-simulate? init-ctx) 
            "There's nothing to simulate. Check Supply, Demand, and Relations!")
    (loop [day    0
           ctx    init-ctx]
      (if (not (keep-simulating? ctx))
          (finalize day ctx) ;base case, return the final state and clean up.
          (let [next-ctx   (sim/advance-time  ctx) ;WRONG
                next-day   (core/get-time     next-ctx)
                next-ctx   (sim-step next-day next-ctx)] ;Transition to next state.
            (recur next-day next-ctx))))))



;;testing 
(comment 
(keep-simulating? (sim/add-time  22 emptysim))
)
