(ns spork.sim.discrete
  (:use [sim.events])
  (:require [sim [schedule :as agenda]]))

;a simulation is a just an event source/repository (the schedule) and 
;event processors. 

;In the vba incarnation of marathon, we created a specific case of the event 
;framework, in that we have a single-threaded, shared event repository (called 
;the event stream), which implements the observer design pattern.  During setup
;subscribers add themselves to events they care about, which means that the 
;centralized eventq will inform them of the passage of an event. 

;Sub processes trigger or raise events during execution, rather than queuing 
;them up explicitly for later execution.  Part of this was to take advantage of 
;ostensbily object-oriented coding facilities...it was hard if not impossible to
;provide dynamic processes in VBA.  As a result, I relegated events to 
;effectively triggering side-effects in a single-threaded context.  

;Much of the operational logic was single-threaded, serialized processing. 
;The "eventful" piece happened to be the daily update functions.  Clients would
;be allowed to request updates at specific times, upon which the manager of time 
;would add unique "eventful days" for updates.  On these days, a serialized 
;process logic would flow over the entire ARFORGEN system...processing only 
;things affected by the update on the day.  So, the net result was a system of 
;supply, demand, policy, executed in a serial fashion upon eventful days.

;Downsides: this required extra book-keeping in the form of an update-manager.
;Despite the illusion of coarse-grained, serialized access to state mangement, 
;much of the event traffic (particularly updates) gets hidden, or has to be 
;explicitly managed....which means more unique event types added to the event 
;vocabulary. 

;There is an arguement for fine-grained event processing....if one can package 
;functions as events, then it becomes quite easy to apply said functions to the 
;active simulation state at a given time.  For logging purposes, we get as much 
;fine-grained event-logging as we want.  We also get extremely fine-grained 
;control (or coarse-grained.....depending on the fn we package) over the 
;changes in simulation state. 

;I want to define a simulation as any process that evolves state as a function 
;of explicit events.  It essentially threads a core notion of state...the 
;simulation state, through a sequence of explicit processes (state transfer 
;functions) that evaluate the next state in the process, culminating in some 
;termination, at which point the final state is returned. 

;The simulation defines an ordering of these processes, and threads state 
;through them.  

;Note -> component functions or processes are packaged as "events" which are 
;sequenced according to some absolute notion of order (generally an absolute 
;notion of time).  

;The simulation is simply a generating function that unfolds a sequence of 
;of state.  Events are evaluated by applying their encapsulated fn to the 
;existing state, and combining the result with the existing state to generate 
;the next state.

;Several things may happen as the result of an event process.  One : state can 
;change.  In fact, the only time state can change is when an event happens, so 
;we have an explicit, fine-grained notion of the simulation history built-in.
;Another possibility is that future changes are required to complete the 
;processing.  These take the form of pending events.  Pending events are 
;typically conjoined to the underlying event sequencing mechanism.  

;Typical execution of the simulation continues until no events remain, or until 
;a predicate condition is reached. 

;Note, if we define a protocol for the sim, we can either do immutable or 
;mutable simulations....just implement the protocol appropriately. 
(defprotocol ISimulation 
  (get-state [x])
  (next-state [x event])
  (get-time [x])
  (next-time [x])
  (add-event [x event])
  (get-events [x])
  (next-event [x]) 
  (keep-simulating? [x]))

(defrecord sim [events state transition])  

;transition function's responsibility is to tell us how to apply events to 
;state to generate next state. 
(defn make-sim 
  ([state ftransition] (make-sim agenda/initial-schedule state ftransition))
  ([events state ftransition] (sim. events state ftransition)))

;default ISimulation implementation using sim. 
;(def default 
;  {:get-state (fn [x] (:state x))
;   :next-state (fn [x event] ((:packet :data event) (:state x)))
;   :get-time (fn [x] (get-time :events x))
;   :next-time (fn [x] (next-time (:events x)))
;   :add-event (fn [x event] (sc 
;   :get-events (fn [x] (:events x))
;   :next-event (fn [x] (next-event (:events x)))
;   :keep-simulating? (fn [x] (empty-schedule? (:events x)))}
;  
; (extend ISimulation sim  
;   default)


;These are universal events that communicate "big" things....like changes
;
(defevents [(initialized "Initialized an eventful process")
            (state-changed [f] "Applies f to the state of an eventcontext.")
            (events-changed [f] "Applies f to the events of an eventcontext.")
            (terminated "Terminated an eventful process.")])

;These are basic events that will happen in any system that manages an
;entity store / state.  0
(defevents [(added-entities "One or more entities added to the state")           
            (dropped-entities "One or more entities dropped from the state")           
            (added-record    
             "Added component data to the state for entity id"
             [{:keys [id component data] :as record}] record)          
            (dropped-record  
             "Dropped component record for entity id"
             [id component] {:id id :component component})           
            (got-text             
             "Receieved text from IO"
              [type data time] {:type type :data data :time time})])           
           
(defevents [(test-noargs)])

