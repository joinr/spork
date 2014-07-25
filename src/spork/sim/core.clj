;;An API for defining entity-based, event-driven simulations.
(ns spork.sim.core
  (:require [spork.sim [simcontext :as ctx]]
            [spork.entitysystem [store :as store]]))

;;The core abstraction here is a simcontext.
;;Given a simcontext, we have some state, which will be 
;;occupied by an entitystore.

;;We'll build the context by hand, then look at defining 
;;a nice API to build it for us.

;;Since simcontext already implements IEntityStore, we can 
;;use it like an entity container.

(def emptysim (assoc ctx/empty-context
                :state store/emptystore))

;;It'd be nice to be able to define systems in a high-level 
;;capacity...so that the propogation context is already 
;;set up and wired for us.  Additionally, it'd be 
;;nice to have singleton "entities" registered to the 
;;systems, so that they can recieve messages and stuff, if 
;;we want to use message passing.

;;For example, a legacy marathon sim could be specified
;;by this: 

(make-sim 
    empty-sim    
    {:SystemUpdate 
     [:begin-day 
      :manage-supply
      :manage-policies  ;Apply policy changes, possibly affecting supply.
      :manage-demands  ;Activate/DeActiveate demands, handle affected units.
      :fill-demands  ;Try to fill unfilled demands in priority order.
      :manage-follow-ons  ;Resets unused units from follow-on status.
      :end-day ;End of day logic and notifications.
      :manage-changed-demands]
     (fn []
       )})
(make-sim empty-sim         
          (fn [event 

 
   

