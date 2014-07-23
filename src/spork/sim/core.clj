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




                       
