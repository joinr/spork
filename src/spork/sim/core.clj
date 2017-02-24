;;An API for defining entity-based, event-driven simulations.
;;TODO: Pull in all of the disparate namespace vars into a
;;convenient central location.  Currently, spork.sim.simcontext
;;serves that purpose, providing a decent API for general purpose
;;simulation stuff.  However, there are cases where we pull in
;;from spork.sim.agenda and other areas.  Currently a placeholder
;;until we reorganize the API.
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

;;An evented system takes a context, and event dispatch, 
;;and a SystemUpdate function.

;;Note:
;;THe current "API" is in spork.sim.simcontext, more or less.
;;This would just be importing vars from there, honestly.

