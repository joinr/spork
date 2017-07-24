;;A set of utilities and protocols shared between AI behavior implementations.
;;Note: this is infrastructure designed to support the implementation of entity
;;behavior in general simulations (i.e. games, analytic simulations, etc.).
;;Thus, we are not referring to classical AI realms such as search and (necessarily
;;planning), although the facilities provided here may be used as such.  The goal is
;;to provide a common interface through which multiple AI implementations may work and
;;cooperate.   
(ns spork.ai.core
  (:require #_[spork.ai.behavior :as b :refer [return! swap!! beval befn success]]
           ))

;;Disclaimer
;;==========
;;The spork.ai.core namespace is currently the remnant of an rapidly
;;prototyped experiment into behavior trees.  Some of the functions -
;;and protocols - are generally useful and are retained for use in
;;behavior tree applications.  There is a simple message-driven
;;event-step simulation implementation that should ideally be moved to
;;a separate example namespace.  For now, please accept that this
;;"core" namespace is not necessarily a porcelain api, or a hub of
;;functions and vars.  For specific information on behavior trees, see
;;spork.ai.behavior.  Core will be refactored in the possibly near
;;future...

;;probably turn this into a record type...
(defmacro ->msg
  ([t msg]  `{:t ~t :from (:from ~msg) :to (:to ~msg) :msg ~msg})
  ([from to t msg] `{:t ~t :from ~from :to ~to :msg ~msg}))

;;"faster" versions of get and assoc that avoid hitting clojure.lang.RT, doing direct
;;method dispatch instead.
(definline fget [m k]
  (let [coll (with-meta (gensym "coll") {:tag 'clojure.lang.ILookup})]
    `(let [~coll ~m]
       (.valAt ~coll ~k))))

(defmacro fassoc [obj k v]
  (let [o (with-meta (gensym "obj")  {:tag 'clojure.lang.Associative})]
    `(let [~o ~obj]
       (.assoc ~o ~k ~v))))


;works on mutables too...
(defmacro deref! [atm]
  `(if (instance? clojure.lang.IDeref ~atm)
     (.deref ~(with-meta atm {:tag 'clojure.lang.Atom}))
     ~atm))

(def ^:dynamic *debug* false)

;;Performance note:  This kills us on hotspots...
(defmacro debug
  ([lvl msg]
   `(when (and ~'spork.ai.core/*debug*
               (>= ~'spork.ai.core/*debug* ~lvl))
      (when-let [res#  ~msg] (println res#))))
  ([msg] `(when ~'spork.ai.core/*debug*
            (when-let [res# ~msg]
              (println res#)))))
;;we establish our behavior context...which allows us to use monadic
;;operations...


;;we "Should" be able to perform a very rudimentary simulation of two
;;entities oscillating between states and waiting for random intervals
;;of time.

;;the goal is to refine this setup so that it's trivial to describe
;;behaviors and actions; specifically to reduce the cognitive overhead
;;on the part of the individual describing the simulation.

;;load-entity!    :: ent  -> ctx -> benv
;;;;note, we can have variations on load-entity.
;;;;this is desirable since we can do things like push a message to
;;;;the entity on load.
;;behave-entity!  :: benv -> benv
;;commit-entity!  :: benv -> ctx 
;;bind!           :: map  -> benv -> benv 


;;General functions for applying behaviors to a context.  Using
;;transactional semantics, we liken the processing of entity behaviors
;;via a behaviorsystem akin to a database transaction.  We prepare a
;;behavior environment by loading the entity, typically creating local
;;state for the evaluation of the behavior.  From there, we define how
;;to apply a behavior to a behavior environment (typically loaded from
;;the entity).  Behaviors return new or possibly final environments.
;;After the last environment is computed, we in turn commit the entity
;;via the environment, assuming the environment contains all necessary
;;changes to either compute or extract the resulting context.  We may
;;not need this...in fact, we could just define a map to hold the
;;requisite functions.
;; (defprotocol IBehaviorSystem
;;   (load-entity!   [sys ent ctx] ":: ctx -> entity -> behaviorenv")
;;   (behave-entity! [sys b benv]  ":: behavior -> benv -> benv")
;;   (commit-entity! [sys benv]    ":: benv -> ctx"))

;Ideally...we simulate this stuff persistently, and allow a mutable
;;version to be swapped out as needed (for efficiency sake...)

;;looking for a loose protocol that makes this underlying functionality simpler.
;;Is load-entity redundant?

(defprotocol IEntityStorage
  (commit-entity- [s] "benv->ctx"))

(defprotocol IBehaviorProvider
   (load-entity-   [s ent msg] "ctx->benv"))

;;This is loosely analogous to simcontext...
(defprotocol IEnvironmentProvider  
  (get-entities-  [obj])
  (set-entities-  [obj xs])
  (set-entity-    [obj id ent])
  (get-pending    [obj])
  (get-t          [obj]))

;;The implication of handling communication between parties is a higher
;;layer of abstraction though; more than primitive events; although it
;;gets reifed as 1 or more primitive events (or immediate actions).  Ideally,
;;we defer all of our operations to this though..

;;Immediate goal is to abstract this stuff behind a concrete protocol.
;;You know, we could override the protocols for defrecord...
 
;;If we view the events as messages, then we get this as a default.
(defprotocol IEntityScheduler
  (next-recepients- [e])
  (advance-time-    [e t]))

;;How we accomplish pushing a message is up to us.
(defprotocol IEntityMessaging
  (entity-messages- [e id])
  (push-message-    [e from to m])
  (handle-message-  [e msg]))

