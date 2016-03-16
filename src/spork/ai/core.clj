;;A set of utilities and protocols shared between AI behavior implementations.
;;Note: this is infrastructure designed to support the implementation of entity
;;behavior in general simulations (i.e. games, analytic simulations, etc.).
;;Thus, we are not referring to classical AI realms such as search and (necessarily
;;planning), although the facilities provided here may be used as such.  The goal is
;;to provide a common interface through which multiple AI implementations may work and
;;cooperate.   
(ns spork.ai.core)

;;General functions for applying behaviors to a context.
;;Using transactional semantics, we liken the processing of
;;entity behaviors via a behaviorsystem akin to a database
;;transaction.  We prepare a behavior environment by loading
;;the entity, typically creating local state for the evaluation
;;of the behavior.  From there, we define how to apply a behavior
;;to a behavior environment (typically loaded from the entity).
;;Behaviors return new or possibly final environments.
;;After the last environment is computed, we in turn commit the
;;entity via the environment, assuming the environment contains
;;all necessary changes to either compute or extract the resulting
;;context.    We may not need this...in fact, we could
;;just define a map to hold the requisite functions.
(defprotocol IBehaviorSystem
  (load-entity!   [sys ent ctx] ":: ctx -> entity -> behaviorenv")
  (behave-entity! [sys b benv]  ":: behavior -> benv -> benv")
  (commit-entity! [sys benv]    ":: benv -> ctx"))

;;Not sure we actually need this.  We might be better off letting contexts define their
;;own blackboard and just let the trees/fsms work on the context.
(comment
;;Coordination
;;============
;;The primary communication/referential mechanism is a blackboard
;;abstraction.  The blackboard serves as a conceptual backplane for the propogation of
;;facts to support reasoning.
(defprotocol IBlackBoard
  (-get-bb [ctx k else])
  (-set-bb [ctx k])
  (-merge-bb [ctx m]))

;;Not really a fan of using extend-protocol, since we end up
;;getting implementation lookup costs.  Maybe we just define
;;as-bb, and define ID for types that implement blackboard semantics
;;precisely.  The other option is to detect map at runtime and dispatch
;;based on that.  We're also duplicating functionality based on maps.
;;We can just use get/assoc, and force the type to handle the semantics
;;(i.e. mutation vs. other stuff). or....
;;we can define a specific blackboard type.
(extend-protocol IBlackBoard
  clojure.lang.PersistentArrayMap
  (-get-bb [ctx k]     (get   ctx k))
  (-set-bb [ctx k v]   (assoc ctx k))
  (-merge-bb [ctx m]   (merge ctx m))
  clojure.lang.PersistentHashMap
  (-get-bb [ctx k] (get ctx k))
  (-set-bb [ctx k]            )
  (-merge-bb [ctx m]          )
  )

;;Accessors
;;=========

;;Accessors for our behavior context.
;;We don't "have" to do this, but I wanted to lift up
;;from raw lookups and formalize the interface.  we seem to be using
;;these alot, enough to warrant a new idiom possibly.

;;Get an item from the blackboard, which is stored in the simstate of 
;;the context, under :state 
(defn get-bb   
  ([ctx k]      (get (core/get-blackboard ctx) k))
  ([ctx k else] (if-let [res (get (core/get-blackboard ctx) k)]
                  res 
                  else)))
(defn set-bb   [ctx k v]  (core/set-blackboard ctx (assoc (core/get-blackboard ctx) k v)))
(defn merge-bb [ctx m]    (core/set-blackboard ctx (merge (core/get-blackboard ctx) m)))

(defmacro with-bb [[opts ctx] & expr]
  (let [lhs (cond (map? opts) (if (contains? opts :as) opts (assoc opts :as 'bb))
                  (vector? opts) {:as 'bb :keys opts} 
                  :else (throw (Exception. (str "Options need to be a vector or a map... " opts))))]
    `(let [~lhs (core/get-blackboard ~ctx)]
       ~@expr)))
  
)
