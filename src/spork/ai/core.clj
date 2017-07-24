;;A set of utilities and protocols shared between AI behavior implementations.
;;Note: this is infrastructure designed to support the implementation of entity
;;behavior in general simulations (i.e. games, analytic simulations, etc.).
;;Thus, we are not referring to classical AI realms such as search and (necessarily
;;planning), although the facilities provided here may be used as such.  The goal is
;;to provide a common interface through which multiple AI implementations may work and
;;cooperate.   
(ns spork.ai.core
  (:require #_[spork.ai.behavior :as b :refer [return! swap!! beval befn success]]
            [spork.ai.behavior :refer :all]
            [spork.entitysystem.store :refer :all]
            [spork.sim.simcontext :as sim]))

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

;;Implementation of an evaluation context for entity behaviors.
;;Specifically, we capture the current entity behing "updated" via its
;;behavior, the target behavior (as in spork.ai.behavior or
;;compatible), some message state, and other relevant context -
;;currently the spork.sim.simcontext simulation context/entitystore,
;;the current message being processed - if any - the time of update,
;;elapsed time delta, and any state data associated with the entity.
;;Some of these are captured as references via atoms, and are amenable
;;to side-effecting updates during evaluation.

;;Since it acts as a map (via defrecord) the behavior environment
;;allows us to add or update additional "lexical vars" for use in the
;;evaluation of behavior functions by associng things onto the
;;environment.  This works nicely with the behavior function (befn)
;;idiom, which when coupled with map destructuring, allows us to
;;define which "bindings" from the behavior environment the behavior
;;function cares about, and to trivially modify the environment for
;;other "downstream" behavior evaluation.  The end result is that the
;;behavior environment acts as a state accumulator, and as a
;;blackboard for communicating information across behaviors - all in a
;;data-driven and idiomatic manner with "optional" side-effects.
(defrecord behaviorenv [entity behavior current-messages new-messages ctx current-message
                        tupdate deltat statedata]
  IEntityMessaging
  (entity-messages- [e id] current-messages)
  (push-message-    [e from to msg] ;should probably guard against posing as another entity
    (let [t        (.valAt ^clojure.lang.ILookup  msg :t)
          _        (ai/debug [:add-new-messages-to new-messages])
          additional-messages (spork.ai.behavior/swap!! (or new-messages  (atom []))
                                 (fn [^clojure.lang.IPersistentCollection xs]
                                   (.cons  xs
                                      (.assoc ^clojure.lang.Associative msg :from from))))]                            
      (behaviorenv. entity
                    behavior
                    current-messages
                    additional-messages
                    ctx
                    current-message
                    tupdate
                    deltat
                    statedata
                    )))
  IEntityStorage ;we could just have commit-entity- return something we can append...another idea.
  (commit-entity- [env]
    (let [ctx      (deref! ctx)
          ent      (-> (deref! entity) (assoc  :statedata statedata :last-update tupdate))
         ; existing-messages (atom (:messages ent))          
          id       (:name ent)
;          _       (debug  [:committing ent])
;          _       (debug  [:new-messages new-messages])
          ]
      (reduce
       (fn [acc m]
         (do 
          ;(println [:pushing m :in acc])
          (sim/trigger-event m acc)))
       (mergee ctx (:name ent) ent)
       new-messages))))

;;WARNING: Could be a memory leak here!

;;debugging info for the behavior environment
;;constructor.
;;Note: this is a source of garbage / memory leaks.
;(def args  (atom  nil))

;;global var...
;;The default global entity behavior.
(def default-behavior (atom nil))
;;NOTE: currently not used, DEPRECATE?
(def behaviors (atom nil))

;;note: if we change over to a set of coroutines running the ECS, we
;;can just put the message on their channel and let the coro handle
;;updates.

;;We have a way to send messages now....dispatch is handled through
;;send-message, which uses step-entity! to handle the message.


;;Note: we could go ahead and extend-protocol to simcontext.

(defn ->benv
  "Default constructor for preparing behavior environment(s) and 
   loading the appropriate information relative to the entity 
   in question.  Provides a convenient way to create lexical 
   behavior environments for entities behaving in response to 
   discrete messages.  Takes an optional default behavior - default -
   which if not provided defers to
   @marathon.basebehavior/default-behavior ."
  [ctx e msg default]
    (let [^clojure.lang.ILookup  e  (if (map? e) e (get-entity ctx e))
         ent    (atom e)
         beh   (.valAt e :behavior default)
         beh   (cond (identical? beh :default)
                       (do  (swap! ent assoc :behavior default)
                            default)
                       (nil? beh) (throw (Exception. "No behavior defined..."))
                       :else  beh)
         tupdate (:t msg)
        ;;load a behavior context for the entity to behave in.
        ;;Note: if we're using stateful object-like entities,
        ;;they'll maintain a stateful behavior environment.
        benv (behaviorenv.  ent
             beh ;right now there's only one behavior :default
             [msg]
             nil
             (atom ctx)
             msg     
             tupdate ;current time.
             (if-let [tprev (:last-update e)] ;deltat
               (- tupdate tprev)
               0)
             (:statedata e))        
          ;_ (reset! args benv)
          ]
      benv))

;;Marker used to determine if an entity is being actively observed
;;for debugging.
(def ^:dynamic *observed* nil)

;;immediate steps happen with no time-delta.
;;like ai/step-entity!, we should find a way to reuse it.

(defn step-entity!
  "High-level API for functionally stepping an entity via an optional
   behavior - default - , under the context of receiving a 'message' in
   a simulation context. If the behavior is not provided the
   default-behavior reference is used.  Encapsulates the process of
   creating the behavior environment, computing the reduction using
   spork.ai.behavior/beval in the context of the behavior environment,
   commits the entity into the reduced simcontext, and returns the
   simcontext."
  ([ctx e msg default]
   ;;TODO Check this for performance hits...
   (binding [spork.ai.core/*debug* (or spork.ai.core/*debug*  (= (:name e) *observed*))]
     (let [^behaviorenv benv (->benv ctx e msg default)
           _    (ai/debug  [:<<<<<< :STEPPING (:name e) :last-update (:last-update e)  msg :>>>>>>])]
           (-> (beval (.behavior benv) benv)
               (return!)
               (ai/commit-entity-))
           )))
  ([ctx e msg] (step-entity! ctx e msg @default-behavior)))
