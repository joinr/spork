;;Integration functions for a spork simulation context where entity
;;behavior is controlled by spork.ai behavior trees, and entities are
;;stored in a spork.entitysystem.store entity store.

;;This was originally prototyped in an actual simulation, and as
;;the implementation solidified, it's been moved to a general,
;;portable namespace for reuse.
(ns spork.ai.behaviorcontext
  (:require [spork.ai.core            :as ai]
            [spork.ai.behavior        :as b]
            [spork.entitysystem.store :as store]
            [spork.sim.simcontext     :as sim]))

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
  ai/IEntityMessaging
  (entity-messages- [e id] current-messages)
  (push-message-    [e from to msg] ;should probably guard against posing as another entity
    (let [t        (.valAt ^clojure.lang.ILookup  msg :t)
          _        (ai/debug [:add-new-messages-to new-messages])
          additional-messages (b/swap!! (or new-messages  (atom []))
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
  ai/IEntityStorage ;we could just have commit-entity- return something we can append...another idea.
  (commit-entity- [env]
    (let [ctx      (ai/deref! ctx)
          ent      (-> (ai/deref! entity) (assoc  :statedata statedata :last-update tupdate))     
          id       (:name ent)
;          _       (ai/debug  [:committing ent])
;          _       (ai/debug  [:new-messages new-messages])
          ]
      (reduce
       (fn [acc m]
         (do 
          ;(println [:pushing m :in acc])
          (sim/trigger-event m acc)))
       (store/mergee ctx (:name ent) ent)
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
   loading the appropriate information relative to the entity in
   question.  Provides a convenient way to create lexical behavior
   environments for entities behaving in response to discrete messages.
   Takes an optional default behavior - default - which if not provided
   defers to @marathon.basebehavior/default-behavior ."
  [ctx e msg default]
    (let [^clojure.lang.ILookup  e  (if (map? e) e (store/get-entity ctx e))
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
           (-> (b/beval (.behavior benv) benv)
               (b/return!)
               (ai/commit-entity-))
           )))
  ([ctx e msg] (step-entity! ctx e msg @default-behavior)))

