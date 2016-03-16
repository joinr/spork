(ns spork.ai.testing
  (:require [spork.ai.behavior :as behavior
             :refer [beval
                     success?
                     success
                     run
                     fail
                     behave
                     ->seq
                     ->elapse
                     ->not
                     ->do
                     ->alter
                     ->elapse-until
                     ->leaf
                     ->wait-until
                     ->if
                     ->and
                     ->pred
                     ->or
                     ->bnode
                     ->while
                     always-succeed
                     always-fail
                     bind!
                     merge!
                     push!
                     befn]]
            [spork.entitysystem.store :as store]
            [spork.data.priorityq :as pq]
            [clojure.core.reducers :as r]))



;;this only works with path literals...
;;allows us to define paths at compile time.
(defmacro path! [lits]
  `~(keyword (clojure.string/join "/" (map name lits))))

;;we establish our behavior context...which allows us to
;;use monadic operations...


;;we "Should" be able to perform a very rudimentary simulation of
;;two entities oscillating between states and waiting for random
;;intervals of time.

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

;;A message is just a packet of information.
;;We can disperse these packets.
(defn ->msg [t msg] {:t t :msg msg})

(defn insert-msg [{:keys [t] :as msg} mq]  (conj mq [msg t]))
;;messaging
;;one way to do this is to round-robin the messages.
;;since we're pushing messages to entities, we should know who has pending messages.
;;specifically, which entities need to be updated at the current interval.  That's the
;;old update logic creeping in.
(defn pop-message [e ctx]
  (let [msgs (get-in ctx [:messages e])
        msg  (first msgs)]
    [msg (assoc-in ctx [:messages e (pop msgs)])]))

(defn push-message [from to msg ctx]
  (let [t (:t msg)]
    (-> ctx
        (update-in 
         [:entities to :messages]
         conj
         [(assoc msg :from from) t])
        (update :pending conj (clojure.lang.MapEntry. t to)))))
                             
;;this is a really simple context for prosecuting a discrete event
;;simulation.  We have a global time, a map of entities, and a map of
;;messages for said entities.  From this, we can define a step function,
;;which is composed of entity behaviors, and proceed to update the simulation
;;context (or compute a new context).  The simulation proceeds by
;;delivering messages.  Messages are kept in sorted order, by time (not unlike
;;a priority queue).  Entities send messages for delivery to other entities or
;;even themselves.  The simulation proceeds by delivering messages. Note:
;;we can open this up to concurrency constructs later.  For now, it's a nice
;;simple framework for building a toy simulation.

;;This abstraction is pretty minimalist.  We maintain a set of entity
;;state, each with messages.  We're basically simulating a simple actor
;;system.  The pending clock tells us which entities want to be checked,
;;providing us with an ordered list of entities to update.  I'm hoping this
;;provides us with a simple means for locality of reasoning.  Specifically,
;;the entities have an associated set of messages they want to process.
;;The cool thing here is, we don't have a global event queue.  We maintain
;;a global set of pending events for entities, but we can process them in parallel
;;if we want to.
(defn ->simple-ctx [& {:keys [n] :or {n 2}}]
  (let [ids (range n)
        ctx  {:entities (into {} (map (fn [n] [n
                                               {:name n
                                                :messages pq/minq}]))
                              ids)
              :t 0
              :pending  (sorted-set)}]
    (reduce (fn [ctx id]              
              (push-message :system id (->msg 0 :spawn) ctx))
            ctx ids)))

;;so, system time is zero, or we can define it as the minimum of the known messages.
;;This is consistent with the agenda from sicp.

;;operations on the context.
(def simple-ctx (->simple-ctx))

;;creates a benv (a map)
;;:: ent -> msg -> ctx -> benv
(defn load-entity! [ent msg ctx]
  {:entity (atom (get-in ctx [:entities ent]))
   :msg     msg
   :ctx    (atom ctx)})
;;behaviors

;;logs the state of the entity.
(befn notify-change [entity]
      (let [{:keys [state t name]} @entity
            ]
        (->do (fn [_] (println (str "Entity " name
                            " is now in state " state
                            " for " t " steps"))))
        ))

;;if we have a message, and the message indicates
;;a time delta, we should wait the amount of time
;;the delta indicates
(befn wait-in-state [entity msg ctx]
   (when-let [delta (:delta msg)]
     (when-let [t (:wait-time @entity)]
       (if (<= t delta) ;time remains or is zero.
         (push! entity :t (- t delta)) ;update the time.
         (->and
          (push! entity :t 0)
          ;;have we handled the message?
          ;;what if time remains? this is akin to roll-over behavior.
          ;;we'll register that time is left over. We can determine what
          ;;to do in the next evaluation.  For now, we defer it.
          (bind! {:msg (assoc msg :delta (- delta t))}
                 ))))))
                
;;choose-state only cares about the entity.
(befn choose-state  [entity]
      (let [nxt   (case (:state @entity)        
                    :dwelling  :deploying
                    :deploying :dwelling)]       
        (push! entity :state nxt)))

(befn choose-time [entity]
      (let [t  (case (:state @entity)
                 :dwelling  (+ 365 (rand-int 730))
                 :deploying (+ 230 (rand-int 40)))]
        (push! entity :t t)))

;;pick a move and a wait time, log the move.
(befn move [entity]
      (->and [choose-state
              choose-time
              notify-change]))

;;pick an initial move, log the spawn.
(befn spawn [entity]
      (when (identical? (:state @entity) :spawning)
        (->and [(fn [_] (println "Entity " (:name @entity) " spawned"))
                move])))

;;the basic idea is this.
(befn default [entity]
      (->or [spawn
             wait-in-state
             move]))

;;as it stands, we may have overstep our current state.
;;so, we'd like to wire this into the commit behavior somehow.
;;this is equivalent to our roll-forward behavior, where we
;;continue bevaling, until we get a behavior that intersects
;;with the current time.  Our typical medium for governing
;;this process is to use the messages, and to return the
;;time remaining as a part of the context.  If the step
;;was incomplete, we can evaluate another step from the
;;outside, using the leftover message from the context.
         
;;committing
(defn commit-entity! [{:keys [entity msg ctx] :as benv}]
  (let [ctx @ctx
        ent @entity
        nm  (:name ent)]
    (assoc-in ctx [:entities nm] ent)))

;;step function.
;;given an entity id, pop its next message.
;;results in a new context in which the messages are advanced.
                                
;;In this system, stepping consists of finding all entities with the
;;current time; then processing the entities as a batch.  Note, we
;;can collect messages - to other entities - for dispersal at the
;;end of the individual entity step to allow us to send messages
;;efficiently.

(defn get-updates [{:keys [pending] :as ctx}]
  (let [t0  (key     (first pending))]
    (->>  pending
          (r/take-while (fn [[t _]] (== t t0)))
          (r/map val))))

;;The step is conceptually simple; we just pull off the next batch
;;of entities that share the same time coordinate.  The system time
;;becomes that.  We then update the entities (currently sequentially)
;;allowing them to make progress.  The update involves popping the
;;entity's message and sending it to the entity.  Abstractly, the
;;entity has no explicit knowledge of its messages.  During a
;;transaction, it recieves a message and utilizes the subsystem
;;to request the sending of messages to other entities.
(defn step! [e init-ctx]
  (reduce-kv (fn [ctx e _]
               (->> ctx
                    (load-entity!   e)
                    (beval    default) ;should parameterize this.
                    (commit-entity!)))
             ctx (get-updates init-ctx)))

;;There are a couple of possible outcomes in an update step.
;;Entities might send eachother messages concurrently, and
;;expect to them to arrive "now", or at the same time, i.e.
;;instantenous messages.  We could add a slight offset to
;;indicate the order of the messages happened a bit "later",
;;to help with debugging.  Or, we could just examine the queue
;;of messages the entity reacted to.  Both provide an entity-view
;;of the history of the system.

;(defn simulate! [& {:keys [n] :or {n 100}}]
  

      


  
                           


