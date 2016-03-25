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
                     ->reduce
                     always-succeed
                     always-fail
                     bind!
                     merge!
                     push!
                     return!
                     befn]]
            [spork.entitysystem.store :as store]
            [spork.data.priorityq :as pq]
            [clojure.core.reducers :as r]
            [clojure.data.avl :as avl]))

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
(defn ->msg
  ([t msg] {:t t :msg msg})
  ([from to t msg] {:from from :to to :t t :msg msg}))
(defn insert-msg [{:keys [t] :as msg} mq]  (conj mq [msg t]))
;;This is forming a loose protocol for a messaging system....
;;messaging
;;one way to do this is to round-robin the messages.
;;since we're pushing messages to entities, we should know who has pending messages.
;;specifically, which entities need to be updated at the current interval.  That's the
;;old update logic creeping in.
(defn get-messages [e ctx] (get-in ctx [:entities e :messages]))
;;we may want to get concurrent messages for all entities and process
;;them in bulk.  Remember, this is faster than going round-robin, for
;;the persistent structure.
(defn pop-message [e ctx]
  (let [msgs (get-messages e  ctx)
        msg  (first msgs)]
    [msg (assoc-in ctx [:entities e :messages]  (pop msgs))]))
(defn msg-time [e] (first (key e)))

;;get all the messages for the entities the we currently (relative
;;to tnow) know about.  If we use a pending cache, we already have
;;the information precomputed; else we can fallback to checking each
;;entity for messages.
(defn next-recepients [ctx]
  (if-let [pending (get ctx :pending)]
    (.nth ^clojure.lang.Indexed pending 0 nil)
    (let [res (->>  (:entities ctx)
                    (reduce-kv (fn [[t ids :as acc] id {:keys [messages]}]
                                 (if-let [m (first (get-messages id ctx))]
                                   (let [tm (msg-time m)]
                                     (cond (== tm t)
                                           [t (conj ids id)]
                                           (< tm t)
                                           [tm [id]]
                                           :else acc))))                              
                               [Double/MAX_VALUE []]))]
      (when (second res) res))))

;;We may also have a very small priority queue...so the set of
;;messages may be easily handled via insertion sort rather than using
;;our large-growth queue...That would be far more efficient than
;;going the route where we're paying the cost to balance a red-black tree.


;;we send a message to the mailbox associated with 'to.  We also keep
;;track of the set of units with pending messages.  We could not
;;cache the pending information (and save the cost of maintaining a
;;sorted set on every message push), instead just traverse the entities
;;with mailboxes and check to see if they have messages at each step.
;;That's an option...or, we could be lazy about computing the pending
;;messages (and the pending next-times for the mailboxes)
;;We will refactor this to allow efficient dispatch of immediate
;;messages so that we don't have to keep queuing messages that
;;are happening at the current time.
(defn push-message [from to msg ctx]
  (let [t (:t msg)]
    (-> ctx
        (update-in  ;;bottleneckin'
         [:entities to :messages]
         conj
         [t (assoc msg :from from)])
        (update :pending ;cache the pending info; other option is to poll.
                (fn [m]  (assoc m t (conj (get m t #{})
                                           to)))))))

;; (defn push-message! [msgs pending from to msg]
;;   (let [t (:t msg)]
;;     (conj! msgs
;;            [t (assoc msg :from from)])
;;     (assoc! pending ;cache the pending info; other option is to poll.
;;             t (conj (get m t #{})
;;                                       to)))))))

;;bulk push multiple messages....hmm...
;; (defn push-messages [xs ctx]
;;   (let [t       (:t msg)
;;         pending (volatile! (transient! (:pending ctx)))]
;;     (-> ctx
;;         (update-in 
;;          [:entities to :messages]
;;          conj
;;          [t (assoc msg :from from)])
;;         (update :pending ;cache the pending info; other option is to poll.
;;                 (fn [m]  (assoc m t (conj (get m t #{})
;;                                           to)))))))

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
        ctx  {;;records of entity data, with an associated messagequeue.
              :entities (into {} (map (fn [n] [n
                                               {:name n
                                                :messages pq/minpri
                                                :t 0}]))
                              ids)
              ;;the system time.  We can compute this by pending, actually.
              :t 0
              ;sets of entity ids with pending messages, mapped to time.
              :pending  (avl/sorted-map)}]
    (reduce (fn [ctx id]              
              (push-message :system id (->msg 0 :spawn) ctx))
            ctx ids)))

;;so, system time is zero, or we can define it as the minimum of the known messages.
;;This is consistent with the agenda from sicp.

;;operations on the context.
(def simple-ctx (->simple-ctx))     

(def ^:dynamic *debug* false)

;;creates a benv (a map)
;;:: ent -> msg -> ctx -> benv
(defn load-entity!
  [ent msg ctx]
;  (println [:loading ent msg])
  {:entity (atom (get-in ctx [:entities ent]))
   :current-messages   [msg]
   :ctx    (atom ctx)})

;;behaviors

;;logs the state of the entity.
(befn notify-change [entity]
      (let [{:keys [state wait-time name]} @entity
            ]
        (->do (fn [_] (when *debug* (println (str "Entity " name
                                                  " is now in state " state
                                                  " for " wait-time " steps")))))
        ))

;;if we have a message, and the message indicates
;;a time delta, we should wait the amount of time
;;the delta indicates.  Waiting induces a change in the
;;remaining wait time, as well as a chang
(befn wait-in-state [entity msg ctx]
  (let [;_ (println [:wait-in-state entity msg])
        t     (:t msg)
        delta (- t (:t @entity))]
    (when-let [duration (:wait-time @entity)]
      (if (<= delta duration) ;time remains or is zero.
        (do ;(println [:entity-waited duration :remaining (- duration delta)])
            (->and  [(push! entity :wait-time (- duration delta))
                     (push! entity :t t)])) ;;update the time.
        (do ;can't wait out entire time in this state.
          (push! entity :wait-time 0)
          (push! entity :t (- t duration)) ;;still not up-to-date
           ;;have we handled the message?
           ;;what if time remains? this is akin to roll-over behavior.
           ;;we'll register that time is left over. We can determine what
           ;;to do in the next evaluation.  For now, we defer it.
          (bind! {:msg (assoc msg :delta (- delta duration))}
                 ))))))
                
;;choose-state only cares about the entity.
(befn choose-state  [entity]
      (let [nxt   (case (:state @entity)        
                    :dwelling  :deploying
                    :deploying :dwelling
                    :dwelling
                    )]       
        (push! entity :state nxt)))

(befn choose-time [entity]
      (let [twait  (case (:state @entity)
                 :dwelling  (+ 365 (rand-int 730))
                 :deploying (+ 230 (rand-int 40)))]
        (push! entity :wait-time twait)))

(defn up-to-date? [e ctx] (== (:t e) (:t ctx)))

(befn schedule-update  [entity ctx new-messages]
      (let [nm       (:name @entity)
            duration (:wait-time @entity)
            tnow     (:t @ctx)
            tfut     (+ tnow duration)
            ;_ (println [:entity nm :scheduled :update tfut])
            ;_ (when new-messages (println [:existing :new-messages new-messages]))
            ]        
        (bind! {:new-messages (swap! (or new-messages (atom []))
                                 conj (->msg nm nm tfut :update))})))
                      
;;pick a move and a wait time, log the move.
(befn move [entity]
      (->and [choose-state
              choose-time
              notify-change
              schedule-update]))

;;pick an initial move, log the spawn.
(befn spawn [entity]
      (when (identical? (:state @entity) :spawning)
        (->and [(->do (fn [_] (when *debug* (println "Entity " (:name @entity) " spawned"))))
                move])))

;;the entity will see if a message has been sent
;;externally, and then compare this with its current internal
;;knowledge of messages that are happening concurrently.
(befn check-messages [entity current-messages ctx]
  (do ;(println @entity)
      (when-let [msgs (pq/chunk-peek! (:messages @entity))]
        (let [new-msgs (into  (mapv val  msgs) current-messages)
              _        (swap! entity (fn [m] (update m :messages pq/chunk-pop msgs)))]
          (bind! {:current-messages new-msgs})))))

(declare default advance)
;;this is a dumb static message handler.
;;It's a simple little interpreter that
;;dispatches based on the message information.
;;Should result in something that's beval compatible.
;;we can probably override this easily enough.
(defn message-handler [msg {:keys [entity current-messages ctx] :as benv}]
  (do ;(println (str [(:name @entity) :handling msg]))
      (beval 
       (case (:msg msg)
         :update (if (== (:t @entity) (:t @ctx))
                   (do ;(println [:entity-already-updated])
                       (success benv)) ;entity is current
                   (->and [(->alter #(assoc % :msg msg))
                           advance]))  ;(->do (fn [_] (println :update-stub))) ;default
         :spawn  (do (push! entity :state :spawning)
                     spawn)
         :do     (->do (:data msg))
         (throw  (Exception. (str [:unknown-message-type (:msg msg) :in  msg]))))
       benv)))

;;handle the current batch of messages that are pending for the
;;entity.  We currently define a default behavior.
(befn handle-messages {:keys [entity current-messages ctx] :as benv}
      (when current-messages
        (reduce (fn [env msg]
                  (message-handler msg (second env)))
                (success benv)
                current-messages)))

;;we need the ability to loop here, to repeatedly
;;evaluate a behavior until a condition changes.
;;->while is nice, or ->until
;;We'd like to continue evaluating the behavior (looping)
;;until some predicate is tripped.  Another way to do
;;this is to send a message....If there's time remaining,
;;we tell ourselves to keep moving, and move again prior to
;;committing.
(befn advance [entity ctx]
      (if (not (identical? (:state @entity) :spawn))
        (->and [wait-in-state move])
        spawn))
      
;;Basic entity behavior is to respond to new external
;;stimuli, and then try to move out.
(befn default [entity]
      (->or [(->and [check-messages
                     handle-messages])             
             advance]))

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
(defn commit-entity! [{:keys [entity msg ctx new-messages] :as benv}]
  (let [;_   (println :committing @entity)       
        ctx @ctx
        ent @entity
        nm  (:name ent)
        ;_   (println :new-messages new-messages)
        ]
    (as-> (assoc-in ctx [:entities nm] ent) ctx
      (reduce (fn [acc m]
                (push-message (:from m) (:to m) m acc)) ctx
                new-messages))))

;;step function.
;;given an entity id, pop its next message.
;;results in a new context in which the messages are advanced.
                                
;;In this system, stepping consists of finding all entities with the
;;current time; then processing the entities as a batch.  Note, we
;;can collect messages - to other entities - for dispersal at the
;;end of the individual entity step to allow us to send messages
;;efficiently.
(defn get-updates [{:keys [pending] :as ctx}] (next-recepients ctx))
;;just a placeholder.
(defn advance-time [t ctx]      
  (let [res (update ctx :pending dissoc t)]
    (do (when *debug* println [:advancing-time t])
          (if  (==  (:t ctx) t) res
               (assoc res :t t)
               ))))

;;The step is conceptually simple; we just pull off the next batch
;;of entities that share the same time coordinate.  The system time
;;becomes that.  We then update the entities (currently sequentially)
;;allowing them to make progress.  The update involves popping the
;;entity's message and sending it to the entity.  Abstractly, the
;;entity has no explicit knowledge of its messages.  During a
;;transaction, it recieves a message and utilizes the subsystem
;;to request the sending of messages to other entities.
;;This is cool;  Entities are interpreters governed by their behavior.
;;They may be seen as continuations as well, or "reactive" functions of
;;time, or state machines, etc.
(defn step!
  ([t es ctx]
   (reduce (fn [acc e]
             (->> acc
                  (load-entity!  e  {:t t :msg :update})
                  (beval    default) ;should parameterize this.
                  (return!)
                  (commit-entity!)))
           (advance-time t ctx) es))
  ([ctx]
   (when-let [res (next-recepients ctx)]
     (let [[t xs] res]
       (step! t xs ctx)))))

;;There are a couple of possible outcomes in an update step.
;;Entities might send eachother messages concurrently, and
;;expect to them to arrive "now", or at the same time, i.e.
;;instantenous messages.  We could add a slight offset to
;;indicate the order of the messages happened a bit "later",
;;to help with debugging.  Or, we could just examine the queue
;;of messages the entity reacted to.  Both provide an entity-view
;;of the history of the system.
;;The goal is to enable multiple views of time from the
;;entity's perspective; specifically the shared-nothing 

;;This is a basic simulation loop...
;;Note: we can actually reify this as a reducer, or a sequential, or
;;whatever we want.  So we have options as to how we see things;
;;we can push the simulation history onto a channel, and diff it in
;;another thread.
(defn simulate! [& {:keys [n continue? tmax]
                    :or {n 2
                         tmax 4500
                         }}]
  (let [continue? (or continue? (fn [ctx] (< (:t ctx) tmax)))
        init-ctx  (->simple-ctx :n n)]
    (loop [ctx init-ctx]
      (if (not (continue? ctx)) ctx
          (if-let [res (next-recepients ctx)] ;;identify the next set of entities to run.
            (let [[t es] res]
              (do ;(Thread/sleep 200)
                  (recur  (step! t es ctx))))
            ctx)))))         
;;so...
;;can we define this in a nice, pretty, packaged result?
;;Is it a simulator?
;;A simulator needs to be able to
;;load entities from a context
;;schedule entities in a context
;;message/update entities in a context
;;commit entities in a context.
;;Know what the next time on the schedule is.

;;can we compose simulators?
;;As it stands, a simulator is composed of multiple entities.
;;Its current time is tied to the earliest time of the pending
;;entities we wish to wake.



      


  
                           


