(ns spork.ai.demo
  (:require [spork.ai.core :refer :all]
            [spork.ai.behavior :as b
             :refer [return! swap!! beval befn success]]
            [spork.data.priorityq :as pq]
            [clojure.data.avl :as avl]))

;;in the parlance of events,
;;updating entities / stepping entities is an event.
;;If we have all entity messages living in a
;;central queue, we'd merely pop the events in order.
;;Since we're not currently dealing with a monolithic queue (which is
;;an implementation detail...), we view events as messages pending
;;delivery....at least one facet of the event system is this
;;message disperal system, which works very well for generalized DEVS
;;modeling.  We haven't defined how to define messaging networks -
;;right not entities must notify other entities directly (they have
;;access to the context, so this is possible), or generally
;;broadcast information on the global event channel.

;;Do we need to have a separation between messaging and events ?  Can
;;we unify events under messaging or vice versa?   Which is simpler...
;;Ordered message delivery and processing is a useful abstraction for
;;event processing (actually, in the case of one named entity, we
;;only have a single set of events, hence the old fashioned monolithic
;;event queue....)

;;We now have cooperative entities communicating via the message queue.
;;Messages are handled via behaviors (more generally they are typically
;;handled via behaviors; we could eschew this interface in favor of a
;;more universal handling function...

;;the evaluation context for the entity's behavior.  we'll probably
;;want to mutate this, but for now see what kind of benefits we can
;;get just using records and field access.

;;we need to know how to push messages...this is our interface
;;to the entity's evaluation model.  Right now, this is
;;being accomplished via send, and binding.
(defrecord behaviorenv [entity behavior current-messages new-messages ctx current-message]
  IEntityMessaging
  (entity-messages- [e id] current-messages)
  (push-message-    [e from to msg] ;should probably guard against posing as another entity.
    (let [t        (.valAt ^clojure.lang.ILookup  msg :t)
          _        (debug [:add-new-messages-to new-messages])
          additional-messages (swap!! (or new-messages  (atom []))
                                        (fn [^clojure.lang.IPersistentCollection xs]
                                          (.cons  xs
                                             (.assoc ^clojure.lang.Associative msg :from from))))]
      (behaviorenv. entity
                    behavior
                    current-messages
                    additional-messages
                    ctx
                    current-message)))
  IEntityStorage
  (commit-entity- [env]
    (let [ctx      (deref! ctx)
          ent      (deref! entity)
         ; existing-messages (atom (:messages ent))
          entities (get-entities- ctx)
          id  (:name ent)
          _   (debug  [:committing ent])
          _   (debug  [:new-messages new-messages])
          ]
      (reduce
       (fn [acc m]
         (do 
          ;(println [:pushing m :in acc])
          (push-message- acc (:from m) (:to m) m )))
       (set-entity- ctx   id ent)
       new-messages))))

;;batch message delivery.
;;We can be much more efficient in our messaging service if, as we
;;process messages in a step, we avoid paying the cost of sending
;;individual messages all the time.  Currently, the costs are:
;;a) looking up the (destination) entity's messages.
;;Rather than performing a lookup on every message (of which there may be
;;millions, we can chunk the messages out by time.  So, if we collect
;;a batch of messages from a step, after the step is completed (i.e.
;;all entities have pushed out messages), we can commit the messages
;;collectively.
;;Even better, when we send messages from the entity's current time,
;;if the message is "to itself" at the same time, we can treat this
;;message being handled instantaneously instead of queuing it (going
;;through the rigamarole of conjing, dissocing, and all the machinery
;;therein.  So, having the concept of instantaneous messages, and further
;;self-messages, should help performance significantly (no need to
;;dispatch).

(comment 
  (defrecord message-batch [^java.util.ArrayList  messages ^long t ])
  (defn push-messages      [xs {:keys [entities pending t]}])
)

;;This is an example of a standalone entity-context.
;;We can extend these protocols to the entitystore to create a more sophisticated
;;context.  In fact, we should...

;;From an entitystore perspective.

;;note:
;;Interestingly, small structures are quick to update using the object constructor...
;;implementing a record type because maps aren't efficiently specialized.
;;So, we have a context that supports passing messages between entities.
(defrecord entityctx [^clojure.lang.IPersistentMap entities pending t]
  IBehaviorProvider
  (load-entity-   [ctx ent msg]
    (let [e         (fget  entities ent)]
      (behaviorenv. (atom e)
                    (.valAt ^clojure.lang.ILookup e :behavior) 
                    [msg]
                    nil
                    (atom ctx)
                    msg)))
  IEnvironmentProvider
  (get-entities- [obj]     entities)
  (set-entities- [obj xs] (entityctx. xs pending t))
  (set-entity- [ctx id ent]
    (entityctx. (.assoc entities id ent) pending t))
  (get-pending [obj] pending)
  (get-t [obj] t)
  IEntityScheduler
  (next-recepients- [ctx] (.nth ^clojure.lang.Indexed pending 0 nil))
  (advance-time-    [ctx tnext]
    (let [remaining (dissoc pending tnext)]
      (if  (==   t tnext)
        (entityctx. entities remaining t)
        (do (debug [:advancing-time t])               
            (entityctx. entities
                        remaining                       
                        tnext)))))
  IEntityMessaging
  (entity-messages- [ctx id]
    (.valAt ^clojure.lang.IPersistentMap
            (.valAt  entities id) :messages))
  (push-message-    [ctx from to msg]
      (let [tfut     (.valAt ^clojure.lang.ILookup msg :t)
            e        (.valAt ^clojure.lang.ILookup entities to)
            msgs     (.valAt ^clojure.lang.ILookup e    :messages)
            new-msgs (.cons ^clojure.lang.IPersistentCollection msgs
                            [tfut (.assoc ^clojure.lang.Associative msg :from from)])           
            eset     (.valAt ^clojure.lang.ILookup  pending tfut #{})]                    
        (entityctx.                  
         (.assoc ^clojure.lang.Associative entities to
                 (.assoc ^clojure.lang.Associative e :messages new-msgs))
         (.assoc ^clojure.lang.Associative pending tfut
                 (.cons
                  ^clojure.lang.IPersistentCollection
                  eset
                  to))
         t))))
                       

;;__API For Message-Step Simulations__
;;step function.
;;given an entity id, pop its next message.
;;results in a new context in which the messages are advanced.
                                
;;In this system, stepping consists of finding all entities with the
;;current time; then processing the entities as a batch.  Note, we
;;can collect messages - to other entities - for dispersal at the
;;end of the individual entity step to allow us to send messages
;;efficiently.

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
(defn step-entity!
  ([ctx e t msg behavior]
   (-> (beval behavior (load-entity- ctx e  msg)) ;optionally bypass the entity's behavior.
       (return!)
       (commit-entity-)))
  ([ctx e t msg]
   (let [^behaviorenv benv (load-entity- ctx e  msg)
         ;_ (println benv)
         ]
     (-> (beval (.behavior benv) benv) ;should parameterize this.
         (return!)
         (commit-entity-))))
  ([ctx e t]
   (step-entity! ctx e t (->msg t :update))))

;;Events and messages are duals...
;;Message = end-to-end comms between two points
;;If the type of the message is :message, then
;;the intent is to deliver to an entity's mailbox
;;and let the entity handle it, or
;;depending on invocation, handle the event synchronously
;;with the entity via step-entity 
;;This is a synchronous function that lets the entity
;;handle a message.  Basically, we immediately handle
;;the message.  Simple wrapper...
(defn handle-message! [ctx e msg]
  (step-entity! ctx e (:t msg) msg))

;;So, behavioral entities are event handlers.
;;We have a couple of options...
;;Distinguish between messages sent to entities.

;;We can extend handle-message to see if it the event-type is a message.
;;If it is, we dispatch to the entity directly, calling it as if the
;;entity was an event handling function and the message was an event.

;;If we view the simulation itself as an entity, this is its step function;
;;or the system-wide step function.  This allows us to compose simulations...
;;A step is just mapping, possible fmap.
(defn step!
  ([t es ctx]
   (do (debug [:stepping t])
       (reduce (fn [acc e] (step-entity! acc e t))
               (advance-time- ctx t) es)))
  ([ctx]
   (when-let [res (next-recepients- ctx)]
     (let [[t xs] res]
       (step! t xs ctx)))))

;;Basic entity behavior, .
(befn +default-behavior+ ^behaviorenv {:keys [entity current-message] :as ctx}
      (do (debug [entity :saw current-message])
          (success ctx)))

;;This is forming a loose protocol for a messaging system....
;;messaging
;;one way to do this is to round-robin the messages.
;;since we're pushing messages to entities, we should know who has pending messages.
;;specifically, which entities need to be updated at the current interval.  That's the
;;old update logic creeping in.


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
;(declare default)
(defn ->simple-ctx [& {:keys [n default] :or {n 2}}]
  (let [ids  (range n)
        ctx  {;;records of entity data, with an associated messagequeue.
              :entities (into {} (map (fn [n] [n
                                               {:name n
                                                :messages pq/minpri
                                                :t 0
                                                :behavior default}]))
                              ids)
              ;;the system time.  We can compute this by pending, actually.
              :t 0
              ;sets of entity ids with pending messages, mapped to time.
              :pending  (avl/sorted-map-by (fn [^long l ^long r]
                                             (cond (< l r) -1
                                                   (== l r) 0
                                                   :else 1)))
              }]
    (reduce (fn [ctx id]
              (push-message- ctx :system id (->msg 0 :spawn)))
            (map->entityctx ctx) ids)))

;;so, system time is zero, or we can define it as the minimum of the known messages.
;;This is consistent with the agenda from sicp.

;;operations on the context.
(def simple-ctx (->simple-ctx))

;;There are a couple of possible outcomes in an update step.
;;Entities might send eachother messages concurrently, and
;;expect to them to arrive "now", or at the same time, i.e.
;;instantenous messages.  We could add a slight offset to
;;indicate the order of the messages happened a bit "later",
;;to help with debugging.  Or, we could just examine the queue
;;of messages the entity reacted to.  Both provide an entity-view
;;of the history of the system.
;;The goal is to enable multiple views of time from the
;;entity's perspective; specifically the shared-nothing simulated
;;nature...

;;This is a basic simulation loop...
;;Note: we can actually reify this as a reducer, or a sequential, or
;;whatever we want.  So we have options as to how we see things;
;;we can push the simulation history onto a channel, and diff it in
;;another thread.
(defn simulate! [& {:keys [init-ctx n continue? tmax post]
                    :or {n 2
                         tmax 4500
                         }}]
  (let [continue? (or continue? (fn [ctx] (< (:t ctx) tmax)))
        init-ctx  (or init-ctx (->simple-ctx :n 2))
        init-ctx  (if post (post init-ctx) init-ctx)]
    (loop [ctx init-ctx]
      (if (not (continue? ctx)) ctx
          (if-let [res (next-recepients- ctx)] ;;identify the next set of entities to run.
            (let [[t es] res]
              (do ;(Thread/sleep 200)
                  (recur  (step! t es ctx))))
            ctx)))))

(defn history [& {:keys [init-ctx n continue? tmax post]
                  :or {n 2
                       tmax 4500
                       }}]
  (let [continue? (or continue? (fn [ctx] (< (:t ctx) tmax)))
        init-ctx  (or init-ctx (->simple-ctx :n 2))
        init-ctx  (if post (post init-ctx) init-ctx)
        fwd      (fn fwd [ctx]
                    (if-let [res (next-recepients- ctx)] ;;identify the next set of entities to run.
                      (let [[t es] res]
                        (step! t es ctx))))]
        (reify
          clojure.core.protocols/CollReduce
          (coll-reduce [this f]
            (loop [acc init-ctx
                   current-ctx (fwd init-ctx)]
              (cond (reduced? acc) @acc
                    (not (continue? current-ctx)) acc
                    :else (recur (f acc current-ctx)
                                 (fwd current-ctx)))))
          (coll-reduce [this f init]
            (loop [acc init
                   current-ctx init-ctx]
              (cond  (reduced? acc) @acc
                     (not (continue? current-ctx)) acc
                     :else (recur (f acc current-ctx)
                                  (fwd current-ctx)))))
          clojure.lang.ISeq
          (seq [this]  (take-while (fn [x] (and (identity x) (continue? x)))
                                   (iterate fwd init-ctx))))))

