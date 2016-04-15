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
                     ->and!
                     ->pred
                     ->or
                     ->bnode
                     ->while
                     ->reduce
                     always-succeed
                     always-fail
                     bind!
                     bind!!
                     merge!
                     merge!!
                     push!
                     return!
                     val!
                     befn
                     ] :as b]
            [spork.util.general     :as gen]
            [spork.util.collections :as fast]
            [spork.data.mutable :as mut]
            [spork.entitysystem.store :as store]
            [spork.data.priorityq :as pq]
            [clojure.core.reducers :as r]
            [clojure.data.avl :as avl]))

;works on mutables too...
(defmacro deref! [atm]
  `(if (instance? clojure.lang.IDeref ~atm)
     (.deref ~(with-meta atm {:tag 'clojure.lang.Atom}))
     ~atm))

(def ^:dynamic *debug* false)

(defmacro debug
  ([lvl msg]
   `(when (and ~'spork.ai.testing/*debug*
               (>= ~'spork.ai.testing/*debug* ~lvl))
      (when-let [res#  ~msg] (println res#))))
  ([msg] `(when ~'spork.ai.testing/*debug*
            (when-let [res# ~msg]
              (println res#)))))
;;Convert this to a record.
;;A message is just a packet of information.
;;We can disperse these packets.

(comment ;optimization testing..
  (defrecord emsg [from to t msg])
  (defmacro ->msg
    ([t msg] `(emsg. nil nil ~t  ~msg))
    ([from to t msg]
     `(emsg. ~from  ~to  ~t  ~msg)))
)

(defmacro ->msg
   ([t msg] `{:t ~t :msg ~msg})
   ([from to t msg] `{:from ~from :to ~to :t ~t :msg ~msg}))

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

;;Ideally...we simulate this stuff persistently, and allow a mutable
;;version to be swapped out as needed (for efficiency sake...)

;;looking for a loose protocol that makes this underlying functionality simpler.
;;Is load-entity redundant?

(defprotocol IEntityStorage  (commit-entity- [s] "benv->ctx"))
(defprotocol IEnvironmentProvider
  (load-entity-   [s ent msg] "ctx->benv") 
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
  (push-message-    [e from to m]))

;;the evaluation context for the entity's behavior.  we'll probably
;;want to mutate this, but for now see what kind of benefits we can
;;get just using records and field access.

;;we need to know how to push messages...this is our interface
;;to the entity's evaluation model.  Right now, this is
;;being accomplished via send, and binding.
(defrecord behaviorenv [entity current-messages new-messages ctx current-message]
  IEntityMessaging
  (entity-messages- [e id] current-messages)
  (push-message-    [e from to msg] ;should probably guard against posing as another entity.
    (let [t        (.valAt ^clojure.lang.ILookup  msg :t)
          _        (debug [:add-new-messages-to new-messages])
          additional-messages (b/swap!! (or new-messages  (atom []))
                                        (fn [^clojure.lang.IPersistentCollection xs]
                                          (.cons  xs
                                             (.assoc ^clojure.lang.Associative msg :from from))))]                            
      (behaviorenv. entity
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
       (set-entities- ctx  (.assoc ^clojure.lang.Associative entities id ent))
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

;;note:
;;Interestingly, small structures are quick to update using the object constructor...
;;implementing a record type because maps aren't efficiently specialized.
(defrecord entityctx [^clojure.lang.IPersistentMap entities pending t]
  IEnvironmentProvider
  (get-entities- [obj] entities)
  (set-entities- [obj xs] (entityctx. xs pending t))
  (set-entity- [ctx id ent]
    (entityctx. (.assoc entities id ent) pending t))
  (get-pending [obj] pending)
  (get-t [obj] t)
  (load-entity-   [ctx ent msg]
    (behaviorenv. (atom (fget (fget ctx :entities) ent))
                  [msg]
                  nil
                  (atom ctx)
                  msg))
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
      (let [tfut        (.valAt ^clojure.lang.ILookup msg :t)
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
(defn ->simple-ctx [& {:keys [n] :or {n 2}}]
  (let [ids  (range n)
        ctx  {;;records of entity data, with an associated messagequeue.
              :entities (into {} (map (fn [n] [n
                                               {:name n
                                                :messages pq/minpri
                                                :t 0}]))
                              ids)
              ;;the system time.  We can compute this by pending, actually.
              :t 0
              ;sets of entity ids with pending messages, mapped to time.
              :pending  (avl/sorted-map)
              }]
    (reduce (fn [ctx id]              
              (push-message- ctx :system id (->msg 0 :spawn)))
            (map->entityctx ctx) ids)))

;;so, system time is zero, or we can define it as the minimum of the known messages.
;;This is consistent with the agenda from sicp.

;;operations on the context.
(def simple-ctx (->simple-ctx))     

;;__behaviors__

;;logs the state of the entity.
(befn notify-change {:keys [entity] :as ctx}
 (do (debug 
       (let [{:keys [state wait-time name t]} (deref! entity)
             ]
         (str "<"  t "> " "Entity " name
              " is now in state " state
              " for " wait-time " steps")))
     (success ctx)))

;;if we have a message, and the message indicates
;;a time delta, we should wait the amount of time
;;the delta indicates.  Waiting induces a change in the
;;remaining wait time, as well as a chang
(befn wait-in-state ^behaviorenv [entity current-message ctx]
  (let [;_ (println [:wait-in-state entity msg])
        msg    current-message
        t     (fget msg :t)
        delta (- t (fget (deref! entity) :t))]
    (when-let [duration (fget  (deref! entity) :wait-time)]
      (if (<= delta duration) ;time remains or is zero.
         ;(println [:entity-waited duration :remaining (- duration delta)])
        (merge!!  entity {:wait-time (- duration delta)
                          :t t}) ;;update the time.
        (do ;can't wait out entire time in this state.
          (merge!! entity {:wait-time 0
                           :t (- t duration)}) ;;still not up-to-date
           ;;have we handled the message?
           ;;what if time remains? this is akin to roll-over behavior.
           ;;we'll register that time is left over. We can determine what
           ;;to do in the next evaluation.  For now, we defer it.
          (bind!! {:current-message (.assoc ^clojure.lang.Associative msg :delta (- delta duration))}
                 )

          )))))
                
;;choose-state only cares about the entity.
(befn choose-state  ^behaviorenv [entity]
      (let [nxt (gen/case-identical? (:state (deref! entity))        
                    :dwelling  :deploying
                    :deploying :dwelling
                    :dwelling
                    )]       
        (push! entity :state nxt)))

(befn choose-time ^behaviorenv [entity]
      (let [twait
            (gen/case-identical? (:state (deref! entity))
              :dwelling  (+ 365  (rand-int 730))
              :deploying (+ 230  (rand-int 40)))]
        (push! entity :wait-time twait)))

(defn up-to-date? [e ctx] (== (:t e) (:t ctx)))

;;This will become an API call...
;;instead of associng, we can invoke the protocol.
(befn schedule-update  ^behaviorenv {:keys [entity ctx new-messages] :as benv}
      (let [st       (deref! entity)
            nm       (:name st)
            duration (:wait-time st)
            tnow     (:t (deref! ctx))
            tfut     (+ tnow duration)
            _        (debug 4 [:entity nm :scheduled :update tfut])
            ;_ (when new-messages (println [:existing :new-messages new-messages]))
            ]        
        (success (push-message- benv nm nm (->msg nm nm tfut :update)))))

(definline move [ctx]
  `(->and! [choose-state
            choose-time
            notify-change
            schedule-update] ~ctx))

;;pick an initial move, log the spawn.
(befn spawn ^behaviorenv {:keys [entity] :as ctx}
      (when (identical? (:state (deref! entity)) :spawning)
        (->and! [(->do (fn [_] (debug (str  "Entity " (:name (deref! entity)) " spawned"))))
                 move]
                ctx)))
(defn rconcat
  ([& colls]
   (reify clojure.core.protocols/CollReduce
     (coll-reduce [this f1]
       (let [c1   (first colls)
             init (reduce (fn [acc x] (reduced x)) (r/take 1 c1))
             a0   (reduce f1 init (r/drop 1 c1))]
         (if (reduced? a0) @a0
             (reduce (fn [acc coll]
                       (reduce (fn [acc x]
                                 (f1 acc x)) acc coll)) a0 (r/drop 1 colls)))))
     (coll-reduce [this f init]
       (reduce (fn [acc coll]
                (reduce (fn [acc x]
                          (f acc x)) acc coll)) init colls))
     clojure.lang.ISeq
     (seq [this] (seq (into [] (r/mapcat identity colls) )))
     )))
       
;;the entity will see if a message has been sent
;;externally, and then compare this with its current internal
;;knowledge of messages that are happening concurrently.
(befn check-messages ^behaviorenv [entity current-messages ctx]
  (let [old-msgs (fget (deref! entity) :messages)]
    (when-let [msgs (pq/chunk-peek! old-msgs)]
      (let [new-msgs (rconcat (r/map val  msgs) current-messages)
            _        (b/swap!! entity (fn [^clojure.lang.Associative m]
                                        (.assoc m :messages
                                                (pq/chunk-pop! old-msgs msgs)
                                                )))]
            (bind!! {:current-messages new-msgs})))))

;;we need the ability to loop here, to repeatedly
;;evaluate a behavior until a condition changes.
;;->while is nice, or ->until
;;We'd like to continue evaluating the behavior (looping)
;;until some predicate is tripped.  Another way to do
;;this is to send a message....If there's time remaining,
;;we tell ourselves to keep moving, and move again prior to
;;committing.
(befn advance ^behaviorenv [entity ctx]
      (if (not (identical? (:state (deref! entity)) :spawn))
        (->and [wait-in-state move])
        spawn))

;;this is a dumb static message handler.
;;It's a simple little interpreter that
;;dispatches based on the message information.
;;Should result in something that's beval compatible.
;;we can probably override this easily enough.
;;#Optimize:  We're bottlnecking here, creating lots of
;;maps....

;;type sig:: msg -> benv/Associative -> benv/Associative
;;this gets called a lot.
(defn message-handler [msg ^behaviorenv benv]
  (let [entity (.entity benv)
        current-messages (.current-messages benv)
        ctx  (.ctx benv)]
    (do ;(println (str [(:name (deref! entity)) :handling msg]))
      (beval 
       (gen/case-identical? (:msg msg)
           ;;generic update function.  Temporally dependent.
           :update (if (== (:t (deref! entity)) (:t (deref! ctx)))
                     (do (success benv)) ;entity is current
                     (->and [(fn [^clojure.lang.Associative ctx] (success (.assoc ctx :current-message msg)))                           
                             advance
                             ]))
           :spawn  (->and [(push! entity :state :spawning)                        
                           spawn]
                          )
           ;;allow the entity to change its behavior.
           :become (push! entity :behavior (:data msg))
           :do     (->do (:data msg))
           (throw  (Exception. (str [:unknown-message-type (:msg msg) :in  msg]))))
       benv))))

;;handle the current batch of messages that are pending for the
;;entity.  We currently define a default behavior.
(befn handle-messages ^behaviorenv {:keys [entity current-messages ctx] :as benv}
      (when current-messages
        (reduce (fn [acc msg]                  
                  (do ;(debug [:handling msg])
                    (message-handler msg (val! acc))))
                (success benv)
                current-messages)))

;;Basic entity behavior is to respond to new external
;;stimuli, and then try to move out.

;;This is a pretty typical prototype for entities to follow.
(befn default ^behaviorenv [entity]
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

;;this is on the hot path, so we'll optimize it.

;;really, this is assoc-in using dispatched types.
;;If we make some guarantees about the underlying data
;;structure, namely that it's composed of places, we
;;can get something useful out of it.
;;Namely, places don't change (i.e. they always exist
;;as long as the container exists).

;;Places can be nested paths into objects.

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
  ([ctx e t msg]
   (-> (beval default (load-entity- ctx e  msg)) ;should parameterize this.
       (return!)
       (commit-entity-)))
  ([ctx e t]
   (step-entity! ctx e t (->msg t :update))))
  
(defn step!
  ([t es ctx]
   (do (debug [:stepping t])
       (reduce (fn [acc e] (step-entity! acc e t))
               (advance-time- ctx t) es)))
  ([ctx]
   (when-let [res (next-recepients- ctx)]
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
;;entity's perspective; specifically the shared-nothing simulated
;;nature...

;;This is a basic simulation loop...
;;Note: we can actually reify this as a reducer, or a sequential, or
;;whatever we want.  So we have options as to how we see things;
;;we can push the simulation history onto a channel, and diff it in
;;another thread.
(defn simulate! [& {:keys [n continue? tmax post]
                    :or {n 2
                         tmax 4500
                         }}]
  (let [continue? (or continue? (fn [ctx] (< (:t ctx) tmax)))
        init-ctx  (->simple-ctx :n n)
        init-ctx  (if post (post init-ctx) init-ctx)]
    (loop [ctx init-ctx]
      (if (not (continue? ctx)) ctx
          (if-let [res (next-recepients- ctx)] ;;identify the next set of entities to run.
            (let [[t es] res]
              (do ;(Thread/sleep 200)
                  (recur  (step! t es ctx))))
            ctx)))))


(defn history [& {:keys [n continue? tmax post]
                  :or {n 2
                       tmax 4500
                       }}]
  (let [continue? (or continue? (fn [ctx] (< (:t ctx) tmax)))
        init-ctx  (->simple-ctx :n n)
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
    
  
(defn stress-test []
  (time (dotimes [i 1]
          (simulate! :n 8000))))

(defn stress-test-np []
  (time (dotimes [i 1]
          (simulate! :n 8000 :post #(dissoc % :pending)))))

(defn stress-test2 []
  (time (dotimes [i 1000]
          (simulate! :n 60))))

(defn stress-test3 [ & {:keys [n] :or {n 1}}]
  (time (dotimes [i 1]
          (r/fold n
                  (fn ([] nil)
                      ([l r] nil))
                  (fn ([] nil)
                      ([l r] nil))                  
                  (r/map (fn [n] (simulate! :n 60))
                         (vec (range 1000)))))))

(defn stress-test4 [ & {:keys [n] :or {n 1}}]
  (time (dotimes [i 1]
          (doall
           (pmap (fn [n]
                   (simulate! :n 60))
                 (range 1000))))))


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
(comment
  
(defn case-test [kw]
  (case kw
    :update 0
    :spawn  1
    :do     2
    (throw  (Exception. (str [:unknown-message-type])))))

;;this is about 3x faster than case....we're not hashing.
(defn cond-test [kw]
  (cond (identical? kw :update) 0
        (identical? kw :spawn)  1
        (identical? kw :do)     2
        :else
        (throw  (Exception. (str [:unknown-message-type])))))

(defn condp-test [kw]
  (condp identical? kw
    :update 0
    :spawn  1
    :do     2
    (throw  (Exception. (str [:unknown-message-type])))))

(defn case-of-test [kw]
  (case-of kw
    :update 0
    :spawn  1
    :do     2
    (throw  (Exception. (str [:unknown-message-type])))))
           
)


(comment ;obe
  (defmacro set-entity! [ctx nm v]
  (let [c       (with-meta ctx {:tag 'clojure.lang.Associative})
        entities (with-meta (gensym "entities") {:tag 'clojure.lang.Associative})]        
    `(let [~c ~ctx
           ~entities (.valAt ~c :entities)]
       (.assoc ~ctx :entities
               (.assoc ~entities ~nm
                       ~v)))))

;;__OBE__
(defn get-messages [e ctx] (get-in ctx [:entities e :messages]))
;;we may want to get concurrent messages for all entities and process
;;them in bulk.  Remember, this is faster than going round-robin, for
;;the persistent structure.
;;__OBE__
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
      (when (.nth ^clojure.lang.Indexed res 1 nil) res))))
  
  )
