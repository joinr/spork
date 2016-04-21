(ns spork.ai.testing
  (:require [spork.ai.core :as core :refer
             [deref! fget fassoc  push-message- map->entityctx debug ->msg]]
            [spork.ai.behavior :as behavior
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
            [spork.data.priorityq   :as pq]
            [clojure.core.reducers  :as r])
  (:import [spork.ai.core behaviorenv]))
;;Interesting quote today:
;;"rules are for fools"
;;hmm  

;;this only works with path literals...
;;allows us to define paths at compile time.
(defmacro path! [lits] `~(keyword (clojure.string/join "/" (map name lits))))

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
              :dwelling  (unchecked-add (int 365)  (rand-int 730))
              :deploying (unchecked-add (int 230)  (rand-int 40)))]
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

;;Where does this live?
;;From an OOP perspective, every actor has a mailbox and a message handler.
;;

;;type sig:: msg -> benv/Associative -> benv/Associative
;;this gets called a lot.
(defn message-handler [msg ^behaviorenv benv]
  (let [entity           (.entity benv)
        current-messages (.current-messages benv)
        ctx              (.ctx benv)]
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

;;message handling is currently baked into the behavior.

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

;;entities are message handlers.
;;We can view them as functions that handle messages (events) just
;;like any other event handler; If the entity has a behavior, then
;;the behavior is incorporated into the message handling.
;;Specifically, we have to create a behavior context to eval the
;;entity's message handling in.
;;This means, there's a trivial wrapper function for handling
;;generic entity messages (Even for complex behavior-driven
;;ents), that is :: ctx -> msg -> entity -> ctx
;;That's basically what step! does.


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

;;__Testing Functions__
(defn simulate! [& {:keys [n default]
                    :or {n 2 default default}}]
  (core/simulate! :init-ctx (core/->simple-ctx :n n :default default)))

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
