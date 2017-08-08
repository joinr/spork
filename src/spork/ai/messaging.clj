;;A system for entity messaging and behavior environments.
;;Place-holder for generic entity behavior implemented via
;;abstract mailboxes and message-passing.  Currently have an
;;approximate implemention that needs to be generalized into
;;spork.
(ns spork.ai.messaging
  (:require [spork.sim.simcontext :as sim]
            [spork.ai.behaviorcontext :as b]))

(defmacro watching-error
  "Acts like a pass-through version of try-catch, 
   where an automatic generic exception handler - 
   body - is defined and called prior to simply 
   re-throwing the original error.  Used for 
   supplying debugging forensics."
  [body handler & {:keys [err-symbol]
                   :or {err-symbol 'e}}]
  `(try ~body
        (catch ~'Exception ~err-symbol
          (do ~handler
              (throw ~err-symbol)))))

;;Convenience functions for simulated entity messaging.
;;=====================================================

;;Variant of packet.
(defmacro ->msg
  "Convenience macro for constructing spork.sim.simcontext.packet 
   records that correspond to a message between two entities."
  ([t msg] `(sim/->packet ~t :message (:from ~msg) (:to ~msg) ~msg nil))
  ([from to t msg] `(sim/->packet ~t :message ~from ~to ~msg nil))
  ([from to t msg data] `(sim/->packet ~t :message ~from ~to ~msg ~data)))

;;all we need to do is create  a behavior context,
;;eval the behavior, and store the entity in the
;;evaluated context.
;;need to push this into simcontext...
(defn handle-message!
  "Message handling is equivalent to stepping the entity
   immediately.  Synchronously send a message to an entity with
   an associated behavior, evaluate the entity's response in a 
   behavior context derived from the simulation context ctx, 
   and return the resulting simulation context.  Throws an 
   exception if the message is not processed."
  [ctx ent msg]  
  (watching-error
   (b/step-entity! ctx ent msg)
   ;if we observe an error, we'll print out handy forensics before throwing.
   (println [:ERROR
             :messaging-entity!
             {:t    (sim/get-time ctx)
              :name (:name ent)}])))

;; Note: we can easily extend this
;; if we have a communication channel, then we can do async comms.  We
;; need to make sure we update the entity prior.  Either handle that in
;; the message processing or handle it elsewhere...
(defn send!!
 "Convenience function for abstractly sending messages to entities and
  synchronously computing the result via a valid simcontext.  
  Double-bang convention is consistent with synchronous send.  "
  [e type data ctx]
  (handle-message! ctx e
     (->msg (:name e) (:name e)
            (sim/get-time ctx)
            type
            data)))
