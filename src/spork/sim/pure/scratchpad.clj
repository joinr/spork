;this is a functional replacement for an event-based control flow that was 
;previously implemented using the observable/observer design pattern. 
;The previous implementation more or less specified a decoupled system of 
;event propogators (observables), and event consumers (observers).
;The event propogator served as an abstract "event stream", in that it had 
;a set of defined events, which implicitly formed communication channels, 
;as well as a set of observers associated with each event channel.

;Observers registered their interest with an observable, providing a client  
;identifier, along with the msgid (an enumerated type) of the event of interest,
;and a call-back function or a trigger to invoke.  In VBA, the observers were 
;classes of objects that implemented the ITriggerable interface, which forced 
;them to have a consistent 'trigger sub routine that served as the callback. 
;During subscription to an observable, the observer simply passed itself (via 
;an object reference) to the observerable, which saw the observer as a valid 
;instance of ITriggerable to be invoked upon observation of an associated event.

;This allowed the observable to channel event propogation to only the relevant
;parties.  A standard broadcast channel (0) was also provided, so that observers 
;could choose how to handle notifications internally.  

;Notification of events was accomplished with imperative calls to the 'notify 
;method of the observable, which recieved arguments in the form of the msgid 
;(an enumerated type that identified the event), as well as optional data 
;associated with the event. 

;The (msgid, data) pairs were then channeled to appropriately subscribed 
;observers, and the observers'  'trigger callback was invoked with (msgid, data)
;as arguments.  The callback had a type signature of ::integer->maybe(obj)->unit
;As such, the callbacks were fundemantally side-effecting - although generally
;not mutating.  In VBA, most events were treated as instantaneous, which meant
;that as soon as a client "triggered" an event notification, the flow of 
;control would shift to handling the event immediately via notification.  
;Event deferral happened sparingly, with a very rudimentary use of the event 
;queue to schedule state changes.  Specifically, "time" changes were the primary
;mechanism for controlling flow, at which point the entire simulation would 
;"wake" and handle all of the subsystems as part of a large process, again 
;handling instantanteous events/messages during processing of the main
;simulation engine.  

;We still need to retain some concepts from the previous implementation, while 
;adding new features.  The ability to execute "instantaneous" events is useful, 
;particularly in reasoning about the order of state changes.  However, we'd like
;to build in a robust means for defining and composing event-based control 
;structures, specifically networks that propogate event/state contexts, handling
;event-driven state-transitions.  Composing such networks is also of vital 
;importance, since we can describe components independently, and then compose 
;them into a greater whole.  


;Note -> we view the event propogation network as a control structure that 
;communicates via message passing....in this case, the message queue is 
;embedded in the event-context.  Much like Clojure agents, and futures, we 
;should allow the notion of asynchronous and synchronous event-handling...
;For instance, when handling one event, the handler may need to trigger, 
;and subsequently await the arrival of, another event, in order to 
;complete its handling.  

;Need to handle delayed activation of events....probably via storing a 
;continuation as an event....this lets event handlers act as co-routines.
;How do we know when to call the continuation?  
;If it's time-based, like...wait until 220, it's just a scheduled event like 
;every other...

;If it's condition-based, like wait until "Unit B comes home", we need to 
;filter on comes-home events, which are for unit B, and then apply the 
;handler to the state.

;Alternately, we define a new "Unit B comes home" event, which filters 
;supply events looking for unit b's return, and then triggers (or enqueues)
;the "Unit B Comes home" event, which is handled by the continuation. 

;condition-based handlers are typically ad-hoc, and one-time use....so after 
;they're processed, the condition should be removed from the network.....

;so we now have the ability to alter the network (not unlike altering the 
;dynamic signal collection in YAMPA) as we process events....

;So.....our handler actually takes the state, the event, the network, and 
;returns a new (state, network).....

;Propogating an event through the network could result in the network topology
;changing, like defining new events.  


(ns spork.sim.pure.scratchpad)
;Note -> there is currently no order of execution in notifying the observers 
;(actually, the order they register is the order of execution.  The net effect
;of this is that observations should be cummutative i.e., the effect of an
;observation should be independent of order of execution. This is another 
;argument against using observers as mutators of state, or signal chains, since 
;the designer would need to be extra vigilant about the order of execution.
;Note -> all of these concerns are moot in F# and clojure.

;When you boil it down, the observable is just a collection of routing 
;information.  We add routing information to the observable in the form of 
;subscriptions, where unique clients subscribe, or attach, to one or more 
;events.

;All an observer needs to do is provide and interface that allows interested 
;parties to register themselves with the observer.  We can then flex that
;knowledge later to traverse the clients for a specific event.  In the impure
;world, the traversal would cause I/O via evaluating a valueluess callback 
;function associated with the entity.  In the pure world, our traversal will 
;involve invoking a handler function (passed in as a map of handlers) associated
;with the interested entity.  The handler function will take the current state,
;and the event-data, and will return a new state.  This way, we perform an
;event-driven reduction using A: routing information provided by the observable,
;and B: a set of handler functions, and C: an initial state.  
;That allows us to bridge the gap between the typical impure simulation and the 
;desired pure form! 

;Note -> some handler functions may (and likely will!) have side-effects, for 
;logging, visualization, etc.  In this case, they must still return the input
;state after performing the side-effect.


;;Proposed language for defining these phenomena...


;A concrete example....a unit returns from a deployment....
(defn unit-returned? [name event] 
  (= name (get-in [:unit :name] (event-data event))))

(defn log-return [state e] 
  (let [uic (get (event-data e) :unit-name)
        return-id (keyword (str uic "-returned"))
        handle-return (fn [state e] 
                    (do (println [uic "returned"])
                      state))
        new-net (register-routes (get state :net) 
                   {(event-watcher eid) {return-id 
                                           (one-time-only handle-return)}
                    return-id {:all (fn [state e] 
                                      (if (unit-returned? :UIC2 e)
                                        (add-event state 
                                           (->simple-event eid 
                                               (get-time state)))))}})]
    (assoc state :net newnet)))



;;;;;;;

;;Break it apart in event-terms...

;;;;Add Conditional-Event 
;Set up a new handler that watches :all events via some condition. 
  ;Either handles the event by triggering (queuing) a new-event 
  ;  (provided by label). 
  ;Or passes the state unscathed. 

;;;;Handle-Event (this is, technically, just a generic subscription)
;Add a handler for new-event, that handles the observation using supplied 
;handler f.

(defn ->condition [label condition] {:label label :condition condition})
(defn condition-label [c] (get c :label))
(defn condition-pred [c]  (get c :condition))

(defn at-time [t] 
  (->condition :at-time (fn [state e] (= (event-time e) t))))
(defn after-time [t] 
  (->condition :after-time (fn [state e] (>= (event-time e) t))))
(defn before-time [t] 
  (->condition :before-time (fn [state e] (< (event-time e) t))))
(defn between-time [tstart tfinal]
  (->condition :between-time 
     (fn [state e] 
       (let [t (event-time e)]
         (and (>= t tstart) 
              (< t tfinal))))))
      
;; (keyword (str "observed" label))
; a more general notion of waiting
(defn wait-until [state e condition f]
  (let [event-name      (condition-label condition)
        condition-true? (condition-pred condition)]
    (set-network state 
       (register-routes (get-network state)
          {(event-watcher event-name) {event-name f}
           event-name {:all (fn [state e] 
                              (if (condition-true? state e) ;checks for the condition.
                                  (add-event state ;trigger the observation of the event.
                                       (->simple-event eid (get-time state)))
                                  state))}}))))

(defn once-only [event-name f] 
  (fn [state e] 
    (set-network state 
       (un-register (get-network state) evt)

(defn sleep [state entity-id tstart duration]
  (let [tfinal (+ tstart duration)]
      (wait-until (add-time state tfinal)
                  (at-time tfinal)
                  (fn [state e] 
                    (add-event state 
                         (->simple-event :resume tfinal entity-id))))))  


;we can also use conrad barski's notion of state-patching....something to think
;about.

;restated...
(defn log-return 


