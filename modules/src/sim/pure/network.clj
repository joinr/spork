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
(ns spork.sim.pure.network
  (:use [sim.data]))
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


(defrecord event-network [name clients subscriptions])


(defn- drop-event-client
  "Remove the relation from event-type to client-name.  If no relations 
   remain for event-type, drops the event-type from subscriptions."
  [obs client-name event-type] 
  (let [subscriptions (get obs :subscriptions)
        scripts (get subscriptions event-type {})]
    (if (contains? scripts client-name)
      (if (= (count scripts) 1)        
        (assoc obs :subscriptions (dissoc subscriptions event-type))
        (assoc-in obs [:subscriptions event-type] (dissoc scripts client-name)))      
      (throw (Exception. 
               (str "Client subscription does not exist" client-name)))))) 

(defn- drop-client-event
  "Remove the relation from client-name to event-type.  If no relations remain
   for client, client is dropped from clients."
  [obs client-name event-type]
  (let [clients (get obs :clients)
        events (get clients client-name #{})]
    (if (contains? clients client-name)
      (if (= (count events) 1)        
        (assoc obs :clients (dissoc clients client-name))
        (assoc-in obs [:clients client-name] (disj events event-type)))      
      (throw (Exception. 
               (str "Client does not exist" client-name))))))

(defn get-events
  "List the active events in this network."
  [net] (keys (get net :subscriptions)))

(def default-transition ;when handling events, we don't care about the client.
  (fn [ctx e [client-name handler]]
    (handler ctx e client-name)))

(def noisy-transition 
  (fn [ctx e [client-name handler]]
    (do (println (str {:state ctx :event e :client client-name}))
      (handler ctx e client-name))))

(defn ->handler-context
  "Creates a composite data structure that carries the context to be handled by
   handler functions.  Note the shocking similarity to behavior context....."
  ([type data state transition]
    {:type type :data data :state state :transition transition})
  ([type data state] (->handler-context type data state default-transition)))

(defn un-register
  "Drop the relation from client-name to event-type, and the subscription from 
   event-type to client-name."
  [obs client-name event-type]
  (-> (drop-event-client obs client-name event-type)
    (drop-client-event client-name event-type))) 

(defn get-event-clients
  "Return a map of clients to handlers, which will be invoked when this event 
   is traversed."
  [obs event-type]
  (get-in obs [:subscriptions event-type]))
  
(defn get-client-events
  "Return an un-ordered set of the events this client is interested in."
  [obs client-name]
  (get-in obs [:clients client-name]))

(defn universal?
  "Determines if the client is subscribed to all events, or only a specific set
   of events.  If the client has :all as its subscription, it will trigger on 
   any event."
  [obs client-name]
  (contains? (get-client-events obs client-name) :all))

(defn register
  "Adds a bi-directional relation between client-name and event-type, where the 
   abstract traversal cost from event-type to client, is to invoke f.  
   We only want to allow 2 classes of registration: universal and specific.  
   Universal subscribers, tagged by :all, trigger on any event, and require no 
   argument for event-type. Specific subscriptions take an argument for 
   event-type.  Subscribers are either specific (triggering on specific events) 
   or universal, but never both.  If no event-type is supplied, the type will 
   default to :all, or a universal handler."
  ([obs client-name handler event-type]
    (let [{:keys [clients subscriptions]} obs]    
      (let [next-obs  (merge obs 
                             {:clients (assoc clients client-name 
                                (conj (get clients client-name #{}) event-type))
                              :subscriptions (assoc subscriptions event-type 
                                (assoc (get subscriptions event-type {})
                                       client-name handler))})
            evts (get-client-events next-obs client-name)]
        (cond (= event-type :all) ;registered a universal subscription. 
              (reduce (fn [o e-type] (un-register o client-name e-type))
                      next-obs (disj evts :all))
              :else  ;registered a specific subscription.
              (if (or (not (contains? evts :all)) 
                      (= evts #{:all}))                      
                next-obs
                (reduce (fn [o e-type] (un-register o client-name e-type))
                        next-obs 
                        (clojure.set/difference evts #{:all event-type})))))))
  ([obs client-name handler] (register obs client-name handler :all)))

(defn register-routes
  "Register multiple clients associated with multiple event/handler pairs.  
   The routing is passed as a map of maps, where the keys are client-names, and 
   the associated values are maps of event-type -> handler.  Thus, registering 
   a client entity 'Bob with the :shower and :eat events would look like: 
   {'Bob {:shower take-shower :eat eat-sandwich}}"
  [client-event-handler-map obs]
  (reduce (fn [o1 [client-name handler-map]] 
            (reduce  (fn [o2 [etype handler]]
                       (register o2 client-name handler etype)) o1 handler-map))
          obs client-event-handler-map))

(defn drop-client
  "Unregisters client from every event, automatically dropping it from clients."
  [obs client-name] 
  (assert (contains? (:clients obs) client-name) 
          (str "Client " client-name " does not exist!"))
  (reduce (fn [o etype] (un-register o client-name etype))
      obs (get-client-events obs client-name)))      

(defn empty-network
  "Creates an empty network of event-handlers."
  [name] 
  (map->event-network  {:name name :clients {} :subscriptions {}})) 

(defn ->propogation
  "Given an event routing, as specified by m, creates a new event network for
   propogating events."
  [m]
  (->> (empty-network "anonymous")
    (register-routes m)))

(defn simple-handler
  "Defines the simplest possible event-network: a single client called :in 
   that listens for every event, and handles it with the supplied 
   handler-function."
  ([name event-type handler-function]
    (register-routes {name {event-type handler-function}}
                     (empty-network :anonymous)))
  ([handler-function] 
    (simple-handler (keyword (gensym "handler")) :all handler-function)))
 
(defn serial-propogator 
  "This propogation function folds the context through the entire client-handler
   map, acting as close as possible to the original observer pattern.  We're 
   basically simulating the observer pattern by doing this, where instead of 
   calling side-effecting functions that mutate global state, we're accumulating
   the result of each 'handler', where the handler may change anything in the 
   context....Note, this offers a significant amount of control, in that the 
   handler can override the propogation, short-circuit, change the event, 
   change the state, change the propogation network topology, etc."
  [{:keys [state type data transition net] :as ctx} client-handler-map]
  (reduce (fn [context [client-name handler]] 
            (transition context data [client-name handler]))
          ctx client-handler-map))

;For right now....we assume serial propogation...I'll have to figure out how
;to weave in parallel or asynch propogation in the future....Serial is the 
;simplest case, and we might be able to convey parallelism using composed forms
;of serial propogation....or...we push the parallelism into the handler 
;functions, i.e. bulk updates of systems, so we have a coarse-grained event 
;structure.
(defn propogate-event
  "Instead of the traditional notify, as we have in the observable lib, we 
   define a function called propogate-event, which acts akin to a reduction.
   Each client with either an :all routing or a subscription to the event-type 
   will be traversed.  Every step of the walk is treated as a transition, in 
   which the handler function and the context are supplied to a 
   transition-function - transition - which determines the resulting context.  
   event-transition-func:: 
      'a -> sim.data.IEvent -> ('b, (sim.data.IEvent -> 'a -> 'a)) -> 'a        
   The resulting reduction over every transition is the return value of the 
   network.  If no transition function is supplied, we default to simply  
   applying the associated handler to the event-data and the state." 
  [{:keys [type data state] :as ctx} net 
   & {:keys [transition] 
      :or {transition (get ctx :transition default-transition)}}] 
  (let [client-handler-map 
        (merge (get-event-clients net type)
               (if (not= type :all)
                 (get-event-clients net :all) {}))]
    (serial-propogator 
      (assoc ctx :net net :transition transition) client-handler-map)))

(defn handle-event
  "High level function to handle an IEvent, in the context of some state, using 
   an event-network defined by net.  Alternatively, the state and the network 
   can be bundled in a associative structure (a map), in which case only two 
   values are required.  Returns the resulting state, with the context as 
   meta data."
  ([event state net]
    (propogate-event 
      (->handler-context (event-type event) (event-data event) state) net))
  ([event ctx]
    (propogate-event 
      (merge ctx {:type (event-type event) 
                  :data (event-data event)})
      (:net ctx))))

(defn handle-events [init-ctx net xs]
  (reduce (fn [ctx e] (handle-event e ctx)) (assoc init-ctx :net net) xs))

;these are combinators for defining ways to compose event handlers, where an 
;event handler is a function of the form: 
; (event-type -> event-data -> state -> state)

(defn union-handlers
  "This is a simple merge operation that clooges together one or more networks,
   and returns a new network that is the set-theoretic union of clients and 
   events.  The resulting network has every client that the original did, as 
   well as every event, with subscriptions merged.  Caller can supply a new 
   name for the merger."
  ([name nets]
    (reduce 
      (fn [merged net] (register-routes 
                         (reduce (fn [routes e]
                                   (merge routes 
                                          (zipmap (repeat e) 
                                                  (get-event-clients net e)))) 
                                 {} (:subscriptions net)) merged))
    (empty-network name) nets))
  ([nets] (union-handlers :merged nets)))

(defn bind-handlers
  "Creates a new network from one or more networks, that is a logical 
   composition of the event propogation of events handled by from, to identical
   events handled by to.  Every propogation is first handled by from, then by 
   to, effectively chaining propogations on the basis of event names."
  [from to]
  (let [like-events (clojure.set/intersection (set (get-events from))
                                              (set (get-events to)))]
    (simple-handler (fn [{:keys [type] :as ctx} edata name] 
                      (let [nxt (propogate-event ctx from)]
                        (if (contains? like-events type)
                          (propogate-event nxt to)
                          nxt))))))
(defn map-handler
  "Maps a handler to the underlying network.  This is a primitive chaining 
   function.  We return a new network that, when used for propogation, calls
   the handler-func with the resulting state of the propogation."
  [handler-func base]  
  (simple-handler (fn [ctx edata name] 
                    (handler-func (propogate-event ctx base)))))

(defn filter-handler
  "Maps a filtering function f, to a map of the handler-context, namely 
   {:keys [type data state]}"
  [f base]
  (simple-handler (fn [ctx edata name] 
                    (if (f ctx) (propogate-event ctx base) ctx))))

(defn switch-handler
  "Given predicate switch-func, composes two networks as if they were connected
   by a switch.  When split-func is applied to a handler context, true values 
   will cause propogation to continue to true-net, while false propogates to 
   false-net."
  [switch-func true-net false-net base]
  (simple-handler 
    (fn [ctx]
      (if (switch-func  ctx)
        (propogate-event ctx true-net)
        (propogate-event ctx false-net)))))

;(defn one-time-handler 
;  "After handling an event, this handler will alter the network, as provided in
;   the context, to remove itself from any further event handling."
;  [handler-function base]

;note...the analogue for an observable, in this context, is a network....
;in the impure, observable library, we'd write...
;(->> (make-observable)
;  (filter-obs (fn [ctx] (> (-> ctx :state :event) 2))) 
;  (map-obs (fn [ctx] 
;             (let [s (:state ctx)]
;               (if (coll? s) (first s))

;we can imagine this as an event network in that we push a single type around..
;where the medium being propogated is a compatible event context...really 
;a map.

;;We'll see if we actually need this guy later...might not need to macroize 
;this stuff yet....
;(defmacro with-event-context [ctx & body]
;  `(let [~'*context* ~ctx
;         ~'*events*  ~(get ctx :events)
;         ~'*state*   ~(get ctx :state)
;         ~'*t*       (current-time ~'*events*)]
;     ~body))

(comment ;testing 

;these are low level examples of event handler networks.         
(def sample-net 
  (->> (empty-network "Mynetwork")
    (register-routes 
      {:hello-client {:hello-event 
                       (fn [ctx edata name] 
                         (do (println "Hello!") ctx))}})))
(def simple-net 
  (->> (empty-network "OtherNetwork")
    (register-routes 
      {:hello-client {:all 
                      (fn [ctx edata name] 
                        (do (println "I always talk!")) ctx)}})))

;low-level event propogation
(defn prop-hello [] 
  (propogate-event 
    (->handler-context :hello-event "hello world!" nil)
    sample-net))

;high level event propogation
(defn handle-hello [] 
  (handle-event (->simple-event :hello-event 0 nil) nil sample-net))

;using combinators to build networks

(defn echo-hello [] 
  (propogate-event 
    (->handler-context :hello-event "hello world!" nil noisy-transition) 
    sample-net))

(def message-net 
  (->> (empty-network "A message pipe!")
    (register-routes 
      {:echoing {:echo (fn [{:keys [state] :as ctx} edata name] 
                            (do (println "State:" state)
                              ctx))}
       :messaging {:append (fn [{:keys [state] :as ctx} edata name] 
                                   (update-in ctx [:state :message] conj edata))}})))

(defn test-echo [& [msg]]
  (propogate-event (->handler-context :echo nil [msg]) message-net))

;this is a round-about way of doing business....
(defn test-conj [& xs]
  (reduce (fn [acc x] 
            (handle-event (->simple-event :append 0 x) acc message-net))
          {:message []} xs))

;a test of multiple events being 'queued' and handled by the message network.
(defn test-messaging [] 
  (handle-events {:message []}
                 message-net
                 [[:echo]
                  [:append 2]
                  [:append 3]]))

;add some capabilities to the network...
;like a better message.
(use 'clojure.pprint)
(def message-net2 
  (register-routes {:messaging2 {:echo (fn [{:keys [state] :as ctx} edata name]
                                        (do (println "The message is: ")
                                          (pprint (:message state))
                                          ctx))}} message-net)) 
  
;this is an attempt to get at the composable workflow from observable.
(def time-stamped-messages
  (let [print-route (->propogation {:in {:all (fn [ctx edata name] 
                                                (do (pprint (:state ctx))
                                                  ctx))}})
        add-current-time (fn [ctx] (assoc-in ctx [:state :date]
                                             (util.datetime/get-date)))] 
    (->> print-route
      (map-handler add-current-time) ;should wrap the whole thing...
      (union-handlers message-net)))) ;combine it with the message-net.

)