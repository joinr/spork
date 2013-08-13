(ns spork.sim.events
  (:use [util.general :only [unfold generate]]
        [sim.data]))

;Events are descriptions of ways to compute new values from initial values.  
;They are inputs to functions that consume such information to map an 
;initial value, with a description of change, to a new value, with the change 
;implemented (and the pending event removed), or with the change unimplemented 
;(the pending event remains).

;Note -> this is basically a direct copy of the events.base from cljgui
;I'm going to reconcile the two at some point...

;[type data id time from to]

;helper functions for the subsequent defevents and let-events macros.
(defn- emit-event
  "Auxillary function for parsing event specs in defevents/let-events, and
   turning them into event constructing functions."
  ([ename docstring args body]
    (list 'do
      (list 'defn ename (str "->event:" docstring) args
            (list `->event (keyword ename)  body))
      (list 'defn (symbol (str ename "?"))
            (str "Predicate that returns true if the event is " ename)
            '[e]
            (list '= (list :type 'e) (keyword ename))))) 
  ([ename args body]
    (list ename (list `fn  args (list `->event (keyword ename)  body))))
  ([ename docstring] (emit-event ename docstring '[data] 'data))
  ([ename] (emit-event ename
             (str " Anonymous event constructor for " ename)
                 '[data] 'data)))

(defn- emit-letevent
  "Auxillary function for parsing event specs in defevents/let-events, and
   turning them into event constructing functions."
  ([ename args body]
    (list ename ;lexical event constructor fn
              (list `fn  args (list `->event (keyword ename)  body))
          (symbol (str ename "?")) ;event predicate 
              (list 'fn '[e] (list '= (list :type 'e) ename))))
  ([ename] (emit-letevent ename '[data] 'data)))


;Macros for defining event constructors and other goodies! 
(defmacro defevents
  "Macro for defining new event constructors via top-level defn bindings.
   In addition to a like-named constructor of type (args->event), defevents 
   also provides a like-named predicate suffixed with '?'. In lieu of using 
   (fn [e] (= (:type e) :myevent)), one can eval (myevent? e) 
 
   Input bindings are similar to letfn binds:
     (name) 
     (name [args] body) 
     (name doc [args] body)
   If no args are supplied, a generic argument is provided and passed as the 
   event's data field.
   
   If no docstring is supplied, a generic docstring is provided.    
   Args are available inside the evaluation of body."
  [bindings]
  (let [doc (fn [syms]
               (if (= 2 (count syms))
                 (cons 'def syms)
                 syms))]
    `(do ~@(map #(doc (apply emit-event %))  bindings))))

(defmacro let-events
  "Macro for binding (possibly new) events. In addition to a like-named 
   constructor of type (args->event), let-events also provides a like-named 
   predicate suffixed with '?'. 
   
   In lieu of using (fn [e] (= (:type e) :myevent)), one can eval (myevent? e) 
 
   Input bindings are similar to letfn binds:
     (name) 
     (name [args] body) 
   If no args are supplied, a generic argument is provided and passed as the 
   event's data field.  Docstrings are invalid inside of let-events.  
   
   Args are available inside the evaluation of body."     
    [bindings body]
      `(let [ ~@(mapcat #(apply emit-letevent %) bindings)]
         ~body))        

(defn eventdoc
  "Returns the documentation of an event, if any exists.  Not quite working
   how I would like it to...."
  [evt]
  (let [sym (if (= (type evt) DEVS.events.event)
              (symbol (subs (str (:type evt)) 1))
              (quote evt))]
    (:doc (meta (resolve sym)))))


;(def proc-ops 
;  (ref '(conj-events! (fn 
;                        ([ec evts ecoll]
;                          (set-events ec 
;                            (add-events evts ecoll)))
;                       ([ec ecoll]
;                          (set-events ec
;                            (add-events (get-events ec) ecoll)))
;                       ([ecoll] 
;                         (set-events *context* 
;                           (add-events *events* ecoll))))
;         clear-events! (fn ([] (set-events *context* []))
;                           ([ec] (set-events ec [])))
;         set-state!    (fn ([v] (set-state *context* v))
;                           ([ec v] (set-state ec v))))))
    
;Functions for defining and evaluating computations that involve IEventContexts. 
(defmacro with-context
  "Given a context map, with :state and :events keys, typically an eventcontext.
   Additional bindings can be supplied, similar to let, and will be valid inside
   of body."
  ([context bindings body]
  `(let [~'*context* ~context
         ~'*state* (get-state ~context)
         ~'*event* (next-event (get-events ~context))
         ~'*events* (drop-event (get-events ~context))
;         ~@(deref proc-ops)
         ~@bindings]
     ~body))
  ([context body] `(with-context ~context [] ~body))) 


(defmacro process
  "Defines a context processor that operates in/on event contexts.  Binds the 
   [*state* *events* *context* *event*] to the event context.  If body evals 
   to a non-nil value, returns the value (should be an updated eventcontext), 
   else returns the original context."
  [body] 
  `(fn [~'context] 
     (do (if-let [res# (with-context ~'context ~body)]
           res#
           (drop-event ~'context)))))

(defn type-filter
  "Returns a filter function for event contexts, which yields true if the type 
   of the next event is t."
  [t] 
  (fn [ec] (= (next-type ec) t)))

(defn make-route
  "Simple constructor for defining routes.  This is akin to sample from 
   funtional reactive programming (in cljgui.behavior)"
  [f] #(f %))

(defn forever-route
  "Defines a route that returns a constant value, no matter the context."
  [v]
  (make-route (fn [_] v)))

(defn route-value
  "Routes eventcontext through routef."
  [ec routef]
  (routef ec))

(defn map-route  
  [f routef] 
  (make-route (fn [ec] (f (routef ec)))))

(defn lif1-route 
  [f r]
  (map-route f r))

(defn lift2-route 
  [f2 r1 r2]
  (make-route (fn [ec] (f2 (r1 ec) (r2 ec)))))

(defn lift3-route 
  [f3 r1 r2 r3] 
  (make-route (fn [ec] (f3 (r1 ec) (r2 ec) (r3 ec)))))

;(defn wait [shift bf]
;  (sample (fn [{:keys [t] :as bc}] (bf (assoc bc :t (+ shift t))))))
;
;(defn faster [scalar bf]
;  (sample (fn [{:keys [t] :as bc}] (bf (assoc bc :t (* scalar t))))))

;This may be deprecated very very soon....   
(defn filter-route
  "Build a handler that only fires when pred is true, else passes event context"
  [pred r] 
  (make-route 
    (fn [ec] (if (pred ec) (r ec) ec))))

;(defn multi-route 
;  "Given a map of partition names to routefunctions, returns a routing function 
;   that converts an event context into a map of partition names to event 
;   contexts.  This is a 'dumb' mechanism, in that multiple partitions can share
;   identical functions"
;  [routemap]
;  (let [routenames (keys routemap)
;        routes (vals routemap)]
;    (make-route (fn [ec] (zipmap routenames (map #(% ec) routes))))))

;(defn when-route 
;  "Build a routing function that only fires when pred is true, 
;   else passes nil to indicate failed computation."
;  [pred f]
;  (fn [ec] (when (pred ec) (f ec))))

(defn thru-route 
  [f r]
  (fn [ec] (do (f ec) 
               (r ec))))

(defn debug-route [ec] (do (println ec) ec))
;(defn merge-route
;  "Build a routing function that merges the result of one or more routing 
;   functions.  Produces an event context that is the result of applying a 
;   merging function (typically a reduction, but not strictly) to the result 
;   of evaluating rfs against an event context."
;  [mergef & rfs] 
;  (fn [ec] (mergef (map #(% ec) rfs))))

(defn default-halt [ec] (zero? (count (pending-events (get-events ec)))))

(defn event-stepper
  "event-stepper is a general purpose generator that follows an eventful model
   of computation, in which events are drawn from an abstract event queue, and
   state updates are affected in response to the event information.  State
   transitions can only happen if there are pending events, and are realized via
   a state transition function.

   An event-stepper is a generator of type ::(IEventSeq b) =>
   (a->boolean) -> ((a,b) -> (a,b)) -> a -> b -> IEventContext(a, b)
   The final result is an event context (see IEventContext), where
   state is the final state of the computation, and events is an IEventSequence 
   of pending events (if any).  
  
   Stops generating when halt criteria, applied to current eventcontext, 
   returns true.  If no halt criteria is provided, the computation stops when 
   there are no events remaining.  

   If there are events, and the state has not caused a halt, we invoke transf, 
   a transition function, on the current event context."
  ([haltcriteria transf initstate initevents]
    (generate haltcriteria transf (->eventcontext initstate initevents)))
  ([transf ec] 
    (event-stepper default-halt transf (get-state ec) (get-events ec))))

(defn event-seq
  "Identical to event-stepper, except it produces a sequence of event context."
  ([haltcriteria transf initstate initevents]
   (unfold haltcriteria transf  (->eventcontext initstate initevents)))
  ([transf ec] 
    (event-seq default-halt transf (get-state ec) (get-events ec)))) 

;Simple routing function....only dispatchs on a collection of event types for 
;now.  Still in development.
(defn route
  "Return a function:: IEventContext -> a , that applies f to an IEventContext 
   only when the next-event in the context is of a type specified by eventtypes.
   If no eventtypes are supplied, assumes a default match on everything." 
  [typecoll f]
	  (let [typeset (into #{} typecoll)] 
	    (filter-route #(typeset (next-type %)) f)))

(defn map-route [f routef] 
  (fn [ec] ((comp f routef) ec)))

;;testing 
(comment 
;The following functions are simple simulations, in that they represent simple
;functions that use events to communicate significant information, an handlers 
;to propogate changes to the context.  Evaluation continues until there are no 
;events...and hence no further values to compute.  

(defn get-line [] (deref (future (read-line))))
(defn ucase [s] (.toUpperCase s))

(defn simplegreet [n]
  (let-events [(hello)]
      (->> (add-event emptycontext :hello)
           (event-seq (route [:hello] (println "Hello! " (get-line))))
           (take n ))))

(defn ->newname 
  [name] 
  (map->event {:type newname :data name}))

(defn greeter
  "A function that, given an initial event context, will poll for a user's name
   and then reply with a greeting."
  ([ec]
		(let-events [(newname) ;(newname) = (->event :newname :data nil)
	              (getinput)]
		   (let [quit? #(= (ucase (event-data %)) "QUIT")	           
		         ui    (route [:getinput]
                      (process 
	                      (do (println "Please type your name!")
	                          (set-events *context* 
	                            (add-events *events* [(newname (get-line))])))))
		         greet (route [:newname]
	                     (process 
		                     (when (and (not (nil? *state*)) (not= *state* ""))
		                       (do (println (str "Greetings,  " *state*  \!))
		                         (set-events *context* 
		                           (add-event *events* (getinput :keyboard)))))))
		         nm    (route [:newname]
                      (process
			                  (if (quit? *event*)
			                    (set-events *context* [])
	                        (-> *context*
	                          (set-state (event-data *event*))
	                          (greet)))))]	           	 
	      (event-stepper #((comp nm ui) %) ec))))
  ([] (greeter (add-event emptycontext (->event :getinput :keyboard)))))



;;Sample output
;=> (greeter)
;Please type your name!
;=>Tom
;Greetings,  Tom!
;Please type your name!
;=>Kirk
;Greetings,  Kirk!
;Please type your name!
;=>Anon 7
;Greetings,  Anon 7!
;Please type your name!
;=>Garth
;Greetings,  Garth!
;Please type your name!
;=>Quit
;#DEVS.events.eventcontext
;{:state "Garth", 
; :events [#DEVS.events.event{:type :getinput, :data :keyboard}]}

(defn greeter-frp
  "A function that, given an initial event context, will poll for a user's name
   and then reply with a greeting."
  ([ec]
		(let-events [(newname)
	              (getinput)]
		   (let [quit? #(= (ucase (event-data %)) "QUIT")	           
		         ui     (->> (process
                            (set-events *context* 
                                         (add-events *events* 
                                           [(newname (get-line))])))                                                                                     
                         (filter-route (type-filter :getinput)))
		         greet  (->> (process 
                            (when (and (not (nil? *state*)) (not= *state* ""))
                              (do (println (str "Greetings,  " *state*  \!))
                                (set-events *context* 
                                   (add-event *events* (getinput :keyboard))))))
                         (filter-route (type-filter :newname)))
		         nm     (->> (process
                            (if (quit? *event*)
                            (set-events *context* [])
                              (-> *context*
                                (set-state (event-data *event*))
                                (greet))))                          
                         (filter-route (type-filter :newname)))
            terminate nil]	           	 
	      (event-stepper #((comp nm ui) %) ec))))
        ;(event-stepper ui ec))))
  ([] (greeter (add-event emptycontext (->event :getinput :keyboard)))))


 (comment ;in development...
;Event Routing -> I realized, through the course of development, that I'd 
;unintentionally stumbled across the map-based routing functionality that 
;HTTP libraries like Ring and Compojure use to handle HTTP responses.
;I think it makes sense, and provides a simple dispatch mechanism that can be 
;composed and inherited via simple operations on maps. 

;A route is a simple map of event-type to some function that handles 
;event context.

;A route basically encodes a match statement that dispatches on event-type.
;Every route derives from :default, where the associated routing is just the 
;identity function.  
(def default-route {:default identity})

;Should I enforce default-route here? 
(defn make-routenet
  "Given a binding vector of [:eventtype1 routefunction1
                               ...
                              :eventtypeN routefunctionN]
   returns a map of the routes.  Useful for inline route definitions."
  [& binds]
  (reduce conj {} (map vec (partition 2 binds))))

;In observers, we define routes explicitly using maps, filters, merges....
;Can we do the same with event routes? 

;An event route is, after all, just a composition isn't it? 


(defn find-route
  "Given an event, and a routing network, try to route the event through 
   the routing network.  A route is found if the event-type of e is 
   a key in the routenet.  The associated value for (event-type e) will return
   a function that can process the event.  If e is not accounted for, a default
   routing is returned from the routenet."
  [e routenet & default]
  (let [f (get (event-type e) routenet 
               (if (first default) 
                   (first default)
                   (get :default routenet)))]
    (f e)))

(defn serial-handler
  "Returns a new function that is the composition of N handler functions. 
   While there are events, each handler function is invoked, in order, against 
   the output of the previous handler.  If there are no events, then the 
   remaining handlers are bypassed.  Allows for short-circuiting by having a 
   handler remove events."
  [handlers] 
  (fn [ec]
    (loop [ctx ec
           hs handlers]
      (if (and (> 0 (count (get-events ctx))) (seq hs))
        (recur ((first hs) ctx) (rest handlers))
        ctx))))     

(defn route-dispatch
  "Given some criteria, apply the criteria to event e to determine how it 
   it should be routed (a function to use on e)"
  [criteria e]
  (cond (keyword? criteria) (fn [e] (= (event-type e) criteria))
        (set? criteria) (fn [e] (contains? criteria (event-type e))))
        (vector? criteria) (fn [e]
                           (lazy-seq 
                             (or ((first criteria) e) 
                                 (route-dispatch (rest criteria e)))))
                           
        (fn? criteria) (criteria e)
        :else (throw (Exception. (str "unknown dispatch criteria!" criteria))))

(defn event-route
  "Return a function:: IEventContext -> a , that applies f to an IEventContext 
   only when the next-event in the context is of a type specified by eventtypes.
   If no eventtypes are supplied, assumes a default match on everything." 
  ([typecoll f]
	  (let [typeset (into #{} typecoll)] 
	    (filter-h #(typeset ((comp event-type next-event get-events) %)) f)))
  ([f] (fn [ec] (f ec))))

)

)