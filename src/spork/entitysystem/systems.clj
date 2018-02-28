;; ;A namespace for defining systems that operate on entitystores to compute
;; ;results, usually dynamic processes (i.e. simulations or games).
;; (ns spork.entitysystem.systems
;;   (:use [spork.entitysystem.store]
;;         [DEVS.events]))
;; 
;; ;-----Why Systems?
;; ;How do we operate on the entities in the entitystore?  In component based
;; ;architectures, they typically define Systems, or subsystems, which operate on
;; ;pre-defined components (domains of data).  These systems reference an entity
;; ;store and typically mutate the abstract entity state, but only across specific
;; ;components.  This allows both fine-grained, and broad updating of the entities.
;; 
;; ;What are systems?  A system is conceptually a function that operates on
;; ;components (i.e. tables of records of a component type).
;; ;Systems declare which components they operate on during definition, along with
;; ;a function :: entitystore -> [{event records}].  From the entitystore's point
;; ;of view, systems are pure functions; they do not mutate the store.  However,
;; ;to achieve useful effects (like advancing a simulation, or getting a mobile
;; ;entity to change its position), we need to capture the result of processing
;; ;the entity store with a system.  We "can" capture the effects simply by
;; ;having a system produce a collection of event records. 
;; 
;; ;Event records are a means of communicating change, and they can be fine-grained
;; ;(verbose, incremental changes are communicated), or coarse (sparser event
;; ;traffic, with large bulk changes to be executed concurrently).
;; 
;; ;In this case, we can view the system as a function that maps state (an entity
;; ;store) to a set of work to be done, likely to generate a new state. 
;; ; system:: entitystore -> [event]
;; 
;; ;This makes them amenable to static typing, and allows us to determine where
;; ;systems are unrelated (i.e. they operate on independent component data).
;; 
;; ;helper functions...
;; (defn get-line
;;   ([] (deref (future (read-line))))
;;   ([timeout-ms] (deref (future (read-line)) timeout-ms :timeout)))
;; 
;; (defn timeout? [v] (= v :timeout))

;; (defn greeter [ec]
;; 	(letevents [(hello)
;; 	            (getname)]
;; 	   (let [sayhello (fn [ec]
;; 										  (with-context ec
;; 										    (do (println (str "hello " state  \!))      
;; 										         ec)))
;; 	         getname (fn [ec]
;; 	                   (do (println "Please type your name!")
;; 	                     (with-context ec
;; 	                       (-> ec 
;; 	                         (set-state (get-line)) 
;; 	                         (set-events 
;; 	                           (add-events (drop-event events) [:hello :getname]))))))
;; 	         inputhandler (assoc-handler getname :getname)
;; 	         hellohandler (assoc-handler sayhello :hello)]
;; 	     (hellohandler (inputhandler ec)))))
     
;; ;(def quithandler (make-handler (fn [s es] (
;; 

;; (comment 
;; ;we create a predicate that checks the store for "any" entity that has a
;; ;termination request component.  In this case, the mere existence of a request
;; ;will trip the predicate.
;; (defn terminate? [store]
;;   (has-entity? store :terminate)) 
;; 
;; (defn initializer
;;   "Defines behavior that happens when an  event is detected.  This is
;;    the default behavior for most simulations, and ends up as the first event in
;;    the event sequence.  Takes a function f::(IEventSeq b)=>(a,b)->(a,b), and
;;    applies it when : events are next in the queue."
;;   ([f] (partial handle-when initialized? f))           
;;   ([] (initializer (fn [[state events]] [state (drop-event events)]))))
;;    
;; (defn keyboard-sys
;;   "Reads input from the keyboard, conjoins the event onto the event stream."
;;   [[state evts]]
;;   (let [ln (get-line)]
;;    [state (add-event evts (got-text :newline ln 0))]))
;; 
;; ;event-handler functions map the result of an event onto the state.
;; ;They are used by systems to update state.
;; ;However, since we can add new events by handling events....we need to
;; ;allow the handlers to have access to the [state events] pair as well.
;; ;Handlers then are ::(IEntityStore a, IEventSeq b)=> (a,b) -> event -> (a,b)
;; ;I guess the difference is that we use them as building blocks for
;; ;larger systems via functional composition.
;; ;this is gross, but i'll work on it..
;; (defn handle-keys [[s events]]
;;   (->> s
;;     (handle-when got-text?     
;;           #(let [ [[store evts] event] (advance-event %)
;;               entry (->> event :data :data )]
;;                   (if (or (= entry "")
;;                   (= entry "quit"))
;;                     (do (println "KeyHandler: Terminating")
;;                         [(add-record store :terminate :simulation entry) evts])  
;;                     (do (println (str "KeyHandler: Storing " entry))
;;                         [(add-record store :input :keyboard entry) evts]))))))
;; 
;; ;(defn keyboardsim [] 
;; ;  (eventloop emptystore terminate? keyboard-sys key-handler))
;; 
;; (defn keyboardsim [] 
;;   (event-stepper  terminate? (comp println keyboard-sys) emptystore))
;; )
;; 
;; 
;; ;We define a event-derivative as something that describes change, as a function of
;; ;initial state, and pending changes (events), .
;; 
;; ;We define an integral as
;; 
;; ;I think it would be nice to allow a defsystem macro, which allows us to
;; ;define entitsystems in a clear and concise manner....
;; ;(defsystem keyboard-input
;; ;   [binding1 (function-applied-to-state)
;; ;    binding2 (function-applied-to-state)]
;; ;   (function of state & bindings that
;; 
;; ;an entity system should extract the components it cares about from an
;; ;entity store, so we can probably just provide component names for it, and
;; ;those will be automatically bound...
;; 
;; ;the signature is still ::(IEntityStore a, IEventSeq b) =>
;; ;                          (a,b) -> (a,b).....on the surface...
;; ;What's happening inside?
;; ;Try a simple example: for all entities with a textual component,
;; ;render the text.
;; 
;; ;so an entitysystem might look like
;; ;entitysystem textrenderer
;; ;  selectedEntities are IO printed, without performing changes,
;; ;    where selectedEntities are
;; ;       entities in the store, that have textual and active components.  
;; 
;; ;Thus, textrenderer operates on entities that are both textual and visible,
;; ;      passes the selected entities through a function that prints their text
;; 
;; ;textrenderer is a passive system, in that it is incapable of modifying state,
;; ;and its functionality is not dependent on events.
;; 
;; 
;; 
;; ;how would a combat system work?
;; 
;; ;entitysystem healthsystem
;; ;    entitieswithnohealth die
;; ;       changing (dropping from active, adding to dead)
;; ;   entitiesthatlosthealth
;; ;       changing (health -> (decrement health losthealth))
;; ;
;; 
;; ;entity comprehension...
;; ;  (as-events [ent (query-entities store [:health :healthchanged])]
;; ;    (decrement-health! ent))
;; ; or
;; ;  (decrement-health! (query-entities store [:health :healthchanged]))
;; ;  (let [entitiesthatlosthealth (query-entities store [:health :healthchanged])
;; ;        entitiesthatdied (query-entities       
;; 
;; 
;; ;entitysystem attacksystem
;; ;  attackingentities attack
;; ;    changing (decrementing) the health of their targets
;; ;    changing (decrementing/maybe dropping from) their own attacksremaining
;; ;  where attackingentities are entities that (havetargets and attacksremaining)
