;Tom Spoon Feb 13 2012.
;This is a library that aims to port the (IMO) nice events-as-data 
;infrastructure from F# into Clojure.  I have most of the core library 
;implemented, as well as a few extra convenience methods.

;The basic abstraction is that observations are just a sequence of events, 
;likely driven by IO.  We model observations with observables and observers:

;Observables serve as the notification mechanism for registering interested 
;parties (observers) and notifying them of new observations.

;Observerers serve as function appliers (usually in the form of side-effects, 
;but not always!).

;We use protocols to define observables and observers, both of which can be 
;easily extended to new data types or existing data.  This is particularly 
;useful for making "noisy" data, where we want to sync up some stateful 
;structure (a model) with a view.

;The primary motivation for this was a bad experience with event management in 
;Java interop.  It's asinine, and effective at hiding the data that's flowing 
;around, i.e. Events.  
;I remembered the nice stuff about F#, and its implementation of the Rx 
;framework, which allows trivial composition of events.  Why not implement it 
;natively in clojure?  The benefit is, we get a composable
;event framework for clojure, and it goes whereever clojure goes.

;Note -> 
;this is a stateful library. Observers can be composed and routed using all of 
;our favorite seq-related functions, like map, filter, etc.  but they are 
;implemented using a ref.  This is fine if we're dealing with IO, specifically 
;nasty GUI stuff (which Java/Swing make even nastier due their interface-madness
;event implementation).

;TBD: error-handling and stopping/disposal of handlers.  There are probably 
;     really nasty corner cases I am missing that will come back to bite.
;     F# does this via auto-detaching observers that fail gracefully.

;I could probably use a nice :eof message or something that's handled by 
;the observable, that propogates the notion that no more observations are 
;coming down the pipe, which would allow observers to disconnect.

;-------------Implementation------------
;The flow of control as exists in an observable/observer pipe is this: 
;  Notifications flow in to the observable, and notify! is called with the 
;  value of the notification. 
;  The observable passes the notification on to each subscribed observer by 
;  calling their update! function on the notification data.
;    --this propogates, since some observers might be dual-hatted as
;      as observables, and cause further propogation throughout the network.

;The observable/observer symbiosm represents an abstract data flow, not unlike
;sequences, where the data is conceptually a stream of events or notifications.
;Combinators are derived by changing the context in which an observer  
;subscribes to an observable......
;Normal subscription implies a relationship in which the observer's update 
;function is called against the notification. 

;To achieve mapping, filtering, etc., we override the observable's notion of 
;subscribe!, and compose special functions to operate on notifications that 
;implement the desired behavior.  The implementation magic is actually inside 
;of subscribe, since it defines a means of combination (or an adapter) through
;which a context may change.

;The primary mechanism in the observable case is to define compositions in terms
;of how new observers are subscribed to the observable.  So when we define a 
;map-obs, we actually define a function that builds an observer that creates
;a new observable, who's mechanism for subscribing observers is to apply a 
;user-supplied function to the input context, then updating subscribers with
;the result of the function.  The result is a conceptual "map" or a "lift" of 
;a function into the stream of notification....



(ns spork.sim.impure.observe)

(defprotocol observer 
  (update! [obs arg] "Inform the observer obs that arg has happened.  
                      This typically implies that obs will call a function
                      with arg as its argument."))

(defprotocol observable 
  (subscribe! [able obs] "Subscribe observer obs to observable able.")
  (notify! [able arg] 
   "For each subscriber of observable able, call their update! function with arg.")
  (get-subscribers [able] 
   "Return all subscribers associated with observable able")
  (clear-subscribers! [able] 
   "Remove all subscribers from observable able."))
(defn alter-atom! [a v]
  (compare-and-set! a @a v))

(defn make-observer
  "Given a function f, return an observable that maps f to arguments passed to 
   it via update! calls."
  [f]
  (reify observer 
    (update! [obs arg] (f arg))))

(defn subscribe
  "Return the result of subscribing an anonymous observer, that update!s with f,
   through notify! arguments from observable origin."
  [f origin] 
  (do (subscribe! origin (make-observer f))
      origin))

(defn- default-subscribe  [subscribers _ obs] 
    (if (satisfies? observer obs)
      (do (swap! subscribers conj obs)); able)
      (throw (Exception. "not a valid observer!"))))

(defn make-observable
  "Primitive function for building new observables.  Used as an entry point
   for making generic observables (which can be notified using any type)."
  ([subscribef]
	  (let [subscribers (atom [])]
	    (reify observable 
	      (subscribe! [able obs] (subscribef subscribers able obs))
        (notify! [able arg] (doseq [o (deref subscribers)] (update! o arg)))
        (get-subscribers [able] subscribers)
        (clear-subscribers! [able] (do (alter-atom! subscribers []))))))
  
  ([] (make-observable default-subscribe)))

(defn clear-subscriber!
  "Clears an observer from the observable, if it exists."
  [able obs]
  (let [subscribers (get-subscribers able)
        drop-obs (fn [observers] (vec (filter (fn [o] (not= o obs)) 
                                              observers)))]
    (swap! subscribers drop-obs)))
    

(defn observe-mutation
  "Allow agents/refs/atoms to broadcast their changes as observables.
   Creates an observable that notifies clients of pairwise changes in 
   state.  Notifications are vectors of [oldstate newstate].  Note - this 
   is somewhat experimental, and the agent behavior is currently a bit 
   ill-defined.  I am trying to work around threading issues by having an 
   intermediate atom, which actually fires off observation events.  The 
   agent makes changes to the atom asynchronously, which then fires its 
   own watch function."
  [mutable]
  (let [mutation-event (make-observable)]
    (do
      (add-watch mutable :mutation 
             (fn [k m oldstate newstate]
               (notify! mutation-event [oldstate newstate])))       
      mutation-event)))

(defn- bind-observable
  "Bind an observable to a base observable.  We create specialized behavior 
   via the subscriptionf function, which allows us to control the binding 
   context.  We keep track of the base observable source (via reference) 
   so that we can subscribe to observables at any level of composition."
  ([subscribef & base]
	  (let [b (first base)
          subscribers  (if b (get-subscribers b) (atom []))]
	    (reify observable 
	      (subscribe! [able obs] (subscribef subscribers able obs))
        (notify! [able arg] (doseq [o (deref subscribers)] (update! o arg)))
        (get-subscribers [able] subscribers)
        (clear-subscribers! [able] (do (alter-atom! subscribers []))))))
  ([] (make-observable default-subscribe)))

(defn map-obs
  "Map function f to the stream of observations arriving from origin.  Returns 
   a new observable."
  [f origin]
  (bind-observable 
    (fn [subscribers _ obs]
      (subscribe! origin (make-observer #(update! obs (f %)))))
    origin))  

(defn splitmap-obs
  "Useful in conjunction with split and merge.  Takes either a single function, 
   or 2 function args, and applies them to relative args in a vector of 
   observables, returning a vector of new observables."
  ([fl fr [origin1 origin2]]
    [(map-obs fl origin1) (map-obs fr origin2)])
  ([f observers] (splitmap-obs f f observers)))

(defn multimap-obs
  "Return a sequence of observers that are the result of applying f via map-obs 
   over each of them in turn."
  [f observercoll]  
  (map (partial map-obs f) observercoll))

(defn choose-obs
  "Imported from the F# lib.  They use Some and None due to static typing, which 
   are members of the Option type.  We have nil in clojure, so it's used here.
   Might be able to deprecate this, as it's basically a filter."
  [f origin]
  (bind-observable 
    (fn [subscribers _ obs] 
      (subscribe! origin 
        (make-observer (fn [arg] (if-let [v (f arg)]
                                   (update! obs v))))))
    origin))                   

;        [<CompiledName("Filter")>]
;        let filter f (w: IObservable<'T>) =
;            choose (fun x -> if f x then Some x else None) w

(defn filter-obs
  "Filter using a single argument function, filterf.  Returns an 
   observable that fires for observations where (= (filterf arg) true)"
  [filterf origin]
  (choose-obs (fn [arg] (if (filterf arg) arg nil)) origin))

(defn split-obs
  "Split the observation into a vector of 2 observables.  Similar to split-with in 
   clojure's seq library.  The first or left observable in the vector fires on 
   events that passed the filter, while the right fires on everything else."
  [filterf origin]
  [(filter-obs filterf origin) (filter-obs (comp not filterf) origin)])


;untested
(defn reductions-obs
  "This is called scan in F#, but is akin to reductions over a 
   sequence of events.  We maintain some state, inialized by init.
   f is a function of 2 args, state and the next value.  Each reduction
   replaces the old state with the value of the new reduction."
  [f init origin]
  (let [state (atom init)]
	  (bind-observable 
	    (fn [subscribers _ obs]
	      (make-observer 
         (fn [v] (let [init @state
                       result (f init v)]
                   (do (alter-atom! state result)
                     result))))) 
     origin)))

;untested 
(defn sequential-obs 
  "This checks to see if the observed arg is a sequence.
   For each itm in seq, updates subscribers itm under a doseq.
   For non-collections, updates purely on arg."
  [origin]
  (bind-observable 
    (fn [subscribers _ obs]
      (subscribe! origin
	      (make-observer 
	        (fn [arg]
             (if (coll? arg)
			          (doseq [v arg]
			             (update! obs v))
                (update! obs arg)))))) 
    origin))

(defn partition-obs 
  "This observer collects n observations and notifies its subscribers with 
   the result of [n0 n1 n2 N]."
  [n origin]
    (let [state (atom [])]
	    (bind-observable 
	      (fn [subscribers _ obs]
          (subscribe! origin
		        (make-observer 
	            (fn [v] (let [result (conj @state v)]
	                      (if (= n (count result))
	                        (do 
	                          (alter-atom! state []) 
	                          (update! obs result))
	                        (do 
	                          (alter-atom! state result))))))))
       origin)))                                                                                                    

;(defn toggle-obs 
;  "This observer effects a toggle that will apply f1 to the next observation, 
;   then f2 to the next observation, then f1, etc."

(comment 
  ;defines a simple printer that observes sequential events.  It takes 
  ;sequences of observations and applies update! to each, which in turn 
  ;invokes the subscribed println observer, who dutifully prints each 
  ;observation in the repl.
  (def seq-printer (->> (make-observable)
                        (sequential-obs)
                        (subscribe println)))
  (notify! seq-printer (range 10))
  ;defines a simple printer that aggregates events from a sequential 
  ;event source, accumulates them pairwise, applies str to the pairs
  ;and prints the strings.  This is nice from a data-flow perspective. 
  (def pair-printer (->> (make-observable)
                         (sequential-obs)
                         (partition-obs 2)
                         (map-obs (fn [[n1 n2]] (str n1 \, n2)))
                         (subscribe println)))
  (notify! pair-printer (range 200))
)
  

;somewhat tested
(defn merge-obs
  "Merge two observables into a single observable."
  ([origin1 origin2]
    (bind-observable 
      (fn [subscribers able obs]
        (let [h1 (subscribe! origin1 (make-observer #(update! obs %)))
              h2 (subscribe! origin2 (make-observer #(update! obs %)))]))
      origin1))
  ([[origin1 origin2]] (merge-obs origin1 origin2)))


;untested
(defn multimerge-obs
  "Merge multiple observables into a single observable."
  [observercoll]
  (reduce merge-obs observercoll))

;somewhat tested
(defn split-obs [f origin]
  [(choose-obs (fn [v] (if (f v) v nil)) origin)
   (choose-obs (fn [v] (if-not (f v) v nil)) origin)])

;tested
(defn pairwise-obs
  "Returns an observable based on origin, that feeds arguments to 
   subscribed observers in pairs of [oldarg, newarg].  Maintains some state to 
   keep track of the last argument."
  [origin]
    (bind-observable 
      (fn [subscribers able obs]
        (let [oldargs (atom nil)]
          (subscribe! origin
	          (make-observer 
	            (fn [newargs]             
	              (if-not (nil? @oldargs)
                  (update! obs [@oldargs newargs]))
	              (do (alter-atom! oldargs newargs)))))))
      origin)) 

(defn cyclical-obs
  "Returns an observable based on origin, that feeds arguments to 
   subscribed observers in pairs of [oldarg, newarg].  Maintains some state to 
   keep track of the last argument.  When toggleobs updates, oldarg is 
   rendered nil.  This allows the observer to model cyclical processes.  The 
   primary driver for this little FSM was a dumb drawing app, in which mouse 
   coords had to be made contiguous via linear interpolation, but in which 
   interpolation should 'stop' after mouse was released."
  [toggleobs origin]
    (let [cycle (atom :ended)
          endcycle (fn [_] (do
                             ;(println "cycle ended")
                             (alter-atom! cycle :ended)))
          startcycle (fn [r args] 
                       (do
                           ;(println "starting cycle")
                           (alter-atom! cycle :startcycle)
                           (alter-atom! r args)))
          _ (->> toggleobs 
              (subscribe endcycle))]              
			    (bind-observable 
			      (fn [subscribers able obs]
              (let [oldargs (atom nil)]
			          (subscribe! origin
				          (make-observer 
				            (fn [newargs]
                        (let [c @cycle
                              res @oldargs]                  
						              (do                      
			                      (cond (= c :startcycle)
                                    (do (alter-atom! cycle :cycling)
                                        (alter-atom! oldargs newargs))
                                  (= c :cycling) 
                                    (do
                                     (if-not (nil? res)
                                       (update! obs [res newargs]))
			                               (alter-atom! oldargs newargs))
			                            (= c :ended)                            
				                            (do ;(println "recycling" [res newargs])
				                                (startcycle oldargs nil))))))))))
			      origin)))

(defn through-obs 
  "Similar to map-obs, in that it returns an observable based on origin, 
   which evaluates the function (f e), assuming with side-effects, 
   and returns the original event.  This is useful for propogating 
   the original signal, and just chaining side-effects together."
  [f origin]
  (bind-observable 
    (fn [subscribers _ obs]
      (subscribe! origin 
         (make-observer #(update! obs (do (f %)
                                        %)))))
    origin))

;accumulation and iteration.
  
(defn first-observation
  "Sticks around for the next observation, then detaches.  Returns a promise for
   the observed value."
  [origin]
  (let [data (promise)
        ob (atom nil)
        o (make-observer 
            #(do (deliver data %)                       
               (clear-subscriber! origin @ob)))
        _ (swap! ob (fn [_] o))]
    (subscribe! origin o)
    data))

(defn take-observations 
  "Takes n observations from obs, returning a promise for a sequence of values, 
   then detaching like first-obs."
  [n origin]
  (let [data (atom [])
        ob (atom nil) 
        res (promise)
        o (make-observer                  
            #(do (swap! data conj %)
                 (if (= (count @data) n) 
                   (do (deliver res @data)
                     (clear-subscriber! origin @ob)))))
        _ (swap! ob (fn [_] o))]
    (subscribe! origin o)
    res))

(defn await-observation 
  "Waits for the next observation from obs, returning the value received 
   during notification.  Identical to first-observation, except the promise is immediately de-refed, 
   forcing the calling thread to block."
  [origin]
  @(first-observation origin)) 

(defn await-n-observations 
  "Waits for the next observation from obs, returning the value received 
   during notification.  Identical to take-observations, except the promise is immediately de-refed, 
   forcing the calling thread to block."
  [n origin]
  @(take-observations n origin))

(defn add-value [v getnext] 
  (lazy-seq 
    (cons v (add-value (getnext v)))))


(defn observation-seq
  "Generalizes observables into lazy sequences.  Allows us to view them as 
   infinite lists of data values.  Bridges the gap between producing and 
   consuming events.  Note...this will cause a space leak for large event 
   streams.  I have no idea how to mitigate that."
  [origin]
  (let [q (agent clojure.lang.PersistentQueue/EMPTY)
        push-q (fn [v] (send q conj v))
        flush-q (fn []  (loop [xs (seq @q)]
                          (if (empty? xs)
                            (do (await-observation origin) ;blocks 
                              (recur (seq @q)))
                            (do (send q 
                                   (fn [_] clojure.lang.PersistentQueue/EMPTY))
                              xs))))
        buffer (atom [])
        next-value (fn [] (if-let [s (seq @buffer)]
                              (do (swap! buffer subvec 1)
                                (first s))
                              (let [xs  (vec (flush-q))]
                                (do (swap! buffer (fn [_] (vec (rest xs))))
                                  (first xs)))))]
    (subscribe  (fn [observation] (push-q observation)) origin)
    (lazy-seq (concat '() (repeatedly next-value)))))  


(defprotocol IEmitter 
  (begin-emit! [e] "Starts the emitter.")
  (end-emit! [e] "Stops the emitter."))

(defn emit [{:keys [status state]} stepf able]
  (let [nextstate (stepf state)]
    (if (nil? nextstate)
      {:status :halted :state state}
      (do (notify! able nextstate)
        {:status :emit :state nextstate}))))


(defn emit-loop [agtstate stepf able]
    (let [{:keys [status state]} agtstate]
      (case status 
        :emit   (do (send-off *agent* emit-loop stepf able)
                    (emit agtstate stepf able))
        agtstate)))

(defn start-emit [agtstate stepf able]
  (do (send *agent* emit-loop stepf able)
    (assoc agtstate :status :emit)))


;an emitter is a generating function, that wraps an observable. 
;It will repeatedly evaluate it's function f, and notify subscribers of 
;the current state.
(defrecord emitter [f source subscribers]
  observable
  (subscribe! [able obs] (default-subscribe subscribers able obs))
  (notify! [able arg] (doseq [o (deref subscribers)] (update! o arg)))
  (get-subscribers [able] subscribers)
  (clear-subscribers! [able] (do (alter-atom! subscribers [])))
  IEmitter
  (begin-emit! [e] (do (send-off source start-emit f e)
                     e))
  (end-emit! [e] (do (send-off source (fn [{:keys [status state]}]
                                        {:status :idle :state state}))
                   e)))
                                        
(defn make-emitter [interval f init-state]
    (->emitter f (agent {:status :idle  :state init-state}) (atom [])))  

(defn get-input [] 
  (let [res (read-line)]
    (if (or (nil? res) (= res "") (= res ":quit"))
      nil 
      res)))


(comment ;testing  

(def line-reader (->emitter (fn [_] (get-input))
                            (agent {:status :idle
                                    :state nil})
                            (atom [])))

;Note -> there is a bug in nREPL that will cause this function to botch.
;Specifically, it has to do with read-line, which is called from the 
;get-input function.  For the time being, the evaluator is disabled. 

(defn simple-evaluator
  "Creates a simple evaluator that can evaluate expressions in context."
  [& {:keys [map-func] :or {map-func identity}}]  
  (->emitter (fn [state]
               (map-func 
                 (try  (eval (read-string (get-input))))
                 (catch Exception e nil)))             
               (agent {:status :idle
                       :state nil})
               (atom [])))

(defn simple-repl [] 
  (let [eva (simple-evaluator)
        _ (subscribe pprint eva)]
    (begin-emit! eva)))

(defn eval-n [n] 
  (let [eva (simple-evaluator) 
        res (take-observations n eva)]       
    (do (begin-emit! eva)
        @res 
        (end-emit! eva)
        @res)))
       

(def word-stream (make-observable))
(def wds (observation-seq word-stream))
(doseq [n '[hello this is a long bunch of words goodbye]]
     (notify! word-stream n))

(def number-stream (->> (make-observable)
                        (sequential-obs)))

(def number-spewer (agent {:state :spew}))
(defn spew-numbers [{:keys [state]}]
  (case state
    :spew  (do (notify! number-stream (rand-int 100))
             (send *agent* spew-numbers)
             (Thread/sleep 100)
             {:state state})
    :stop-spewing  
          (pprint "Agent stopped spewing!")))

(defn sample-numbers [n]
  (let [res  (take-observations n number-stream)]
    (do (send number-spewer spew-numbers)
        (deref res)
        (send number-spewer 
          (fn [s] (assoc s :state :stop-spewing)))
        @res)))
     
)        
        


;/// Creates two observables
;///  – left is triggered when the left mouse button is down
;         and the mouse is in the area (x < 100 && y < 100)
;///  – right is triggered when the right mouse button is down
;         and the mouse is in the area (x < 100 && y < 100)
;
;let left,right =
;  form.MouseDown
;    |> Observable.merge form.MouseMove
;    |> Observable.filter (fun args -> 
;          args.Button = MouseButtons.Left ||
;          args.Button = MouseButtons.Right)
;    |> Observable.map (fun args -> args.X, args.Y, args.Button)
;    |> Observable.filter (fun (x,y,b) -> x < 100 && y < 100)
;    |> Observable.partition (fun (_,_,button) -> button = MouseButtons.Left)




                
              
                  
           