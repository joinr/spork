(ns spork.sim.pure.behavior)

;in our functional reactive view, we have a set of combinators that replace 
;our propogators of data (observables), and our consumers/transformers of 
;data (observers).

;Specifically, observeables are replaced by behaviors...
;we use sample, or sample-behavior to define a new behavior on a function.
;the new behavior is a function of a behavior context, that feeds the context
;to the function (simple function application), or a simple map.

;in FRP, we don't have an exact analogue for subscribers, or functions that 
;maintain state (although we can!).  We use subscribers to serve as a 
;replacement for function composition in the observer world.  What we 
;really mean to do, is to compute a function result through each composed 
;behavior. 


;There are obvious analogues to FRP....
;In FRP, we have a similar idea of an observable/observer relationship, in which
;some notification is passed into a function network.
;In this case, notification is guaranteed to return a result (it ends up being
;function application of a large, complicated function pipeline to the 
;notification data).

;Like the observer/observable symbiosm, we define means to compose functions 
;that generate functions of notifications->'a.

;The context is very similar, in fact we could use pure semantics, with a 
;similar signature, to define the actions of FRP that look identical to 
;the observable case.

;Rather than notify!, we simply read-value (per the examples below).
;There is no update!, since we're building larger and larger composed functions.

;In FRP, then, subscribers are unnecessary....


;(defn sample
;  "Sample is the basic operation for defining a behavior or a reactive function.
;   It implies that we wish to evaluate function f relative to a behaviorcontext.
;   It's just a function builder....that's all FRP ends up being: elegant 
;   function building and composition..."
;  [f]
;  (fn [bc] (f bc)))

(defn reaction
  "Given a function f, return a reactor that maps f to arguments passed to 
   it via react calls."
  [f]  
  (fn [arg] (f arg)))

;(defn read-value
;  "read-value is a convenience function that communicates the sampling or 
;   evaluation of a behavior at time t.  Since we are most often concerned with 
;   time-varying behaviors, we provide a convenient way to 'pass' time to 
;   our behaviors.  Note that behind the scenes, we are just creating a behavior 
;   context, with the value for t supplied.  This allows extension for later 
;   behaviors that may require more than just time as an input."
;  [t bf]
;  (bf (->behaviorcontext t)))

(defn ->env
  "A function to define a simple reactive context.  Most reactions use time as 
   an input, so we bundle a map with some state and a t value."
  [& {:keys [t state] :or {t 0 state nil}}]
  {:t t :state state})

(defn ->time [t] (->env :t t))
(defn time-range [n] (map ->time (range n)))

(defmacro with-time [env & body]
  `(let [t# (get env :t)]
     ~body))

(defn sample-reaction
  "Evaluate the reaction against arg, where arg is a compatible reaction 
   context (likely a map)."
  [arg reaction] 
  (reaction arg)) ;looks like a monad!

(defn sample-across [xs reaction] (map #(sample-reaction % reaction) xs)) 

(defn bind-reaction ;looks like a monad! 
  "Return a reaction that samples reaction1, then samples reaction2 with the
   sample from reaction1."
  [r1 r2] 
  (fn [arg] 
    (sample-reaction r2 (sample-reaction r1 arg))))

(defn alter-atom! [a v]
  (compare-and-set! a @a v))

;behavior context is all the state we need to invoke a functional reactive 
;behavior.  In this case, it's a structure, with a t (for time) element. 
;We define all of our functional reactive combinators as functions of 
;a behaviorcontext to some result....that way, we can expand the behavior 
;context (adding additional parameters to it) as necessary, without breaking 
;functionality.  
;(defrecord behaviorcontext [t])

;behavior context can be abstracted....

;helper functions to wrap some java Math!
(defn degrees->rads [x] (Math/toRadians x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn abs [x] (Math/abs x))
(def pi Math/PI)

(defn forever
  "Forever is incredibly useful, and pops up a lot.  It is a primitive operation
   for defining a behavior that, regardless of the context, returns an initial
   value."
  [n]
  (reaction (fn [_] n)))

;timef is also a primitive block for a lot of reactive composition.  It extracts
;time from the behavior context.
(def timef (reaction (fn [bc] (:t bc))))
;wiggle is a primitive block for defining smoothly varying behaviors.  Using a 
;sine function, it will extract the time component from the behavior context and 
;smoothly vary between [-1, 1]
(def wiggle (reaction (fn [bc] (sin (* (:t bc) pi)))))
;waggle is akin to wiggle, but phase-shifted by (/ 2 pi), and thus uses the 
;cosine function.  It also produces smoothly varying values between [-1, 1], 
;but perpendicular to wiggle.  
(def waggle (reaction (fn [bc] (cos (* (:t bc)  pi )))))

;Combinators! 
(defn map-reaction
  "Mapping is a basic combinator for behaviors (reactive functions).  
   We simple return a new behavior, which first evalutes the input behavior, bf,
   and returns the result of applying f to the evaluated behavior.  The net 
   effect is the composition of f and the input behavior.  This assumes that 
   f is of type ('a->'b), and that reaction is of type (context->'a).
   The result of the map is a new reaction, (context->'b)"
  [f r]
  (reaction (fn [arg] (f (r arg)))))

;Lifting functions...
(defn lift1-reaction
  "Lifting is akin to mapping.  It allows to take a normal function, like a 
   squaring function, which operates on numbers, and wire it so that it will  
   operate on behaviors.  Citizens of the Republic of Haskell use this a lot, 
   particularly in the context of Monads.  All we need to know is that 
   f is a 'normal' function, bf is a function that requires a behavior context 
   for evaluation, and that lift1 will allow us to create a new behavior that 
   applies f to the result of applying bf to a behavior context."
  [f r]
  (map-reaction f r))

(defn lift2-reaction
  "Lift2 allows us to lift functions that have 2 arguments - like +.  The 
   concept is identical to map or lift1."
  [f r1 r2]
  (reaction (fn [arg] (f (r1 arg) (r2 arg)))))

(defn lift3-reaction
  "Lift3 allows us to lift functions that have 3 arguments into new behaviors."
  [f r1 r2 r3]
  (reaction (fn [arg] (f (r1 arg) (r2 arg) (r3 arg)))))

(defn liftn-reaction 
  "Lift a a function over an arbitrary number of reactions.  Thanks lisp!"
  [f & rs]
  (reaction (fn [arg]
           (apply f (map #(% arg) rs)))))

(defn filter-context 
  "Returns a behavior that engages the behavior for values of t where the 
   predicate is true, nil otherwise.  This is a useful combinator, but I have
   not engaged it - yet!"
  [predicate r] 
  (reaction (fn [arg] (if (predicate arg) 
                        (r  arg)
                        nil))))
 
(defn filter-reaction
  "Returns a behavior that engages the behavior for values of t where the 
   predicate is true, nil otherwise.  This is a useful combinator, but I have
   not engaged it - yet!"
  [predicate r] 
  (reaction (fn [arg] (let [res (r arg)]
                        (if (predicate res) 
                          res
                          nil)))))

(defn wiggle-between [bound] 
  (lift2-reaction * wiggle (forever bound)))

(defn waggle-between [bound]
  (lift2-reaction * waggle (forever bound)))

(def wiggle1 (wiggle-between 1))
(def wiggle100 (wiggle-between 100))
(def waggle1 (waggle-between 1))
(def waggle100 (waggle-between 100))

(defn wait-reaction [shift r]
  (reaction 
    (fn [{:keys [t] :as ctx}] (r (assoc ctx :t (+ shift t))))))

(defn faster [scalar r]
  (reaction (fn [{:keys [t] :as ctx}] (r (assoc ctx :t (* scalar t))))))

(comment ;testing 

;Example behavior.  v is bound to a function of type 
;(behaviorcontext -> float).  We can apprehend the value of v at any given time, 
;which will always be 123.0, by evaluating v with a behaviorcontext.
(def v (forever 123.0))
         
;example of reading a constant behavior that evaluates to 42.0 
;At time t <- 1.5, we evaluate the forever 42.0 behavior and return, not 
;surprisingly, 42.0 !
(->> (forever 42.0)
     (sample-reaction 1.5))
;example of reading a temporal identitiy behavior (= time-out time-in), 
;that, sampled at t <- 1.5, returns 1.5! 
(->> timef
     (sample-reaction {:t 1.5}))
;equivalently...
(->> timef
     (sample-reaction (->time 1.5)))

;example of reading a wiggle behavior, based on the sin function, which 
;shoud continously vary between [-1, 1] over input values of 
;[(* t -pi),(* t pi)]
(->> wiggle
     (sample-reaction (->time 1.5)))

;We exploit our newfound power of composition by squaring any time passed into
;a behavior.  We simply map a square function, which returns (* n n) for any n,
;onto the timef behavior, which extracts time from any behavior context.  
;Note the compositional style and the use of the ->> macro.  This allows us 
;to cleanly illustrate the causal chain, where timef is passed as the last 
;argument to map-b (which expects a behavior function).  map-b is then evaluated
;with the (non-behavior) squaring function as its functional argument, and 
;timef as its behavioral argument.
(def squared (->> timef
               (map-reaction (fn [n] (* n n)))))
;Evaluating our squared behavior at time 9.0, or a behaviorcontext with t <- 9.0
;yields 81.
(->> squared
    (sample-reaction (->time 9.0)))

;Some simple examples of lifting, and how they apply to behaviors.....
;added is bound to the result of lifting the + function onto two behaviors: 
; wiggle (our sin-based varying behavior between [-1, 1], and timef, our 
;temporal identity function ({:time a} -> a).
(def added (lift2-reaction + wiggle timef))

;We read the value of added at t <- 20, and should get:
;(+ (sin (* 20 pi)) 20)

;  (sin (* 2 pi)) = 0
;  (identity 20) = 20
(->> added 
  (sample-reaction (->time 20)))
;19.999999999999996  we get less than 20 due to floating point error, 
;since sin ends up being a VERY small negative value near 0, but close enough!

;We should get the value of (sin (* 0.5 pi)),  which is (sin (/ pi 2)), which is
(->> wiggle
   (sample-reaction (->time 0.5)))
;1.0 

)

;I wonder how we'll do this...
;I think we intrinsically need to have observers notifying mutation or state
;changes, that's what they're good at.

;meanwhile, reactions evaluate complex function networks... 

(defn state-reaction
  "Allow agents/refs/atoms to mirror their changes as reactive values.
   Creates a reaction that signals pairwise changes in 
   state.  Signal values are vectors of [oldstate newstate].  Note - this 
   is somewhat experimental, and the agent behavior is currently a bit 
   ill-defined.  I am trying to work around threading issues by having an 
   intermediate atom, which actually fires off observation events.  The 
   agent makes changes to the atom asynchronously, which then fires its 
   own watch function."
  [mutable]
  (let [state (atom nil)]
    (do 
      (add-watch mutable :mutation 
             (fn [k m oldstate newstate]
               (alter-atom! state [oldstate newstate])))       
      (reaction 
        (fn [_] @state)))))

(defn splitmap-reaction
  "Useful in conjunction with split and merge.  Takes either a single function, 
   or 2 function args, and applies them to relative args in a vector of 
   reactions, returning a vector of reactions."
  ([fl fr [r1 r2]]
    (lift2-reaction vector (map-reaction fl r1) (map-reaction fr r2)))
  ([f reactions] (splitmap-reaction f f reactions)))

(defn multimap-reaction
  "Return a sequence of reactions that are the result of applying f via map-obs 
   over each of them in turn."
  [f reactioncoll]  
  (map (partial map-reaction f) reactioncoll))

(defn choose-reaction
  "Imported from the F# lib.  They use Some and None due to static typing, which 
   are members of the Option type.  We have nil in clojure, so it's used here.
   Might be able to deprecate this, as it's basically a filter."
  [f r]
  (reaction (fn [arg] 
              (if-let [v (f arg)]
                (sample-reaction r v)))))


(defn split-reaction
  "Split the reaction into a vector of 2 reactions.  Similar to split-with in 
   clojure's seq library.  The first or left reaction in the vector reacts on 
   args that passed the filter, while the right reacts on everything else."
  [filterf r]
  [(filter-reaction filterf r) (filter-reaction (comp not filterf) r)])


(defn reductions-reaction
  "This is called scan in F#, but is akin to reductions over a 
   sequence of events.  We maintain some state, inialized by init.
   f is a function of 2 args, state and the next value.  Each reduction
   replaces the old state with the value of the new reduction."
  [f init r]
  (let [state (atom init)]
    (reaction 
      (fn [arg] (let [init @state
                      result (f init (sample-reaction r arg))]
                  (do (alter-atom! state result)
                    result))))))
;
(defn sequential-reaction  ;might make sense to drop this one...
  "Produces a reaction that serializes input args. This checks to see if the 
   input arg is a non-map sequence.  Maps the reaction to each itm.  For 
   non-collections, reacts as if arg were a singleton."
  [r]
  (reaction
    (fn [arg]
      (if (and (not (map? arg)) (coll? arg))
        (map (fn [x] (sample-reaction arg x)) arg)
        (sample-reaction arg r))))) 

;makes more sense in the observer....
(defn partition-reaction 
  "This reaction decomposes results f n serial reactions and evaluates 
   [n0 n1 n2 N]."
  [n r] 
  (let [part-r (lift1-reaction (partial partition n) r)]   
    (reaction  
      (fn [arg]
        (if (and (not (map? arg)) (coll? arg))
          (sample-reaction arg part-r))))))  

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
  

(defn merge-reaction
  "Merge two reactions into a single reaction.  Effectively concatenates the 
   results of both reactions in a sequence."
  ([r1 r2]
    (lift2-reaction vector r1 r2))
  ([[r1 r2]] (merge-reaction r1 r2)))

(defn multimerge-reaction
  "Merge multiple reactions into a single reaction."
  [reactioncoll]
  (reduce merge-reaction reactioncoll))

(defn split-reaction [f r]
  [(choose-reaction (fn [arg] (if (f arg) arg nil)) r)
   (choose-reaction (fn [arg] (if-not (f arg) arg nil)) r)])

(defn pairwise-reaction
  "Returns a reaction based on r, that feeds arguments to 
   reactions in pairs of [oldarg, newarg].  Maintains some state to 
   keep track of the last argument."
  [r]
  (let [oldargs (atom nil)]
    (reaction 
      (fn [newargs]             
        (if-not (nil? @oldargs)
          (sample-reaction r [@oldargs newargs]))
        (do (alter-atom! oldargs newargs))))))
      
;(defn cyclical-reaction
;  "Returns a reaction based on r, that feeds arguments to 
;   reactions in pairs of [oldarg, newarg].  Maintains some state to 
;   keep track of the last argument.  When toggle-reaction updates, oldarg is 
;   rendered nil.  This allows the reaction to model cyclical processes.  The 
;   primary driver for this little FSM was a dumb drawing app, in which mouse 
;   coords had to be made contiguous via linear interpolation, but in which 
;   interpolation should 'stop' after mouse was released."
;  [toggle-reaction r]
;    (let [cycle (atom :ended)
;          endcycle (fn [_] (do
;                             ;(println "cycle ended")
;                             (alter-atom! cycle :ended)))
;          startcycle (fn [x args] 
;                       (do
;                           ;(println "starting cycle")
;                           (alter-atom! cycle :startcycle)
;                           (alter-atom! x args)))
;          _ (->> toggleobs 
;              (subscribe endcycle))]              
;			    (bind-observable 
;			      (fn [subscribers able obs]
;              (let [oldargs (atom nil)]
;			          (subscribe! origin
;				          (make-observer 
;				            (fn [newargs]
;                        (let [c @cycle
;                              res @oldargs]                  
;						              (do                      
;			                      (cond (= c :startcycle)
;                                    (do (alter-atom! cycle :cycling)
;                                        (alter-atom! oldargs newargs))
;                                  (= c :cycling) 
;                                    (do
;                                     (if-not (nil? res)
;                                       (update! obs [res newargs]))
;			                               (alter-atom! oldargs newargs))
;			                            (= c :ended)                            
;				                            (do ;(println "recycling" [res newargs])
;				                                (startcycle oldargs nil))))))))))
;			      origin)))
;
;(defn through-obs 
;  "Similar to map-obs, in that it returns an observable based on origin, 
;   which evaluates the function (f e), assuming with side-effects, 
;   and returns the original event.  This is useful for propogating 
;   the original signal, and just chaining side-effects together."
;  [f origin]
;  (bind-observable 
;    (fn [subscribers _ obs]
;      (subscribe! origin 
;         (make-observer #(update! obs (do (f %)
;                                        %)))))
;    origin))

;;accumulation and iteration.
  
;(defn first-observation
;  "Sticks around for the next observation, then detaches.  Returns a promise for
;   the observed value."
;  [origin]
;  (let [data (promise)
;        ob (atom nil)
;        o (make-observer 
;            #(do (deliver data %)                       
;               (clear-subscriber! origin @ob)))
;        _ (swap! ob (fn [_] o))]
;    (subscribe! origin o)
;    data))
;
;(defn take-observations 
;  "Takes n observations from obs, returning a promise for a sequence of values, 
;   then detaching like first-obs."
;  [n origin]
;  (let [data (atom [])
;        ob (atom nil) 
;        res (promise)
;        o (make-observer                  
;            #(do (swap! data conj %)
;                 (if (= (count @data) n) 
;                   (do (deliver res @data)
;                     (clear-subscriber! origin @ob)))))
;        _ (swap! ob (fn [_] o))]
;    (subscribe! origin o)
;    res))
;
;(defn await-observation 
;  "Waits for the next observation from obs, returning the value received 
;   during notification.  Identical to first-observation, except the promise is immediately de-refed, 
;   forcing the calling thread to block."
;  [origin]
;  @(first-observation origin)) 
;
;(defn await-n-observations 
;  "Waits for the next observation from obs, returning the value received 
;   during notification.  Identical to take-observations, except the promise is immediately de-refed, 
;   forcing the calling thread to block."
;  [n origin]
;  @(take-observations n origin))
;
;(defn add-value [v getnext] 
;  (lazy-seq 
;    (cons v (add-value (getnext v)))))
;
;
;(defn observation-seq
;  "Generalizes observables into lazy sequences.  Allows us to view them as 
;   infinite lists of data values.  Bridges the gap between producing and 
;   consuming events.  Note...this will cause a space leak for large event 
;   streams.  I have no idea how to mitigate that."
;  [origin]
;  (let [q (agent clojure.lang.PersistentQueue/EMPTY)
;        push-q (fn [v] (send q conj v))
;        flush-q (fn []  (loop [xs (seq @q)]
;                          (if (empty? xs)
;                            (do (await-observation origin) ;blocks 
;                              (recur (seq @q)))
;                            (do (send q 
;                                   (fn [_] clojure.lang.PersistentQueue/EMPTY))
;                              xs))))
;        buffer (atom [])
;        next-value (fn [] (if-let [s (seq @buffer)]
;                              (do (swap! buffer subvec 1)
;                                (first s))
;                              (let [xs  (vec (flush-q))]
;                                (do (swap! buffer (fn [_] (vec (rest xs))))
;                                  (first xs)))))]
;    (subscribe  (fn [observation] (push-q observation)) origin)
;    (lazy-seq (concat '() (repeatedly next-value)))))  
;
;
;(defprotocol IEmitter 
;  (begin-emit! [e] "Starts the emitter.")
;  (end-emit! [e] "Stops the emitter."))
;
;(defn emit [{:keys [status state]} stepf able]
;  (let [nextstate (stepf state)]
;    (if (nil? nextstate)
;      {:status :halted :state state}
;      (do (notify! able nextstate)
;        {:status :emit :state nextstate}))))
;
;
;(defn emit-loop [agtstate stepf able]
;    (let [{:keys [status state]} agtstate]
;      (case status 
;        :emit   (do (send-off *agent* emit-loop stepf able)
;                    (emit agtstate stepf able))
;        agtstate)))
;
;(defn start-emit [agtstate stepf able]
;  (do (send *agent* emit-loop stepf able)
;    (assoc agtstate :status :emit)))


;;an emitter is a generating function, that wraps an observable. 
;;It will repeatedly evaluate it's function f, and notify subscribers of 
;;the current state.
;(defrecord emitter [f source subscribers]
;  observable
;  (subscribe! [able obs] (default-subscribe subscribers able obs))
;  (notify! [able arg] (doseq [o (deref subscribers)] (update! o arg)))
;  (get-subscribers [able] subscribers)
;  (clear-subscribers! [able] (do (alter-atom! subscribers [])))
;  IEmitter
;  (begin-emit! [e] (do (send-off source start-emit f e)
;                     e))
;  (end-emit! [e] (do (send-off source (fn [{:keys [status state]}]
;                                        {:status :idle :state state}))
;                   e)))
;                                        
;(defn make-emitter [interval f init-state]
;    (->emitter f (agent {:status :idle  :state init-state}) (atom [])))  
;
;(defn get-input [] 
;  (let [res (read-line)]
;    (if (or (nil? res) (= res "") (= res ":quit"))
;      nil 
;      res)))
;
;
;(comment ;testing  
;
;(def line-reader (->emitter (fn [_] (get-input))
;                            (agent {:status :idle
;                                    :state nil})
;                            (atom [])))

;;Note -> there is a bug in nREPL that will cause this function to botch.
;;Specifically, it has to do with read-line, which is called from the 
;;get-input function.  For the time being, the evaluator is disabled. 

;(defn simple-evaluator
;  "Creates a simple evaluator that can evaluate expressions in context."
;  [& {:keys [map-func] :or {map-func identity}}]  
;  (->emitter (fn [state]
;               (map-func 
;                 (try  (eval (read-string (get-input))))
;                 (catch Exception e nil)))             
;               (agent {:status :idle
;                       :state nil})
;               (atom [])))
;
;(defn simple-repl [] 
;  (let [eva (simple-evaluator)
;        _ (subscribe pprint eva)]
;    (begin-emit! eva)))
;
;(defn eval-n [n] 
;  (let [eva (simple-evaluator) 
;        res (take-observations n eva)]       
;    (do (begin-emit! eva)
;        @res 
;        (end-emit! eva)
;        @res)))
;       
;
;(def word-stream (make-observable))
;(def wds (observation-seq word-stream))
;(doseq [n '[hello this is a long bunch of words goodbye]]
;     (notify! word-stream n))
;
;(def number-stream (->> (make-observable)
;                        (sequential-obs)))
;
;(def number-spewer (agent {:state :spew}))
;(defn spew-numbers [{:keys [state]}]
;  (case state
;    :spew  (do (notify! number-stream (rand-int 100))
;             (send *agent* spew-numbers)
;             (Thread/sleep 100)
;             {:state state})
;    :stop-spewing  
;          (pprint "Agent stopped spewing!")))
;
;(defn sample-numbers [n]
;  (let [res  (take-observations n number-stream)]
;    (do (send number-spewer spew-numbers)
;        (deref res)
;        (send number-spewer 
;          (fn [s] (assoc s :state :stop-spewing)))
;        @res)))
;     
;)        
        


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




                
              
                  
           



