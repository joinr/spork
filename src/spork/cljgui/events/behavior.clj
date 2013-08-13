(ns spork.cljgui.behavior)

;behavior context is all the state we need to invoke a functional reactive 
;behavior.  In this case, it's a structure, with a t (for time) element. 
;We define all of our functional reactive combinators as functions of 
;a behaviorcontext to some result....that way, we can expand the behavior 
;context (adding additional parameters to it) as necessary, without breaking 
;functionality.  
(defrecord behaviorcontext [t])

;helper functions to wrap some java Math!
(defn degrees->rads [x] (Math/toRadians x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(defn abs [x] (Math/abs x))
(def pi Math/PI)

(defn sample
  "Sample is the basic operation for defining a behavior or a reactive function.
   It implies that we wish to evaluate function f relative to a behaviorcontext.
   It's just a function builder....that's all FRP ends up being: elegant 
   function building and composition..."
  [f]
  (fn [bc] (f bc)))

(defn forever
  "Forever is incredibly useful, and pops up a lot.  It is a primitive operation
   for defining a behavior that, regardless of the context, returns an initial
   value."
  [n]
  (sample (fn [_] n)))

;timef is also a primitive block for a lot of reactive composition.  It extracts
;time from the behavior context.
(def timef (sample (fn [bc] (:t bc))))
;wiggle is a primitive block for defining smoothly varying behaviors.  Using a 
;sine function, it will extract the time component from the behavior context and 
;smoothly vary between [-1, 1]
(def wiggle (sample (fn [bc] (sin (* (:t bc) pi)))))
;waggle is akin to wiggle, but phase-shifted by (/ 2 pi), and thus uses the 
;cosine function.  It also produces smoothly varying values between [-1, 1], 
;but perpendicular to wiggle.  
(def waggle (sample (fn [bc] (cos (* (:t bc)  pi )))))

;Example behavior.  v is bound to a function of type 
;(behaviorcontext -> float).  We can apprehend the value of v at any given time, 
;which will always be 123.0, by evaluating v with a behaviorcontext.
(def v (forever 123.0))

(defn read-value
  "read-value is a convenience function that communicates the sampling or 
   evaluation of a behavior at time t.  Since we are most often concerned with 
   time-varying behaviors, we provide a convenient way to 'pass' time to 
   our behaviors.  Note that behind the scenes, we are just creating a behavior 
   context, with the value for t supplied.  This allows extension for later 
   behaviors that may require more than just time as an input."
  [t bf]
  (bf (->behaviorcontext t)))

;example of reading a constant behavior that evaluates to 42.0 
;At time t <- 1.5, we evaluate the forever 42.0 behavior and return, not 
;surprisingly, 42.0 !
(->> (forever 42.0)
     (read-value 1.5))
;example of reading a temporal identitiy behavior (= time-out time-in), 
;that, sampled at t <- 1.5, returns 1.5! 
(->> timef
     (read-value 1.5))
;example of reading a wiggle behavior, based on the sin function, which 
;shoud continously vary between [-1, 1] over input values of 
;[(* t -pi),(* t pi)]
(->> wiggle
     (read-value 1.5))

;Combinators! 
(defn map-b
  "Mapping is a basic combinator for behaviors (reactive functions).  
   We simple return a new behavior, which first evalutes the input behavior, bf,
   and returns the result of applying f to the evaluated behavior.  The net 
   effect is the composition of f and the input behavior.  This assumes that 
   f is of type ('a->'b), and that bf is of type (behaviorcontext->'a).
   The result of the map is a new behavior, (behaviorcontext->'b)"
  [f bf]
  (sample (fn [bc] 
            (f (bf bc)))))

;We exploit our newfound power of composition by squaring any time passed into
;a behavior.  We simply map a square function, which returns (* n n) for any n,
;onto the timef behavior, which extracts time from any behavior context.  
;Note the compositional style and the use of the ->> macro.  This allows us 
;to cleanly illustrate the causal chain, where timef is passed as the last 
;argument to map-b (which expects a behavior function).  map-b is then evaluated
;with the (non-behavior) squaring function as its functional argument, and 
;timef as its behavioral argument.
(def squared (->> timef
               (map-b (fn [n] (* n n)))))
;Evaluating our squared behavior at time 9.0, or a behaviorcontext with t <- 9.0
;yields 81.
(->> squared
    (read-value 9.0))

;Lifting functions...
(defn lift1
  "Lifting is akin to mapping.  It allows to take a normal function, like a 
   squaring function, which operates on numbers, and wire it so that it will  
   operate on behaviors.  Citizens of the Republic of Haskell use this a lot, 
   particularly in the context of Monads.  All we need to know is that 
   f is a 'normal' function, bf is a function that requires a behavior context 
   for evaluation, and that lift1 will allow us to create a new behavior that 
   applies f to the result of applying bf to a behavior context."
  [f bf]
  (map-b f bf))

(defn lift2
  "Lift2 allows us to lift functions that have 2 arguments - like +.  The 
   concept is identical to map or lift1."
  [f2 bf1 bf2]
  (sample (fn [bc]
            (f2 (bf1 bc) (bf2 bc)))))
(defn lift3
  "Lift3 allows us to lift functions that have 3 arguments into new behaviors."
  [f3 bf1 bf2 bf3]
  (sample (fn [bc]
            (f3 (bf1 bc) (bf2 bc) (bf3 bc)))))

(defn filter-b
  "Returns a behavior that engages the behavior for values of t where the 
   predicate is true, nil otherwise.  This is a useful combinator, but I have
   not engaged it - yet!"
  [predicate bf]
  (sample (fn [bc] 
            (if (predicate bc) 
              (bf bc)
               nil))))

;Some simple examples of lifting, and how they apply to behaviors.....
;added is bound to the result of lifting the + function onto two behaviors: 
; wiggle (our sin-based varying behavior between [-1, 1], and timef, our 
;temporal identity function ({:time a} -> a).
(def added (lift2 + wiggle timef))

;We read the value of added at t <- 20, and should get:
;(+ (sin (* 20 pi)) 20)

;  (sin (* 2 pi)) = 0
;  (identity 20) = 20
(->> added 
  (read-value 20))
;19.999999999999996  we get less than 20 due to floating point error, 
;since sin ends up being a VERY small negative value near 0, but close enough!

;We should get the value of (sin (* 0.5 pi)),  which is (sin (/ pi 2)), which is
(->> wiggle
   (read-value 0.5))
;1.0 

(defn wiggle-between [bound] 
  (lift2 * wiggle (forever bound)))

(defn waggle-between [bound]
  (lift2 * waggle (forever bound)))

(def wiggle1 (wiggle-between 1))
(def wiggle100 (wiggle-between 100))
(def waggle1 (waggle-between 1))
(def waggle100 (waggle-between 100))

(defn wait [shift bf]
  (sample (fn [{:keys [t] :as bc}] (bf (assoc bc :t (+ shift t))))))

(defn faster [scalar bf]
  (sample (fn [{:keys [t] :as bc}] (bf (assoc bc :t (* scalar t))))))



