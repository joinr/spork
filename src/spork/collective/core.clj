;;This is a semi-port of the stochastic optimization algorithms
;;presented in Toby Segaran's excellent 'Collective Intelligence'
;;book. Segaran does a good job generalizing numerical optimization 
;;problems into a protocol-friendly format, which I found amenable 
;;to Clojure.
;;Toby breaks them down into a cost function, a domain, and a solution.  
;;In _this_ port, I define a general solution environment, as well as a 
;;protocol to view the solution environment as a solveable problem.  
;;Mechanisms for searching the problem are encoded in the solution 
;;environment, and allow for hot-swapping, inheriting, or otherwise
;;tweaking solution strategies, solution representations, and any
;;other aspect of the stochastic optimization.  This is pretty useful,
;;since we have a number of strategies (from hill-climbing, to
;;simulated annealing, to tabu search, to genetic algorithms, etc.)
;;that we will impliment and use.  Having a simple, general purpose
;;optimization engine means that we can extend the solution
;;environment to include parameters specific to a strategy, and to
;;decouple the solution representation. 
(ns spork.collective.core)

;;Generic Problem Solving
;;=======================
;;We want a way to define data structures as valid representations of
;;solutions.  Representing solutions is one of the most variable bits
;;of any optimization process.  What functions do we need to support?
;;A means to get the current solution.
;;A means to cost any arbitray solution (a cost function, possibly
;;relative to a solution or solution(s)).
;;A way to find neighboring solutions.
;;A way to tap into problem-specific parameters for the solver to use.

;;__ISolveable__ is a protocol for describing useful information relevant
;;to a problem solving strategy, as well as the basic transforms
;;necessary to derive a solution.
(defprotocol ISolveable
  (-state [sol] "A chunk of state, likely a map, necessary for the solver.")
  (-parameters [sol] "A map of immutable parameters for the solver strategy.")
  (-solution   [sol] "Return the current solution.")
  (-best       [sol] "Return the best solution found.") 
  (-cost       [sol] "Assign a cost to the current solution. Returns a float.")
  (-neighbors  [sol] "Given a seed solution, generate a seq of neighbors.")
  (-push-state [sol s] "Pushes the next state onto the environment")  
  (-continue?  [sol]  "Termination criteria for searching.")
  (-accept?    [sol x1 x2]  "Acceptance criteria for candidate solutions.")
  (-decay      [sol] "Hook for implementing parametric/state variations"))

(defn get-parameter 
  "Auxillary function to fetch parameter k from the solution environment s."
  [s k] (-> (-parameters s) (get k)))

;;A bundle of data pertinent to any search.  Intended to be widely
;;applicable across a number of different search strategies.
(defrecord search-state [currentsol currentcost bestsol bestcost t iter n])
(defn ->initial-search-state [solution cost]
  (->search-state solution cost solution cost 1 0 0))

;;__search-env__ is the default implementation for __ISolveable__.  
;;Given a set of parameters and an initial state, we have a search
;;environment.  The search environment is meant to bundle all the
;;information we need to prosecute a search using whatever hueristic
;;we want to.  In other words, we define new search strategies on top
;;of the search environment framework.
;;WE HAVE REFLECTION WARNINGS HERE!
;;=================================
(defrecord search-env [parameters state]
  ISolveable
  (-state      [s] state)
  (-parameters [s] parameters)  
  (-solution   [s] (:currentsol state))
  (-best       [s] (:bestsol state))
  (-cost       [s] ((:costf parameters) (-solution s)))
  (-neighbors  [s] ((:neighborf parameters) (-solution s)))
  (-push-state [s new-state]  (->search-env parameters new-state))
  (-continue?  [s] ((:continuef parameters) parameters state))
  (-accept?    [s x1 x2] (apply (:accept? parameters) x1 x2 s))
  (-decay      [s] ((:decayf parameters) s)))

(defn- iterated? 
  "Helper function to allow us to define a simple notion of searches unbound
   iteration limits.  If itermax is :inf, the iteration is unconstrained."
  [iter itermax] 
  (when (not= itermax :inf) (> itermax iter)))

(defn default-continuef 
  "The standard mechanism for determining if a given state should continue 
   solving or not.  If the state's iteration limit is exceeded, or, if the 
   strategy has a notion of temperature, if the temperature drops below a 
   thresh-hold, the search should stop." 
  [{:keys [tmin itermax] :as params} {:keys [t iter] :as state}] 
  (or (contains? state :converged) 
      (and (> t tmin) (not (iterated? iter itermax)))))

(defn improved? 
  "The default acceptance criterion.  Combined  with other  default stochastic 
   optimization parameters, this should result in a simple stochastic 
   hill climber."
  [currentcost nextcost & rest] (< nextcost currentcost))

;;Operations for Manipulating A Solution
;;======================================

;;operations for changing solution state.  We only have a few cases
;;to cover.

;;record the result of choosing a new solution, and moving the state.
(defn- move-state [newsol newcost prevstate]
  (merge prevstate {:currentsol newsol :currentcost newcost}))

;;increment the iteration count, advancing the search.
(defn- increment-state [{:keys [iter] :as prevstate}]
  (update-in prevstate [:iter] inc))

;;check to see if we're in a better state, record it if so.
(defn- better-state [newsol newcost prevstate] 
  (if (>= newcost (:bestcost prevstate)) 
    prevstate
    (merge prevstate {:bestsol newsol :bestcost newcost})))

;;There are times when we want to notify the solver that we 
;;have converged.  By default, the solver will check for a 
;;:converged key in the environment, and if it exists, will
;;stop solving.  We can use convergence to halt early, for 
;;instance, if there are no valid neighboring solutions, 
;;we can bail out.
(defn- set-converged-state [env]
  (-push-state env (assoc (-state env) :converged :true)))

(defn simple-decay
  "Auxillary function to help advance the search using the notion 
   of decay, typically associated with aging state that 
   changes over time.  Depending on the search strategy, these 
   changes may influence the selection criteria for future 
   state transitions, or they may simply be as mundane as incrementing
   the iteration count.  decay provides a simple hooking mechanism into
   the search process, and allows us access to the state."
  [env]
  (let [state (-state env)
        t (:t state)
        n (:n state)
        decay-rate (or (get-parameter env :decay-rate) 1.0)
        equilibration (get-parameter env :equilibration)]
    (-> state
        (assoc :t (if (= n 0) (* decay-rate t) t)) ;temperature decay
        (assoc :n (if (< n equilibration) (inc n) 0)) ;equilibration
        (increment-state) 
        (-push-state env))))

(defn default-transition
  "Given a solveable environment, returns the result of choosing a 
   neighboring state from the current  state, using the search parameters
   in the environment."        
  [env] 
  (if-let [candidates  (-neighbors (-solution env))] ;if we can move..
    (let  [{:keys [t currentcost bestcost iter] :as state} (-state env)
           nextsol (rand-nth candidates)   ;find a new state
           nextcost ((-cost env) nextsol)] ;cost the new state
      (-decay   ;decay/increment the result of...        
       (-push-state env ;incoporating the new solution where...
          (if (-accept? currentcost nextcost env)   ;found a desireable state.
              (->> (move-state nextsol nextcost state) ;record transition
                   (better-state nextsol nextcost)) ;check improvement
              ;or we discard the new state.
              state))))
    ;otherwise we have converged on an absorbing state.
    (set-converged-state env)))

;;Default set of search parameters.  We will typically overload these guys.
(def default-parameters {:costf (fn [& args] 0.0) 
                         :neighborf (fn [& args] nil)
                         :decayf     simple-decay
                         :continuef  default-continuef
                         :itermax    0
                         :accept?    improved?
                         :transition default-transition})

(defn ->basic-parameters
  "A map of basic search parameters required for any search environment.
   Certain search strategies will no doubt inject additional parameters, 
   but these are the bare bones needed for generic, pluggable search."
  [& {:keys [costf neighborf decayf continuef itermax accept? transition] 
      :as m}]  
  (merge default-parameters m))

(defn ->solver [init-solution cost-function neighbor-function & extra-params]
  (->search-env 
   (reduce merge 
     (concat [(->basic-parameters :cost-function cost-function 
                                  :neighborf neighbor-function)]  extra-params))
   (->initial-search-state init-solution (cost-function init-solution))))

(defmacro defsolver
  "Convenience macro to define solver setups for us.  Yields a named function
   that evaluates map-body, under lexical scope of the supplied params, and pipes
   the resulting map"
  [name params & map-body]  
  `(defn ~name [~'init-solution ~'cost-function ~'neighbor-function ~params]
     (let [processed-map# (~'or ~@map-body {})]
       (assert (map? processed-map#) 
               "map-body must evaluate to a valid parameter map")
       (->solver ~'init-solution ~'cost-function ~'neighbor-function processed-map#))))

;;Solver API
;;==========
;;If our problem is represented as a solveable environment, solve
;;will induce a series of state transitions for us, using a supplied 
;;transition function.

(defn solve
  "The generic solver will utilize a cost function to evaluate solutions.  The 
   initial solution is assumed to be a representation of the structure and form 
   of all solutions, and the costf is assumed to return a float."
  [env]
  (let [trans (get-parameter env :transition)]
    (loop [current-env env]
      (if (-continue? current-env) 
        (recur (-push-state current-env (trans current-env)))
        current-env))))

(defn solutions
  "Like clojure.core/reductions.  Returns a - potentially infinite -
   sequence of intermediate solution environments, starting with the initial
   environment.  Each element of the sequence is the result of applying the
   transition function to the current environment. Terminates the sequence
   when -continue? no longer succeeds."
  [env]
  (let [trans (get-parameter env :transition)]
    (take-while -continue? (iterate trans env))))










