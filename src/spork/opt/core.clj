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
(ns spork.opt.core)

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
  (solve-state      [sol] "A chunk of state, likely a map, necessary for the solver.")
  (solve-parameters [sol] "A map of immutable parameters for the solver strategy.")
  (solve-push-state [sol s] "Pushes the next state onto the environment"))  

;;A bundle of data pertinent to any search.  Intended to be widely
;;applicable across a number of different search strategies.
(defrecord search-state 
  [currentsol currentcost bestsol bestcost t iter n accepted])
(defn ->initial-search-state 
  [solution cost & [opts]]
  (let [t    (get opts :t 10000) 
        iter (get opts :t 0) 
        n    (get opts :n 0)
        accepted (get opts :accepted 0)]
    (->search-state solution cost solution cost t iter n accepted)))

;;__search-env__ is the default implementation for __ISolveable__.  
;;Given a set of parameters and an initial state, we have a search
;;environment.  The search environment is meant to bundle all the
;;information we need to prosecute a search using whatever hueristic
;;we want to.  In other words, we define new search strategies on top
;;of the search environment framework.

(defrecord search-env [parameters state]
  ISolveable
  (solve-state       [s] state)
  (solve-parameters  [s] parameters)  
  (solve-push-state  [s new-state]  (->search-env parameters new-state)))

;;Protocol-derived functionality.  This is kind of a weak wrapping around maps.

(defn get-parameter 
  "Auxillary function to fetch parameter k from the solution environment s."
  [s k] (-> (solve-parameters s) (get k)))

(defn solve-continue? "Termination criteria for searching."  
  [s] ((:continuef (solve-parameters s)) s))  
(defn solve-solution "Return the current solution."
  [s] (:currentsol (solve-state s) ))
(defn solve-best  "Return the best solution found."
  [s] (:bestsol (solve-state s)))
(defn solve-optimum  "Returns the cost of the best solution found."
  [s] (:bestcost (solve-state s)))
(defn solve-cost  "Returns a cost function that maps solutions to floats."
  [s] (:costf (solve-parameters s)))
(defn solve-neighbors "Given a seed solution, generate a seq of neighbors."
  [s] ((:neighborf (solve-parameters s)) s))
(defn solve-continue? "Continuation criteria for the solve."  
  [s] ((:continuef (solve-parameters s)) s))
(defn solve-accept? "Acceptance criteria for candidate solutions."
  [s] (:accept? (solve-parameters s)))
(defn solve-decay "Hook for implementing parametric/state variations"
  [s] ((:decayf (solve-parameters s) s)))

(defmacro with-solve [sol & body]
  `(let [~'*state*      (solve-state ~sol)
         ~'*parameters* (solve-parameters ~sol)
         ~'*best*       (solve-best ~sol)
         ~'cost         (solve-cost ~sol)
         ~'neighbors    (fn [] (solve-neighbors ~sol))
         ~'push-state   (partial solve-push-state ~sol)
         ~'continue?    (solve-continue? ~sol)
         ~'accept?      (solve-accept? ~sol)
         ~'decay        (solve-decay ~sol)]
     ~@body))

(defn- iterated? 
  "Helper function to allow us to define a simple notion of searches unbound
   iteration limits.  If itermax is :inf, the iteration is unconstrained."
  [iter itermax] 
  (when (not= itermax :inf) (< itermax iter)))

(defn default-continuef 
  "The standard mechanism for determining if a given state should continue 
   solving or not.  If the state's iteration limit is exceeded, or, if the 
   strategy has a notion of temperature, if the temperature drops below a 
   thresh-hold, the search should stop." 
  [env]
  (let [{:keys [tmin itermax] :as params} (solve-parameters env) 
        {:keys [t iter] :as state}        (solve-state env)]
    (or (contains? state :converged) 
        (and (> t tmin) (not (iterated? iter itermax))))))

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
  (merge prevstate {:currentsol newsol :currentcost newcost 
                    :accepted (inc (:accepted prevstate))}))

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
  (solve-push-state  env (assoc (solve-state env) :converged :true)))

(defn simple-decay
  "Auxillary function to help advance the search using the notion 
   of decay, typically associated with aging state that 
   changes over time.  Depending on the search strategy, these 
   changes may influence the selection criteria for future 
   state transitions, or they may simply be as mundane as incrementing
   the iteration count.  decay provides a simple hooking mechanism into
   the search process, and allows us access to the state."
  [env]
  (let [state (solve-state env)
        t (:t state)
        n (:n state)
        decay-rate (or (get-parameter env :decay-rate) 1.0)
        equilibration (get-parameter env :equilibration)]
    (solve-push-state env
      (-> state
          (assoc :t (if (= n 0) (* decay-rate t) t)) ;temperature decay
          (assoc :n (if (< n equilibration) (inc n) 0)) ;equilibration
          (increment-state)))))

(defn ->candidate [cost solution]
  '(cost solution))

(defn candidate-cost     [c]     (first c))
(defn candidate-solution [c]     (second c))
(defn best-candidate     [env] (->candidate (solve-best env) (solve-optimum env)))
(defn current-candidate  [env]
  (let [s (:state env)]
    (->candidate (get s :currentcost) 
                 (get s :currentsol) )))

(defn default-transition
  "Given a solveable environment, returns the result of choosing a 
   neighboring state from the current  state, using the search parameters
   in the environment."        
  [env] 
  (if-let [candidates  (solve-neighbors  env)] ;if we can move..
    (let  [{:keys [t currentcost bestcost iter] :as state} (solve-state env)
           nextsol  (rand-nth candidates)   ;find a new state
           nextcost ((solve-cost  env) nextsol)] ;cost the new state
      (solve-decay   ;decay/increment the result of...        
        (solve-push-state env ;incoporating the new solution where...
          (if ((solve-accept? env) ;found a desireable state.
                (current-candidate env) (->candidate nextcost nextsol) env)                                                                       
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

(defn naive-neighbor
  "Applies f to the current neighbor to generate new neighbors.
   f is ignorant of anything beyond the current solution."
  [f]
  #(f (solve-solution %)))

(defn parametric-neighbor
  "Applies f to the current neighbor and the environment to generate new 
   neighbors. f can use the entire environment to generate neighbors."
  [f]
  #(f (solve-solution %) %))

(defn ->basic-parameters
  "A map of basic search parameters required for any search environment.
   Certain search strategies will no doubt inject additional parameters, 
   but these are the bare bones needed for generic, pluggable search."
  [& {:keys [costf neighborf decayf continuef itermax accept? transition] 
      :as m}]  
  (merge default-parameters m))

(defn ->solver [init-solution cost-function neighbor-function & extra-params]
  (let [params  (reduce merge 
                  (concat [(->basic-parameters :costf cost-function 
                             :neighborf neighbor-function)]  extra-params))]
    (->search-env params 
      (apply ->initial-search-state init-solution 
             (cost-function init-solution) 
             params))))

(defmacro defsolver
  "Convenience macro to define solver setups for us.  Yields a named function
   that evaluates map-body, under lexical scope of the supplied params, and pipes
   the resulting map"
  [name params & map-body]  
  `(defn ~name [~'init-solution ~'cost-function ~'neighbor-function ~params]
     (let [processed-map# (~'or ~@map-body {})
           blah# (println processed-map#)]
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
      (if (solve-continue?  current-env) 
        (recur (solve-push-state  current-env (trans current-env)))
        current-env))))

(defn solutions
  "Like clojure.core/reductions.  Returns a - potentially infinite -
   sequence of intermediate solution environments, starting with the initial
   environment.  Each element of the sequence is the result of applying the
   transition function to the current environment. Terminates the sequence
   when -continue? no longer succeeds."
  [env]
  (let [trans (get-parameter env :transition)]
    (take-while solve-continue? (iterate trans env))))
