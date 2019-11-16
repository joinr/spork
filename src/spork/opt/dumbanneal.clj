;;A temporary namespace for a simple simulated annealer.  While I get the opt 
;;stuff ironed out, this will serve in the meantime.  This is basically a 
;;subset of stuff ripped from spork.opt.annealing  
(ns spork.opt.dumbanneal
  (:require [spork.util.numerics :refer :all]
            [spork.util [vectors :as v]]
            [spork.opt  [annealing :as ann]]))

;;Simulated annealing just overrides the default acceptance criterion,
;;and uses the Boltzmann energy equation to biase search toward
;;exploration early, and exploitation as the search proceeds.
;;__blank-sa-params__ provides a sane set of defaults for simulated
;;annealing, and will garantee that a solve will terminate.
(def blank-sa-params 
  (ann/->sa-params 
    (ann/geometric-decay 0.9) 
    1000000 0.00000001 1000 1
    ann/boltzmann-accept?))

;;default step function takes a vector of
;;[lower upper] bounds defining continuous variables
;;to search over.  The solution is implicitly a
;;vector.
(defn make-step-fn [ranges & {:keys [step-func] 
                              :or {step-func ann/asa-stepper}}]
  (let [steps  (vec (for [[lower upper] ranges] (step-func lower upper)))]
    (fn [temp xs]
      (v/map-vec-indexed xs (fn [idx x0] ((get steps idx) temp x0))))))

(defn deviation [c x] (abs (- c x))) 
(def unconstrained [-100000  100000])


;;Defines a simple annealing function with configuration criteria to
;;admit a variety of composed problems.  User can supply their own
;;step functions, cost functions, temperature, iteration, equilibration,
;;acceptance criteria, temperature decay function, etc.  Defaults to
;;geometric decay (0.9) and a boltzmann acceptance criteria (typical
;;of metropolis-hastings, or classical SA).  Assumes a numeric vector
;;by default, with unconstrained ranges.  If user supplies a custom
;;solution representation, they should also define custom cost functions
;;and step functions.
(defn simple-anneal
  [cost-function init-solution &
   {:keys [t0 tmin itermax equilibration accept? init-cost decay step-function
           ranges]
    :or {t0 10000000 tmin 0.0000001  itermax 100000  equilibration 1
         accept? ann/boltzmann-accept?  init-cost (cost-function init-solution)
         decay  (ann/geometric-decay 0.9)
         ranges (for [x init-solution] unconstrained)}}]
  (assert (coll? init-solution)
          "Solutions must be vectors of at least one element")
  (let [step-function (or step-function (make-step-fn ranges))]
    (loop [temp t0
           n 1
           i 0
           converged?   false
           current-sol  init-solution
           current-cost init-cost
           best-sol     init-solution
           best-cost    init-cost]
    (if converged?
      {:temp temp :n n :i i    :current-solution current-sol
       :best-solution best-sol :best-cost best-cost}
      (let [temp (double (if (= n 0) (decay temp i) temp))
            n    (long (if (= n 0) equilibration n))]
        (if (or (< temp tmin) (> i itermax))
            (recur temp  n i true current-sol current-cost best-sol best-cost) ;exit
            (let [new-sol  (step-function temp current-sol)
                  new-cost (cost-function new-sol)
                  n (dec n)
                  i (inc i)]
              (if (accept? temp current-cost new-cost)
                (recur temp n i converged?
                       new-sol new-cost (if (< new-cost best-cost) new-sol best-sol)
                       (min new-cost best-cost))
                (recur temp n i converged? current-sol
                       current-cost best-sol best-cost)))))))))

;;testing
(comment
(def the-solution [5])
(def zero-ten [0 10])
(def the-spec [zero-ten])
(def the-step (make-step-fn the-spec))
(def the-cost-func (fn [xs]
                     (reduce + (map (fn [x]
                                      (abs (- x 2))) xs))))

(simple-anneal the-cost-func the-solution
               :step-function the-step
               :decay (ann/geometric-decay 0.8)
               :equilibration 30)

;;Can we get a decent solution to the function
;;ArgMin x,  f(x) = |x - 2|

(simple-anneal (fn [v] (abs (- (nth v 0) 2)))
               [100]
               :decay (ann/geometric-decay 0.8)
               :equilibration 30)

;;how about a root for f(x) =  3x + 2?
(let [f (fn [x]
          (abs (+ (* 3 x) 2)))]
  (simple-anneal (fn [v] (f (nth v 0)))
                 [100]
                 :decay (ann/geometric-decay 0.8)
                 :equilibration 30))

;;how about a 2d function?
;;f(x,y) = 2x - 3y + 100

(let [f (fn [x y]
          (abs (+ (- (* 2 x)
                     (* 3 y))
                  100)))]
  (simple-anneal (fn [v] (f (nth v 0) (nth v 1)))
                 [0 0]
                 :decay (ann/geometric-decay 0.8)
                 :equilibration 30))

;;Same function, but constrained.
;;f(x,y) = 2x - 3y + 100
;;s.t. 100 <= x <= 200
(let [f (fn [x y]
          (abs (+ (- (* 2 x)
                     (* 3 y))
                  100)))]
  (simple-anneal (fn [v] (f (nth v 0) (nth v 1)))
                 [100 0]
                 :ranges [[100 200]
                          unconstrained]
                 :decay (ann/geometric-decay 0.9)
                 :equilibration 30))

;;s.t. (and (integer? x) (even? x))
(let [f (fn [x y]
          (abs (+ (- (* 2 x)
                     (* 3 y))
                  100)))]
  (simple-anneal (fn [v] (f (nth v 0) (nth v 1)))
                 [100 0]
                 :ranges [[100 200]
                          unconstrained]
                 :decay (ann/geometric-decay 0.9)
                 :equilibration 30))

;;let's optimize a knapsack problem.
(let [items {:green  {:value 4 :weight  12}
             :grey   {:value 2 :weight  1}
             :blue   {:value 2 :weight  2}
             :orange {:value 1 :weight  1}
             :yellow {:value 10 :weight 4}}
      ks    (vec (keys items))
      max-weight 15
      init-pack  {:weight 0 :items #{} :value 0}
      cost  (fn [pack]
              ( + (:value pack)
                  ( * -100 (Math/abs (- max-weight (:weight pack))))))
      random-step (fn [_ pack]
                    (let [item    (rand-nth ks)
                          loaded  (pack :items)
                          weight  (pack :weight)
                          value   (pack :value)]
                      (if (loaded item)
                        {:weight (- weight (-> item items :weight))
                         :value  (- value  (-> item items :value))
                         :items  (disj loaded item)}
                        {:weight (+ weight (-> item items :weight))
                         :value  (+ value  (-> item items :value))
                         :items  (conj loaded item)})))]
  (-> (simple-anneal (comp - cost)
                     init-pack
                     :decay (ann/geometric-decay 0.8)
                     :equilibration 30
                     :step-function random-step
                     )
      :best-solution))

 )
