;;A temporary namespace for a simple simulated annealer.  While I get the opt 
;;stuff ironed out, this will serve in the meantime.  This is basically a 
;;subset of stuff ripped from spork.opt.annealing  
(ns spork.opt.dumbanneal
  (:require [spork.util.numerics :refer :all]
            [spork.util [vectors :as v]]
            [spork.opt  [annealing :as ann]]))

;;Simulated annealing just overrides the default acceptance criterion,
;;and uses the Boltzmann energy equation to bias the search toward
;;exploration early, and exploitation as the search proceeds.
;;__blank-sa-params__ provides a sane set of defaults for simulated
;;annealing, and will garantee that a solve will terminate.
(def blank-sa-params 
  (ann/->sa-params 
    (ann/geometric-decay 0.9) 
    1000000 0.00000001 1000 1
    ann/boltzmann-accept?))

(defn make-step-fn [ranges & {:keys [step-func] 
                              :or {step-func ann/asa-stepper}}]
  (let [steps  (vec (for [[lower upper] ranges] (step-func lower upper)))]
    (fn [temp xs]
      (v/map-vec-indexed xs (fn [idx x0] ((get steps idx) temp x0))))))

(defn dev [c x] (abs (- c x))) 
(def unconstrained [-100000  100000])
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


 )