;;An implementation of generic packing problem instances, i.e.
;;knapsack.  Currently used to implement packing problem solvers 
;;via dynamic programming, so that packing can be composed.

;;Note: Looks like an experiment.  MARKED FOR DEPRECATION.
(ns spork.opt.packing
  (:require [clj-tuple :refer [tuple]]))

;;this is a sample implementation.
;;We should be able to pull out some nice scaffolding from it.

(def resources 
  {:AC       400
   :RC       600
   :RCAD     25
   :RCAD-BIG 25})

;;Preferences for resources.
(def preferences 
  {:init       [:AC :RCAD     :RCAD-BIG]
   :rotational [:RC :AC :RCAD :RCAD-BIG]
   :hld        [:RC :AC :RCAD :RCAD-BIG]})
  
(defn as-costs [from xs]
  (let [red-fun (cond (vector? xs)  (fn [m w k] (conj! m [from k w])) 
                      (map? xs)     (fn [m k w] (conj! m [from k w]))
                      :else (throw (Exception. "non-exhaustive match")))]
    (persistent! (reduce-kv red-fun (transient []) xs))))

;(defn as-cost-table [kvps]
;  (reduce-kv (fn [m xs] (reduce 
  

(defn get-costs [m]
  (persistent!
   (reduce-kv (fn [acc from tos]
                (reduce (fn [edges to]
                          (conj! edges [from to (get m from to)]))
                        acc
                        tos))
              (transient []) m)))
  

