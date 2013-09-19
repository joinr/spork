;;A namespace for defining common neighborhood functions for solutions.
;;Flexing the ideas present in the presentation of generic solutions from
;;__spork.opt.representations__ , we can define general neighborhood functions
;;for any representation.
(ns spork.opt.neighborhood
  (:require [spork.opt [representation :as rep]]
            [spork.util [vectors :as v]]))

;;Generic Neighborhoods
;;=====================

;;Given a solution representation, from spork.opt.representation/defsolution or
;;a compatible means, we would like to define ways to permute the solutions 
;;according to some strategy.  As our solution representations efficiently
;;map complex variable domains to an underlying normalized representation, we 
;;can define neighborhood functions as either manipulations of the normalized 
;;form, or as transforms of the "nicer" solution domain.  

;;Randomly Sampled Neighborhoods
;;==============================

;;The simplest neighborhood is to range over the normalized form of the 
;;solution, in each dimension, using a uniform distribution.  The normal form 
;;is defined specifically to make this easy to do.  Since the solution takes 
;;care of encoding everything, any range-based constraints will enforced 
;;automatically.

(defn random-neighbor
  "Generates a random neighbor based on the solution's normalized encoding."
  [sol] 
  (rep/from-normal sol (rep/random-normal-vector (rep/basis-vector sol))))
(defn ^double square [^double x] (* x x))

(defn ^double cauchy [^double loc ^double scale]
  (* (/ 1.0 Math/PI) (/ scale (+ (square (- (- (rand) 0.5) loc)) (square scale))))) 
  

(defn cauchy-vec [v]
  (let [x (double (rand))]    
    (v/map-vec (rep/random-normal-vector v) #(/ (- % 0.5) x))))

(defn cauchy-neighbor [sol]
  (rep/from-normal sol (cauchy-vec (rep/basis-vector sol)))) 

                   
;;Another general way to sample an n-dimensional space is to use some 
;;distribution.

;;Testing 
(comment 
(rep/defsolution one-d [x [0 50]])
(def simple-vec  (v/->vec1 0.0))
(defn cauchy-samples [n] 
  (into [] (take n (map #(v/vec-nth % 0) (repeatedly #(cauchy-vec simple-vec)))))) 
)

