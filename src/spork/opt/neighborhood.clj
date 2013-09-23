;;A namespace for defining common neighborhood functions for solutions.
;;Flexing the ideas present in the presentation of generic solutions from
;;__spork.opt.representations__ , we can define general neighborhood functions
;;for any representation.
(ns spork.opt.neighborhood
  (:require [spork.opt [representation :as rep]]
            [spork.util [vectors :as v] [stats :as s]]))

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

(defn cauchy-between [^double lower ^double upper]
  (^double fn [^double u]
              (stats/
  
;;Composite Neighborhoods
;;=======================
;;A more sophisticated way to generate neighborhoods using our normalized 
;;solution form, is to treat each element of the normal vector as a stochastic
;;variable drawn from a unique distribution.  This is identical to the default 
;;behavior above, where we effectively treat any n-element normalized vector 
;;as the scaled output of a random variate based on a neighborhood spec.
;;The neighborhood spec is a simple data structure that maps elements in the 
;;normal vector to distributions.  It also maintains any data for the 
;;distributions, which will help us mix arbitrary variables. 

(defn sample-across
  "Given an input distribution, we sample from the distribution n times, once 
   for each element of the solution, and project the result back onto the 
   normalized solution vector."
  [sol dist]
  )  

(defn cauchy-vec [v]
  (let [x (double (rand))]    
    (v/map-vec (rep/random-normal-vector v) #(/ (- % 0.5) x))))

(defn cauchy-neighbor [sol]
  (rep/from-normal sol (cauchy-vec (rep/basis-vector sol)))) 
                  


(defn cauchy-from [temp x0 x1]

;;Another general way to sample an n-dimensional space is to use some 
;;distribution.

;;Testing 
(comment 
(rep/defsolution one-d [x [0 50]])
(def simple-vec  (v/->vec1 0.0))
(defn cauchy-samples [n] 
  (into [] (take n (map #(v/vec-nth % 0) (repeatedly #(cauchy-vec simple-vec)))))) 
)

