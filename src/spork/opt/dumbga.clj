;;A collection of libraries for implementing population-based
;;stochastic optimization methods, i.e. Genetic Algorithms,
;;Evolutionary Algorithms.  The terms have become somewhat
;;interchangeable, so if you see me using them I mean the process of
;;optimizing a population of solutions based on some notion of
;;fitness, selection, and a stochastic process that creates new
;;candidate solutions.
(ns spork.opt.dumbga
  (:require [spork.util.numerics :refer :all]
            [spork.util [vectors :as v]]))


;;genetic algorithms will tend to look similar to our generic solution
;;representations.  They will be generically encoded as real valued
;;vectors.  Optionally, we may choose to use a bitvector encoding, but
;;we'll currently with RVV.

;;To get a simple genetic algorithm together, we need a few things: 
;;A means of reproduction, selection, and fitness. 
;;I will follow Sean Luke's excellent treatise in Essentials of
;;Metahueristics.

;;Sean describes an abstract evolutionary algorithm in terms of how it 
;;defines a population, assesses the fitness of a population, selects 
;;individuals from a previous population for "breeding", breeds
;;individuals, and joins the results of breeding into the new
;;population.

;;We'll assume that individuals are homogenous (i.e. the same
;;dimension) real valued vectors.  Thus, a population is merely a
;;collection of these vectors.  This makes our fitness function easy
;;to define as well: it's simply a mapping of a real valued vector to
;;a floating point value.

(defrecord ga-parameters [size fitness breed select join])

;;Both selection and breeding are pretty diverse topics.  We'll allow 
;;the caller to define specific implementations for both.  Breeding,
;;in general, maps a population to another population.  Selection is 
;;usually a function that maps a population to a sub-population.
;;We use selection, however it's performed, to provide an initial
;;population argument for breeding.  Breeding returns a set of
;;new candidates derived from the initial population (somehow).  The
;;result of breeding is then passed to a join operator, which
;;integrates the new population with the old. 

(defn get-best [m]   (get m :best)
(defn assess-fitness [f xs]
  (persistent! 
   (reduce (fn [acc [idx x]] 
             (let [^double v (fitness x)]                                                                                         (-> (if (> v (get-best m))                                                                                            (assoc! acc :best idx)                                                                                             acc)                                                                                                             (assoc! acc idx (fitness x)))))
           (transient {}) (map-indexed xs))))

;;A simple implementation of a genetic algorithm: 
(defn ga-solve 
  "Given parameters for a genetic algorithm, and an initial population, 
   continually breeds, selects, joins, the population until a stopping 
   criterion is met.  Fitness is inferred to mean more fit = higher value. 
   This is a stylistic choice, which differs from other methods.  As such, 
   individuals compared by fitness will prefer a higher fitness value.  In 
   the context of ga, I think it's more intuitive.  Callers need to enforce
   this mechanism."
  [{:keys [size fitness breed select join] :as params} init-pop terminate?]
  (let [asess (partial asses-fitness fitness)]
    (loop [pop    init-pop
           scores (asses-fitness init-pop)
           best   
           ]

  
