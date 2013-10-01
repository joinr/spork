;;A collection of libraries for implementing population-based
;;stochastic optimization methods, i.e. Genetic Algorithms,
;;Evolutionary Algorithms.  The terms have become somewhat
;;interchangeable, so if you see me using them I mean the process of
;;optimizing a population of solutions based on some notion of
;;fitness, selection, and a stochastic process that creates new
;;candidate solutions.
(ns spork.opt.dumbga
  (:require [spork.util.numerics :refer :all]
            [spork.util [vectors :as v] [bitset :as b]]))


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

;;Most results of genetic algorithms will return a map of useful 
;;information.
(defn ->ga-result [best gen pop scores]
  {:best best :generation gen :population pop :scores scores})

;;Both selection and breeding are pretty diverse topics.  We'll allow 
;;the caller to define specific implementations for both.  Breeding,
;;in general, maps a population to another population.  Selection is 
;;usually a function that maps a population to a sub-population.
;;We use selection, however it's performed, to provide an initial
;;population argument for breeding.  Breeding returns a set of
;;new candidates derived from the initial population (somehow).  The
;;result of breeding is then passed to a join operator, which
;;integrates the new population with the old. 

(defn get-best         [m]   (get m :best))
(defn evaluate-fitness! [m k v] 
  (let [m (assoc! m k v)]
    (if-let [vbest (get-best m :best)]
      (if (> v vbest) (assoc! m :best v) m)
      m)))
    
;;We can use an array internally is speed becomes an issue.
;;Currently, I'm using a transient map for storing fitness data.

(defn assess-fitness 
  "Using fitness function f, which maps a member of xs to a floating point 
   value, returns data that indicates the fitness of each member of xs, 
   along with the fittest member."
  [f xs]
  (persistent! 
   (reduce (fn [acc [idx x]] (evaluate-fitness! acc idx (f v)))                               
           (transient {}) (map-indexed vector xs))))

;;A simple implementation of a genetic algorithm: 
(defn evo-solve 
  "Given parameters for a genetic algorithm, and an initial population, 
   continually breeds and joins, the population until a stopping 
   criterion is met.  Fitness is inferred to mean more fit = higher value. 
   This is a stylistic choice, which differs from other methods.  As such, 
   individuals compared by fitness will prefer a higher fitness value.  In 
   the context of ga, I think it's more intuitive.  Callers need to enforce
   this mechanism.  Note: both breeding and joining will likely involve some 
   form of selection, however the implementation is up to the caller."
  [init-pop terminate? & {:keys [size fitness breed join] :as ga-params}]
  (let [assess (partial assess-fitness fitness)
        init-scores (assess init-pop)
        step (comp join breed)]
    (loop [pop    init-pop
           scores init-scores
           best   (:best init-scores)
           gen 0]
      (if (terminate? gen pop scores best) 
        (->ga-result best gen pop scores)
        (let [new-scores (assess pop)             
              new-pop    (join pop (breed pop scores))]
          (recur new-pop new-scores (get-best new-scores) (unchecked-inc gen)))))))

;;Given the simple framework ga-solve provides, we implement some
;;techniques in Essential Metahueristics.

;;Replaces the old population with a new population.
(defn replacing-join [old new] new)
;;Adds the new population to the old population.
(defn additive-join  [old new] (reduce conj old new))

(defn truncation-selection 
  "Uses a (mu,lambda) truncation selection evolutionary strategy.  Every generation, 
   the n fittest parents are selected (truncation).  Each of these parents produces 
   population-size / n  children via mutation.  Children replace the parents."
  [init-pop terminate? n population-size mutate] 
  (let [truncate (fn [pop scores] 
                   (for [[idx score] (take n (sort-by second (seq scores)))]
                     (get pop idx)))
        k-spawn   (quot population-size n)
        get-mutants (fn [xs] (mapcat #(take k-spawn (repeatedly mutate %)) xs))]
    (evo-solve init-pop terminate? 
        :size population-size :breed (comp get-mutants truncate) :join replacing-join)))

(defn additive-truncation-selection
  "Uses a (mu + lambda) truncation selection evolutionary strategy.  Every generation, 
   the n fittest parents are selected (truncation).  Each of these parents produces 
   population-size / n  children via mutation. Children join the parents for the 
   next round.  Maintains a population size of mu + lambda"
  [init-pop terminate? n population-size mutate] 
  (let [truncate (fn [pop scores] 
                   (for [[idx score] (take n (sort-by second (seq scores)))]
                     (get pop idx)))
        k-spawn   (quot population-size n)
        get-mutants (fn [xs] (mapcat #(take k-spawn (repeatedly mutate %)) xs))
        conj-mutants (fn [pop scores] (let [parents (truncate pop scores)
                                            children (get-mutants parents)]
                                        (reduce conj parents children)))
    (evo-solve init-pop terminate? 
               :size population-size :breed conj-mutants :join replacing-join))))


