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

;;we'll assume that individuals are homogenous (i.e. the same
;;dimension) real valued vectors.  Thus, a population is merely a
;;collection of these vectors.  This makes our fitness function easy
;;to define as well: it's simply a mapping of a real valued vector to
;;a floating point value.

