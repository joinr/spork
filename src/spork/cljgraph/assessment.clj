(ns spork.cljgraph.assessment
  (:use [cljgraph.graph]
        [cljgraph.jungapi]
        [clojure.contrib.combinatorics]
        ))

(def decisions {"Start" ["Root"]
                "Demand Futures" ["SSW" "GFMAP + MCO" "ISC-B"]
                "Force Management" ["ARFORGEN" "DIVFORGEN" "Tiered Readiness"]
                "Modular Basis" ["3BN BCT" "DIV"]})

;(defn get-decisions [decisionmap]
;  (let [decision-vars (vals decisionmap)
;        groups (keys decisionmap)]
;    (cartesian-product decision-vars)))

;;I reinvented something already in contrib.combinatorics! ha!
(defn cart-product
  ([coll] coll)
  ([coll1 coll2]
    (let [distribute (fn [a coll]
                     (let [acoll (if (coll? a) a [a])]
                       (map #(conj acoll %) coll)))]
      (mapcat #(distribute % coll2) coll1)))
  ([coll1 coll2 & colls]
    (let [nextcoll (cart-product coll1 coll2)
          remaining (seq colls)]
      (if remaining
        (recur nextcoll (first colls) (rest colls))
        nextcoll)))) 
          
(defn get-decisions [decisionmap]  (reduce cart-product (vals decisionmap)))
(def paths 
  (distinct (mapcat #(partition 2 1 %) (get-decisions decisions))))

(def path-arcs
  (->> paths
    (map #(apply make-arc %))))

(def decisiongraph
  (add-arcs (make-graph) path-arcs))