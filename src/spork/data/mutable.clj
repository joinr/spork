;;A set of idiomatic wrappers for mutable datastructures from
;;java.util.  These collections are treated like transients, allowing
;;for conj!. 
;; Work in progress.
(ns spork.data.mutable  
  (:require [spork.protocols [core :as generic]])
  (:import [java.util ArrayList PriorityQueue ArrayDeque]))


(defn ^ArrayList make-array-list [] (ArrayList.))
(defn ^ArrayList array-list [xs] 
  (reduce (fn [^ArrayList acc x] (doto acc (.add x))) (make-array-list) xs))

(defn ^ArrayDeque make-queue [] (ArrayDeque.))
(defn ^ArrayDeque queue [xs] 
  (reduce (fn [^ArrayDeque acc x] (doto acc (.add x))) (make-queue) xs))

(defn entry-comparer [l r] 
  (let [pl (generic/entry-priority l)
        pr (generic/entry-priority r)]
    (cond (< pl pr) -1 
          (> pl pr) 1
          :else 0)))   

(defn ^ArrayList  add-list   [^ArrayList l obj]  (doto l  (.add obj)))
(defn ^ArrayDeque add-q      [^ArrayDeque q obj]  (doto q (.add obj)))


(defn ^PriorityQueue make-pq [] (PriorityQueue. 11 entry-comparer))
(defn ^PriorityQueue pq [xs] 
  (reduce (fn [^PriorityQueue acc x]   
            (doto acc (.add x))) (make-pq) xs))
            
(defn ^PriorityQueue add-pq  [^PriorityQueue q obj]  (doto q (.add obj)))
(defn ^PriorityQueue pop-pq  [^PriorityQueue q    ]  (do (.poll q) q))


  ;; ArrayDeque
  ;; (conj [o v] (add-q o v))
  ;; (persistent [o] (into [] (iterator-seq o)))
  ;; PriorityQueue
  ;; (conj [o v] (add-pq o v))
  ;; (persistent [o] (into [] (map first (iterator-seq o))))

