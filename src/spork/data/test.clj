(ns spork.data.test
  (:require   [clojure.test :refer :all]
              [spork.protocols [core :as generic]]
              [spork.data [priorityq :as pq]
                          [fringe    :as fr]]))

;;priorityq testing
;;=================
(def samples [[:a 0.884] [:b 0.899] [:c 0.589] [:d 0.761]])
(def ordered-samples (map first (sort-by second samples)))
(def ordered-vec     (vec ordered-samples))
(def regular-queue   (into empty-entries ordered-samples))
(def the-q (into pq/emptyq samples))
(deftest pq-tests 
  (is (= the-q ordered-samples))
  (is (= the-q ordered-vec)) 
  (is (= ordered-vec the-q))) 

;;changing values.
;;:c has the lowest priority.  changing it to 2.0 should move it.
(deftest pq-alteration
  (is (=  (alter-value the-q :c 2.0)
          (alter-value the-q :c 0.589 2.0)
         '(:d :a :b :c)))
  (is (= (pq/alter-value the-q :c 0.589 2.0 (fn [nd] :Balls))
         '(:d :a :b :Balls))))
  

;;Search fringe testing 
;;=====================

(def nodes [[:a 2]
            [:b 3]
            [:c 10]
            [:d 11]
            [:e 0]])
(defn load-fringe [f &{:keys [xs] :or {xs nodes}}] 
  (reduce (fn [acc [n w]] (generic/conj-fringe acc n w)) f xs))
(deftest fringe-creation 
  (is  (= (generic/fringe-seq (load-fringe fr/priority-fringe))
          '([:e 0] [:a 2] [:b 3] [:c 10] [:d 11])))
  (is  (= (generic/fringe-seq (load-fringe fr/depth-fringe))
          '([:e 0] [:d 11] [:c 10] [:b 3] [:a 2])))
  (is  (= (generic/fringe-seq (load-fringe fr/breadth-fringe))
          '([:a 2] [:b 3] [:c 10] [:d 11] [:e 0]))))
 

