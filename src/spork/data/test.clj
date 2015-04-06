(ns spork.data.test
  (:require   [clojure.test :refer :all]
              [spork.protocols [core :as generic]]
              [spork.data [priorityq :as pq]
                          [fringe    :as fr]
                          [mpq :as mpq]    ]))

;;priorityq testing
;;=================
(def samples [[:a 0.884] [:b 0.899] [:c 0.589] [:d 0.761]])
(def ordered-samples (map first (sort-by second samples)))
(def ordered-vec     (vec ordered-samples))
(def regular-queue   (into pq/empty-entries ordered-samples))
(def the-q (into pq/emptyq samples))
(deftest pq-tests 
  (is (= the-q ordered-samples))
  (is (= the-q ordered-vec)) 
  (is (= ordered-vec the-q))) 

;;changing values.
;;:c has the lowest priority.  changing it to 2.0 should move it.
(deftest pq-alteration
  (is (=  (pq/alter-value the-q :c 2.0)
          (pq/alter-value the-q :c 0.589 2.0)
         '(:d :a :b :c)))
  (is (= (pq/alter-value the-q :c 0.589 2.0 (fn [nd] :Balls))
         '(:d :a :b :Balls))))
  

;;Search fringe testing 
;;=====================

(def fringe-nodes [[:a 2]
                   [:b 3]
                   [:c 10]
                   [:d 11]
                   [:e 0]])
(defn load-fringe [f & {:keys [xs] :or {xs fringe-nodes}}] 
  (reduce (fn [acc [n w]] (generic/conj-fringe acc n w)) f xs))

(deftest fringe-creation 
  (is  (= (generic/fringe-seq (load-fringe fr/priority-fringe))
          '(:e :a :b  :c  :d)))
  (is  (= (generic/fringe-seq (load-fringe fr/depth-fringe))
          '(:e :d :c :b :a)))
  (is  (= (generic/fringe-seq (load-fringe fr/breadth-fringe))
          '(:a :b :c :d :e))))
 

;;testing 

(def nodes [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p])
(defn fresh-pq 
  ([] 
     (reduce (fn [acc [node w]]
               (assoc acc node w))
             (mpq/->min-pq)
             (map-indexed (fn [i n] [n i]) (reverse nodes))))
  ([bound] 
     (reduce (fn [acc [node w]]
               (assoc acc node w))
             (mpq/->min-bounded-pq bound)
             (map-indexed (fn [i n] [n i]) (reverse nodes)))))

(let [pq (fresh-pq)]
  (deftest counting   
    (is (= (count pq) (count nodes)))
    (is (= (generic/get-min pq) :p))
    (is (= (generic/get-max pq) :a))))
  

(let [pq (fresh-pq)
      _  (dotimes [i 10] (generic/pop-min pq))]
  (deftest popleft
    (is (= (count pq) (- (count nodes) 10)))
    (is (= (generic/get-min pq) :f))
    (is (= (generic/get-max pq) :a))))


(let [pq (fresh-pq)
      _  (dotimes [i 10] (generic/pop-max pq))]
  (deftest popright
    (is (= (count pq) (- (count nodes) 10)))
    (is (= (generic/get-min pq) :p))
    (is (= (generic/get-max pq) :k))))

(let [pq (-> (fresh-pq) (assoc :a 0) (assoc :e 200))]
  (deftest reweighting
    (is (= (count pq) (count nodes)))
    (is (= (generic/get-min pq) :p))
    (is (= (second pq) [0 :a]))
    (is (= (generic/get-max pq) :e))))

(let [pq (fresh-pq 5)]
  (deftest countingb   
    (is (= (count pq) 5))
    (is (= (generic/get-min pq) :p))
    (is (= (generic/get-max pq) :l))))
  

(let [pq (fresh-pq 5)
      _  (dotimes [i 2] (generic/pop-min pq))]
  (deftest popleftb
    (is (= (count pq) 3))
    (is (= (generic/get-min pq) :n))
    (is (= (generic/get-max pq) :l))))


(let [pq (fresh-pq 5)
      _  (dotimes [i 2] (generic/pop-max pq))]
  (deftest poprightb
    (is (= (count pq) 3))
    (is (= (:distance (generic/expose pq)) 2))
    (is (= (generic/get-min pq) :p))
    (is (= (generic/get-max pq) :n))))

(let [pq (-> (fresh-pq 5) 
             (assoc :a 0)  ;adds :a, bumps :n
             (assoc :e 200) ;should be ignored
             )]
  (deftest addingb
    (is (= (count pq) 5))
    (is (= (generic/get-min pq) :p))
    (is (= (second pq) [0 :a]))
    (is (= (generic/get-max pq) :m))))

