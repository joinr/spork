(ns spork.sketches 
  (:require [spork.sketch :refer :all]
            [spork.util [sampling :as s]]
            [spork.util [stats :as stats]]))

(def sampler (s/sample-data))

(def random-tracks 
  {:SAMPLE (s/sample-from sampler (s/->constrain {:tfinal 1000} :sample))
   :CASE1  (s/sample-from sampler (s/->constrain {:tfinal 1000} :case1))})

(let [sampler {:foo {:name "foo" :start 0 :duration 1}
               :bar {:name "bar" :start 0 :duration 10}
               :baz {:name "baz" :start 0 :duration 5}
               :random-records  (s/->transform 
                                 {:start    (stats/uniform-dist 0 2000)
                                  :duration (stats/normal-dist 100 10)}
                                 (s/->choice [:foo :bar :baz]))
               :clamped-records  (s/->constrain {:tfinal 5000 
                                                 :duration-max 5000}
                                                 [:random-records])}]  
  (defn random-track! [& {:keys [n] :or {n 10}}]
    (->> :clamped-records
         (s/->replications n)
         (s/->flatten)
         (s/sample-from sampler))))


(let [normal    (stats/normal-dist 0 50)
      add-noise (fn [x] (+ x (normal)))
      corpus   {:Big        {:name "Big"    :start 0 :duration 1095 :quantity 1000}                
                :Medium     {:name "Medium" :start 0 :duration 365  :quantity 500}
                :Small      {:name "Small"  :start 0 :duration 30   :quantity 100}
                :Sporadic   {:name "Tiny"   :start 0 :duration 4    :quantity 20}
                :Year-Round {:name "Year round demand" :start 0 :duration 5000 :quantity 15}}
      sampling-rules {:ramp-up       (s/->flatten (s/->chain [:Small :Medium :Big]))
                      :random-surge  (s/->transform 
                                      {:start      (stats/uniform-dist 1 3000)}
                                      (s/->without-replacement [:Big :Small :Medium :ramp-up]))                      
                      :smallish-surge  (s/->transform 
                                        {:start    (stats/uniform-dist 1 4000)}
                                        (s/->choice [:Small :Small :Small :Medium]))
                      :random-sporadic  (s/->transform 
                                          {:start    (stats/uniform-dist 1 4500)}
                                          :Sporadic)
                      :random-case      (s/->concatenate [(s/->replications 2  :random-surge)
                                                          (s/->replications 10 :smallish-surge)
                                                          (s/->replications 2 :random-sporadic)
                                                          :Year-Round
                                                          ])
                      :clamped-records  (s/->constrain {:tfinal 2000 
                                                        :duration-max 2000}
                                                        [:random-case])}
      sampler (merge sampling-rules corpus)]  
  (defn random-track! [& {:keys [n] :or {n 1}}]
    (->> :clamped-records
         (s/->replications n)
         (s/->flatten)
         (s/sample-from sampler))))


(def last-data (atom 0))

(defn n-bad-tracks [n] 
  (let [bad-data (take n @last-data)]
    (sketch-image
     (->tracks (zipmap (map #(str "Track" %) (range n)) bad-data)))))
  
(defn random-tracks! [& {:keys [n] :or {n 4}}]
  (let [data (take n (repeatedly random-track!))
        _    (do (reset! last-data data))]
    (sketch-image
     (->tracks (zipmap (map #(str "Track" %) (range n)) data)))))
