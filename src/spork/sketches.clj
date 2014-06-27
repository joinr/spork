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
      ramptime  (stats/uniform-dist 1 3000)
      add-noise (fn [x] (+ x (normal)))
      corpus   {:Big        {:name "Big"    :start 0 :duration 1095 :quantity 1000}                
                :Medium     {:name "Medium" :start 0 :duration 365  :quantity 500}
                :Small      {:name "Small"  :start 0 :duration 30   :quantity 100}
                :Sporadic   {:name "Tiny"   :start 0 :duration 4    :quantity 20}
                :Year-Round {:name "Year round demand" :start 0 :duration 5000 :quantity 15}}
      sampling-rules {:random-surge   (s/->transform                                       
                                        {:start      (stats/uniform-dist 1 3000)}
                                        (s/->without-replacement [:Big :Small :Medium]))                      
                      :ramp            (s/->chain [:Small :Medium :Big])
                      :random-buildup  (s/->transform 
                                          (fn [xs] (let [offset (ramptime)]
                                                     (map #(update-in % [:start] + offset) xs)))
                                          :ramp)
                      :smallish-surge   (s/->transform 
                                           {:start    (stats/uniform-dist 1 4000)}
                                        (s/->choice [:Small :Small :Small :Medium]))
                      :random-sporadic  (s/->transform 
                                          {:start    (stats/uniform-dist 1 4500)}
                                          :Sporadic)
                      :random-case      (s/->concatenate [(s/->replications 2  :random-surge)
                                                          (s/->replications 4  :smallish-surge)
                                                          (s/->replications 10 :random-sporadic)                                                          
                                                          :random-buildup
                                                          :Year-Round])}
      sampler (merge sampling-rules corpus)]  
  (defn random-track! [& {:keys [n entry-case] :or {n 1 entry-case :random-case}}]
    (->> (s/->constrain {:tfinal 1500 
                         :duration-max 1500}
                        [entry-case])
         (s/->replications n)
         (s/->flatten)
         (s/sample-from sampler))))

(def linked-case (s/->chain (s/->replications 10 :random-sporadic)))


(def last-data (atom 0))

(defn n-bad-tracks [n] 
  (let [bad-data (take n @last-data)]
    (sketch-image
     (->tracks (zipmap (map #(str "Track" %) (range n)) bad-data)))))
  
(defn random-tracks! [& {:keys [n entry-case] :or {n 4 entry-case :random-case}}]
  (let [data (take n (repeatedly #(random-track! :entry-case entry-case)))
        _    (do (reset! last-data data))]
    (sketch-image
     (->tracks (zipmap (map #(str "Future" %) (range n)) data)))))
