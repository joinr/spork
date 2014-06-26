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
                                  :duration (stats/normal-dist 100 50)}
                                 (s/->choice [:foo :bar :baz]))
               :clamped-records  (s/->constrain {:tfinal 5000 
                                                 :duration-max 5000}
                                                 [:random-records])}]  
  (defn random-track! [& {:keys [n] :or {n 10}}]
    (->> :clamped-records
         (s/->replications n)
         (s/->flatten)
         (s/sample-from sampler))))

(defn random-tracks! [& {:keys [n] :or {n 4}}]
  (sketch-image
   (->tracks (zipmap (map #(str "Track" %) (range n)) (repeatedly random-track!)))))
