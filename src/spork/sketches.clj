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


(let [ubound     1500
      normal    (stats/normal-dist 0 50)
      ramptime  (stats/uniform-dist 1 ubound)
      add-noise (fn [x] (+ x (normal)))
      corpus   {:Big        {:name "Big"    :start 0 :duration 1095 :quantity 1000}                
                :Medium     {:name "Medium" :start 0 :duration 365  :quantity 500}
                :Small      {:name "Small"  :start 0 :duration 30   :quantity 100}
                :Sporadic   {:name "Tiny"   :start 0 :duration 4    :quantity 20}
                :Year-Round {:name "Year round demand" :start 0 :duration ubound :quantity 15}}
      sampling-rules {:random-surge   (s/->transform                                       
                                        {:start      (stats/uniform-dist 1 ubound)}
                                        (s/->without-replacement [:Big :Small :Medium]))                      
                      :ramp            (s/->chain [:Small :Medium :Big])
                      :random-buildup  (s/->transform 
                                          (fn [xs] (let [offset (ramptime)]
                                                     (map #(update-in % [:start] + offset) xs)))
                                          :ramp)
                      :smallish-surge   (s/->transform 
                                           {:start    (stats/uniform-dist 1 ubound)}
                                        (s/->choice [:Small :Small :Small :Medium]))
                      :random-sporadic  (s/->transform 
                                          {:start    (stats/uniform-dist 1 ubound)}
                                          :Sporadic)
                      :random-case      (s/->concatenate [(s/->replications 2  :random-surge)
                                                          (s/->replications 4  :smallish-surge)
                                                          (s/->replications 10 :random-sporadic)                                                          
                                                          :random-buildup
                                                          :Year-Round])}
      sampler (merge sampling-rules corpus)]  
  (defn random-track! [& {:keys [n entry-case] :or {n 1 entry-case :random-case}}]
    (->> (s/->constrain {:tfinal ubound 
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



(def event-colors 
  (zipmap 
   ["Tiny"
    "Year round demand"
    "Small"             
    "Medium"            
    "Big"]
   (map #(java.awt.Color. (nth % 0) (nth % 1) (nth % 2)) (spork.graphics2d.canvas/random-color-palette 0.2 0.65))))

;;another way to color this is to use increasing saturation..
;; (def event-colors 
;;   (zipmap 
;;    ["Tiny"
;;     "Year round demand"
;;     "Small"             
;;     "Medium"            
;;     "Big"]
;;    (map #(java.awt.Color. (nth % 0) (nth % 1) (nth % 2)) (rest (spork.graphics2d.canvas/mono-color-palette 0.2 [255 0 0])))))

(defn ->legend-entry [txt color]
  (let [lbl (spork.geometry.shapes/->plain-text :black  (str txt "  ") 0 10)
        h   (spork.protocols.spatial/get-height (spork.protocols.spatial/get-bounding-box lbl))]
    (beside (spork.geometry.shapes/->rectangle color 0 0 10 h)
            lbl)))

(def legend 
   (shelf (for [[lbl clr] event-colors]
                 (->legend-entry lbl clr))))

(defn event->color [e] 
  (if-let [clr (get event-colors (get e :name))]
    clr
    (throw (Exception. (str ["Unknown event!" e])))))
    
(defn random-tracks! [& {:keys [n entry-case] :or {n 4 entry-case :random-case}}]
  (let [data (take n (repeatedly #(random-track! :entry-case entry-case)))
        _    (do (reset! last-data data))]
    (with-event->color event->color 
      (sketch-image
       (scale 1.0 1.5
              (stack [(->tracks (zipmap (map #(str "Future" %) (range n)) data))
                      (translate 10 5 legend)]))))))
  

(comment ;sample-code
  ;;We set an upper bound for our simulated demand future at 1500 days,
  ;;with the intent that no sampled event will extend paste 1500.:
  (def ubound 1500)
  ;;We can sample random times in our future using a PRNG drawing from
  ;;a uniform distribution: 
  (def get-random-time (stats/uniform-dist 1 ubound))
  ;;We have a primitive corpus of sampling rules that we use to
  ;;develop arbitrarily complex sampling rules for building demand
  ;;futures.
  ;;These are the conceptual "building blocks" for generating
  ;;stochastic futures:
  ;;``Big" demands last about 3 years and requires 1000 utils.
  ;;``Medium"" demands last for a year, requiring 500 utils.
  ;;``Small"" demands last for a month, requiring 100 utils.
  ;;``Tiny" demands are short, low-cost demands lasting only 4 days.
  ;;Finally, a ``year round" demand exists across the entire future,
  ;;requiring a relatively modest amount of resources.
  (def corpus {:Big        {:name "Big"    :start 0 :duration 1095 :quantity 1000}                
               :Medium     {:name "Medium" :start 0 :duration 365  :quantity 500}
               :Small      {:name "Small"  :start 0 :duration 30   :quantity 100}
               :Sporadic   {:name "Tiny"   :start 0 :duration 4    :quantity 20}
               :Year-Round {:name "Year round demand" :start 0 :duration ubound :quantity 15}})  
  ;;With these building blocks, we can use our DSL to gradually express
  ;;more sophisticated sampling rules:
  ;;/textit{random-surge} chooses from a Big, Medium, or Small demand,
  ;;and places the demand somewhere on the timeline.  In this case,
  ;;the choice will not be available for future samples, until all
  ;;choices have been exhausted.
  (def random-surge (s/->transform                                       
                      {:start      random-time}
                      (s/->without-replacement [:Big :Small :Medium])))
  ;;A /textit{ramp} demand is a sequential chain of /textit{Small}, /textit{Medium}, and /textit{Big}
  ;;demands that are temporally adjacent.  This rule composes three
  ;;primitives from the corpus into, effectively, a new primitive demand:
  (def ramp (s/->chain [:Small :Medium :Big]))
  ;;A /textit{random-buildup} demand extends /textit{ramp} demands by
  ;;randomly placing them along the timeline, preserving the ordering
  ;;and contiguiuty of the /textit{ramp} demand.    
  (def random-buildup
    (s/->transform 
     (fn [xs] (let [offset (get-random-time)]
               (map #(update-in % [:start] + offset) xs)))  :ramp))
  ;;A /textit{smallish-surge} chooses between a /textit{Small} and a
  ;;/textit{Medium} demand,  with the former being chosen 3 times as
  ;;often as the latter.  The chosen demand is again placed randomly
  ;;on the timeline:
  (def smallish-surge
    (s/->transform 
     {:start    (get-random-time)}
     (s/->choice {:Small 3 :Medium 1})))
  (def random-sporadic (s/->transform 
                        {:start    get-random-time}
                        :Sporadic))
  ;;Given the plethora of partial sampling rules we defined, as well
  ;;as the primitive rules in the corpus, we can compose them into yet
  ;;another sampling rule, /textit{random-case}.  The new rule
  ;;collects the results of executing multiple replications of random
  ;;surge events, sporadic events, and small surge events.
  ;;Additionally, /textit{random-case} ensures that the
  ;;/textit{Year-Round} demand is present, as well as one
  ;;/textit{random-buildup}.
  (def random-case (s/->concatenate [(s/->replications 2  :random-surge)
                                     (s/->replications 4  :smallish-surge)
                                     (s/->replications 10 :random-sporadic)
                                     :random-buildup
                                     :Year-Round]))
  ;;For clarity, we establish a sampling context by merging the corpus
  ;;of primitive rules with the newly defined, complex sampling rules.
  (def sampler (merge corpus
                 {:random-surge     random-surge                     
                  :ramp             ramp
                  :random-buildup   random-buildup
                  :smallish-surge   smallish-surge   
                  :random-sporadic  random-sporadic
                  :random-case      random-case})) 
  ;;Finally, we can generate a random sample, in this case a notional
  ;;demand future for /textit{Small, Medium, Big, Tiny, Year-Round}
  ;;demands.  The function /textit{random-sample!} takes a number of
  ;;replciations, n, and a rule to sample from, entry-case.  Given
  ;;these arguments, the function defines a constrained sampling rule
  ;;that limits generated samples to the established timline, and also
  ;;limits the maximum duration of any demand to 1500 days.  The
  ;;inlined constraints are then sampled n times to generate a set of
  ;;demand records, or a stochastic demand future:
  (defn random-sample! [n entry-case]
    (->> (s/->constrain {:tfinal ubound 
                         :duration-max 1500}
                        [entry-case])
         (s/->replications n)
         (s/->flatten)
         (s/sample-from sampler)))
    )
