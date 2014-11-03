;;A package of utilites for working with time, and data that has some time 
;;ordered quality, like event streams, data that has a notion of start and 
;;duration, etc. 
(ns spork.util.temporal
  (:require [spork.util [generators :as gen]]
            [spork.data [priorityq :as pq]]))

;;Note -> sorted set was originally a problem here.  I have resulted
;;to using a priority queue.

;;remixed
(defn temporal-profile 
  "Extracts an event-driven profile of the concurrent records over time from a 
   sequence of values, where start-func is a function that yields a start time 
   for each record, and duration-func is a function that yields a numeric 
   duration for each record."
  [xs & {:keys [start-func duration-func]
         :or {start-func :Start duration-func :Duration}}] 
  (let [add-demand  (fn [t x] {:t t :type :add  :data x})
        drop-demand (fn [t x] {:t t :type :drop :data x})
        resample    (fn [t]   {:t t :type :resampling :data nil})
        earliest    (fn [l r] (compare (:t l) (:t r)))
        handle (fn [e [es actives state]]
                 (let [t       (:t e)
                       data    (:data e)]
                   (case (:type e)
;                     :resampling [es actives :changed]
                     :add (let [to-drop (drop-demand (+ t (duration-func data)) data)
                                nxt     (conj es [to-drop (:t to-drop)])]
                            [nxt 
                             (conj actives data)
                             :added])
                     :drop (let [res (disj actives data)]
                             [es res :dropped])
                     (throw (Exception. (str "unknown event" e))))))
        initial-events (into pq/emptyq
                             (map (fn [x] [(add-demand (start-func x) x) (start-func x)]) xs))]
  (gen/unfold (fn [state]  (empty? (first state)))  ;halt when no more events.            
              (fn [state]                
                (let [es      (first state)
                      actives (second state)
                      s       (nth state 2)
                      event               (peek es)
                      remaining-events    (pop es)
                      current-time        (:t event)]
                  (handle event [remaining-events actives s]))) 
              [initial-events #{} :init])))

;; (defn temporal-profile
;;   "Extracts an event-driven profile of the concurrent records over time from a 
;;    sequence of values, where start-func is a function that yields a start time 
;;    for each record, and duration-func is a function that yields a numeric 
;;    duration for each record."
;;   [xs & {:keys [start-func duration-func]
;;          :or {start-func :Start duration-func :Duration}}] 
;;   (let [add-demand  (fn [t x] {:t t :type :add  :data x})
;;         drop-demand (fn [t x] {:t t :type :drop :data x})
;;         resample    (fn [t]   {:t t :type :resampling :data nil})
;;         earliest    (fn [l r] (compare (:t l) (:t r)))
;;         handle (fn [e estate]
;;                  (let [es      (first estate)
;;                        actives (second estate) 
;;                        state   (nth estate 2)
;;                        t       (:t e)
;;                        data    (:data e)]
;;                    (case (:type e)
;;                      :resampling [es actives :changed]
;;                      :add [(-> es 
;;                                (conj (drop-demand (+ t (duration-func data)) data))
;;                                (conj (resample t)))
;;                            (conj actives data)
;;                            :added]
;;                      :drop [es (disj actives data) :dropped])))
;;         initial-events (into pq/emptyq
;;                              (map (fn [x] [(add-demand (start-func x) x) (start-func x)]) xs))]
;;   (gen/unfold (fn [state]  (empty? (first state)))  ;halt when no more events.            
;;               (fn [state]                
;;                 (let [es      (first state)
;;                       actives (second state)
;;                       s       (nth state 2)
;;                       event               (peek es)
;;                       remaining-events    (pop es)
;;                       current-time        (:t event)]
;;                   (handle event [remaining-events actives s]))) 
;;               [initial-events #{} :init])))

;;Pretty general function.
(defn activity-profile
  "Given a sequence of records, xs, with :Start and :Duration keys, computes 
   a sorted map of {t {:actives #{...} :count n}} for each discrete time in 
   the records.  Each sample will have the records that were concurrently 
   active at the sample time, and a count of the records."
  [xs & {:keys [start-func duration-func] 
         :or {start-func :Start duration-func :Duration}}]
  (->> (temporal-profile xs :start-func start-func :duration-func duration-func)
       (map (fn [[es actives s]] {:t (:t (first es)) :actives actives :s s}))
       (partition-by :t)
       (map last)
       (concat)
       (map (fn [x] [(:t x) (-> x (dissoc :t) 
                                  (dissoc :s)
                                  (assoc  :count (count (:actives x))))]))
       (into (sorted-map)))) ;;sorted map may be gratuitous         

(defn peak-activities
  "Computes peak concurrent activities, as per activity-profile, for a sequence 
   of temporal records xs.  Returns the top N active days.  Caller may supply 
   a custom peak-function, which operates on records of 
   {t {:actives #{...}} :count n}"
  [xs & {:keys [start-func duration-func peak-function] 
         :or   {start-func :Start duration-func :Duration 
                peak-function (fn [r] (:count r))}}]
  (let [sorted  (->> (activity-profile (sort-by start-func xs) 
                            :start-func start-func 
                            :duration-func duration-func)
                     (sort-by (fn [[t r]] (peak-function r))) 
                     (reverse))
        peak    (peak-function (second (first sorted)))]
    (take-while (fn [[t r]] (= (peak-function r) peak))
                sorted))) 

;;given a sequence of demands, we need a way to compute peak demand for 
;;each src.
(defn peaks-by
  "Groups xs by a key function, f, and for each group, returns the peak activity
   sample as defined by an optional peak-function.  Defaults to the number of 
   active records in an activity sample as the peak."
  [f xs & {:keys [start-func duration-func peak-function] 
           :or   {start-func :Start duration-func :Duration
                  peak-function (fn [r] (:count r))}}]
  (into {} 
    (for [[k recs] (group-by f xs)]
      (let [[t peak-record] 
                (first (peak-activities recs :start-func start-func 
                                        :duration-func duration-func
                                        :peak-function peak-function))] 
        [k (assoc peak-record :t t)]))))

;;New functions for working on temporal data:
(defn active-intervals 
  [activities]
  (filter (fn [[delta rs]]
            (seq (:actives rs)))
          (for [[[from ls] [to rs]] (partition 2 1 activities)]
            [[from to]
             rs])))

(defn weighted-sample-by [sample-func actives]
  (reduce (fn [{:keys [low hi n sum]} [[tprev t] {:keys [count actives]}]]
            (let [delta (- t tprev)
                  qs (map sample-func actives)
                  total (reduce + qs)
                  weighted (* total delta)]
              {:low (min low total)
               :hi  (max hi total)
               :n   (+ n delta)
               :sum (+ sum weighted)}))
          {:low Long/MAX_VALUE :hi 0 :n 0 :sum 0} (active-intervals actives)))

;;This is a bit of a hack for now, but not too bad.
(defn weighted-stats [xs sample-func]
  (let [{:keys [low hi n sum]} (weighted-sample-by sample-func xs)]
    {:min low :max hi :average (float (/ sum n)) :n n :sum sum}))

;;It'd probably be much nicer to build interval trees...
;;Then we can answer queries over intervals..
;;Interval trees are just like quad trees, with the exception that we
;;only have one dimension.  So we don't have to split....


;;test data
(comment
(def recs [{:name :blah 
            :Start 10
            :src 1
            :Duration 20
            :Quantity 2}          
           {:name :blee
            :Start 15 
            :src 1
            :Duration 20
            :Quantity 33}
           {:name :foo 
            :Start 23
            :src 2
            :Duration 33
            :Quantity 56}
           {:name :fizz
            :Start 2 
            :src 1
            :Duration 4
            :Quantity 9}])

;; (assert (= (weighted-stats (activity-profile recs) :Quantity)
;;            {:min 2, :max 91, :average 51.68, :n 50, :sum 2584}))
)
