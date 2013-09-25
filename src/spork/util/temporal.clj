;;A package of utilites for working with time, and data that has some time 
;;ordered quality, like event streams, data that has a notion of start and 
;;duration, etc. 
(ns spork.util.temporal
  (:require [generators :as gen]))

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
        handle (fn [{:keys [t type data]} [es actives state]]
                  (case type
                    :resampling [es actives :changed]
                    :add [(-> es 
                            (conj (drop-demand (+ t (duration-func data)) data))
                            (conj (resample t)))
                          (conj actives data)
                          :added]
                    :drop [es (disj actives data) :dropped]))
        initial-events (into (sorted-set-by earliest) 
                             (map (fn [x] (add-demand (start-func x) x)) xs))]
  (gen/unfold (fn [[es _ _]]  (empty? es))  ;halt when no more events.            
              (fn [[es actives s]]                
                (let [event               (first es)
                      remaining-events    (disj es event)
                      current-time        (:t event)]
                  (handle event [remaining-events actives s]))) 
              [initial-events #{} :init])))

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
   of temporal records xs.  Returns the top N active days."
  [xs & {:keys [start-func duration-func] 
         :or   {start-func :Start duration-func :Duration}}]
  (let [active-count (fn [r] (:count r))
        sorted  (->> (activity-profile (sort-by start-func xs) 
                            :start-func start-func 
                            :duration-func duration-func)
                     (sort-by (fn [[t r]] (active-count r))) 
                     (reverse))
        peak (active-count (second (first sorted)))]
    (take-while (fn [[t r]] (= (active-count r) peak))
                sorted))) 

;;given a sequence of demands, we need a way to compute peak demand for 
;;each src.
(defn peaks-by [f xs & {:keys [start-func duration-func] 
                        :or   {start-func :Start duration-func :Duration}}]
  (into {} 
    (for [[k recs] (group-by f xs)]
      [k (first (peak-activities recs :start-func start-func 
                                      :duration-func duration-func))])))

