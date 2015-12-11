;;A package of utilites for working with time, and data that has some time 
;;ordered quality, like event streams, data that has a notion of start and 
;;duration, etc. 
(ns spork.util.temporal
  (:require [spork.util [generators :as gen]]
            [spork.data [priorityq :as pq]]
            [clojure.core [reducers :as r]]))

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
                     :resampling [es actives :sampled]
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
                (let [es      (first  state)
                      actives (second state)
                      s       (nth state 2)
                      event               (peek es)
                      remaining-events    (pop  es)
                      current-time        (:t   event)]
                  (if (or (= s :sampled)
                          (when-let [ne (peek remaining-events)]
                            (= current-time (:t ne))))
                    (handle event [remaining-events actives s])
                    (handle event [(conj remaining-events [(resample current-time)
                                                           current-time])
                                   actives s])))) 
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



;;Imported from proc.

;;once we have the demandrecords, we'd "like" to slurp in the records
;;that are of interest, to augment our demand meta data.

;;Specifically, we want to append the demandnames to the
;;demandrecords. this is pretty simple...
;;we can slurp the demandrecords into a stream of records, traverse
;;it,  unpack the known demandnames

;;we need to define ranges for how
;;long things were at locations, i.e. we need to insert records.                 
                
;;locations are currently encoded in string form, the location name is
;;assumed to be unique.  So, we can parse the location name to get
;;some information out of the loc.

;;Given a set of demandtrends, we can compute a fill-function.
;;That is, a function that knows the fill/unfill status for every 
;;demand as a function of time.

;;We can do the same thing from a location-table.
;;The big idea is to have these functions built, or derived,
;;and then systematically choose points to sample.

;;So, using vector representations present in the table, we 
;;have discrete functions of time over which nothing changes.
;;So, the most useful datastructure here is to build up an
;;interval tree for each unit in supply (out of the location data), 
;;and an interval tree for each demand in the demand trends. 
;;The interval tree sparsely encodes the temporal data and 
;;allows us to sample discretely at points in time.


;;This allows us to find the sample closest in time to 
;;the desired sample.
(defn previous-sample [ts t] (first  (rsubseq ts <= t)))
(defn previous-entry [m k t]
  (let [es (get m k)]
    (previous-sample es t)))

;;We to sample exactly, else return nothing.
(defn sample  [ts t] (get ts t))

;;the idea is to traverse the demandtrends, and build up
;;a function that can sparsely sample a given trend at a point in
;;time.

;;For each demandname, we want to keep a sorted map of samples 
;;as soon as they show up. 
;;The map is the sample.

(defn sample-trends 
  ([xs trendkey samplekey samplefunc]  
     (persistent!
      (reduce (fn [acc sample]
                (let [trend (trendkey  sample)
                      t     (samplekey sample)]
                  (if-let [samples (get acc trend)]
                    (assoc! acc trend (assoc samples t (samplefunc sample)))
                    (let [samples (sorted-map)]
                      (assoc! acc trend (assoc samples t (samplefunc sample)))))))
              (transient {})
              xs)))
  ([xs trendkey samplekey] (sample-trends xs trendkey samplekey identity)))

(defn zero-trends 
  ([m tzero zerof] 
     (persistent! 
      (reduce-kv (fn [acc trend samples]
                   (assoc! acc trend
                           (assoc samples tzero (zerof (second (first samples))))))
                 (transient {}) 
                 m)))
  ([m] (zero-trends m 0 identity)))

(defn get-samples [samplers]  
  (for   [s samplers
          [trend ts] s]
    (keys ts)))

;;loke some, but returns the nth position of the element found, or nil
;;if no pred yields true.
(defn some-n [pred xs] 
  (let [found (atom nil)]
        (do (reduce (fn [acc x]
                       (if (pred x) 
                         (do (reset! found acc)
                             (reduced acc))
                         (inc acc)))
                     0 xs)
            @found)))

;;we want to combine samples from one or more samplers.
;;In this case, we're adding a new set of samples, derived from the
;;demand trends.  We know that we have comparitively sparse samples 
;;in the locations trends.  So, we can build a new sampler according 
;;to the demandtrends samples.

;;Alternately, we can just sample t = 1...n at daily intervals and 
;;pull out the quantities (that sounds palatable, possibly
;;unmanageable though).

;;The downside here is that we end up with fine-grained sampling, 
;;but we may not need that resolution.  We can always alter the 
;;sampling frequency too though....

;;So one idea is to sample from t = 1...some tmax for every member 
;;of the population in the locations.
;;Do the same for the demand trends.

(defn samples-at 
  ([m t intersectf]
     (reduce-kv (fn [acc u samples]
                  (conj acc [u (second (intersectf samples t))]))
                '() 
                m))
  ([m t] (samples-at m t previous-sample)))

;;note: there is another type of sampling, existence sampling.  We
;;only sample items that actually exist, so there is the possibility
;;of returning nil.

;;Trying to compress the sampling so we can postproc faster.
;;If we include a deltat in the time, we can always upsample (drop) or 
;;downsample (expand) as needed.  Will not add samples earlier then 
;;the observed min, and later than the observerd max.  To accomplish
;;that, add your own min/max as an entry in the colls.
(defn minimum-samples 
  ([pad colls]
     (let [min (atom (first (first colls)))
           max (atom @min)]
       (->> (r/mapcat identity colls)
            (reduce (fn [acc x]
                      (do (when (< x @min) (reset! min x))
                          (when (> x @max) (reset! max x))
                          (-> acc
                              (conj! x)
                              (conj! (+ x pad))
                              (conj! (- x pad)))))
                    (transient #{}))
            (persistent!)          
            (filter (fn [x]
                      (and (>= x @min)
                           (<= x @max))))
            (sort))))
  ([colls] (minimum-samples 1 colls)))

(defn smooth-samples 
  ([pad colls]
     (let [ts  (minimum-samples pad colls)]
       (minimum-samples pad [ts])))
  ([colls] (smooth-samples 1 colls)))
       
;;#Tom Note# - formalize a sampling API, to include a time-series 
;;with start,  deltat (time since last sample). This will allow us to 
;;more cleanly work with temporal, time-weighted data.

;;allows us to sample only when the time intersects....
;;this is hackish.
(defn start-stop-sample [start stop tmap t]
  (->> (samples-at tmap t)
       (r/filter (fn [tr-sample]
                   (when-let [x (second tr-sample)]
                     (and (<= (start x) t) (>= (stop x) t)))))))

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


;testing
(comment
  (def sampledata [[:c 1 4]
                   [:c 4 7]
                   [:c 8 4]
                   [:b 2 8]
                   [:b 9 2]
                   [:b 5 8]
                   [:a 0 3]
                   [:a 7 2]
                   [:a 6 2]
                   [:a 3 2]])
  
  (def trs (sample-trends sampledata first second (fn [[tr x y]] y)))

  ;;we can compute the minimum sampling points...
  (def mins (minimum-samples (get-samples [trs])))
  
  )
