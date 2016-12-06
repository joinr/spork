;This is a simple data structure, implemented on top of the event sequence 
;substrate from sim.data, which supposes an ordering of events by time.  
;The agenda is drawn from Abelson and Sussman's Structure and Interpretation of
;Computer Programs (SICP), chapter 3 circuit simulator.
(ns spork.sim.agenda
;  (:refer-clojure :exclude [contains?])
  (:require [spork.sim [data :as sim]]
            [spork.data [cell :as cell]]
            [clojure.core [reducers :as r]]))

;Defined a simple protocol for agendas.  The operations on schedules are 
;similar to Abelson and Sussman's agenda from Structure and Interpretation of 
;computer programs.  The difference is that we allow one to bound the time 
;horizon (in the case of a desire to truncate the processing of events on the 
;schedule).
(defprotocol IAgenda
  (previous-time [a] "Return the time of the last step, if any.")
  (final-time [a] "Peek at the final time of the event on the schedule.")
  (set-final-time [a t] "Set an upper bound on the final time of the schedule.")
  (agenda-count [a] "Return the count of the items on the agenda.")
  (time-segments [a] "Return a map of agenda events, keyed by time segment.")
  (add-times [a ts] "Add a elements of time to the agenda.")
  (get-times   [a]  "Return an unordered set of all times in the agenda."))

(defn feasible-time? [a t]
  (let [tf (final-time a)]
    (cond (or (= tf :inf) (nil? tf)) true
          (<= t tf) true 
          :else false)))

;;We have some overhead in the calls to current-time on this guy,
;;since it delegates to first-event.  If we could cache the
;;first event, we don't have to do the traversal over and
;;over, which saves us a lot of time.  So, the typical
;;option is to have a mutable value that serves as a one-time
;;cache, where if we ask for the first event, we save it behind
;;the scenes and cache the value (like a thunk).
;;We can use a single-cell object array to act as our mutable
;;value...that'd be compatible with our serialization...
;;The cached version is significantly faster at access the
;;head event now.
(defrecord agenda [tprev tfinal schedule item-count times
                   ^spork.data.cell.cell head ^spork.data.cell.cell t]
  IAgenda 
  (previous-time  [a] tprev)
  (final-time     [a] tfinal)
  (set-final-time [a tf]  (agenda. tprev tf schedule item-count times head t))
  (agenda-count   [a]     item-count)
  (time-segments  [a]     schedule)
  (add-times [a ts] 
    (let [itms          (atom item-count)
          [nsched nt i] (reduce (fn [[sched knowns i :as acc] t]
                                  (if (knowns t) acc
                                      [(sim/add-event sched (sim/->simple-event :time  t))
                                       (conj! knowns t)
                                       (inc i)]))
                              [schedule (transient times) item-count]  (r/filter #(not (contains? times %)) ts))]
      (agenda. tprev tfinal nsched  i (persistent! nt) (cell/->cell) (cell/->cell))))
  (get-times   [a] times)
  spork.sim.data.IEventSeq 
  (add-event  [a e] ;note->allowing the agenda to have events beyond tfinal  
    (agenda. tprev tfinal (sim/add-event schedule e) (inc item-count)
             (conj times (sim/event-time e)) (cell/->cell) (cell/->cell))) 
  (drop-event  [a]  
    (if (> item-count 0)  
       (let [tnext (sim/current-time schedule)
             snext (sim/drop-event schedule)]
         (agenda. tnext tfinal snext (dec item-count)
                  (if (not= tnext 
                        (sim/current-time snext))
                    (disj times tnext)
                    times) (cell/->cell) (cell/->cell)))
       (throw (Exception. "No items left in the agenda!"))))    
  (first-event [a] (if (.isRealized head) (.deref head)
                       (let [fe (sim/first-event schedule)
                             _  (reset! head fe)]
                         fe)))                         
  (nth-event [a n] (sim/nth-event schedule n))
  sim/IEventSchedule
  (current-time [obj] (if (.isRealized t) (.deref t)
                          (let [ct (sim/current-time schedule)
                                _ (reset! t ct)]
                            ct)))
  (next-time    [obj] (sim/next-time    schedule)))

(def empty-agenda (->agenda nil nil nil 0 #{} (cell/->cell) (cell/->cell)))

;; (defrecord agenda [tprev tfinal schedule item-count times
;;                    ^spork.data.cell.cell head]
;;   IAgenda 
;;   (previous-time  [a] tprev)
;;   (final-time     [a] tfinal)
;;   (set-final-time [a tf]  (agenda. tprev tf schedule item-count times head))
;;   (agenda-count   [a]     item-count)
;;   (time-segments  [a]     schedule)
;;   (add-times [a ts] 
;;     (let [itms          (atom item-count)
;;           [nsched nt i] (reduce (fn [[sched knowns i :as acc] t]
;;                                   (if (knowns t) acc
;;                                       [(sim/add-event sched (sim/->simple-event :time  t))
;;                                        (conj! knowns t)
;;                                        (inc i)]))
;;                               [schedule (transient times) item-count]  (r/filter #(not (contains? times %)) ts))]
;;       (agenda. tprev tfinal nsched  i (persistent! nt) (cell/->cell))))
;;   (get-times   [a] times)
;;   spork.sim.data.IEventSeq 
;;   (add-event  [a e] ;note->allowing the agenda to have events beyond tfinal  
;;     (agenda. tprev tfinal (sim/add-event schedule e) (inc item-count)
;;              (conj times (sim/event-time e)) (cell/->cell))) 
;;   (drop-event  [a]  
;;     (if (> item-count 0)  
;;        (let [tnext (sim/current-time schedule)
;;              snext (sim/drop-event schedule)]
;;          (agenda. tnext tfinal snext (dec item-count)
;;                   (if (not= tnext 
;;                         (sim/current-time snext))
;;                     (disj times tnext)
;;                     times) (cell/->cell)))
;;        (throw (Exception. "No items left in the agenda!"))))    
;;   (first-event [a] (if (.isRealized head) (.deref head)
;;                        (let [fe (sim/first-event schedule)
;;                              _  (reset! head fe)]
;;                          fe)))                         
;;   (nth-event [a n] (sim/nth-event schedule n)))


;;(def empty-agenda (->agenda nil nil nil 0 #{} (cell/->cell)))


(defn get-quarter [day] ((comp inc int) (/ day 90)))

(defn quarter
  "Assumes time is measured in days.  Converts time into a quarterly measure."
  [a] (get-quarter (sim/current-time a))) 
(defn elapsed
  "Report the amount of time that elapsed since the last event.  Useful for 
   operations requiring time deltas, such as integration."
  [a] (if-let [t (sim/current-time a)]
        (- t (or (previous-time a) 0.0))
        0.0))

(defn unbounded?
  "Predicate indicating that the agenda has no upper bound on its time horizon."
  [a]
  (let [ft (final-time a)]
    (or (nil? ft) 
        (identical? ft :inf))))

;;This is causing a bit of a slowdown...
;;We end up calling it pretty often.
(defn still-time?
  "Predicate indicating that the agenda still has work remaining."
  [a] (and (not= (agenda-count a) 0)
           (or (unbounded? a)
               (<= (sim/next-time a) (final-time a)))))

(defn has-time?
  "Predicate indicating if the agenda exists over a specific point in time."
  [a t] (contains? (get-times a) t))

(defn add-time
  "Add at least one time event to the agenda for time t.  If an entry for t
   already exists, the time is ignored (since the agenda already has activity 
   on said day."
  [a t]  (if (has-time? a t)
           a
           (add-times a #{t})))

(defn advance-time
  "Advance the agenda to the next event."
  [a] (sim/drop-event a))

(defn agenda-seq
  "Unfold a sequence of intermediate agendas as time advances."
  [a] (take-while still-time? 
          (iterate (fn [ag] (if (still-time? ag) 
                              (advance-time ag))) a)))

(comment ;testing 
(def simple-agenda (add-time empty-agenda 2))
(def larger-agenda (add-times empty-agenda (range 100)))
;;much faster....
;(def larger-cached (add-times empty-agenda-cache (range 100)))
(defn time-pairs [] 
  (map (juxt sim/previous-time sim/current-time)
       (agenda-seq larger-agenda)))
)

;;older uncached version, mucho mas slower...

;; (defrecord agenda [tprev tfinal schedule item-count times]
;;   IAgenda 
;;   (previous-time  [a] tprev)
;;   (final-time     [a] tfinal)
;;   (set-final-time [a tf]  (agenda. tprev tf schedule item-count times))
;;   (agenda-count   [a]     item-count)
;;   (time-segments  [a]     schedule)
;;   (add-times [a ts] 
;;     (let [itms (atom item-count)
;;           [nsched nt i] (reduce (fn [[sched knowns i :as acc] t]
;;                                   (if (knowns t) acc
;;                                       [(sim/add-event sched (sim/->simple-event :time  t))
;;                                        (conj! knowns t)
;;                                        (inc i)]))
;;                               [schedule (transient times) item-count]  (r/filter #(not (contains? times %)) ts))]
;;       (agenda. tprev tfinal nsched  i (persistent! nt))))
;;   (get-times [a] times)
;;   spork.sim.data.IEventSeq 
;;   (add-event  [a e] ;note->allowing the agenda to have events beyond tfinal  
;;     (agenda. tprev tfinal (sim/add-event schedule e) (inc item-count)
;;              (conj times (sim/event-time e)))) 
;;   (drop-event  [a]  
;;     (if (> item-count 0)  
;;        (let [tnext (sim/current-time schedule)
;;              snext (sim/drop-event schedule)]
;;          (agenda. tnext tfinal snext (dec item-count)
;;                   (if (not= tnext 
;;                         (sim/current-time snext))
;;                     (disj times tnext)
;;                     times)))
;;        (throw (Exception. "No items left in the agenda!"))))    
;;   (first-event [a] (sim/first-event schedule))
;;   (nth-event [a n] (sim/nth-event schedule n)))

;(def empty-agenda (->agenda nil nil nil 0 #{}))
