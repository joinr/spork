(ns spork.sim.data
  (:use [spork.util.datetime])
  (:require [clojure.core.reducers :as r]))

(defprotocol IEvent
  (event-type [e] 
   "Given a chunk of data, tries to coerce its event-type.  Keyword values are  
   viewed as an intrinsic event type.  Maps (and records) dispatch off a :type 
   key.  Any other generic sequences dispatch off the first value.")
  (event-data [e]   
  "Extracts event data from e.  Handles maps/records, sequences, and keywords.
   Keywords are assumed to have no data.  Sequences are assumed to have data 
   as the second element of the sequence.")
  (event-id [e] "Return a (usually numeric) ID associated with event e.")
  (event-time [e] "Report the timestamp for event e.")
  (event-from [e] "Return the originating source of event e, usually numeric." )
  (event-to [e] "Return the intended destination for event e, if any."))

(def sequential-event 
  {:event-type (fn [e] (nth e 0 nil))
   :event-data (fn [e] (nth e 1 nil))
   :event-id   (fn [e] (nth e 2 nil))
   :event-time (fn [e] (nth e 3 nil))
   :event-from (fn [e] (nth e 4 nil))
   :event-to   (fn [e] (nth e 5 nil))})

;basic implementations for core structures.
(extend clojure.lang.PersistentVector
  IEvent 
  sequential-event)
(extend clojure.lang.PersistentList
  IEvent
  sequential-event)
(extend-protocol IEvent 
  nil
  (event-type [e] nil)
  (event-data [e] nil)
  (event-id [e] nil)
  (event-time [e] nil)
  (event-from [e] nil)
  (event-to [e] nil)
  clojure.lang.PersistentArrayMap
  (event-type [e] (:type e))
  (event-data [e] (:data e))
  (event-id [e]   (:id e)) 
  (event-time [e] (:time e))
  (event-from [e] (:from e) )
  (event-to [e] (:to e))
  clojure.lang.Keyword ;keywords are events...
  (event-type [e] e)
  (event-data [e] e)
  (event-id [e]   e) 
  (event-time [e] nil)
  (event-from [e] nil)
  (event-to [e] nil))

(defn ->simple-event
  "Constructs a simple map-based event."
  ([type t] {:time t :type type})
  ([type t data] {:time t :type type :data data}))

;An event structure with more complex data, used primarily for scheduling.
(defrecord event [type data id time from to]
  IEvent 
  (event-type [e] type)
  (event-data [e] data)
  (event-id   [e] id)
  (event-time [e] time)
  (event-from [e] from)
  (event-to   [e] to))

(defn time-event
  "Default constructors for building temporal events; really just a nice
   way of representing time-stamped packets.  The contextual information is
   contained in the packet (another record).  We use the time-stamp on events
   to keep them sorted in a schedule.  The time stamp is somewhat arbitrary,
   in that we could use any comparable key for comparison.  However, we tend
   to denote an absolute notion of time or ordering, hence the convention of
   using a float as our :time key."
  ([t] (->simple-event :time t))
  ([t evt]
    (if (= (class evt) event)
      (merge {:time t} evt)
      (->simple-event :generic t evt))))

;;A sim engine is quite simple...I will adapt this from the F# version.
;;We basically just need a priority q.
;;Events are queued in priority order according to time t (some single floating
;;point val).
;;This implemention will be similar to the Agenda from SICP.
;;We have multiple events associated with time, however...
;;I want to use sorted-map to keep this straight.  It has logarithmic
;;performance....
;;Another idea is to have a sorted-map based on float keys (time), whose values
;;are queues.  Could use vectors to add to the queue.
;;Allows for insertion order.
;;Should be pretty fast...
;;Dunno.

(def emptyq clojure.lang.PersistentQueue/EMPTY)
(defmethod print-method clojure.lang.PersistentQueue [q,w]
  (print-method (seq q) w))


(defn first-entry [m] (reduce-kv (fn [acc k v] (reduced (clojure.lang.MapEntry. k v))) nil m))
(defn first-key [m] (reduce-kv (fn [acc k v]   (reduced k ))) nil m)
(defn first-val [m] (reduce-kv (fn [acc k v]   (reduced v)) nil m))

;define a schedule, a sorted-map of event queues keyed by time.
(def empty-schedule (sorted-map))

(defn empty-schedule? [s]
  "If schedule s has one eventqueue, and it's emptyq, the schedule is empty"
  (when-let [q (first-val s)]
    (identical? q emptyq)))

(defn get-segment
  "[s] retrieve next queue of events from schedule s
   [s t] retrieve queue of events for time t from schedule s"
  ([s]    (first-val s))
  ([s t]  (get s t emptyq)))

(defn active?
  "Determine if the schedule has any events, or if [s t] if events exist for a
   specific time"
  ([s]   (not (identical? (get-segment s) emptyq)))
  ([s t] (not (identical? (get-segment s t) emptyq))))   

(defn get-time [s]
  "[s] Determine the :time of the pending event in the schedule, if no events
   are pending (an empty schedule) we return nil"
  (first-key s))

(defn next-active [s]
  "Clear empty queues.  If our current queue is no longer active (has no events)
   we remove it from consideration iff there are more pending queues."
  (loop [sched s]    
    (cond (empty-schedule? s) s 
          (active? s)  s
          :else   (recur (dissoc s (get-time s))))))

(defn next-time [s]
  "[s] Get the next active time"
  (get-time (next-active s)))

(defn next-event
  "[s] retrieve first event in next active queue of events from schedule s
   [s t] retrieve first event in next active queue of events for time t from s"
  ([s]   (peek (get-segment (next-active s))))
  ([s t] (peek (get-segment (next-active s) t)))) 

(defn put-event
  "[s e] insert one or more events into a schedule, based on event time.
   Note - we allow arbitrary event times to be added, since this is a primitive
   data structure.  In real applications, we would likely have a limitation on 
   the times that could be added to the queue (i.e. only times >= current-time)"
  ([s e] (let [t (or (event-time e) (get-time s) 0.0)
               q (get-segment s t)]
             (assoc s t (conj q e)))))

(defn put-events [s es] 
  "Insert multiple events (es) into the schedule."
  (reduce #(put-event %1 %2) s es))

(defn zip-events [ts packets] 
  "Zip a sequence of times and packets to produce a sequence of events."
  (map #(apply time-event %) (seq (zipmap ts packets))))

(defn ->schedule 
  "Unified constructor for building schedules.  No args invokes a reference to 
   the empty-schedule.  Providing a sequence of events inserts them into an 
   empty schedule.  Providing a sequence of times and packets zips and inserts 
   them into an empty schedule."
  ([] empty-schedule)
  ([evts] (put-events empty-schedule evts))
  ([ts packets] (put-events empty-schedule (zip-events ts packets)))) 
                            
(defn take-event [s]
  "[s] Remove the next event from schedule s, returning remaining schedule."
  (let [snext     (next-active s)
        remaining (pop (get-segment snext))]
    (if (empty? remaining)
      (next-active (dissoc snext (get-time snext)))
    (next-active
      (assoc snext (get-time snext) (pop (get-segment snext)))))))

;protocol for operating on abstract event collections.
(defprotocol IEventSeq
  (add-event [ecoll e] "add an event to the collection of events")
  (drop-event [ecoll]  "remove an event from the collection of events")
  (first-event [ecoll] "return the next event in the collection"))

(extend-protocol IEventSeq
  clojure.lang.PersistentVector
  (add-event [v e] (conj v e))
  (drop-event [v] (cond (or (= 1 (count v)) 
                            (= v [])) []
                        (> (count v) 1) (subvec v 1)))                       
  (first-event [v] (first v))  
  clojure.lang.PersistentTreeMap 
  (add-event [m e] (put-event m e)) 
  (drop-event [m] (take-event m))                       
  (first-event [m] (next-event  m))
  nil 
  (add-event [m e] (add-event empty-schedule e)) 
  (drop-event [m] (throw (Exception. "Empty schedule")))                       
  (first-event [m] nil))

;;protocol-derived functionality
(defn add-events
  "Add multiple events to the event sequence."
  [ecoll es] 
  (reduce add-event ecoll es)) 

(defn current-time 
  "Return the current time of the event collection (i.e. the time of the first 
   event)."
  [ecoll]
  (event-time (first-event ecoll)))
  
(defn next-time
  "Compute the time of the next event in the sequence."
  [ecoll]  
  (when (first-event ecoll)
    (event-time 
     (first-event 
      (drop-event ecoll)))))

(defn event-seq 
  "Return a lazy seq of ordered events."
  [ecoll]
  (take-while #(not (nil? %)) 
    (map first (iterate (fn [[x xs]] 
                          (when-let [nxt (first-event xs)]
                            [nxt (drop-event xs)]))
                        [(first-event ecoll) (drop-event ecoll)]))))
(defn do-events
  ([s f n] (doseq [evt (take n (event-seq s))] (f evt)))
  ([s f]   (doseq [evt (event-seq s)] (f evt))))

(defn print-events  
  "print the first n items of the schedule, produces a lazy seq...
   Planning to provide a pretty-printer for events."
  ([s]   (do-events s println)) 
  ([s n] (do-events s println n))) 

(comment 
;;Testing.....

;;schedule events
(defn random-schedule [n tmax]
  "Define a simple random schedule, with up to 10 events for n days randomly 
   spread across time [0.0 tmax]"
    (->> 
      (repeatedly n #(rand-int tmax)) 
      (map (fn [t] 
             (for [i (map inc (range (rand-int 10)))] 
               (map->event {:id 1
                            :type :task 
                            :time t 
                            :data (keyword (str 'task-type i))}))))
      (flatten)
      (map-indexed (fn [i evt] (assoc evt :id i)))
      (add-events empty-schedule)))

(def sample-schedule 
  (let [wednesdays (take 6 (map date->num (daystream "Wednesday")))
        chores (repeat ["wake-up" "take out trash" "eat cereal" "pet cat" 
                        "work"])]
    (->schedule (zip-events wednesdays chores))))  

(defn bullet-list [coll] (map #(str "->" % \newline) coll))
(def easy-schedule (->schedule (take 10 (repeat :multiply))))
)





