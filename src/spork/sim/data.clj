(ns spork.sim.data
  (:use [spork.util.datetime])
  (:require  [spork.util.reducers]
             [clojure.core [reducers :as r]]))

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
  (event-type [e] (get e :type))
  (event-data [e] (get e :data))
  (event-id [e]   (get e :id)) 
  (event-time [e] (get e :time))
  (event-from [e] (get e :from) )
  (event-to [e] (get e :to))
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

;;Overrides the built in map->event, defined by defrecord.
;;That turned out to be surprisingly slow...as it uses the 
;;event class's static method /create, passes a map, and then
;;interns a bunch of symbols....weird.
(definline map->event [{:keys [type data id time from to]}]
  `(event. ~type ~data ~id ~time ~from ~to))

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

(definline first-entry  [m] `(reduce-kv (fn [acc# k# v#] (reduced (clojure.lang.MapEntry. k# v#))) nil ~m))
(definline first-key    [m] `(reduce-kv (fn [acc# k# v#] (reduced k# )) nil ~m))
(definline first-val    [m] `(reduce-kv (fn [acc# k# v#] (reduced v#)) nil ~m))
(definline nth-entry  [n m] 
  `(let [bound# ~n
         n#     (long-array [0])] 
     (reduce-kv (fn [acc# k# v#] 
                  (if (== @n# bound#)
                    (reduced (clojure.lang.MapEntry. k# v#))
                    (do (aset n# 0 (unchecked-inc (aget n# 0)))
                        acc#))) nil ~m)))
(definline nth-key   [n m] 
  `(let [bound#  ~n
         n#     (long-array [0])]
     (reduce-kv (fn [acc# k# v#] 
                  (if (== (aget n# 0) bound#)
                    (reduced k#)
                    (do (aset n# 0 (unchecked-inc (aget n# 0)))
                        acc#))) nil ~m)))
(definline nth-val   [n m] 
  `(let [bound#  ~n
         n#     (long-array [0])] 
     (reduce-kv (fn [acc# k# v#] 
                  (if (== @n# bound#)
                    (reduced v#)
                    (do (aset n# 0 (unchecked-inc (aget n# 0)))
                        acc#))) nil ~m)))

;;using faster numeric comparisons now.
(defn ^long fast-comp [x y] (if (== x y) 0 (if (> x y) 1 -1)))

;;okay....we can obviate some of the problems with the schedule if we create
;;a custom type.  basically a wrapper around a priority-map.

;;define a schedule, a sorted-map of event queues keyed by time.
(def empty-schedule (sorted-map-by fast-comp))
(definline empty-schedule? [s] `(identical? ~s spork.sim.data/empty-schedule))

(defn get-segment
  "[s] retrieve next queue of events from schedule s
   [s t] retrieve queue of events for time t from schedule s"
  ([s]    (first-val s))
  ([s t]  (get s t emptyq)))

(defn active?
  "Determine if the schedule has any events, or if [s t] if events exist for a
   specific time"
  ([s]   (not (identical? s empty-schedule)))
  ([s t] (get s t)))

(defn get-time [s]
  "[s] Determine the :time of the pending event in the schedule, if no events
   are pending (an empty schedule) we return nil"
  (first-key s))

;;#May be unncessary
;; (defn next-active [s]
;;   "Clear empty queues.  If our current queue is no longer active (has no events)
;;    we remove it from consideration iff there are more pending queues."
;;   (loop [sched s]    
;;     (cond (empty-schedule? s) s 
;;           (active? s)  s
;;           :else   (recur (dissoc s (get-time s))))))

;; (defn next-time [s]
;;   "[s] Get the next active time"
;;   (get-time (next-active s)))

#_(defn next-time [s]
   "[s] Get the next active time"
   (get-time s))

(defn next-event
  "[s] retrieve first event in next active queue of events from schedule s
   [s t] retrieve first event in next active queue of events for time t from s"
  ([s]   (peek (first-val s)))
  ([s t] (when-let [res (get s t)] (peek res))))

(defn put-event
  "[s e] insert one or more events into a schedule, based on event time.
   Note - we allow arbitrary event times to be added, since this is a primitive
   data structure.  In real applications, we would likely have a limitation on 
   the times that could be added to the queue (i.e. only times >= current-time)"
  ([s e] (let [t (or (event-time e) (get-time s) 0.0)
               q (or (get-segment s t) emptyq)]
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
  (let [fe (first-entry s)
        t  (key fe)
        q  (pop (val fe))]
    (if (empty? q) (dissoc s t)
        (assoc s t q))))

(defprotocol IEventReducer
  (event-reducer [xs]))

;protocol for operating on abstract event collections.
(defprotocol   IEventSeq
  (add-event   [ecoll e] "add an event to the collection of events")
  (drop-event  [ecoll]   "remove an event from the collection of events")
  (first-event [ecoll]   "return the next event in the collection")
  (nth-event   [ecoll n] "Return the nth event in the seq"))

(defprotocol IChunkedEventSeq
  (event-chunks [ecoll] "returns a lazy seq of chunks of ordered events."))

;;added to allow caching.
(defprotocol IEventSchedule
  (current-time [obj]
    "Return the current time of the event collection
    (i.e. the time of the first  event).  It's much faster to lazily 
     cache the result instead of computing it over and over.")
   (next-time [obj] "Compute the time of the next event in the sequence."))

;;This is going to cost us in practice, because we're
;;going to get hammered on the lookup cost due to extending the type
;;vs. implementing inline directly...
;;It'd make sense to just derive from persistentTreeMap and provide
;;an implementation for the schedule, or wrap the map using deftype...
;;Since we're not using the tree-map directly, it makes sense to
;;just wrap it in a deftype (downside is we have to define serialization
;;primitive for it...)

(extend-type clojure.lang.PersistentTreeMap
  IEventSeq
  (add-event   [m e] (put-event m e)) 
  (drop-event  [m]   (take-event m))                       
  (first-event [m]   (next-event  m))
  (nth-event  [m n] 
    (assert (pos? n) (throw (Exception. "index out of bounds in nth-event")))
    (let [cnt (long-array [-1])]
      (reduce-kv (fn [acc t q]
                   (if (< (+ (aget cnt 0) (count q)) n)
                     (do (aset cnt 0 (+ (aget cnt 0)  (count q)))
                         acc)
                     (let [offset (- (dec n) (aget cnt 0))]
                       (reduced (nth q offset)))))
                 nil m)))
  ;;PERFORMANCE NOTE:
;;This is a known hotspot....should be cached internally.
;;We end up repeatedly computing this, which typically calls
;;a bunch of times into the same sorted seq, which is expensive.
;;So, it's far faster to go ahead and cache the result in the
;;container, and propogate it internally.  We're also suffering
;;a little bit due to extending vs inline implementation.
  IEventSchedule 
  (current-time 
    [ecoll]
    (event-time (first-event ecoll)))
  (next-time
    [ecoll]
    (event-time (nth-event ecoll 1)))
  IChunkedEventSeq
  (event-chunks [ecoll] (vals ecoll))
  IEventReducer
  ;;This is a more general reducer....could probably lift it up into 
  ;;spork.util.reducers...
  (event-reducer [sched]
    (reify 
      clojure.core.protocols/CollReduce
      (coll-reduce [coll f]
        (reduce-kv (fn [acc k q]
                     (if (reduced? acc) @acc
                         (reduce (fn [inner evt]
                                   (if (reduced? inner) @inner
                                       (f inner evt)))
                                 acc q))) sched))
      (coll-reduce [coll f init]  
        (reduce-kv (fn [acc k q]
                     (if (reduced? acc) @acc
                         (reduce (fn [inner evt]
                                   (if (reduced? inner) @inner
                                       (f inner evt)))
                                 acc q)))   init sched)))))
(extend-protocol IEventSeq
  clojure.lang.PersistentVector
  (add-event [v e] (conj v e))
  (drop-event [v] (cond (or (= 1 (count v)) 
                            (= v [])) []
                        (> (count v) 1) (subvec v 1)))                       
  (first-event [v] (first v))
  (nth-event [v n] (nth v n))                                     
  nil 
  (add-event [m e] (add-event empty-schedule e)) 
  (drop-event  [m] (throw (Exception. "Empty schedule")))                       
  (first-event [m]  nil)
  (nth-event   [m]  nil))

;;protocol-derived functionality
(defn add-events
  "Add multiple events to the event sequence."
  [ecoll es] 
  (reduce add-event ecoll es)) 



;;#THis is a potential drag.  We don't really want to do this..we
;;should be returning chunks...
(defn event-seq 
  "Return a lazy seq of ordered events."
  [ecoll]
  (if (satisfies? IChunkedEventSeq ecoll)
    (concat (event-chunks ecoll))
    (take-while #(not (nil? %)) 
                (map first (iterate (fn [[x xs]] 
                                      (when-let [nxt (first-event xs)]
                                        [nxt (drop-event xs)]))
                                    [(first-event ecoll) (drop-event ecoll)])))))
(defn do-events
  ([s f n] (if (satisfies? IEventReducer s)
             (do (reduce (fn [acc v] (f v)) nil (r/take n (event-reducer s))))
             (doseq [evt (take n (event-seq s))] (f evt))))
  ([s f]   (if (satisfies? IEventReducer s)
             (do (reduce (fn [acc v] (f v)) nil (event-reducer s)))
             (doseq [evt (event-seq s)] (f evt)))))

(defn print-events  
  "print the first n items of the schedule, produces a lazy seq...
   Planning to provide a pretty-printer for events."
  ([s]   (do-events s println)) 
  ([s n] (do-events s println n))) 

(defmacro make-event 
  "Optimized constructor for events.  Designed to supplant map->event, 
   since that ends up being very slow.  Allows us to retain the versailitiy 
   of building events using map args.  Not callable as a function, but 
   may be wrapped in a function."
  [{:keys [type data id time from to]}]
  `(event. ~type ~data ~id ~time ~from ~to))

(comment 
;;Testing.....

;;schedule events
(defn random-schedule [n tmax]
  "Define a simple random schedule, with up to 10 events for n days randomly 
   spread across time [0.0 tmax]"
    (->> (r/repeatedly n #(rand-int tmax)) 
         (r/map inc)
         (r/map (fn [t] 
                  (make-event {:type :task 
                               :data t
                               :id   1
                               :time t})))
         (r/map-indexed (fn [i evt] (assoc evt :id i)))
         (add-events empty-schedule)))

(def sample-schedule 
  (let [wednesdays (take 6 (map date->num (daystream "Wednesday")))
        chores (repeat ["wake-up" "take out trash" "eat cereal" "pet cat" 
                        "work"])]
    (->schedule (zip-events wednesdays chores))))  

(defn bullet-list [coll] (map #(str "->" % \newline) coll))
(def easy-schedule (->schedule (take 10 (repeat :multiply))))
)





