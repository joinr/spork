;;A generic finite state machine representation.
;;Provides a set of data generally useful to any FSM.
(ns spork.ai.machine)
(def inf
  "Internal constant for the upper bound for ticks in a state."
  (java.lang.Double/POSITIVE_INFINITY))

;;we might want to get away from records due to readability issues.
;;statedata could easily be map...although the records give use
;;fast field access for free.
(defrecord statedata [curstate
                      prevstate
                      nextstate 
                      timeinstate
                      timeinstateprior
                      duration
                      durationprior
                      statestart
                      statehistory])
(def blank-data
  "blank slate for finite state machine contexts."
  (statedata. nil nil nil 0 0 inf inf 0 []))

(def spawning-data
  "A new finite state machine prepped to engage some kind of spawning behavior."
  (statedata. :spawning nil nil 0 0 inf inf 0 [])) 

(defn change-state
  "Represents a change in the finite state machine's state, to include 
   optional following states."
  [^statedata fsm newstate & [newduration followingstate instant]]
  (merge fsm {:curstate newstate 
              :nextstate followingstate
              :statehistory (conj (:statehistory fsm) newstate)
              :duration newduration
              :timeinstate 0}))

;;Added an option to not automatically record the state
;;transition.
(defn ^statedata change-statedata
  "More performant version of change-state, which allows the 
   caller to alter the statedata with a new to-state, duration, 
   potential following state, and whether we should record 
   the transition.  Default lower-arity version is to not 
   record the transition."
  ([^statedata  sdata tostate duration followingstate record?]
   (statedata. 
    tostate
    (.curstate sdata)
    followingstate
    0
    (.timeinstate sdata)
    duration
    (.duration sdata)
    (.statestart sdata)
    (if record? (.cons ^clojure.lang.PersistentVector (.statehistory sdata) tostate)
        (.statehistory sdata))))
  ([sd tostate duration followingstate]
   (change-statedata sd tostate duration followingstate false)))

(defn remaining
  "Reports the expected time remaining in the current state."
  [fsm]  
   (- (:duration fsm) (:timeinstate fsm)))

(defn progress
  "Reports the progress, as a value over the interval [0.0 1.0]
   to indicate the distance in state-time coordinates the entity 
   has traveled per expected duration."
  [^statedata sd ]
  (let [duration (.duration sd)
        timeinstate (.timeinstate sd)]
    (if (and (pos? duration) (not= duration inf))
      (double (/ timeinstate duration))
      0)))

(defn add-duration
  "Accounts for the passage of time, updating the 
   statedate by incrementing the entity's time in state.
   If the increment overflows the expected duration, an 
   error is thrown."
  [^statedata sd  amt]
  (let [res  (+ (.timeinstate sd) amt)
        diff (- (.duration sd) res)
        ]
    (if (not (neg? diff))
      (assoc sd :timeinstate res)
      (throw (Exception. (str ["time-in-state exceeds duration in fsm statedata."
                               :tis      res
                               :duration (.duration sd)
                               :diff     diff]))))))
