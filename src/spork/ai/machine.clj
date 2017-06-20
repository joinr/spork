;;A generic finite state machine representation.
;;Provides a set of data generally useful to any FSM.
(ns spork.ai.machine)

;'Provides a set of data useful to any FiniteStateMachine.
(def inf (java.lang.Double/POSITIVE_INFINITY))

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

(def blank-data (statedata. nil nil nil 0 0 inf inf 0 [])) 
(def spawning-data (statedata. :spawning nil nil 0 0 inf inf 0 [])) 

(defn change-state [^statedata fsm newstate & [newduration followingstate instant]]
  (merge fsm {:curstate newstate 
              :nextstate followingstate
              :statehistory (conj (:statehistory fsm) newstate)
              :duration newduration
              :timeinstate 0}))

(defn ^statedata change-statedata [^statedata  sdata tostate duration followingstate]
  (statedata. 
   tostate
   (.curstate sdata)
   followingstate
   0
   (.timeinstate sdata)
   duration
   (.duration sdata)
   (.statestart sdata)
   (.cons ^clojure.lang.PersistentVector (.statehistory sdata) tostate)))

(defn remaining [fsm]  
   (- (:duration fsm) (:timeinstate fsm)))
    
(defn progress [^statedata sd]
  (let [duration    (.duration    sd)
        timeinstate (.timeinstate sd)]
    (if (and (pos? duration) (not= duration inf))
      (double (/ timeinstate duration))
      0)))

(defn add-duration [^statedata sd  amt]
  (let [res  (+ (.timeinstate sd) amt)
        diff (- (.duration sd) res)
        ]
    (if (not (neg? diff))
      (assoc sd :timeinstate res)
      (throw (Exception. (str ["time-in-state exceeds duration in fsm statedata."
                               :tis      res
                               :duration (.duration sd)
                               :diff     diff]))))))

