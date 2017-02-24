;;A generic finite state machine representation.
;;Provides a set of data generally useful to any FSM.
(ns spork.ai.machine)

(def inf (java.lang.Double/POSITIVE_INFINITY))

(defrecord statedata [curstate ;Description of the current state
                      prevstate
                      nextstate 
                      timeinstate
                      timeinstateprior
                      duration
                      durationprior
                      statestart
                      statehistory])

(def blank-data (statedata. nil nil nil 0 0 inf inf 0 [])) 
  

(defn change-state [fsm newstate & [newduration followingstate instant]]
  (merge fsm {:curstate newstate 
              :nextstate followingstate
              :statehistory (conj (:statehistory fsm) newstate)
              :duration newduration
              :timeinstate 0}))

(defn remaining [fsm]  
   (- (:duration fsm) (:timeinstate fsm)))

            

;'Quick, instantaneous blips between state changes, usually with the intent to revert back soonafter
;Public Sub BlipState(newstate As String, Optional followingstate As String)
;
;If CurrentState <> newstate Then
;    StateHistory.add CurrentState
;End If
;
;CurrentState = newstate
;PreviousState = CurrentState
;
;If followingstate = vbNullString Then  'followingstate
;    nextstate = followingstate
;    duration = 0
;End If
;End Sub


;'TODO -> wrap this into a list of states ....
;'newstate
;'Write now, it only goes back one, then cycles on infinitely between 2 states, only really good for
;'quick blips, but it's efficient

;Public Sub RevertState()
;Dim tmpstate As String
;Dim tmpdur As Single
;Dim tmptimeinstate As Single
;
;StateHistory.add CurrentState
;
;tmpdur = duration
;tmpstate = CurrentState
;tmptimeinstate = timeinstate
;
;
;CurrentState = PreviousState
;duration = DurationPrior
;timeinstate = timeinstatePrior
;

;'PreviousState
;'DurationPrior
;'tmpstate
;'tmpdur
;StateHistory.add CurrentState 'shows the reversion
;End Sub

(defn progress [{:keys [duration timeinstate] :as sd}] 
  (if (and (pos? duration) (not= duration inf))
    (double (/ timeinstate duration))
    0))


(defn add-duration [{:keys [timeinstate] :as sd} amt]
  (assoc sd :timeinstate (+ timeinstate amt)))
