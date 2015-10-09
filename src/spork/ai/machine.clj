;a generic finite state machine.
(ns spork.ai.machine)

;'Provides a set of data useful to any FiniteStateMachine.
;Option Explicit
;Private updatestack() As Single
;Private updateIdx As Long
;Public updateCount As Long
;
;Public CurrentState As String 'Description of the current state
;Public PreviousState As String 'Description of the previous state
;Public nextstate As String 'Description of the next state, if not known, will be currentstate
;Public timeinstate As Single 'time in current state
;Public timeinstatePrior As Single
;Public duration As Single 'duration of the state, if not specified, const infinite
;Public DurationPrior As Single
;Public StateStart As Single 'time state started
;Private Const infinite As Single = 999999.999
;Private Const buffer As Long = 50
;Public StateHistory As Collection

(def inf (java.lang.Double/POSITIVE_INFINITY))

(defrecord statedata [curstate prevstate nextstate 
                      timeinstate timeinstateprior duration durationprior
                      statestart statehistory])

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
