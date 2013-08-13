(ns spork.cljgui.tst
  (:use  [spork.cljgui.gui]
         [spork.cljgui.events base observe native]))

(def init-timer {:time 0 :running false})
(def dumb-timer (agent {:time 0 :running false}))


(defn check-time [state]
    (if (:running state)
      (do
        (send *agent* check-time)        
        (Thread/sleep 100)
        (assoc state :time (System/currentTimeMillis)))
      state))

(defn start-time [state]
  (do
    (send *agent* check-time)        
    (assoc state :running true)))

(defn stop-time [state] (assoc state :running false))
(defn get-time [agt] (:time @agt))


(defn view-timer [agt] 
  (let [counter (label (str (get-time agt)))
        tickevents  (observe-mutation agt)
        currenttime  (->> tickevents
                       (filter-obs (fn [[o n]] (not= (:time o) (:time n) )))
                       (map-obs (comp :time fnext)))
        labelupdate (subscribe 
                      (fn [t] 
                        (run-now (fn [] (.setText counter (str t))))) 
                      currenttime)]
    (display (empty-frame)
             (shelf (label "This is a timer, synced to an agent.")                    
                    counter))))

;(defmacro swing [exp]
;        (list 'if '(. javax.swing.SwingUtilities (isEventDispatchThread))
;                exp
;                (list '. 'javax.swing.SwingUtilities (list 'invokeAndWait
;                                                                (list 'proxy
;'[Runnable] '[] (list 'run '[] exp))))))