;courtesy of Stuart Sierra 2012
(ns spork.cljgui.flippersample
  (:use [spork.cljgui.components.swing]
        [spork.cljgui.events base observe native]))

;(import-swing)

(defn new-flipper []
  (agent {:total 0, :heads 0,
          :running false,
          :random (java.util.Random.)}))

;note, most agent "actions" are f::agentstate -> agentstate
;side-effects are wrapped inside by sending messages to the agent, then 
;processing.
;I THINK we can post messages any time (even at the end), but we need to 
;return state....

(defn calculate [state]
  (if (:running state)
    (do (send *agent* calculate) ;if we're running, keep running by queueing msg
        ;(Thread/sleep 100)
        (assoc state ;update state now....
          :total (inc (:total state))
          :heads (if (.nextBoolean (:random state))
                   (inc (:heads state))
                   (:heads state))))
    state)) ;else, bypass. return the state....

(defn start [state]
  (send *agent* calculate) ;<--queue a message to the agent, keeps a loop going.
  (assoc state :running true)) ;<----update the state in this thread.

(defn stop [state]
  (assoc state :running false)) ;<----notice the lack of any new message.  just 
                                ;updates the state.....effectively stops the 
                                ;loop.

(defn error [state]
  (if (zero? (:total state)) 0.0
      (- (/ (double (:heads state))
            (:total state))
         0.5)))

(def flpr (new-flipper))  ;flipper agent lives here.

;Note....most of this stuff lives in swing, which means any events dispatched
;here, dispatch on the Swing EDT (event dispatch thread).  That means that 
;there is a barrier between us and Swing.  It's something we have to consider
;--or have considered for us---when dealing with Swing/clojure interop.

(defn flipper-app [flipper]
  ;; Construct components:
  (let [b-start (JButton. "Start")
        b-stop (doto (JButton. "Stop")
                 (.setEnabled false))
        total (text-field "0")
        heads (text-field "0")
        t-error (text-field "0.0")
        timer (Timer. 100 nil)] ;<---there's polling going on in this one.

    ;; Setup actions:
    (with-action timer e
      (let [state @flipper]
        (.setText total (str (:total state)))
        (.setText heads (str (:heads state)))
        (.setText t-error (format "%.10g" (error state)))))
    (with-action b-start e
      (send flipper start)
      (.setEnabled b-stop true)
      (.setEnabled b-start false)
      (.start timer))
    (with-action b-stop e
      (send flipper stop)
      (.setEnabled b-stop false)
      (.setEnabled b-start true)
      (.stop timer))

    ;; Create window and layout:
    (doto (JFrame. "Flipper")
      (.setContentPane
       (doto (JPanel.)
         (.add (JLabel. "Total:"))
         (.add total)
         (.add (JLabel. "Heads:"))
         (.add heads)
         (.add (JLabel. "Error:"))
         (.add t-error)
         (.add b-start)
         (.add b-stop)))
      (.pack)
      (.setVisible true))))

;run-app lets us call this function on another thread, basically like running 
;asynch in F#.  The difference is, Swing has its own dedicated EDT....
;so we have to use run-app (which delegates to the SwingUtils/InvokeLater
;lib call, to compute our function on the right thread.
(defn engage []
  (run-app (fn [] (flipper-app flpr)))) 
                                          
;The cool thing is, we can run as many swing apps as we want, all of which 
;share the same (non-swing) flipper agent.  This allows us to have true MVC.

;One possible workflow then, is to have some clojure concurrency construct 
;maintain state (the model).  Define functions to concurrently change said state.

;Build the Swing application, and parameterize it based on the aforementioned 
;concurrency item, using the aforementioned modification functions.

;Launch the swing application on a separate thread using run-app or invokelater.
 ;This allows us to execute MANY swing apps that synchronize with the 
 ;concurrency data in a thread-safe mannner.

;---------

;;I want to transform the flipper app to use observables instead of action
;;listeners and low-level swing ops....

;What if we pass in an observable flipper?  Flipevent.  
;That way, we recast our swing implementation to respond to a stream of 
;events, rather than directly manipulating the flipper.  Also, we 
;can have it only refresh as often as the events come in....

;We need to get some events from swing components, and handle them inside of 
;Swing components.  


;how do we get notified if the flipper changes? 
;Rather than have a timer to poll off of, let's just have the agent 
;broadcast events when it changes, and then filter these relative to some 
;time interval (to avoid overloading).   

(def init-timer {:time 0 :running false})
(def dumb-timer (agent {:time 0 :running false}))

(defn check-time [state]
    (if (:running state)
      (do
        (send *agent* check-time)        
        (Thread/sleep 50)
        (assoc state :time (System/currentTimeMillis)))
      state))

(defn start-time [state]
  (do
    (send *agent* check-time)        
    (assoc state :running true)))

(defn stop-time [state] (assoc state :running false))
(defn get-time [agt] (:time @agt))

(defn get-flipevent [agt] 
  (->> (observe-mutation agt)
   (filter-obs (fn [[o n]] (not= (:time o) (:time n) )))
   (map-obs (comp :time fnext))))


(defn eventful-flipper
  "Given flipper, an agent, and flipevent, an observed event, returns a 
   modelview record that has the agent as the model, and a reactive flipper GUI
   as the view.  The flipper GUI handles events on the Swing thread (mostly 
   button clicks), as well as an external (i.e. non-swing thread) event...
   the flipevent.  The flipper serves as a separate model and controller, while
   the GUI provides a view of the model and a means to control via IO."
  [flipper flipevent]
  ;; Construct components:
  (let [b-start (JButton. "Start")
        b-stop (doto (JButton. "Stop")
                 (.setEnabled false))
        total (text-field "0")
        heads (text-field "0")
        t-error (text-field "0.0")

        time-change (->> flipevent
                    (subscribe  
                        (fn [e]
                          (let [state @flipper]
                            (.setText total (str (:total state)))
                            (.setText heads (str (:heads state)))
                            (.setText t-error (format "%.10g" (error state)))))))

        startclick (->> (get-observer b-start :action)
                        (:actionPerformed)
		                    (subscribe 
		                       (fn [e]       
		                         (send flipper start)
		                         (.setEnabled b-stop true)
		                         (.setEnabled b-start false)
		                         )))
		        
        stopclick (->> (get-observer b-stop :action)
                       (:actionPerformed)
                       (subscribe 
                         (fn [e]
                           (send flipper stop)
                           (.setEnabled b-stop false)
                           (.setEnabled b-start true)
                           )))]      

    ;; Create window and layout:
    (make-modelview
      flipper
      (shelf (label "Total:") total (label "Heads:") heads (label "Error:")
              t-error b-start b-stop)
      {:time-change time-change
       :startclick startclick
       :stopclick stopclick})))

(defn test-flippers
  "Generate a flipper based on the eventful-flipper model.  A modelview is built
   based on a new flipper agent, and a flipevent derived from the agent's state.
   The modelview is then displayed in a new JFrame, which is returned.
   If n > 1, then n flippers GUIs will be created, each synced to the same
   underlying flipper model.  However, the GUIs themselves (particularly the 
   button interactions) will not communicate between threads.  This means that
   they'll send messages to the flipper (events) to start and stop, but the 
   guis (i.e. the start and stop buttons) aren't syncronized between each gui.
   I'll fix that in another iteration."
  ([n] (let [flp       (new-flipper) 
             flipevent (get-flipevent dumb-timer)
             panelkey  (fn [idx] (keyword (str "Panel_" idx)))
             submodels (take n 
                         (repeatedly 
                           (fn [] (eventful-flipper flp flipevent))))             
             mvc (make-modelview 
                   flp 
                   (apply stack (map get-view submodels)) 
                   (reduce merge 
                           (map-indexed 
                             (fn [idx m] {(panelkey idx) (get-events m)}) 
                                submodels)))]           
		    (send dumb-timer start-time)
		    (display (JFrame. "Flipper") (get-view mvc))))
  ([] (test-flippers 1)))

  


