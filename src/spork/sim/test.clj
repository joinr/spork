;;A set of tests and examples for the simcontext API.
(ns spork.sim.test
  (:use [spork.sim.simcontext])
  (:require [spork.util.reducers :as r]))

(def ctx (add-time 0 empty-context))

(defn lots-of-events [n]
  (time (dotimes [i 1]
          (reduce (fn [ctx n] (request-update n :some-entity :generic ctx))
                  ctx
                  (r/range n)))))

(defn lots-of-events! [n]  
  (time (dotimes [i 1]
          (request-updates (r/map (fn [n] [n :some-entity :blah]) (r/range n))
                           ctx))))

(defn lots-of-events!! [n]    
  (time (dotimes [i 1]          
          (request-updates (r/map (fn [n] [n :some-entity :blah]) (r/range n))
                           (transient ctx)))))
                                  
(defn lots-of-events!!! [n]
  (time (dotimes [i 1]
          (reduce (fn [ctx n] (request-update n :some-entity :generic ctx))
                  (transient ctx)
                  (r/range n)))))


;;This is an old testing scheme.  Port it later
(comment 

(defn get-line [] (deref (future (read-line))))
(defn ucase [s] (.toUpperCase s))

(defn gen-test []
  (loop [s "hello"]
    (let [nxt (do (println "please type name!")
                  (get-line))]
	    (if (= nxt "quit")
          (do (println "quitting!")
              nxt)
	        (do (println (str "hello" s))
	            (recur nxt))))))

(defn simplegreet [n]
  (let-events [(hello)]
      (->> (add-event emptycontext :hello)
           (event-seq (route [:hello] (println "Hello! " (get-line))))
           (take n ))))

(defn greeter
  "A function that, given an initial event context, will poll for a user's name
   and then reply with a greeting."
  ([ec]
     (let-events [(newname)
                  (getinput)]
                 (let [quit? #(= (ucase (event-data %)) "QUIT")
                       terminate (route [:newname :getinput] 
                                        (process (set-events *context* [])))
                       
                       ui    (route [:getinput]
                                    (process 
                                     (do (println "Please type your name!")
                                         (set-events *context* 
                                                     (add-events *events* [(newname (get-line))])))))
                       greet (route [:newname]
                                    (process 
		                     (when (and (not (nil? *state*)) (not= *state* ""))
		                       (do (println (str "Greetings,  " *state*  \!))
                                           (set-events *context* 
                                                       (add-event *events* (getinput :keyboard)))))))
                       nm    (route [:newname]
                                    (process
                                     (if (quit? *event*) 
                                       (terminate *context*)                       
                                       (-> *context*
                                           (set-state (event-data *event*))
                                           (greet)))))]                              	           	 
                   (event-stepper #((comp nm ui) %) ec))))
  ([] (greeter (add-event emptycontext (->event :getinput :keyboard)))))



;;Sample output
;=> (greeter)
;Please type your name!
;=>Tom
;Greetings,  Tom!
;Please type your name!
;=>Kirk
;Greetings,  Kirk!
;Please type your name!
;=>Anon 7
;Greetings,  Anon 7!
;Please type your name!
;=>Garth
;Greetings,  Garth!
;Please type your name!
;=>Quit
;#DEVS.events.eventcontext
;{:state "Garth", 
; :events [#DEVS.events.event{:type :getinput, :data :keyboard}]}

(defn greeter-frp
  "A function that, given an initial event context, will poll for a user's name
   and then reply with a greeting."
  ([ec]
     (let-events [(newname)
                  (getinput)]
                 (let [terminate (route [:newname :getinput]
                                        (do (println "computation finished!") 
                                            (process (set-events *context* []))))
                       quit? #(= (ucase (event-data %)) "QUIT")
                       view-state (fn [ec] (do (println ec)
                                               ec))
                       eventlog (ref [])
                       logevent! (fn [msg ec] (dosync (alter eventlog conj msg)
                                                      ec)) 
                       ui     (->> (process
                                    (logevent! "ui" (set-events *context* 
                                                                (add-events *events* 
                                                                            [(newname (get-line))]))))
                                   (filter-route (type-filter :getinput)))                      
                       greet  (->> (process 
                                    (logevent! "greet"
                                               (when (and (not (nil? *state*)) (not= *state* ""))
                                                 (do (println (str "Greetings,  " *state*  \!))
                                                     (set-events *context* 
                                                                 (add-event *events* (getinput :keyboard)))))))
                                   (filter-route (type-filter :newname)))                                              
                       nm     (->> (process
                                    (logevent! "name"
                                               (if (quit? *event*)
                                                 (terminate *context*)
                                                 (-> *context*
                                                     (set-state (event-data *event*))
                                                     (greet)))))                          
                                   (filter-route (type-filter :newname)))]                   
                   [(event-stepper #((comp view-state nm ui) %) ec) @eventlog])))
                                        ;(event-stepper ui ec))))
  ([] (greeter-frp (add-event emptycontext (->event :getinput :keyboard)))))
)
