(ns spork.sim.pure.test
  (:require [spork.sim.pure.network :refer :all]
            [spork.sim.data :refer [event-type event-data]]))

;these are low level examples of event handler networks.         
(def sample-net 
  (->> (empty-network "Mynetwork")
    (register-routes 
      {:hello-client {:hello-event 
                       (fn [ctx edata name] 
                         (do (println "Hello!") ctx))}})))
(def simple-net 
  (->> (empty-network "OtherNetwork")
    (register-routes 
      {:hello-client {:all 
                      (fn [ctx edata name] 
                        (do (println "I always talk!")) ctx)}})))

;low-level event propogation
(defn prop-hello [] 
  (propogate-event 
    (->handler-context [:hello-event "hello world!"] nil)
    sample-net))

;high level event propogation
(defn handle-hello [] 
  (handle-event  [:hello-event 0] nil sample-net))

;using combinators to build networks
(defn echo-hello [] 
  (propogate-event 
    (->handler-context [:hello-event "hello world!"] nil noisy-transition) 
    sample-net))

(def message-net 
  (->> (empty-network "A message pipe!")
    (register-routes 
      {:echoing {:echo (fn [{:keys [state] :as ctx} edata name] 
                            (do (println "State:" state)
                              ctx))}
       :messaging {:append (fn [ctx edata name] 
                             (update-in ctx [:state :message] conj 
                                        (event-data edata)))}})))

(defn test-echo [& [msg]]
  (propogate-event (->handler-context :echo [msg]) message-net))

;this is a round-about way of doing business....
(defn test-conj [& xs]
  (let [ctx (->handler-context nil {:message []} default-transition message-net)]
    (reduce (fn [acc x] 
              (handle-event [:append x] acc))
            ctx  xs)))

;a test of multiple events being 'queued' and handled by the message network.
(defn test-messaging [] 
  (handle-events  {:message []}
                  message-net
                  [:echo
                   [:append 2]
                   [:append 3]
                   :echo]))

;add some capabilities to the network...
;like a better message.
(use 'clojure.pprint)
(require 'spork.util.datetime)
(def message-net2 
  (register-routes {:messaging2 {:echo (fn [{:keys [state] :as ctx} edata name]
                                        (do (println "The message is: ")
                                          (pprint (:message state))
                                          ctx))}} message-net)) 
  
;this is an attempt to get at the composable workflow from observable.
(def time-stamped-messages
  (let [print-route (->propogation {:in {:all (fn [ctx edata name] 
                                                (do (pprint (:state ctx))
                                                  ctx))}})        
        add-current-time (fn [ctx] (let [t (spork.util.datetime/date->time (spork.util.datetime/get-date))]
                                     (do (println (str "recording time " t))
                                         (assoc-in ctx [:state :date] t))))] 
    (->> print-route
      (map-handler add-current-time) ;should wrap the whole thing...      
      (vector message-net)
      (union-handlers)))) ;combine it with the message-net.


(def limited-messages   
  (let [print-route (->propogation {:in {:all (fn [ctx edata name] 
                                                (do (pprint (:state ctx))
                                                          ctx))}})]
    (one-time-handler print-route)))

(def n-messages   
  (let [print-route (->propogation {:in {:all (fn [ctx edata name] 
                                                (do (pprint (:state ctx))
                                                          ctx))}})]
    (n-time-handler 4 print-route)))
  

(def exploded-example 
  (let [print-route (->propogation {:in {:all (fn [ctx edata name] 
                                                (do (pprint (:state ctx))
                                                  ctx))}})
        add-current-time (fn [ctx] (let [t (spork.util.datetime/date->time (spork.util.datetime/get-date))]
                                     (do (println (str "recording time " t))
                                         (assoc-in ctx [:state :date] t))))
        handler-function (fn [ctx edata name] 
                             (add-current-time (propogate-event ctx print-route)))
        mapped (register-routes {:blah {:all handler-function}}
                                (empty-network :anonymous))] 
    (->> mapped
        (vector message-net)
        (union-handlers)))) 

;;What if we could alter the network, like adding event-handlers
;;during evaluation? 


(defn test-stamp []
  (handle-event :echo {:state {:message "Hey!"}} time-stamped-messages))
(defn test-stamps []
  (handle-events  {:message []}
                  time-stamped-messages
                  [:echo 
                   [:append 2] 
                   [:append 3] 
                   [:append 50] 
                   :echo]))
;;Works correctly.
(defn tst []
  (handle-event [:append 2] {:message []} 
    (register-routes 
     {:stamper 
      {:all (fn [ctx e nm] 
              (let [t (spork.util.datetime/date->time 
                       (spork.util.datetime/get-date))]
                (do (println (str "recording time " t))
                    (assoc-in ctx [:state :date] t))))}} (empty-network "h"))))

;;This works appropriately.  Happened so fast I missed the timing
;;change earlier.
(defn tsts []
  (->> (map (fn [n] (keyword (str "a" n))) (range 100))
       (handle-events {:message []}  
                      (register-routes 
                       {:stamper 
                        {:all (fn [ctx e nm] 
                                (let [t (spork.util.datetime/date->time 
                                        (spork.util.datetime/get-date))]
                                  (do (println (str "recording time " t))
                                      (assoc-in ctx [:state :date] t))))}} 
                       (empty-network "h")))))

