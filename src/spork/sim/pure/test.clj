(ns spork.sim.pure.test
  (:require [spork.sim.pure.network :refer :all]))

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
    (->handler-context :hello-event "hello world!" nil)
    sample-net))

;high level event propogation
(defn handle-hello [] 
  (handle-event  [:hello-event 0] nil sample-net))

;using combinators to build networks
(defn echo-hello [] 
  (propogate-event 
    (->handler-context :hello-event "hello world!" nil noisy-transition) 
    sample-net))

(def message-net 
  (->> (empty-network "A message pipe!")
    (register-routes 
      {:echoing {:echo (fn [{:keys [state] :as ctx} edata name] 
                            (do (println "State:" state)
                              ctx))}
       :messaging {:append (fn [{:keys [state] :as ctx} edata name] 
                                   (update-in ctx [:state :message] conj edata))}})))

(defn test-echo [& [msg]]
  (propogate-event (->handler-context :echo nil [msg]) message-net))

;this is a round-about way of doing business....
(defn test-conj [& xs]
  (reduce (fn [acc x] 
            (handle-event [:append 0 x] acc message-net))
          {:message []} xs))

;a test of multiple events being 'queued' and handled by the message network.
(defn test-messaging [] 
  (handle-events {:state {:message []}}
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
        add-current-time (fn [ctx] (assoc-in ctx [:state :date]
                                             (spork.util.datetime/get-date)))] 
    (->> print-route
      (map-handler add-current-time)))) ;should wrap the whole thing...
;      (vector message-net)
;      (union-handlers message-net)))) ;combine it with the message-net.


;;(handle-event :echo {:state {:message "Hey!"}} message-net2)
;;(handle-events  {:state {:message []}} message-net2  [:echo [:append 2] [:append 3] [:append 50] :echo])

