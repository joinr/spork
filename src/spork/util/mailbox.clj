(ns spork.util.mailbox
  (:require [clojure [pprint :as pprint]]))

;project-manager is a simple agent that maintains internal state. 
;we design the event handling functions as simple events that the user enters,
;can communicate them to project-manager. 

;Project-manager then routes tasks, using the currently-loaded project as its 
;environment. 

;These are generic routing functions for a service built on message-passing.
;I could abstract this into a simple library.

(defrecord packet [msg-type msg-data])

;These are generic routing functions for a service built on message-passing.
;I could abstract this into a simple library.


(defmacro message-handler [& body]
  `(fn ~'[{:keys [routes state] :as env} 
          {:keys [msg-data msg-type] :as msg}]
       ~@body))

(defmacro pass-through [f]
  `(message-handler (do (~f ~'state) ~'state)))

(defmacro effect [f] 
  `(message-handler (do (~f) ~'state)))


(def empty-control-state {:routes {} :state {}})

(defn route-message [{:keys [routes state] :as s} msg]
  (if-let [f (get routes (:msg-type msg))] ;finds a route if applicable
    (f s msg)
    (do ((get s :printer pprint/pprint) 
          ["Message type unknown" msg]) s))) 

(defn send-and-ignore 
  ([agt msg msgdata]
    (do (send-off agt route-message (->packet msg msgdata))
      :sent))
  ([agt msg] (send-and-ignore agt msg nil)))

(defn dosend
  "Sends a series of instructions to the agent."
  [agt packets]
  (doseq [p packets]
    (send-and-ignore agt (:msg-type p) (:msg-data p))))


(defn mvc->model [mvc]   (deref (:model mvc)))
(defn mvc->routes [mvc]  (comp (:routes mvc->model) mvc))
(defn mvc->view [mvc]    (:view mvc))

;default routes for our project management controller...
;all we have to do is wire up a gui to send messages to the controller, and 
;voila...

(def default-routes 
  {:ping         (effect #(pprint/pprint :pong))
   :set-routes   (message-handler
                   (assoc env 
                          :routes msg-data)) ;state->msg->state
   :assoc-route  (message-handler  
                   (assoc-in env [:routes (first msg-data)]                            
                             (second msg-data)))
   :dissoc-route (message-handler  
                   (assoc-in env [:routes (first msg-data)]                            
                             (second msg-data)))
   :print        (message-handler 
                  (do ((get env :printer pprint/pprint) 
                     msg-data)
                    env))
   :eval         (message-handler 
                   (do ((get env :printer pprint/pprint) 
                             (eval msg-data))
                     env))
   :set-state    (message-handler 
                   (assoc env :state msg-data))})

