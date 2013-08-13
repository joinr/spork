(ns spork.cljgui.events.react
  (:require [cljgui.events [observe :as obs]
                           [behavior :as beh]]))

(defprotocol IReaction
  (react [r t]))

;(defrecord context [t])

(defmacro defsignal [context-fields])

;forever 
;get-time  
;get-event
;map 
;lift1..liftn 




(defrecord reactor [rfun]
  IReaction 
  (react [r t] (rfun t)))

(defn make-reactor [f]
  (reactor. 
    (fn [t] (f t))))

(defn map-reactor [f reaction] 
  (make-reactor #(f (react reaction %))))

(defn scale [scalar reaction]
  (make-reactor #(react reaction (* scalar %))))

(defn offset [amount reaction]
  (make-reactor #(react reaction (+ % amount))))

(defn constant [c]
  (reactor. (fn [_] c)))

(defn varying
  ([init] (reactor. (fn [t] (+ t init))))
  ([] (reactor. (fn [t] t))))
  

;lifting n-arity functions allows us to use normal 
;functions of n-arity, applying them to multiple reactions.
;we basically evaluate the reaction at a given point in time
;and feed the values into f, returning in turn a function 
;of reaction context that can be operated on by f.
(defn lift1 [f reaction]
  (map-reactor f reaction))

(defn lift2 [f2 r1 r2]
  (reactor.
    (fn [t] 
      (f2 (react r1 t) (react r2 t)))))

(defn lift3 [f3 r1 r2 r3]
  (reactor.
    (fn [t]
      (f3 (react r1 t) (react r2 t) (react r3 t)))))

;this is probably error prone, but may be highly useful.
(defn lift [fmulti coll]
  (reactor. 
    (fn [t] 
      (apply fmulti (map #(react % t) coll)))))
(defn sin [^Double v]
  (Math/sin v))

(defn degrees [v]
  (Math/toDegrees v))
(defn radians [v]
  (Math/toRadians v))

(defn wiggle [r]
  (make-reactor (->> (lift2 * r (constant Math/PI)) 
                     (lift1 sin))))           

  
;(react  (scale 2.0 
;           (map-reactor (fn [t] [t]) 
;               (scale 0.25 
;                     (map-reactor (fn [t] [(* 100 t)])  
;                         (varying))))) 10)  


