;a namespace for defining pure functionally reactive values....
;this is not dissimilar to FRP....
;Inspired by samples from Tomas Petricek's awesome Real World 
;Functional Programming, the first book that got me to "grok" 
;functional reactive programming....
;He, in turn, based his implementation on Elliot and Hudak's FRAN 
;library for Haskell.  Well, this is my interpretation.  
;Aside from theoretical problems that the FRP heavy-hitters have noted, 
;like time and space leaks, I think the primitive form shown here can 
;be very useful for scripting tasks, like animation.  I don't know how 
;well it'll hold up to large problems, only time will tell.  However, 
;I am very very excited to find out.

;The ultimate aim is to do declarative animation, animation being 
;a rather nebulous term.  The inspiration for this is to easily 
;define animated shapes for my gui library, to form a nice 
;visualization framework.  However, the method is entirely 
;generalizable, and could easily be repurposed for functional 
;reactive simulation. 

;Right now, we assume that dt is a change in time, so that its
;value is numeric.  
(ns spork.cljgui.reactive
  (:use spork.cljgui.gui))

(defprotocol IReaction
  (react [r t]))

;(defrecord context [t])

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


