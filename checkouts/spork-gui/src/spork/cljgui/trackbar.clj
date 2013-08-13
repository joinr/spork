;Tom Spoon Feb 2012 

;the beginnings of a small utility to create track bars....specifically, to 
;show timed events along a horizontal axis.

;I'd like for users to be able to define an event, 
;and then add instances of it to a horizontal track representing time. 
  ;Not sure how to abstract time yet, probably stick with integral values for 
  ;now to give an absolute notion of time....

;First task is to render and event, on a scrollable axis. 
;Atomic events are visualized as simple colored rectangles, whose duration 
;is indicated by the length of the rectangle. 

;For a given time, we have events of class A.
;  Class A events happen at time 10, 20 ,30, each for duration of 5.

;That means, given a representation of event data, I should be able to 
;draw a scrollable horizontal panel, with 3 rectangular boxes, each the same 
;unit height, and 5 units of length.

;|         ========        ========        ========
;| | | | | | | | | | | | | | | | | | | | | | | | | | | | |

;In the end, we simply need a list of events, ordered by time, 
;which can be consulted to get a rendering of an event track. 

(ns spork.cljgui.trackbar
  (:use [cljgui gui asyncdrawing scenegraph  grid]))

;Focus on drawing a single instance of an event.
(defprotocol ITemporal
  (start-time [t])
  (duration [t]))

(extend-protocol ITemporal
  clojure.lang.PersistentVector
  (start-time [t] (first t))
  (duration [t] (fnext t)))


;this is a minimal event context for stuff that gets put into a trackbar.
(defrecord trackevent [name t duration data]
  ITemporal
  (start-time [e] t)
  (duration [e] duration))

;protocol-derived functionality....
(defn end-time
  "Report a track's end-time"
  [t] (+ (start-time t) (duration t))) 

(defn event-bounds
  "Compute the horizontal bounds of an event (typically start and end times)."
  [e]
  [(start-time e) (end-time e)])

(defn compare-ranges
  "Compare two events ev1 and ev2, ordered by start"
  ([[s1 e1] [s2 e2]] (compare-ranges s1 e1 s2 e2)) 
  ([s1 e1 s2 e2]     
    (cond  (< s1 s2) -1 ;event1 starts earlier
           (> s1 s2) 1 ;event1 starts later
           (= s1 s2) ;when start is equal
             (cond (< e1 e2)  -1 ;event1 ends earlier
                   (< e2 e1) 1 ;event2 ends earlier
                   :else      0))))

(defrecord track [name data events])

(def simpletrack 
  (->track "track1" (->rectangle :black 0 0 1 1) nil))

;(defn add-event [t e]
;  (

(defn random-track [n step]
  (let [revents (take n (iterate (fn [n] (+ n (rand-int step))) 0))]
    (assoc simpletrack :events revents)))

(def track2 
  (let [events (for [ev (:events simpletrack)]
                 [ev (max (- 50 (* ev 0.5)) 1)])]
    (assoc simpletrack :events events))) 

(defn track->scene
  ([{:keys [name data events] :as track} trackheight]
	  (let [{:keys [width height]} (get-bounds data)
	        scaled  (let [scale (/ trackheight height)]
                   (if (= scale 1) 
                     data                     
                     (scale-scene scale scale data)))]
	    [(->rectangle :white 0 0 (reduce max events) trackheight)
       (fade-scene 0.5                 
          (for [[x dur] events]          
            (translate-scene x 0 scaled)))]))
  ([track] (track->scene track 20)))
       
(defn paint-track [t] (paint-scene (track->scene t)))   


(comment 
(def sample-events [(->trackevent :e1 0 20) (->trackevent :e2 -4 10)
                    (->trackevent :e3 5 30) (->trackevent :e4 5 29)])
(defn event-test [] 
  (let [e1 (->trackevent :e1 0 20)
        e2 (->trackevent :e2 -4 10)
        e3 (->trackevent :e3 5 30)
        e4 (->trackevent :e4 5 29)]
    (sort-by event-bounds compare-ranges [e1 e2 e3 e4])))

(defprotocol ITrack
  (conj-event [t e]) 
  (drop-event [t e]))

;A track is a logical association of ITrackables, basically temporally-sequenced
;events.  This is derived from a multi-track recording background.
;We define a set of resources (events), that have some context (data) and a 
;unique name.  
;a track should also be seen as a function that, when 
;evaluated at a time t, will produce a collection of events at t.
(defrecord track [name events schedule]
  ITrack
  (conj-event [t e] (track. name (assoc events (event-name e) e) 
                                 (schedule-event schedule e))
  (drop-event [t e] (track. name (dissoc events (event-name e)) 
                                 (drop-event schedule e)))
  ITrackable 
  (start-time [t] (start-time (next-event schedule)))
  (duration [t] (- (end-time (last-event schedule)) 
                   (start-time (first-event schedule))))))



(defn shift-track
  "Adjust a track's positioning.  Equal to a horizontal shift in time."
  [t x])


;(def empty-track 
;  (sorted-map-by (fn [e1 e2] (compare-ranges (event-bounds e1)
;                                             (event-bounds e2)))))

;(defn into-track
;  "Conj one or more events, sorted by time, into a track."
;  [es]
;  (reduce (fn [acc evt] (assoc acc (:name evt) evt)) empty-track es))
;
;(defn
;  "Conjoins track t2 onto track t1. Returns a new track, in which 
;   t2's start time is shifted to the end-time of t1."
;  conj-track 
;  [t1 t2]
;  (->> (shift-track t2 (end-time t1)))
;       (into-track t1))
   
(defn ->canvas [w h]
  (->rectangle :white 0 0 w h))

(defn trackable->rect
  "Generate a visual representation of a trackable.  Just a rectangle."
  [tracked] 
  (->rectangle :black (start-time tracked) 0 (duration tracked) 1))

(defn track-visual
  "Generate a track from one or more trackables"
  [events] (map trackable->rect events))

)

;(defn draw-event 
;  ([^Graphics2D g start duration color]
;    (draw-shape (->rectangle start 0 (+ start duration) 10 color)))
;  ([^Graphics2D g start duration]
;    (draw-event g start duration :red)))
;
;(defn draw-events [^Graphics2D g eventlist]
;  (for [[start duration color] eventlist]
;    (doseq (draw-event g start duration color))))


;start-time ^ = 0 
;duration ^ = (+ 30 (dur (crop 0 30 x))) = 60 





