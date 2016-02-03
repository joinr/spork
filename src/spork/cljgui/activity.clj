;;new holding place for old
;;event-based widgetry I used to render
;;event tracks.
(ns spork.cljgui.activity
  (:require [spork.sketch :refer :all]))

;;Drawing events and tracks.
(def simple-activity {:start 0 :duration 100 :name "The Activity!" :quantity 10} )
(def simple-track
  [{:start 15 :duration 25   :name "A" :quantity 10}
   {:start 200 :duration 250 :name "B" :quantity 10}
   {:start 150 :duration 10  :name "C" :quantity 10}
   {:start 22  :duration 5   :name "D" :quantity 10}])
(def random-track 
  (vec  (map (fn [idx] {:start (inc (rand-int 600)) :duration (inc (rand-int 100)) :name (str "event_" idx) :quantity (inc (rand-int 30))})
             (range 100))))

;;For drawing Activities, we'll allow a dynamic color map.  This lets
;;us change colors and stuff...

(def ^:dynamic *color-map* {:default :blue})
(defmacro with-color-map [key->color & body]
  `(binding [~'*color-map* ~key->color]
     ~@body))
;;Allows us to define a bunch of colors, ala let-binding, 
;;and have them inserted into the color-map.  High level method 
;;for defining color pallettes.
(defmacro with-colors [color-binds & body]
  `(with-color-map (into *color-map* (partition 2 ~color-binds))
     ~@body))

;;We have a binding that maps events to colors, a single-arity
;;function.  By default, we look at the event name.   Callers can 
;;overload this by supplying their own event->color function, or 
;;by altering the color-map.
(def ^:dynamic *event->color* (fn [e] (get *color-map* (get e :name :default))))
(defmacro with-event->color [event->color & body]
  `(binding [~'spork.sketch/*event->color*  ~event->color]
     ~@body))

(def ^:dynamic *track-options* {:activity-labels true 
                                :track-labels true})
(defmacro hiding-labels [& body]
  `(binding [~'spork.sketch/*track-options* (merge ~'spork.sketch/*track-options* 
                                                   {:activity-labels nil 
                                                    :track-labels nil})]
     ~@body))

(defmacro with-track-scale [expr & body]
  `(binding [~'spork.sketch/*track-options* (assoc ~'spork.sketch/*track-options* 
                                                   {:track-scale ~expr})]
     ~@body))
  
(defn activity-labels? [] (get *track-options* :activity-labels))
(defn track-labels? [] (get *track-options*    :track-labels))

(defn ->activity 
  [{:keys [start duration name quantity] :as e} & {:keys [get-color label-color event->color] 
                                                   :or   {event->color *event->color* label-color :white}}]
  (let [h  (if (= quantity 10) quantity (+ 10 (* 3 (Math/log10 quantity))))
        b  (if (activity-labels?) 
               (->labeled-box name label-color (or (event->color e) :blue) start 0 duration h)
               (->rectangle (or (event->color e) :blue) start 0 duration h))               
           ;;(->rectangle (get color-map name :blue) start 0 duration  h)
        ]
    (outline b)))


(defn elevated-activities [records event->color]
  (let [sorted (sort-by (juxt :start :duration) records)  
        [elevated hmax wmax] (reduce (fn [[xs height width] x] 
                                       (let [act (->activity x :event->color event->color)]
                                         [(conj xs (translate 0.0 height act)) 
                                          (+ height (:height (shape-bounds act)))
                                          (max width (+ (:start x) (:duration x)))]))
                                     [[] 0.0 0.0] sorted)]
    [elevated hmax wmax]))    
  
;;should render us a track of each event, with a track-name to the
;;left of the track.  Events in the track are rendered on top of each other.
(defn ->track [records & {:keys [track-name track-height track-width event->color] 
                          :or   {track-name (str (gensym "Track ")) 
                                 track-height 400 
                                 track-width  400
                                 event->color *event->color*}}]
  (let [label  (->label (str track-name) 0 0)
        lwidth 100 ; (:width (shape-bounds label))            
        [elevated hmax wmax] (elevated-activities records event->color)
        hscale 0.5
        background    ;(when (> (:start (first sorted)) 0)
               (->rectangle :lightgray 0 0  wmax track-height)
        vscale (/ track-height hmax)
        track-box (stack [[background
                           (scale 1.0 vscale (cartesian (into [] elevated)))]
                          (->ggscale [0 wmax] :black wmax 30)
                          (scale 2.0 2.0 (->label "Start Time" (/ wmax 4.0) 0))])]
    (if (track-labels?)
      (beside [(->rectangle :white 0 0 lwidth track-height)
               label]
              track-box)
      track-box)))

(defn ->tracks [track-seq]
  (delineate 
   (into [] (for [[name records] (sort-by first track-seq)]           
              (->track records :track-name name)))))
