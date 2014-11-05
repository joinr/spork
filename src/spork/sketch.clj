;;A simple handle for bindings to the underlying spork graphics
;;package.  This is a convenience namespace that pulls in all the
;;low-level dependencies, and provides a user-friendly API for newbs
;;to perform declarative rendering.  I really need to pull in the 
;;full scene graph library for this.  For now, it's being used as 
;;a sort of skeleton scene graph, for simple 2d diagrams and plotting.
(ns spork.sketch
  (:require [spork.graphics2d.canvas :refer :all]
            [spork.graphics2d [image :as image]
                              [swing :as provider]
                              [font :as f]]
            [spork.protocols [spatial :as space]]
            [spork.geometry.shapes :refer :all]
            [spork.cljgui.components [swing :as gui]]))

;;These are brittle, but work until I found a better way around the problem.

;;Currently used in ->label, should be removed.
(def ^:dynamic *font-height* 14)
(def ^:dynamic *font-width*  5.5)

(def ^:dynamic *current-sketch* nil)
      
(defn sketch-image [the-shapes]
  (->> the-shapes 
       (image/shape->img)
       (gui/view)))

;;image combinators 
(defn beside [s1 s2]
  (let [bounds1 (shape-bounds s1)
        bounds2 (shape-bounds s2)
        hmax (max (:height bounds1) (:height bounds2))
        width (+ (:width bounds1) (:width bounds2))
        new-bounds (space/bbox 0 0 width hmax)]
  (reify IShape 
    (shape-bounds [s] new-bounds)
    (draw-shape   [s c] (with-translation (:width bounds1) 0 
                          (draw-shape s1 c) (partial draw-shape s2))))))

(defn background [color shp]
  (let [{:keys [x y width height]} (shape-bounds shp)]
    [(->rectangle color 0 0 (+ x width) (+ y height))
     shp]))

(defn translate [tx ty shp]
  (reify IShape 
    (shape-bounds [s] (space/translate-bounds tx ty (shape-bounds shp)))
    (draw-shape   [s c] (with-translation tx ty 
                          c (partial draw-shape shp)))))

(defn above [s1 s2]
  (let [bounds1 (shape-bounds  s1)
        bounds2 (shape-bounds  s2)
        wmax   (max (:width bounds1)  (:width bounds2))
        height (+   (:height bounds1) (:height bounds2))
        new-bounds (space/bbox 0 0 wmax height)]
  (reify IShape 
    (shape-bounds [s] new-bounds)
    (draw-shape   [s c] (with-translation  0 (:height bounds1) 
                          (draw-shape s1 c) (partial draw-shape s2))))))
(defn fade [alpha shp]
  (reify IShape 
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape   [s c] (with-alpha  alpha 
                          c (partial draw-shape shp)))))
(defn rotate [theta shp]
  (reify IShape 
    (shape-bounds [s]   (space/rotate-bounds theta (shape-bounds shp)))
    (draw-shape   [s c] (with-rotation theta  c (partial draw-shape shp)))))

;;rotates about a point....we probably should factor out spin-bounds
;;from this guy.
(defn spin   [theta shp]
  (throw (Exception. "Rotation on bounds is currenty jacked up, not working.  Need to fix spatial."))
  (let [bnds  (shape-bounds shp)
        [x y] (space/get-center bnds)
        spun  (space/spin-bounds theta bnds)
        rotated (fn [canv] (with-rotation theta canv #(draw-shape shp %)))]
    (reify IShape 
      (shape-bounds [s]   spun)
      (draw-shape   [s c] 
        (with-translation x y c  rotated)))))

(defn scale [xscale yscale shp]
  (reify IShape 
    (shape-bounds [s]   (space/scale-bounds xscale yscale (shape-bounds shp)))    
    (draw-shape   [s c] (with-scale xscale yscale c (partial draw-shape shp)))))

(def ^:dynamic *cartesian* nil)
(defn cartesian [shp]
  (let [bounds    (shape-bounds shp)
        reflected (scale 1.0 -1.0 shp)]
    (reify IShape 
      (shape-bounds [s] bounds)
      (draw-shape [s c] 
        (if *cartesian* (draw-shape s c)
            (binding [*cartesian* true]
              (with-translation 0 (:height bounds) c 
                #(draw-shape reflected %))))))))

(defn uncartesian [shp]
  (let [bounds    (shape-bounds shp)
        reflected (scale 1.0 -1.0 shp)]
    (reify IShape 
      (shape-bounds [s] bounds)
      (draw-shape [s c] 
        (if *cartesian* (binding [*cartesian* nil]
                          (with-translation 0 (:height bounds) c 
                            #(draw-shape reflected %)))
            (draw-shape shp c))))))

(defn outline [s & {:keys [color] :or {color :black}}]
  (let [bounds (shape-bounds s)]
    [s
     (->wire-rectangle color (:x bounds) (:y bounds) (:width bounds) (:height bounds))]))


;; (defn underline [s & {:keys [color] :or {color :black}}]
;;   (let [bounds (shape-bounds s)]
;;     [s
;;      (->line color (:x bounds) (:y bounds) (:width bounds) (:height
;;   bounds))]))


(defn stack [shapes] (reduce above  shapes))
(defn shelf [shapes] (reduce beside shapes))


(defn delineate [xs] 
  (let [group-bounds (shape-bounds xs)
        width        (- (:width group-bounds) 3)
        separator    (->line :black (:x group-bounds)
                                    (dec (:y group-bounds))
                                    width 
                                    (dec (:y group-bounds)))]
    (stack (interleave xs (repeat separator)))))

;;work in progress.
;(defn at-center [shp]
;  (let [bounds (shape-bounds shp)
;        centerx (/ (:width bounds) 2.0)
;        centery (/ (:heigh bounds) 2.0)]    
;  (reify IShape 
;    (shape-bounds [s] bounds)
;    (draw-shape   [s c] (with-translation centerx centery c
;                          (partial draw-shape shp))))))

(defn ->ticks [color width height step]
  (let [tick   (:source (make-sprite :translucent (->line color 0 0 0 height) 0 0))
        n      (quot width step)
        bound  (unchecked-inc n)
        bounds (space/bbox 0 0 width height)]                                                                                    
    (reify IShape
      (shape-bounds [s]   bounds) 
      (draw-shape   [s c]
        (loop [idx 0
               canv c]
          (if (== idx bound) 
            canv
            (recur (unchecked-inc idx)
                   (draw-image canv tick :translucent (* idx step) 0))))))))
        ;; half-height (/ height 2.0)
        ;; ln     (->line color 0 half-height width half-height)          
  
(defn ->vticks [color width height step]
  (let [tick   (:source (make-sprite :translucent (->line color 0 0 width 0) 0 0))
        n      (quot height step)
        bound  (unchecked-inc n)
        bounds (space/bbox 0 0 width height)]                                                                                    
    (reify IShape
      (shape-bounds [s]   bounds) 
      (draw-shape   [s c]
        (loop [idx 0
               canv c]
          (if (== idx bound) 
            canv
            (recur (unchecked-inc idx)
                   (draw-image canv tick :translucent 0 (* idx step)))))))))

(defn ->ggscale [data color width height & {:keys [steps] :or {steps 6}}]
  (let [tick-height (/ height 2.0)
        tick    (:source (make-sprite :translucent (->line color 0 0 0 tick-height) 0 0))
        sorted  (vec (sort data))
        l       (first sorted)
        r       (last  sorted)
        lbounds (f/string-bounds (str l))
        lwidth  (:width lbounds)
        rwidth  (:width (f/string-bounds (str r)))
        nheight (:height lbounds)
        lheight (+ tick-height nheight)
        spread  (- r l)
        step    (long (/ spread steps))
        nums    (mapv #(+ l (* step %)) (range steps))
        bound   (unchecked-inc steps)
        bounds  (space/bbox 0 0 (+ width lwidth rwidth) (max height lheight))
        centered-numb (fn [canv n x]
                          (let [lbl (str n)
                                halfw (/ (:width (f/string-bounds lbl)) 2.0)]
                            (draw-string canv :black :default lbl (- x halfw) (+ tick-height nheight))))]
    (reify IShape
      (shape-bounds [s]   bounds) 
      (draw-shape   [s c]
        (loop [idx 0
               offset l
               canv c]
          (if (== idx bound) 
            canv
            (recur (unchecked-inc idx)
                   (+ offset step)
                   (-> canv
                       (draw-image tick :translucent (* idx step) 0)
                       (centered-numb offset (* idx step))))))))))

(defn ->scaled-border [color width height step]
  [(translate height 0 (spork.sketch/->ticks color width height step)) 
   (translate 0 height (spork.sketch/->vticks color  height width step))])

;;need to change this to use ->text, which has bounds based off font metrics...
(defn ->label [txt x y & {:keys [color] :or {color :black}}]
  (reify IShape
    (shape-bounds [s]   (space/bbox x y (* (count txt) *font-width*) *font-height*))    
    (draw-shape   [s c] (draw-string c color :default txt x (+ y (- *font-height* 2))))))

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

(defn ->labeled-box  [txt label-color color x y w h]
  (if (empty? txt) 
    (->rectangle color x y w h)
    (let [r          (->rectangle  color x y w h)
          half-dur   (/ w 2.0)
          centerx    (+ x 1)
;          half-width (* (/ (count txt) 2.0) *font-width*)        
          scalex     1.0 ;(if (< half-width half-dur) 1.0  (/ half-dur half-width))
          centery    0
          label      (scale scalex 1.0 (uncartesian (->label txt centerx centery :color label-color)))]
      (reify IShape 
        (shape-bounds [s]   (shape-bounds r))
        (draw-shape   [s c] (draw-shape [r label] c))))))

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
  `(binding [~'*event->color*  ~event->color]
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

(def  ->vline (image/shape->img (->line :black 0 0 0 10)))
(defn ->axis  [min max step-width]
  (let [tick   (fn [x] (translate x 0 ->vline))]        
    (translate 0 *font-height*     
     (image/shape->img 
       [(->line :black min 0 max 0)
        (image/shape->img 
          (into [] (map tick (range min (inc max) step-width))))]))))

;; (defrecord event-track [records name height width event->color min max shp]
;;   IShape 
;;   (shape-bounds [s] (shape-bounds shp))
;;   (draw-shape [s c] (draw-shape shp c)))

;;should render us a track of each event, with a track-name to the
;;left of the track.  Events in the track are rendered on top of each other.
(defn ->track [records & {:keys [track-name track-height track-width event->color] 
                          :or   {track-name (str (gensym "Track ")) 
                                 track-height 400 
                                 track-width  400
                                 event->color *event->color*}}]
  (let [label  (->label (str track-name) 0 0)
        lwidth 100 ; (:width (shape-bounds label))
        sorted (sort-by (juxt :start :duration) records)                
        [elevated hmax wmax] (reduce (fn [[xs height width] x] 
                                       (let [act (->activity x :event->color event->color)]
                                         [(conj xs (translate 0.0 height act)) 
                                          (+ height (:height (shape-bounds act)))
                                          (max width (+ (:start x) (:duration x)))]))
                                       [[] 0.0 0.0] sorted)
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

(defn colored-rects [n]
  (let [rects  (->> [:red :blue :green]
                    (map (fn [clr]
                           (spork.geometry.shapes/->rectangle clr
                                                              0  0 100 100)))
                    (map image/shape->img))]
    (image/shape->img (reduce beside
                                (take n (cycle rects))))))
(defn colored-rects! [n]
  (into []
    (take n
     (map-indexed
      (fn [i clr]
        (spork.geometry.shapes/->rectangle clr
          (+ 0 (+ (* i 100) 10)) 0 80 100)) (cycle [:red :blue :green])))))

(defn colored-paper [w h]
  (let [xs (colored-rects w)]
    (delineate (into [] (take h (repeat xs))))))
    

(defn ->hlines [color x1 y1 w n step]
  (let [h (* step n)
        b (space/bbox 0 0 w h)
        hline (image/shape->img (->line color 0 1 w 1))
        bound (inc n)]
    (reify IShape
      (shape-bounds [s] b)
      (draw-shape [s c]
        (loop [acc c 
               idx 0]
          (if (== idx bound) acc
              (recur (draw-shape (translate 0 (* idx step) hline) acc)
                     (unchecked-inc idx))))))))

(defn ->vlines [color x1 y1 h n step]
  (let [w (* step n)
        b (space/bbox 0 0 w h)
        vline (image/shape->img (->line color 1 0 1 h))
        bound (inc n)]
    (reify IShape
      (shape-bounds [s] b)
      (draw-shape [s c]
        (loop [acc c 
               idx 0]
          (if (== idx bound) acc
              (recur (draw-shape (translate (* idx step) 0  vline) acc)
                     (unchecked-inc idx))))))))          

(defn ->graph-paper [color w h & {:keys [n xscale yscale] :or {n 10}}]
  (let [xscale (or xscale (float (/ w n)))
        yscale (or yscale (float (/ h n)))
        b (space/bbox 0 0 w h)
        across (->hlines color 0 0 w n xscale)
        up     (->vlines color 0 0 h n yscale)]
    (reify IShape 
      (shape-bounds [s]   b)
      (draw-shape   [s c] 
        (->> c 
            (draw-shape across)
            (draw-shape up))))))
        

          
                   


          



;;testing
(comment 



  

)
;; (defn ->track2 [records & {:keys [track-name track-height track-width] 
;;                           :or   {track-name (str (gensym "Track ")) 
;;                                  track-height 100 
;;                                  track-width  800}}]
;;   (let [label  (->label (str track-name) 0 0)
;;         lwidth 100 ; (:width (shape-bounds label))
;;         sorted (sort-by (juxt :start :duration) records)
;;         [elevated hmax wmax] (reduce (fn [[xs height width] x] 
;;                                        (let [act (->activity x)]
;;                                          [(conj xs (translate 0.0 height act)) 
;;                                           (+ height (:height (shape-bounds act)))
;;                                           (max width (+ (:start x) (:duration x)))]))
;;                                        [[] 0.0 0.0] sorted)
;;         hscale 0.5
;;         background    ;(when (> (:start (first sorted)) 0)
;;                (->rectangle :lightgray 0 0  wmax track-height)
;;         vscale (/ track-height hmax)
;;         _ (println :buildingimage)
;;         ] ;(/ track-height hmax)]        
;;     (with-meta (beside [(->rectangle :white 0 0 lwidth track-height)
;;                         label]
;;                        [background
;;                         (scale 1.0 vscale (cartesian (into [] elevated)))]) {:records records :vscale vscale :wmax wmax})))

     ;; (beside [(->rectangle :white 0 0 lwidth hmax)
     ;;          label]
     ;;         [background
     ;;          (scale 1.0 vscale (cartesian (into [] elevated)))])))
