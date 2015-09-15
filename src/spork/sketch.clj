;;A simple handle for bindings to the underlying spork graphics
;;package.  This is a convenience namespace that pulls in all the
;;low-level dependencies, and provides a user-friendly API for newbs
;;to perform declarative rendering.  I really need to pull in the 
;;full scene graph library for this.  For now, it's being used as 
;;a sort of skeleton scene graph, for simple 2d diagrams and plotting.
(ns spork.sketch
  (:require [spork.graphics2d.canvas :as canvas :refer :all ]
            [spork.graphics2d [image :as image]
                              [swing :as provider]
                              [font :as f]
                              [stroke :as stroke]]
            [spork.protocols [spatial :as space]]
            [spork.geometry.shapes :refer :all]
            [spork.cljgui.components [swing :as gui]]
            [spork.events [base :as evt]
                          [native :as nat]
                          [observe :as obs]]
            ))

;;These are brittle, but work until I found a better way around the problem.

;;Currently used in ->label, should be removed.
(def ^:dynamic *font-height* 14)
(def ^:dynamic *font-width*  5.5)

(def ^:dynamic *current-sketch* nil)
(def ^:dynamic *anti-aliasing*  nil)

;;current options are :title and :cached?
(defn sketch [the-shapes & opts] (apply gui/view the-shapes opts))

(defn smooth [shp]
  (reify IShape
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape [s c]
      (draw-shape shp (set-state c  {:antialias true})))))

(defn thicken [amount shp]
  (reify IShape
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape   [s c]
      (let [strk (canvas/get-stroke c)
            new-stroke (stroke/widen amount strk)]
        (canvas/with-stroke new-stroke c
          #(canvas/draw-shape shp %))))))
  
(defn stroke-by [width shp]
  (reify IShape
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape   [s c]
      (let [strk (canvas/get-stroke c)
            new-stroke (stroke/stroke-of-width width strk)]
        (canvas/with-stroke new-stroke c
          #(canvas/draw-shape shp %))))))

;;Buffered image was killing us here with memory leakage.  So for now,
;;we just do immediate mode drawing.
(defn sketch-image [the-shapes]
  (->> the-shapes 
;       (image/shape->img)
       (gui/view)))

(defn clear [shp]
  (let [{:keys [x y width height] :as bnds} (shape-bounds shp)]        
    (reify IShape
      (shape-bounds [s] bnds)
      (draw-shape [s c]
        (->>  (spork.graphics2d.image/clear-region c x y width height)
              (draw-shape shp))))))

(defn ->clear-region [x y w h]
  (let [bnds (spork.protocols.spatial/bbox x y w h)]        
    (reify IShape
      (shape-bounds [s] bnds)
      (draw-shape [s c]
       (spork.graphics2d.image/clear-region c x y w h)))))
              

(definline atom? [x]
  `(instance? ~'clojure.lang.Atom ~x))

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
                          (draw-shape s1 c) #(draw-shape s2 %))))))

(defn background [color shp]
  (let [{:keys [x y width height]} (shape-bounds shp)]
    [(->rectangle color 0 0 (+ x width) (+ y height))
     shp]))

(defn translate [tx ty shp]
  (if (not (and (atom? tx) (atom? ty)))
    (reify IShape 
      (shape-bounds [s] (space/translate-bounds tx ty (shape-bounds shp)))
      (draw-shape   [s c] (with-translation tx ty 
                            c #(draw-shape shp %))))
    (reify IShape 
      (shape-bounds [s] (space/translate-bounds @tx @ty (shape-bounds shp)))
      (draw-shape   [s c] (with-translation @tx @ty 
                            c #(draw-shape shp %))))))

(defn above [s1 s2]
  (let [bounds1 (shape-bounds  s1)
        bounds2 (shape-bounds  s2)
        wmax   (max (:width bounds1)  (:width bounds2))
        height (+   (:height bounds1) (:height bounds2))
        new-bounds (space/bbox 0 0 wmax height)]
  (reify IShape 
    (shape-bounds [s] new-bounds)
    (draw-shape   [s c] (with-translation  0 (:height bounds1) 
                          (draw-shape s1 c) #(draw-shape s2 %))))))
(defn fade [alpha shp]
  (if (not (atom? alpha))
    (reify IShape 
      (shape-bounds [s] (shape-bounds shp))
      (draw-shape   [s c] (with-alpha  alpha 
                            c #(draw-shape shp %))))
    (reify IShape 
      (shape-bounds [s] (shape-bounds shp))
      (draw-shape   [s c] (with-alpha  @alpha 
                            c #(draw-shape shp %))))))
(defn rotate [theta shp]
  (if (not (atom? theta))
    (reify IShape 
      (shape-bounds [s]   (space/rotate-bounds theta (shape-bounds shp)))
      (draw-shape   [s c] (with-rotation theta  c #(draw-shape shp %))))
    (reify IShape 
      (shape-bounds [s]   (space/rotate-bounds @theta (shape-bounds shp)))
      (draw-shape   [s c] (with-rotation @theta  c #(draw-shape shp %))))))

;;rotates about a point....we probably should factor out spin-bounds
;;from this guy.
(defn spin   [theta shp]
  (throw (Exception. "Rotation on bounds is currenty jacked up, not working. Need to fix the math on this."))
  (let [bnds  (shape-bounds shp)
        [x y] (space/get-center bnds)
        spun  (space/spin-bounds theta bnds)
        rotated (fn [canv] (with-rotation theta canv #(draw-shape shp %)))]
    (reify IShape 
      (shape-bounds [s]   spun)
      (draw-shape   [s c] 
        (with-translation x y c  rotated)))))

(defn scale [xscale yscale shp]
  (if (not (and (atom? xscale) (atom? yscale)))
    (reify IShape 
      (shape-bounds [s]   (space/scale-bounds xscale yscale (shape-bounds shp)))    
      (draw-shape   [s c] (with-scale xscale yscale c #(draw-shape shp %))))
    (reify IShape 
      (shape-bounds [s]   (space/scale-bounds @xscale @yscale (shape-bounds shp)))    
      (draw-shape   [s c] (with-scale @xscale @yscale c #(draw-shape shp %))))))

(def ^:dynamic *cartesian* nil)
(defn cartesian [shp]
  (let [bounds  (spork.protocols.spatial/scale-bounds 1.0 -1.0 (shape-bounds shp))
        y       (spork.protocols.spatial/get-bottom bounds)
        reflected (scale 1.0 -1.0 shp)]
    (reify IShape 
      (shape-bounds [s] bounds)
      (draw-shape [s c] 
        (if *cartesian* (draw-shape s c)
            (binding [*cartesian* true]
              (with-translation 0 (- (:height bounds) y) c 
                #(draw-shape reflected %))))))))

(defn uncartesian [shp]
  (let [bounds    (spork.protocols.spatial/scale-bounds 1.0 -1.0 (shape-bounds shp))
        y         (spork.protocols.spatial/get-bottom bounds)  
        reflected (scale 1.0 -1.0 shp)]
    (reify IShape 
      (shape-bounds [s] bounds)
      (draw-shape [s c] 
        (if *cartesian* (binding [*cartesian* nil]
                          (with-translation 0 (+ (:height bounds) y) c 
                            #(draw-shape reflected %)))
            (draw-shape shp c))))))

(defn outline [s & {:keys [color] :or {color :black}}]
  (let [bounds (shape-bounds s)]
    [s
     (->wire-rectangle color (:x bounds) (:y bounds) (:width bounds) (:height bounds))]))


;;Operations on images and icons.
(defn fit-ratio [w h maxw maxh]
  (let [ar (min (/ (double maxw)  w) (/  (double maxh)  h))]
    [(/ (* w ar) maxw)
     (/ (* h ar) maxh)]))

(defn fit-ratio [w h maxw maxh]
  (let [ar (min (/ (double maxw)  w) 
                (/ (double maxh)  h))]
    [(* w ar)
     (* h ar)]))

(defn fit-ratio [w h maxw maxh]
  (let [w->h  (double (/ w h))
        ar    (min (/ (double maxw)  w) 
                   (/ (double maxh)  h))]
    [ar
     (* w->h ar)]))


(defn flip [img]
  (let [bnds (canvas/shape-bounds img)]
      (translate (/ (:width bnds) 2.0) (/ (:height bnds) 2.0)                        
         (rotate Math/PI
           (scale -1.0 1.0
                          (translate (/ (:width bnds) -2.0) (/ (:height bnds) -2.0)
                                            img)
                         )
         ))))

(defn iconify [img & {:keys [w h flipped?]
                      :or {w 50 h 50 flipped? false}}]
  (let [{:keys [width height]} (canvas/shape-bounds img)
        [xscale yscale]        (fit-ratio width height w h)
        _                      (println [xscale yscale])
        dest   (spork.graphics2d.image/make-image w h)
        g      (canvas/get-graphics dest)]
    (do  (canvas/with-scale xscale  yscale g
           (if flipped?
             #(canvas/draw-shape    (flip img) %)
             #(canvas/draw-shape img  %)))
         dest)))

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
;                          #(draw-shape shp %))))))

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

;; (defprotocol IScale
;;   (min-val [s])
;;   (max-val [s]))

;(defrecord dynascale [data min max steps tick-height spread])

;; (defn gg-points [data steps]
;;   (let [sorted (vec (sort data))
;;         l (first sorted)
;;         r (last sorted)
;;         spread (- r l)
;;         step (long (/ spread steps))
;;         nums (mapv #(+ 1 (* step %)) (range steps))
;;         bound (unchecked-inc steps)]
    
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

(def  ->vline (image/shape->img (->line :black 0 0 0 10)))
(def  ->hline (image/shape->img (->line :black 0 0 10 0)))
(defn ->axis  [min max step-width]
  (let [tick   (fn [x] (translate x 0 ->vline))]        
    (translate 0 *font-height*     
     (image/shape->img 
       [(->line :black min 0 max 0)
        (image/shape->img 
          (into [] (map tick (range min (inc max) step-width))))]))))

(defn ->xaxis  [min max step-width]
  (let [tick   (fn [x] (translate x 0 ->vline))]        
    (translate 0 *font-height*     
     (image/shape->img 
       [(->line :black min 0 max 0)
        (image/shape->img 
          (into [] (map tick (range min (inc max) step-width))))]))))

(defn ->yaxis  [min max step-width]
  (let [tick   (fn [x] (translate 0 x ->hline))]        
    (image/shape->img 
     [(->line :black  10 min   10 max)
      (image/shape->img 
       (into [] (map tick (range min (inc max) step-width))))])))

;; (defrecord event-track [records name height width event->color min max shp]
;;   IShape 
;;   (shape-bounds [s] (shape-bounds shp))
;;   (draw-shape [s c] (draw-shape shp c)))

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
              (recur (draw-shape (translate (+ (* idx step) x1) y1  vline) acc)
                     (unchecked-inc idx))))))))          

;; (defrecord plot-area [axes x y w h xscale yscale background area]
;;   canvas/IShape
;;   (draw-shape [s c]
;;     (canvas/draw-shape [background
;;                         axes
;;                         area] c))
;;   (shape-bounds [s] (space/bbox x y w h)))

;;so the plot is already at 1..... along the bottom, we want to offset by this amount
(defn ->ax [w h]
  [(->line :black 1 1 1 h)
   (->line :black 1 1 w 1)])

;;coordinates are local to the plot's transform.  So, if we want to plot a shape,
;;the plot transforms the the shape to its local coordinate system, then acts 
(defn ->plot
  ([w h area {:keys [xscale yscale background]
                        :or   {xscale 1.0
                               yscale 1.0
                               background pass}}]                               
   (let [axes [(->line :black 0 0 0 h)
               (->line :black 0 0 w 0)]]             
     (reify canvas/IShape
       (draw-shape [s c]
         (canvas/draw-shape [background
                             axes
                            (scale xscale yscale
                                   area)] c))
       (shape-bounds [s] (space/bbox 0 0 w h)))))
  ([w h area] (->plot w h area {})))


;(defn ->2DPlot [w h {:keys [xscale yscale xmin xmax ymin ymax xmajor ymajor xminor yminor]}]
  ;;we have a plot.
  ;;the plot has labels.

(comment
  (->plot 600 600 1 1 (->plane :grey 0 0 600 600)
          (vec (for [i (range 100)]
                 (->rectangle :red
                              (* i 10)
                              (rand-int 600)  10 10))))
 )

;;We're repeating a shape until we exceed a criteria (outside the view).
;;So, we have a shape defined relative to a view.
;;Say, a line.  We can have infinite lines if, when we move the view,
;;we change the starting point of our stepping criteria.
;;If we offset left, we drawlines until our next line exceeds the view, starting
;;from the first visible line.

(defn next-line [w xprev step]
  (let [xnext (+ xprev step)]
    (when (<= xnext w)
      xnext)))

;;we're repeating a shape, spanning an interval, starting from an initial
;;point.  That's the declarative operation.  Basically, we draw the
;;shape at every point.  Note that if the shape spans vertically, we
;;get an "infinite" arrangement of shapes, i.e. a grid.
(defn repeat-across [shp x1 y w step]
  (let [bbox (space/bbox x1 y  w (:height (shape-bounds shp)))
        x     (atom x1)
        y-rev (atom y)
        cursor (translate x y-rev shp)
        cursor-at! (fn [n] (do (reset! x n)
                               cursor))
        ]
    (reify IShape
      (shape-bounds [s] bbox)
      (draw-shape [s c]
        (let [xpan  (- @canvas/*xpan*)
              step  (* step  @canvas/*xzoom*)
              l     (+ x1    xpan)
              first-step (- l (mod xpan step))
              bound (+ l w)
              _ (reset! y-rev (- y @canvas/*ypan*))]
          (loop [acc c 
                 xprev first-step]
            (if (> xprev bound) acc
                (recur (draw-shape (cursor-at! xprev)  acc)
                       (unchecked-add xprev step)))))))))

(defn repeat-up [shp x y1 h step]
  (let [bbox (space/bbox x y1  (:width (shape-bounds shp)) h)
        y    (atom y1)
        x-rev (atom x)
        cursor (translate x-rev y shp)
        cursor-at! (fn [n] (do (reset! y n)
                               cursor))]
    (reify IShape
      (shape-bounds [s] bbox)
      (draw-shape [s c]
        (let [ypan  (- @canvas/*ypan*)
              step  (* step  @canvas/*yzoom*)
              l     (+ y1   ypan)
              first-step (- l (mod ypan step))
              _     (reset! x-rev (- x @canvas/*xpan*))
              bound (+ l h)]
          (loop [acc c 
                 yprev first-step]
            (if (>= yprev bound) acc
                (recur (draw-shape (cursor-at! yprev)  acc)
                       (unchecked-add yprev step)))))))))

(defn ->scrolling-columns
  ([x1 y1 w h step color]
   (let [ln (image/shape->img (->line color 1 0 1 h))]
     (repeat-across ln x1 y1 w step)))
  ([x1 y1 w h step] (->scrolling-columns x1 y1 w h step :black)))

(defn ->scrolling-rows
  ([x1 y1 w h step color]
   (let [ln (image/shape->img (->line color 0 1 w 1))]
     (repeat-up ln x1 y1 w step)))
  ([x1 y1 w h step] (->scrolling-rows x1 y1 w h step :black)))

;;This is currently a problem, the plane doesn't cover the viewport entirely.
;;Should fix this.
(defn ->plane
  ([color x1 y1 w h]
   (let [pl (image/shape->img (->rectangle color x1 y1  w h))]
     (repeat-up
      pl
                                        ;   (repeat-across  pl x1 y1 w w)
      x1 y1 h h)
     ))
  ([color w h] (->plane color 0 0 w h)))


;;ggplot does the same thing, we just have two scrolling grids.
;;There's a major and a minor grid.  The minor grid is drawn first.
(defn ->scrolling-grid
  ([x1 y1 w h xstep ystep color]
   [(->scrolling-columns x1 y1 w h (/ w xstep) color)
    (->scrolling-rows x1 y1 w h (/ h ystep) color)])
  ([w h xstep ystep color]
   (->scrolling-grid 0 0  w h  xstep ystep color))
  ([w h xstep ystep]
   (->scrolling-grid  w h xstep ystep :black)))

  ;; ([x1 y1 w h n] (->scrolling-grid x1 y1 w h n n))
  ;; ([x1 y1 w h]   (->scrolling-grid x1 y1 w h 10)))

(defn ->scrolling-grid2
  ([x1 y1 w h xstep ystep]
   (let [gridsample (image/shape->img [(->scrolling-columns 0 0 w h (/ w xstep))
                                       (->scrolling-rows 0 0 w h (/ h ystep))])]
     (repeat-up      
      (repeat-across gridsample x1 y1 w w)
      x1 y1 h h)))
  ([x1 y1 w h n] (->scrolling-grid2 x1 y1 w h n n))
  ([x1 y1 w h]   (->scrolling-grid2 x1 y1 w h 10)))


;;gg defaults to having minors be half of majors.
(defn ->gg-scrolling [w h xstep ystep]
  (->scrolling-grid w h xstep ystep :white))

;grammar of graphics style grid plots.
(defn ->gg-plotarea [w h xstep ystep]
  (let [maj (->gg-scrolling w h xstep ystep)
        min (->gg-scrolling w h (/ xstep 2.0) (/ ystep 2.0))]
    [;(->plane :light-grey w h)
     
     maj
     min]))
  
;;we need to have scrolling axes...


;;This allows us to have a concise way to thread user
;;interaction into the scene.
(defn ->interactor
  ([xpan ypan xzoom yzoom shp]
   (let [interactive-shape (translate xpan ypan
                                      (scale xzoom yzoom
                                             shp))]
     (reify
       canvas/IShape
       (shape-bounds [s] (canvas/shape-bounds interactive-shape))
       (draw-shape [s c]
         (canvas/with-movement {:xpan  xpan
                                :ypan  ypan
                                :xzoom xzoom
                                :yzoom yzoom}           
           (canvas/draw-shape interactive-shape c)))
       clojure.lang.IDeref
       (deref [o] {:xpan xpan :ypan ypan :xzoom xzoom :yzoom yzoom}))))
  ([shp] (->interactor (atom 0.0) (atom 0.0) (atom 1.0) (atom 1.0) shp)))

(defn ->painting [w h shp]
   (let [myshape    (->interactor shp)
         {:keys [xpan ypan]} @myshape
         ^spork.cljgui.components.PaintPanel p
         (gui/new-paintpanel w h #(canvas/draw-shape myshape %) )
         m          (nat/get-observer  p :mouse)         
         mousemove  (->> (obs/cyclical-obs (:released m) (:dragged m))
                         (obs/subscribe
                          (fn [[^java.awt.event.MouseEvent l ^java.awt.event.MouseEvent r]]
                            (let [xd (-   (.getX r) (.getX l))
                                  ;;flip the order, since we're in cartesian coords....
                                  yd (-  (.getY l) (.getY r) )]
                              (do (when-not (zero? xd) (swap! xpan + xd))
                                  (when-not (zero? yd) (swap! ypan + yd))
                                  (.repaint p))))))]
     (with-meta p (merge @myshape {:mouse-obs m :mousemove mousemove}))))

(defn paint!
  ([w h shp]
   (gui/view (gui/empty-frame)
             (->painting w h shp)))
  ([shp]
   (let [{:keys [x y width height]} (canvas/shape-bounds shp)]
     (paint! width height shp))))
  
(defn moving-grid [w h n]
  (let [background (->scrolling-grid 0 0 w h n)
        clear      (image/shape->img (->rectangle :grey 0 0 w h))]
    (->painting w h [clear background])))
      
(defn ->grid [w h wn hn]
  [(->vlines :black 0 0 h wn  (/ w wn))
   (->hlines :black 0 0 w  hn (/ h hn))])

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

(defn ->live-axis [])

;(defn ->container [x y width height]


;;If we conceptualize it as a tiled object in which we can compute new tiles...
;;We define a tile by the dimensions of the canvas.   So, a 300x300 is a tile.
;;As we pan l/r, we move from the current tile to the next.
;;Really, we're tiling...
;;the graph lines are a function of the canvas-width and canvas-height.
;;It's a procedural shape.
(defn ->reactive-graph-paper [color w h & {:keys [n xscale yscale] :or {n 10}}]
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

;;how would we tile a rectangle?
(defn ->legend-entry [txt color]
  (let [lbl (spork.geometry.shapes/->plain-text :black  (str txt "  ") 0 10)
        h   (spork.protocols.spatial/get-height (spork.protocols.spatial/get-bounding-box lbl))]
    (beside (spork.geometry.shapes/->rectangle color 0 0 10 h)
            lbl)))

(defn ->legend [m & {:keys [orient] :or {orient shelf}}]
   (orient (for [[lbl clr] m]
                 (->legend-entry lbl clr))))

;;#TODO Change this to be a generic cljgui color, not java awt
(defn palette
  "Generates a random color palette using golden ratio"
  ([s v]  
     (map #(java.awt.Color. ^long (nth % 0) ^long (nth % 1) ^long (nth % 2)) (canvas/random-color-palette s v)))
  ([] (palette 0.2 0.65)))
