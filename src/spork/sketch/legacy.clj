;;the original sketch implementation, based on
;;reified IShapes and function composition.
;;The updated sketch actually uses a
;;map-based data abstraction to define
;;the scene, which lets us do cooler stuff
;;like compile spork sketchs into compatible
;;piccolo2d scenes.  This ns exists for
;;supported legacy sketches, as in
;;spork.examples.activityvis and may be
;;removed in the future.
(ns spork.sketch.legacy
  (:require  [spork.graphics2d.canvas :as canvas :refer :all ]
             [spork.protocols [spatial :as space]]            
             [spork.graphics2d [image :as image]
              [swing :as provider]
              [font :as f]
              [stroke :as stroke]
              [scene :as scene]]
             [spork.geometry.shapes :refer :all]
             [spork.util.metaprogramming :as meta]
             [spork.cljgui.components [swing :as gui]]
             [spork.events [base :as evt]
              [native :as nat]
              [observe :as obs]]))

(defn atom? [x] (instance? clojure.lang.Atom x))

;;Note: these shape combinators are defined
;;in spork.sketch using the scenegraph
;;abstraction, so they're all obe.

;;obe.
(defn smooth [shp]
  (reify IShape
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape [s c]
      (draw-shape shp (set-state c  {:antialias true})))))


;;this is a transform of the rendering
;;state.  All child nodes, i.e. anything
;;later in the pipeline, will have the
;;global stroke set to a thicker amount.
(defn thicken [amount shp]
  (if (= amount 1.0) shp
      (reify IShape
        (shape-bounds [s] (shape-bounds shp))
        (draw-shape   [s c] 
          (let [strk (canvas/get-stroke c)
                new-stroke (stroke/widen amount strk)]
            (canvas/with-stroke new-stroke c
              #(canvas/draw-shape shp %)))))))
  
(defn stroke-by [width shp]
  (reify IShape
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape   [s c]
      (let [strk (canvas/get-stroke c)
            new-stroke (stroke/stroke-of-width width strk)]
        (canvas/with-stroke new-stroke c
          #(canvas/draw-shape shp %))))))
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

(defn <-beside [s1 s2]
  (let [bounds1 (shape-bounds s1)
        bounds2 (shape-bounds s2)
        hmax (max (:height bounds1) (:height bounds2))
        width (+ (:width bounds1) (:width bounds2))
        new-bounds (space/bbox 0 0 width hmax)]
  (reify IShape 
    (shape-bounds [s] new-bounds)
    (draw-shape   [s c] (with-translation (:width bounds2) 0 
                          (draw-shape s2 c) #(draw-shape s1 %))))))

(defn background [color shp]
  (let [{:keys [x y width height]} (shape-bounds shp)]
    [(->rectangle color 0 0 (+ x width) (+ y height))
     shp]))

(defn translate [tx ty shp]
  (if (not (and (atom? tx) (atom? ty)))
    (if (and (zero? tx) (zero? ty) ) shp
        (reify IShape 
          (shape-bounds [s] (space/translate-bounds tx ty (shape-bounds shp)))
          (draw-shape   [s c] (with-translation tx ty 
                                c #(draw-shape shp %)))))
    (reify IShape 
      (shape-bounds [s] (space/translate-bounds @tx @ty (shape-bounds shp)))
      (draw-shape   [s c] (with-translation @tx @ty 
                            c #(draw-shape shp %))))))

;;this becomes a higher-order combinator..
;;a way to define a translation.

;;inverted the order because we have a cartesian coordinate system now.
(defn above [s2 s1]
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
;; (defn spin   [theta shp]
;;   ;(throw (Exception. "Rotation on bounds is currenty jacked up, not working. Need to fix the math on this."))
;;   (let [bnds  (shape-bounds shp)
;;         [x y] (space/get-center bnds)
;;         spun  (space/spin-bounds theta bnds)
;;         rotated (fn [canv] (with-rotation theta canv #(draw-shape shp %)))]
;;     (reify IShape 
;;       (shape-bounds [s]   spun)
;;       (draw-shape   [s c] 
;;         (with-translation x y c  rotated)))))

(defn vertical-text [shp]
  (let [{:keys [x y height width]} (shape-bounds shp)
        new-shp (if (and (zero? x) (zero? y))
                  (translate height 0
                             (rotate (/ Math/PI 2.0) shp))
                  (translate height 0
                             (translate (- x) (- y)
                                        (rotate (/ Math/PI 2.0)
                                                (translate x y shp)))))
        bnds    (spork.protocols.spatial/bbox x y height width)]
    (reify IShape
      (shape-bounds [s] bnds)
      (draw-shape [s c] (draw-shape new-shp c)))))
                 
(defn spin
  ([theta shp]
   (let [{:keys [x y width height] :as bnds} (shape-bounds shp)
         centerx    (+ x (/ width  2.0))
         centery    (+ y (/ height 2.0))]
     (spin theta shp centerx centery)))
  ([theta shp centerx centery]
   (let [bnds (shape-bounds shp)]
     (if (not (atom? theta))
       (let [new-bounds (spork.protocols.spatial/get-bounding-box
                         (spork.protocols.spatial/spin-bounds theta bnds))
             new-shp   (translate centerx centery
                                  (rotate theta
                                          (translate  (- centerx) (- centery)shp)))]
         (reify IShape
           (shape-bounds [c]  new-bounds)
           (draw-shape [s c]  (draw-shape new-shp c)))
       (reify IShape
         (shape-bounds [c] (spork.protocols.spatial/get-bounding-box
                            (spork.protocols.spatial/spin-bounds @theta bnds)))
         (draw-shape [s c]
           (let [newshp (translate centerx centery
                                   (rotate @theta
                                           (translate  (- centerx) (- centery) shp)))]
             (draw-shape newshp c)))))))))

(defn scale [xscale yscale shp]
  (if (not (and (atom? xscale) (atom? yscale)))
    (let [xscale (double xscale)
          yscale (double yscale)]
      (if (and (== xscale 1.0) (== yscale 1.0)) shp
          (reify IShape 
            (shape-bounds [s]   (space/scale-bounds xscale yscale (shape-bounds shp)))    
            (draw-shape   [s c] (with-scale xscale yscale c #(draw-shape shp %))))))
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
;; (defn at-center [shp]
;;  (let [bounds (shape-bounds shp)
;;        centerx (/ (:width bounds) 2.0)
;;        centery (/ (:heigh bounds) 2.0)]    
;;  (reify IShape 
;;    (shape-bounds [s] bounds)
;;    (draw-shape   [s c] (with-translation centerx centery c
;;                          #(draw-shape shp %))))))

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

