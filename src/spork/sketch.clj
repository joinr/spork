;;A simple handle for bindings to the underlying spork graphics
;;package.  This is a convenience namespace that pulls in all the
;;low-level dependencies, and provides a user-friendly API for newbs
;;to perform declarative rendering.  I really need to pull in the 
;;full scene graph library for this.  For now, it's being used as 
;;a sort of skeleton scene graph, for simple 2d diagrams and plotting.
(ns spork.sketch
  (:require [spork.graphics2d.canvas :refer :all]
            [spork.graphics2d [image :as image]
                              [swing :as provider]]
            [spork.protocols [spatial :as space]]
            [spork.geometry.shapes :refer :all]
            [spork.cljgui.components [swing :as gui]]))

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

(defn above [s1 s2]
  (let [bounds1 (shape-bounds s1)
        bounds2 (shape-bounds  s2)
        wmax   (max (:width bounds1)  (:width bounds2))
        height (+   (:height bounds1) (:height bounds2))
        new-bounds (space/bbox 0 0 wmax height)]
  (reify IShape 
    (shape-bounds [s] new-bounds)
    (draw-shape   [s c] (with-translation (:height bounds1) 0 
                          (draw-shape s1 c) (partial draw-shape s2))))))
(defn fade [alpha shp]
  (reify IShape 
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape   [s c] (with-alpha  alpha 
                          c (partial draw-shape shp)))))
(defn rotate [theta shp]
 (reify IShape 
   (shape-bounds [s]   (space/rotate-bounds (shape-bounds shp)))
   (draw-shape   [s c] (with-rotation theta  c (partial draw-shape shp)))))  

(defn stack [shapes] (reduce above  shapes))
(defn shelf [shapes] (reduce beside shapes))

;; IShape 
;; (shape-bounds [s]   (spatial/bbox 0 0 width height))
;; (draw-shape   [s c] (draw-image c data transparency 0 0))
(defn ->activity [{:keys [start duration name quantity]} color-map]
  (->rectangle (get color-map name :blue) start 0 quantity 
