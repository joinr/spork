;;Todo -> Rethink this design, now that we've merged multiple projects.  Might 
;;be able to make it more elegant.
(ns spork.geometry.shapes
  (:require [spork.graphics2d [canvas :as c]
                              [image :as image]]
            [spork.protocols  [spatial  :as spatial]]))

(defmacro defshape [name args bounds draw-body]
  `(defrecord ~name [~@args] 
     ~'c/IShape
     (~'shape-bounds [~'s] ~bounds) 
     (~'draw-shape [~'s ~'c] ~draw-body)
     ~'spatial/IBoundingBox 
     (~'get-bounding-box [~'b] (~'c/shape-bounds ~'b))))

;rectilinear stuff.

(defshape line 
  [color x1 y1 x2 y2] 
  (spatial/bbox  x1 y1 (- x2 x1) (- y2 y1))
  (c/draw-line c color x1 y1 x2 y2))

(defshape rectangle 
  [color x y w h]
  (spatial/bbox x y w h)
  (c/fill-rectangle c color x y w h ))

(defn ->square [color x y w]
  (->rectangle color x y w w))

(defshape wire-rectangle 
  [color x y w h]
  (spatial/bbox x y w h)
  (c/draw-rectangle c color x y w h))

(defn negate [n] (* -1 n))
(defn halve [n] (/ n 2))

(defn relative-coords 
  ([size]
		  [((comp negate halve) size) 
		   ((comp negate halve) size) 
		    size 
		    size])
  ([x1 y1 x2 y2] [x1 y1 x2 y2]))

(defn ->relative-rectangle [color size]
  (let [[x1 y1 x2 y2] (relative-coords size)]
  (->rectangle color x1 y1 x2 y2)))

(defn ->relative-wire-rectangle [color size] 
  (let [[x1 y1 x2 y2] (relative-coords size)]
  (->wire-rectangle color x1 y1 x2 y2)))

(defn circle->fillcoords 
  ([size] 
    (let [[x1 y1 x2 y2] (relative-coords size)]
      [(inc x1) (inc y1) (dec x2) (dec y2)]))
  ([x1 y1 x2 y2] [(inc x1) (inc y1) (dec x2) (dec y2)]))

(defshape ring [color x y w h]
               (spatial/bbox x y w h)
               (c/draw-ellipse c color x y w h))

(defshape circle [color x y w h]
                 (spatial/bbox x y w h)
                 (c/fill-ellipse c color x y w h))

;(defshape points [color coords]  
;  (->group (for [[x y] coords]
;             (->translation (* xscale x) (* yscale (- y 2))
;                            (->relative-rectangle :black 3)))))


(defn ->relative-circle [color size]
  (apply ->circle color (relative-coords size)))

(defn ->relative-ring [color size]
  (apply ->ring color (relative-coords size)))

(defn boundingbox->rectangle [b]
  (->wire-rectangle :black (spatial/get-left b) (spatial/get-bottom b) 
               (spatial/get-width b) (spatial/get-height b)))

(defn ->visual-bounds 
  ([s color]
    (assoc (boundingbox->rectangle (c/shape-bounds s)) :color color))
  ([s] (->visual-bounds s :black)))  

;(extend-protocol IShape
;  cljgui.spatial.boundingbox
;  (draw-shape [s g] (draw s g))
;  (shape-bounds [s] (get-scene-bounds scene)))
  

;(defn draw-rectangles [g rects]
;  (doseq [[x1 y1 x2 y2] rects] 
;    (draw-rectangle g x1 y1 x2 y2)))
;
;
;(defn fill-rectangles [g rects] 
;    (doseq [[x1 y1 x2 y2] rects] 
;    (fill-rectangle g x1 y1 x2 y2)))

;a structure to hold a sprite.  A sprite is just a raster image, built from 
;a source.  Source must be a buffered image.
(defshape sprite [source transparency x y]
  (spatial/bbox x y (c/bitmap-width source) (c/bitmap-height source))
  (c/draw-image c source transparency x y))

(defn make-sprite [s trans x y]
  (->sprite (image/shape->img s trans) trans x y))



;(defshape line3D 
;  [color x1 y1 z1 x2 y2 z2] 
;  (bbox  x1 y1 z1 (- x2 x1) (- y2 y1) (-z2 z1))
;  (draw-line c s))

;(defshape wire-box 
;  [color x y z w h d]
;  (bbox x y z w h d)
;  (fill-bounds c (bbox x y z w h d) color))
; 
;(defshape box3D
;  [color x y z w h d]
;  (bbox x y z w h d)
;  (fill-bounds c (bbox x y w h) color))

;(defprotocol ISpace 
;  (draw-box      [s b])
;  (draw-sphere   [s sph])
;  (draw-cylinder [s cyl])
