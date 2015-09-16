;;Todo -> Rethink this design, now that we've merged multiple projects.  Might 
;;be able to make it more elegant.
(ns spork.geometry.shapes
  (:require [spork.graphics2d [canvas :as c]
                              [image :as image]
                              [font :as f]]
            [spork.protocols  [spatial  :as spatial]]
            [spork.util.general]))

;;Basically a no-op for rendering....we ignore
(def pass
  (let [ebox (spork.protocols.spatial/bbox 0 0 1 1)]
    (reify c/IShape
      (draw-shape [shp c] c)
      (shape-bounds [c] ebox))))

;;The value here is a bit dubious, but it saves on some boilerplate..
(defmacro defshape [name args bounds draw-body & specs]
  `(defrecord ~name [~@args] 
     ~'c/IShape
     (~'shape-bounds [~'s] ~bounds) 
     (~'draw-shape [~'s ~'c] ~draw-body)
     ~'spatial/IBoundingBox 
     (~'get-bounding-box [~'b] (~'c/shape-bounds ~'b))
     ~@specs))

;rectilinear stuff.

(defshape line 
  [color x1 y1 x2 y2] 
  (spatial/bbox  x1 y1 (- x2 x1) (- y2 y1))
  (c/draw-line c color x1 y1 x2 y2))

(defshape rectangle 
  [color x y w h]
  (spatial/bbox x y w h)
  (c/fill-rectangle c color x y w h))

(defn ->square [color x y w]
  (->rectangle color x y w w))

(defshape text [color font txt x y]
  (f/string-bounds font txt x y)
  (c/draw-string c color font txt x y))

(defn ->plain-text 
  ([color s x y]        (->text color (f/get-font :default) s x y))
  ([color size txt x y] (->text color (f/resize-font f/default-font size) 
                                txt x y)))

(defshape wire-rectangle 
  [color x y w h]
  (spatial/bbox x y w h)
  (c/draw-rectangle c color x y w h))

(defn negate [n] (- n))
(defn halve  [n] (/ n 2))

(defn relative-coords 
  ([size]  [((comp negate halve) size) 
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

;;This forms the basis for an easy way to wrap j2d shapes...
;;a wrapper around a java poloygon, somewhat of a hack atm...
;;We should probably formalize this a bit better, make it more
;;portable.
;; (defn ->polygon [color points]
;;   (let [^java.awt.Polygon p (java.awt.Polygon.)
;;         _ (doseq [[^int x ^int y] points]
;;             (.addPoint p x y))
;;         ^java.awt.Rectangle b (.getBounds p)
;;         bnds (spork.protocols.spatial/bbox (.getX b) (.getY b) (.getWidth b) (.getHeight b))]
;;     (proxy [java.awt.Polygon
;;             spork.graphics2d.canvas.IShape
;;             spork.protocols.spatial.IBoundingBox] []       
;;       (get_bounding_box [b] bnds)
;;       (shape_bounds [] bnds)
;;       (draw_shape   [canv]  (c/draw-polygon canv color p)))))

(deftype polygon [^java.awt.Polygon p color bnds drawf]
  c/IShape
  (draw-shape [shp c] (drawf c color p))
  (shape-bounds [shp] bnds)
  spatial/IBoundingBox
  (get-bounding-box [shp] bnds)
  clojure.lang.IDeref
  (deref [obj] p))
  
(defn ->polygon
  ([color points drawf]
   (let [^java.awt.Polygon p (java.awt.Polygon.)
         _ (doseq [[^int x ^int y] points]
             (.addPoint p x y))
         ^java.awt.Rectangle b (.getBounds p)
         bnds (spork.protocols.spatial/bbox (.getX b) (.getY b) (.getWidth b) (.getHeight b))]
     (polygon. p color bnds drawf)))
  ([color points] (->polygon color points c/draw-polygon)))
(defn ->filled-polygon [color points] (->polygon color points c/fill-polygon))


;;Currently dropped.

;; (def colored-ring (spork.util.general/memo-1
;;                    (fn [clr]
;;                      (spork.graphics2d.image/shape->img
;;                       :translucent (->ring clr 0 0 10 10)))))

;; (def colored-point (spork.util.general/memo-1
;;                     (fn [clr]
;;                       (spork.graphics2d.image/shape->img
;;                        :translucent (->circle clr 0 0 10 10)))))
  
  

;;As we add shapes to the stack, we update the buffer (only by drawing the new
;;shape).  This allows us to incrementally render a shape, keeping sort of a
;;"dirty" canvas over time in a controlled fashion.
(defrecord recording [shapes buffer width height]
  c/IShape
  (draw-shape   [shp c] (c/draw-image c buffer :opaque 0 0))
  (shape-bounds [shp]   (spatial/bbox 0 0 width height))
  c/IShapeStack
  (push-shape   [s shp]
    (recording. shapes
                 (do (c/draw-shape shp (c/get-graphics buffer)) buffer)
                 width
                 height))                                     
  (pop-shape    [s]   (let [shps (pop shapes)
                            buff (c/wipe buffer)
                            _    (c/draw-shape shps (c/get-graphics buff))]
                        (recording. shps buff width height)))  
  c/IWipeable
    (wipe [obj]  (recording. '() (c/wipe buffer) width height))
    )

;;Creates a recording (basically a dirty canvas...note that we can use any image
;;as a dirty canvas....we should look into better idioms to describe this....
;;Is there a semantic difference between a canvas (an area of fixed dimension
;;that mutates as it's drawn upon) vs a pure canvas?
(defn ->rec [shps w h]
  (reduce c/push-shape
          (->recording '()
                       (spork.graphics2d.image/make-imgbuffer w h ) w  h)          
          shps))


(defn ->area [color x y pts]
  (let [[bx by] (first pts)
        front (when (not= by y)
                (if (not= bx x)
                  [[x y] [bx y]]
                  [[x y]]))
        [ex ey] (last pts)
        end  (when (not= ey y)
               [ex y])
        mdl  (if front (into front pts) pts)]
   (->filled-polygon color (if end
                      (conj mdl end)
                      mdl))))
(defn ->perimeter [color x y pts]
  (let [[bx by] (first pts)
        front (when (not= by y)
                (if (not= bx x)
                  [[x y] [bx y]]
                  [[x y]]))
        [ex ey] (last pts)
        end  (when (not= ey y)
               [ex y])
        mdl  (if front (into front pts) pts)]
   (->polygon color (if end
                      (conj mdl end)
                      mdl))))  

;;A generic xy pointcloud.
(defn ->point-cloud [xy->point coords]
  (let [draw-point!
;             (if (satisfies? c/IShape xy->point)
        (let [tx   (atom 0)
              ty   (atom 0)
              sprt (spork.graphics2d.image/shape->img xy->point)]
          (fn [c x y]  (c/with-translation x y c
                         #(c/draw-shape  sprt %))))
               ;; (let [get-point! (spork.util.general/memo-fn [x y]
               ;;                    (spork.graphics2d.image/shape->img (xy->point x y)))]
               ;;   (fn [c x y]
               ;;     (c/with-translation x y c
               ;;       #(c/draw-shape (get-point! x y) %))
               ;;     c))
        [xmin xmax ymin ymax]
            (reduce (fn [[xm xmx ym ymx] [x y]]
                      [(min x xm) (max x xmx)
                       (min y ym) (max y ymx)])
                    [Double/MAX_VALUE
                     Double/MIN_VALUE
                     Double/MAX_VALUE
                     Double/MIN_VALUE]
                    coords)
       {:keys [x y width height]} (c/shape-bounds xy->point)
        bnds (spork.protocols.spatial/bbox xmin ymin (max (- xmax xmin) width)  (max (- ymax ymin) height))]
    (reify c/IShape
      (shape-bounds [s] bnds)
      (draw-shape [s c] (reduce (fn [c xy]
                                  (draw-point! c (first xy) (second xy)))
                                c coords)))))
