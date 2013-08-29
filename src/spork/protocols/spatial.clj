;Tom Spoon 16 June 2012 -> A clojure library for spatial data structures

;Starting with QuadTrees.
;Then bounding volume hierarchies.
(ns spork.protocols.spatial)

(defprotocol IBoundingBox
  (get-bounding-box [bv] "Return an appropriate bounding volume."))
  
(defprotocol IBounded
  (get-width [b])
  (get-height [b])
  (get-left [b])
  (get-right [b])
  (get-top [b])
  (get-bottom [b]))

;One way to store bounding information.
;We use meta data attached to symbols to store bounding information.
;(defn set-bounds [obj bounds] 
;  (with-meta obj (assoc (meta obj) :bounds bounds)))
;(defn get-bounds [obj] (get (meta (:data nd)) :bounds nil))
;(defn unbounded? [obj] (nil? (get-bounds obj))
;(defn conj-bounds
;  "Adds a bounds to the spatial data associated with the node."
;  [obj bounds] 
;  (let [old-bounds (get-node-bounds nd)
;        new-bounds (group-bounds [old-bounds bounds])]
;    (with-meta obj (assoc (meta obj) :bounds new-bounds))))

;(get-depth [b])
  ;(get-front [b])
  ;(get-back [b])

;a 3 dimensional vector, or a 3D point... 
;(defrecord vec3 [x y z])
;(defn ->vec2 [x y] (->vec3 x y 0))

;(defn bounding-cuboid [x1 y1 x2 y2 x3 y3])

;protocol-derived functionality.
(defn get-center
  "Compute the center [x y] coordinate pair of IBounded b."
  [b]
  [(+ (get-left b) ( / (get-width b) 2))
   (+ (get-bottom b) (/ (get-height b) 2))])

;Note -> might change this to a protocol at some point, since enclose can 
;mean different things to different types of bounds....

(defn encloses?
  "Predicate to indicate if qbox spacially contains candidatebox, i.e.
   candidate box's bounds fit inside of qbox."
  [b1 b2]
  (and (<= (get-left b1) (get-left b2))
       (<= (get-bottom b1) (get-bottom b2))
       (> (get-top b1)   (get-top b2))
       (> (get-right b1) (get-right b2))))

(defn rotate-xy
  "Rotate a coordinate pair about the origin (0,0) by theta radians."
  [theta x1 y1]
  (let [c (Math/cos theta)
        s (Math/sin theta)]
    [(+ (* x1 c) (* y1 (* -1 s)))
     (+ (* x1 s) (* y1 (* c)))]))

(defn extreme-points
  "Compute the extreme points (min and max) from a sequence of numbers."
  [xs]
  (loop [min :inf 
         max :neginf
         remaining xs]
     (if-let [x (first remaining)]
       (let [min- (cond (keyword? min) x
                        (< x min) x
                        :else min)
             max- (cond (keyword? max) x
                        (> x max) x
                        :else max)]
         (recur min- max- (rest remaining)))
       [min max])))                                           

(defn get-rotated-bounds
  "After rotating a coordinate pair, determine the extreme points from the 
   transformed pairs."
  [theta [xys]]
  (let [rotate (partial rotate-xy theta)         
        [[x1 y1] [x2 y2]] (map #(apply rotate %) (partition 2 xys))
        [[xmin xmax] [ymin ymax]] [(extreme-points [x1 x2]) 
                                   (extreme-points [y1 y2])]]                
    [xmin ymin xmax ymax]))

(defn xywh->coords
  "Convert an x,y origin, plus a width and a height, into absolute coorindates."
  [x y w h]
  [x y (+ x w) (+ y h)])

(defn xywh->rotatedcoords
  "Rotate an x, y origin, plus a width and height, by theta radians."
  [x y w h theta]
  (get-rotated-bounds theta (xywh->coords)))

(defn xywh->rotated-xywh
  "Transform an x, y origin, plus a width and height, into an identical dataset
   transformd by a rotation of theta radians."
  [x y w h theta]
  (let [[x1 y1 x2 y2] (xywh->rotatedcoords x y w h theta)]
    [x1 y1 (- x2 x1) (- y2 y1)]))

(defn rotated-bounds?
  "Returns true if theta indicates a practical rotation, i.e. the value for 
   theta will create a set of bounds different than not having rotated at all."
  [theta]
  (cond (zero? theta) false 
        (zero? (mod theta (* 2 Math/PI))) false
        :else true))

;Define a protocol for spatial structures.
;Ideally we can define a mulititude of spaces here....i.e. multiple 
;dimensions.  Bounds is a general notion....
(defprotocol ISpatialIndex 
  (assoc-bounds [space obj bounds] "Add new bounds for obj to the index.")
  (dissoc-bounds [space obj] "Drop bounds associated with obj from the index.")
  (get-bounds [space obj] "Return the bounds associated with obj.")
  (get-collisions [space bounds] "Return a set of indices that intersect bounds.")  
  (spatial-bounds [space] "Return the bounds associated with the space."))

;protocol derived functions.
(defn unbounded? [space obj]  (nil? (get-bounds space obj)))
(defn conj-bounds
  "Conjoins a bounds to the spatial data associated with the obj, relative to 
   any existing bounds.  Effectively merges the bounds.  Unbounded objects will
   now have bounds."
  [space obj bounds] 
  (let [old-bounds (get-bounds space obj)
        new-bounds (group-bounds [old-bounds bounds])]
    (assoc-bounds space obj new-bounds)))



