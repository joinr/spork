;Tom Spoon 16 June 2012 -> A clojure library for spatial data structures

;Starting with QuadTrees.
;Then bounding volume hierarchies.
(ns spork.cljgui.spatial.core)

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

;Bounding rectangle, origin at x y, width wide and height high.
;Theta can describe a rotation.
(defrecord boundingbox [x y width height theta]
  IBounded 
  (get-width [b] (if (rotated-bounds? theta)
                   (let [[_ _ w _] (xywh->rotated-xywh x y width height theta)]
                     w)
                   width))
  (get-height [b] (if (rotated-bounds? theta)
                    (let [[_ _ _ h] (xywh->rotated-xywh x y width height theta)]
                      h)
                    height))
  (get-left [b] (if (rotated-bounds? theta)
                   (let [[x & rest] (xywh->rotated-xywh x y width height theta)]
                     x)
                   x))
  (get-right [b] (if (rotated-bounds? theta)
                   (let [[x _ w _] (xywh->rotated-xywh x y width height theta)]
                     (+ x w))
                   (+ x width)))
  (get-top [b]   (if (rotated-bounds? theta)
                   (let [[_ y _ h] (xywh->rotated-xywh x y width height theta)]
                     (+ y h)) 
                   (+ y height)))
  (get-bottom [b] (if (rotated-bounds? theta)
                   (let [[_ y _ _] (xywh->rotated-xywh x y width height theta)]
                     y)
                   y))
  IBoundingBox
  (get-bounding-box [bv] (if (zero? theta) 
                           bv
                           (let [[x2 y2 w2 h2] 
                                 (xywh->rotated-xywh x y width height theta)] 
                             (->boundingbox x2 y2 w2 h2 0)))))

(defrecord boundingcircle [x y width height]
  IBounded 
  (get-width  [b] width)
  (get-height [b] height)
  (get-left   [b] x)
  (get-right  [b] (+ x width))
  (get-top    [b] (+ y height))
  (get-bottom [b] y)
  IBoundingBox
  (get-bounding-box [bv] (->boundingbox x y width height 0)))

(defn bbox
  "Defines a bounding box, rooted at (x,y), width wide, height high,
   and rotated by theta radians.  If theta is zero, then no rotation is
   performed."
  ([x y width height theta] (->boundingbox x y width height theta))
  ([x y width height] (->boundingbox x y width height 0)))

(defn bbox-around 
  "Defines a bounding box centered at coordinates x,y, with perimeter extending 
   halfwidth on the x axis, and halfheight on the y axis."
  ([x y halfwidth halfheight] 
    (bbox (- x halfwidth) (- y halfwidth) (* 2 halfwidth) (* 2 halfheight)))
  ([x y halfwidth] (bbox-around  x y halfwidth halfwidth)))

(defn bounds->coords
  "Convert IBounded b to a set of absolute coordinates, using protocol 
   functions."
  [b]
  (let [[x y] [(get-left b) (get-bottom b)]
        [w h] [(+ x (get-width b)) (+ y (get-height b))]]
    [x y w h]))

(defn coords->bounds
  "Convert a vector of coordinates into a generic bounding box."
  [[x1 y1 x2 y2]]
  (bbox x1 y1 (- x2 x1) (- y2 y1)))

(defn scale-bounds
  "Performs a 2D scale of [xscale yscale] on a bounding box." 
  [xscale yscale {:keys [x y width height]}]
  (bbox (* xscale x) (* yscale y) (* xscale width) (* yscale height)))

(defn translate-bounds
  "Translates the bounding box's coordinates by x y."
  [tx ty  {:keys [x y width height]}]
  (bbox (+ tx x) (+ ty y) width height)) 

(defn rotate-bounds
  "Adds theta to the bounding box's rotation angle."
  [t {:keys [x y width height theta]}]
  (bbox x y width height (+ t theta)))

(defn merge-bounds
  "Merge the extreme points from IBounded b1 and b2 into a single bounding box."
  [b1 b2]
  (let [[x0 y0 w0 h0] (bounds->coords b1)
        [x1 y1 w1 h1] (bounds->coords b2)]
    (bbox (min x0 x1)
          (min y0 y1)
          (max (+ w0 x0) (+ w1  x1))
          (max (+ h0 y0) (+ h1 y1)))))

(defn group-bounds
  "Merge multiple bounds into a single bounding box."
  [bs]
  (reduce merge-bounds bs))

(defn line-overlap 
  ([min1 max1 min2 max2]
	  (let [l (max min1 min2)
	        r (min max1 max2)]
	    (cond (>= max1 min2) [l r]
	          :else nil)))
  ([[min1 max1] [min2 max2]] 
    (line-overlap min1 max1 min2 max2)))

(defn intersect-bounds
  "Compute the intersection of the bounds, returning a bounding"
  [b1 b2] 	(let [[x1 y1 x2 y2] (bounds->coords b1)
                 [x3 y3 x4 y4] (bounds->coords b2)
                 l-r         (line-overlap x1 x2 x3 x4)
                 bottom-top (line-overlap y1 y2 y3 y4)]
             (when (and l-r bottom-top)
               (let [[l r] l-r
                     [b t] bottom-top]
                 (bbox l b (- r l) (- t b)))))) 

(defn union-bounds
  "Compute the union of the bounds, returning a bounding"
  [b1 b2] (merge-bounds b1 b2))
    
(defn box->bounds [qbox] ((juxt get-left get-right get-top get-bottom) qbox))
(defn bound-intersects? [b1 b2] (not (nil? (intersect-bounds b1 b2))))


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



