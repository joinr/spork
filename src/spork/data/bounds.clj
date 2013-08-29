(ns spork.data.bounds
  (:require [spork.protocols.spatial :refer :all]))

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