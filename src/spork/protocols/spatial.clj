;Tom Spoon 16 June 2012 -> A clojure library for spatial data structures

;;Generally, we'll be dealing with a class of data structures that have the 
;;ability to partition an n-dimensional space, contain information about items
;;in the space, and to support several forms of spatial queries about items
;;in the space.  

;;In general, space is of arbitrary dimensionality.  How a spatial container 
;;subdivides space is irrelevant.  All that is relevant is that the spatial 
;;container knows how to conjoin items.

;;Going to generalize here..
;;We typically have a notion of bounds as they pertain to 2 or 3 dimensions.
;;Certain data structures, like KD trees an


;Starting with QuadTrees.
;Then bounding volume hierarchies.
(ns spork.protocols.spatial
  (:require [spork.util [vectors :as v]
                        [vecmath :as vmath]]))



;;Some geometric primitives.  I'll probably port these to a more general lib, 
;;but they live here for now.


;;General bounds will be represented using vectors and meta data about the 
;;bound.  We'll use an arbitrary-length vector to communicate the extents of 
;;the bounded region.  

(defprotocol IBounds 
  (bounds-dimension [b] "Return the dimensionality of the bounds")
  (bounds-center [b] "Return the center of the bounds")
  (bounds-half-vector  [b] 
    "Return the half-widths of the bounds, ordered by x,y,z.")
  (bounds-extreme-points [b] 
    "Return dimension pairs of points, representing the min and max.")  
  )

(defn half-width  
    "Return the half-width of the bounds along the y axis"
    [b] (v/vec-nth (bounds-half-vector b) 0))

(defn half-height  
    "Return the half-width of the bounds along the y axis"
    [b] (v/vec-nth (bounds-half-vector b) 1))

(defn half-depth  
    "Return the half-width of the bounds along the z axis."
    [b] (v/vec-nth (bounds-half-vector b) 2))

(defprotocol IRadialBounds
  (bounds-radius [b] "Return the radius of the bounds"))

(defprotocol IBoxBounds
  (bounds-corner [b] "Return the corner of the bounding box")
  )

(defprotocol IFastBounds 
  (-bounds-type    [b]       "Returns the type of bounds...duh") 
  (-compare-bounds [b other] 
     "Exploits information  to avoid comparison via extrema."))

(extend-protocol IBounds 
  clojure.lang.PersistentArrayMap
  (bounds-dimension [b] (get-in b [:bounds :dimension]))
  (bounds-extreme-points [b] (get-in b [:bounds :extreme-points])))

;;standard extreme points for bounds rooted at the origin.
(def unit-extrema  [[0 1] [0 1] [0 1]])
(defn between? [x l r] (and (> x l) (< x r)))

;;It looks like we have a generic set of rejection tests for any two
;;bounding volumes.  It typically boils down to comparing the vector 
;;defined by the vector subtraction of the each volume's center point 
;;to the maximum half-width in each dimension.  From there, we can
;;step through dimensions to see if any axes overlap.

(defn compare-segment 
  ([x1 x2 y1 y2]
      (cond (or (= x1 y1) (= x2 y2)) :intersected
            (between? x1 y1 y2) (if (between? x2 y1 y2) :enclosed :intersected)
            (between? y1 x1 x2) (if (between? y2 x1 x2) :encloses :intersected)
            :otherwise :separated))
  ([xvec yvec] (compare-segment (v/vec-nth xvec 0) (v/vec-nth xvec 1) 
                                (v/vec-nth yvec 0) (v/vec-nth yvec 1))))


(defn compare-extrema
  "Compare two sets of extreme points.  This is the slow, but guaranteed way
   to compare any two spatial objects.  If we have sets of extremes in each 
   dimension, we can walk the points, starting in the first dimension, and 
   do a simple line-segment containment test.  Fails as soon as a non-containing
   case is found.  If we run out of points, i.e. xs is of lower dimension than
   ys, we stop. Yields :encloses if xs encloses ys, :enclosed if ys encloses xs,
   :intersected if xs intersects with ys anywhere, :separated"
  [xs ys]
  (let [comps (map compare-segment xs ys)]
    (loop [acc         (first comps)
           comparisons (rest comps)]
      (if (empty? comparisons) acc
        (let [res (first comparisons)]
          (if (or (not= res acc) (= res :intersected)) :intersected
            (recur res (rest comparisons)))))))) 


(defn as-3d [v] 
  (if (= (v/dimension v) 2) (v/->vec3 (v/vec-nth v 0) (v/vec-nth v 1) 0)
    v))


;;pending...
;(defn ray-sphere-feasible? [ray sphere]
;  (let [origin (:origin ray)
;        dir    (:dir ray)
;        center (bounds-center sphere)
;        rad (bounds-radius sphere)
;        v (vecmath/v- o c)
;        res (vecmath/dot v dir) ;project the difference vector onto the ray dir.
;        ]
;    (when (> res 0) {:v v :center center :origin origin 
    

;;Given function f, which applied to l and r yields a half-width 
;;along the same dimension, compares the squared result to a squared 
;;distance to determine if the axis separates l and r.
(defn axis-separated? [f l r sq-distance]
  (let [left-width  (f l)
        right-width (f r)]
    (> sq-distance 
       (+ (* left-width left-width) (* right-width right-width)))))

(defn compare-widths 
  ([left-width right-width]
     (compare-segment 0 left-width 0 right-width))
  ([f l r] (compare-segment 0 (f l) 0 (f r))))

;;We develop a quick rejection test for bounds checking using a 
;;fast variant of the Separating Axis theorem (more to follow on SAT).
(defn sphere-compare [l r & {:keys [test] :or {test :intersection}}]
  (let [cl (bounds-center l)
        cr (bounds-center r)
        center-line (vmath/v- cr cl)
        sq-distance (vmath/v-norm-squared center-line) ;dist^2 between spheres
        rl     (bounds-radius l) 
        rr     (bounds-radius r)
        ]
    (if (axis-separated? bounds-radius l r sq-distance) :separated
        ;;otherwise we use our line-segment test.
        (if (= test :containment) (compare-widths rl rr)
            :intersected ))))

(defn box-compare [l r {:keys [test] :or {test :intersection}}]
  (let [cl (bounds-center l)
        cr (bounds-center r)
        center-line (vmath/v- cr cl)
        sq-distance (vmath/v-norm-squared center-line) ;dist^2 between bounds
        ]
    (if (or (axis-separated? half-width  l r) ;rejection test, fail fast.
            (axis-separated? half-height l r)
            (axis-separated? half-depth  l r)) :separated
        ;;otherwise we use our line-segment test.
        (if (= test :containment) 
            (or (compare-widths half-width  l r)
                (compare-widths half-height l r)
                (compare-widths half-depth  l r)) 
            :intersected ))))

(defn ->sphere-bounds [center radius]
  (let [x   (v/vec-nth center 0) 
        y   (v/vec-nth center 1)
        z   (v/vec-nth center 2)
        extrema [(v/->vec2 (- x radius) (+ x radius))
                 (v/->vec2 (- y radius) (+ y radius))
                 (v/->vec2 (- z radius) (+ z radius))]]
    (reify
      IBounds 
      (bounds-dimension [b] 3)
      (bounds-center    [b] center)
      (bounds-extreme-points [b] extrema)
      (bounds-half-vector [b] (v/->vec1 radius))
      IRadialBounds
      (bounds-radius [b] radius)
      IFastBounds 
      (-bounds-type [b] :sphere)
      (-compare-bounds [b other] 
        (when (= (-bounds-type other) :sphere)
           (sphere-compare b other))))))

(defn ->box-bounds [center width height depth]
  (let [center (as-3d center)        
        x      (v/vec-nth center 0) 
        y      (v/vec-nth center 1)
        z      (v/vec-nth center 2)
        half-width   (/ width 2.0)
        half-height  (/ height 2.0)
        half-depth   (/ depth 2.0)                            
        extrema [(v/->vec2 (- x width) (+ x width))
                 (v/->vec2 y (+ y height))
                 (v/->vec2 z (+ z depth))]
        halves (v/->vec3 half-width half-height half-depth)]
    (reify 
      IBounds 
      (bounds-dimension [b] 3)
      (bounds-center    [b] center)  
      (bounds-half-vector [b] halves)
      (bounds-extreme-points [b] extrema)
      IBoxBounds
      (bounds-corner [b] (v/->vec3 (- x half-width)
                         (- y half-height)
                         (- z half-depth)))
      IFastBounds 
      (-bounds-type [b] :box)
      (-compare-bounds [b other] 
        (when (= (-bounds-type other) :box)
          (box-compare b other))))))

(defprotocol IBoundingBox
  (get-bounding-box [bv] "Return an appropriate bounding volume."))
  
(defprotocol IBounded
  (get-width  [b])
  (get-height [b])
;  (get-depth  [b])
  (get-left   [b])
  (get-right  [b])
  (get-top    [b])
  (get-bottom [b])
;  (get-front [b])
;  (get-back [b])
)

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



;;Some implementations
;;These need to be rethought...Too much mixing data with protocol.

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
  ([x y width height theta] (->boundingbox x y (Math/abs width) (Math/abs height) theta))
  ([x y width height] (->boundingbox x y (Math/abs width) (Math/abs height) 0)))

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
  (let [x  (get-left b) 
        y  (get-bottom b)
        w  (+ x (get-width b)) 
        h  (+ y (get-height b))]
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

;;This is erroneously labeled; w and h are actually x and y coords, 
;;extrema, not widths.
(defn merge-bounds
  "Merge the extreme points from IBounded b1 and b2 into a single bounding box."
  [b1 b2]
  (let [[x0 y0 w0 h0] (bounds->coords b1)
        [x1 y1 w1 h1] (bounds->coords b2)]
    (let [xnew (min x0 x1)
          ynew (min y0 y1)]
      (bbox xnew 
            ynew          
            (- (max w0  w1) xnew) 
            (- (max h0 h1) ynew)))))

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

;;spatial index....work on this later.

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

;;re-write..
(comment 

;;Tests for later 
(deftest segment-comparisons
  (is (= (compare-segment [-1 4] [-1 45]) :intersected))
  (is (= (compare-segment [-1 4] [-1 2])  :intersected))
  (is (= (compare-segment [-1 4] [-2 2])  :intersected)) 
  (is (= (compare-segment [-1 4] [0 2])   :encloses))
  (is (= (compare-segment [-1 4] [-2 5])  :enclosed)))


(deftest extrema-testing 
  (is (= (compare-extrema [[0   1]] 
                          [[-10 10]]) :enclosed))
  (is (= (compare-extrema [[0 1]] 
                          [[0 10]]) :intersected))
  (is (= (compare-extrema [[0    1] [0 2]]
                          [[-10 10] [0 2]]) :intersected))
  (is (= (compare-extrema [[0 1]    [1 3]] 
                          [[-10 10] [0 5]])) :enclosed)
  (is (= (compare-extrema [[0 1]    [1 3]] 
                          [[-10 10] [0 5] [-100 100]])) :enclosed)
  (is (= (compare-extrema [[-10 10] [0 5] [-100 100]]
                          [[0 1]    [1 3]]))  :encloses))

)

