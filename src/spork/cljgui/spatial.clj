;Tom Spoon 16 June 2012 -> A clojure library for spatial data structures

;Starting with QuadTrees.
;Then bounding volume hierarchies.
(ns spork.cljgui.spatial)

(defprotocol IBoundingBox
  (get-bounding-box [bv] "Return an appropriate bounding volume."))
  
(defprotocol IBounded
  (get-width [b])
  (get-height [b])
  (get-left [b])
  (get-right [b])
  (get-top [b])
  (get-bottom [b]))

;Define a protocol for spatial structures.
;Ideally we can define a mulititude of spaces here....i.e. multiple 
;dimensions.  Bounds is a general notion....
;(defprotocol ISpatialIndex 
;  (assoc-bounds [space obj bounds] "Add new bounds for obj to the index.")
;  (dissoc-bounds [space obj] "Drop bounds associated with obj from the index.")
;  (get-bounds [space obj] "Return the bounds associated with obj."))

;protocol derived functions.
;(defn unbounded? [space obj]  (nil? (get-bounds space obj)))
;(defn conj-bounds
;  "Conjoins a bounds to the spatial data associated with the obj, relative to 
;   any existing bounds.  Effectively merges the bounds.  Unbounded objects will
;   now have bounds."
;  [space obj bounds] 
;  (let [old-bounds (get-bounds space obj)
;        new-bounds (group-bounds [old-bounds bounds])]
;    (assoc-bounds space obj new-bounds)))

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
  (if (not (and (unbounded? b1) (unbounded? b2))) 
           (let [[x0 y0 w0 h0] (bounds->coords b1)
                 [x1 y1 w1 h1] (bounds->coords b2)]
             (bbox (min x0 x1)
                   (min y0 y1)
                   (max (+ w0 x0) (+ w1  x1))
                   (max (+ h0 y0) (+ h1 y1)))))
    (if (unbounded? b1)
      b2
      b1))   

(defn group-bounds
  "Merge multiple bounds into a single bounding box."
  [bs]
  (reduce merge-bounds bs))




;build a structure and operations to define a quadtree.
;for geometric primitives.

;a quadtree is a hierarchical data structure that subdivides a 2 dimensional
;space into 4 sub regions of equal area.  Note, the regions may be square or 
;rectangular, depending on the bounds of the root.

(defrecord spatial-tree [bounds capacity floor items children])
(defn ->spatial-leaf
  "Constructs a spatial tree with no children.  Capacity defines an upper bound
   on the number of items each node in the tree contains before subdivision, 
   and floor defines a lower bound on the size of subdivided quads."
  ([bounds capacity floor items]
    (->spatial-tree bounds capacity floor  items nil))
  ([bounds capacity floor] (->spatial-leaf bounds capacity floor [])))

;changed the implementation....
;instead of an items vector 4 keys, just have an items map.
;this makes things more generic...

;Note this is one implementation, based on spatial leaves and nested trees. 
;We could just as easily define a geohash function that spatially hashes our 
;2d plane, and inserts items as [k v] pairs into an associative structure like
;a hashmap.
(defn make-quad-root
  "Define the root of a quad tree.  Quad trees are parameterized on capacity,
   which defines the number of elements a node can contain, either a bounds
   record, or an x y coordinate with a halfwidth that describes the bounds.
   areafloor is a parameter that indicates subdivisions should not occur if 
   they resulting area of the quads is < areafloor."
  ([capacity quadbounds areafloor] 
    (->spatial-leaf quadbounds capacity  areafloor))
  ([capacity areafloor x y width height]
    (make-quad-root capacity (bbox x y width height) areafloor)))

(def directions [:topleft :topright :bottomleft :bottomright])

(defn bounded->quads
  "Compute the canonical directional quadrants, or sub quads, of bounding box 
   b."
  [b]
  (let [x  (get-left b)
        y  (get-bottom b)
        w  (get-width b)
        h  (get-height b)
        wnext (/ w 2.0)
        hnext (/ h 2.0)]
      {:topleft (bbox x y wnext hnext)
       :topright (bbox (+ x  wnext) y wnext hnext)
       :bottomleft (bbox x (+ y hnext) wnext hnext)
       :bottomright (bbox (+ x wnext) (+ y hnext) wnext hnext)}))

(defn equal-bounds?
  "Determine if the region bounded by b is square."
  [b] (= (get-width b) (get-height b)))

(defn bounded-area
  "Compute the area of the bounded region b."
  [b] (* (get-width b) (get-height b)))

(defn sub-divide
  "Divide an existing spatial leaf into 4 sub-regions, returning a tree with 
   the subdivided regions as child leaves."
  [{:keys [capacity bounds children floor] :as qtree}]
  (let [leaves (for [[d b] (bounded->quads bounds)] ;can probably make this a reduce
                 [d (make-quad-root capacity b floor)])]                                   
    (merge qtree {:children (into {} leaves)})))

;general to spatial trees -> these could form a protocol.
(defn get-bounds [qtree] (:bounds qtree))
(defn get-floor [qtree] (:floor qtree))
(defn get-child [qtree dir] (get (:children qtree) dir))
(defn get-capacity [qtree] (:capacity qtree))
(defn get-items [qtree] (:items qtree))
(defn has-children? [qtree] (empty? (:children qtree)))
(defn get-children [qtree] (vals (:children qtree)))
(defn leaf? [qtree] (has-children? qtree))
(defn subdivided? [qtree] (not (leaf? qtree)))
(defn divisible? [qtree] (>= (/ (bounded-area (get-bounds qtree)) 4) 
                            (get-floor qtree)))
(defn drop-items [qtree] (assoc qtree :items []))
(defn has-capacity? [qtree] (<= (inc (count (get-items qtree))) 
                                (get-capacity qtree)))
(defn insert-child [qtree k childtree]
  (assoc-in qtree [:children k] childtree))
(defn set-items [qtree itms]
  (assoc qtree :items itms))
(defn insert-item [qtree itm]
  (set-items qtree (conj (get-items qtree) itm)))
(defn node-seq [qtree]
  (lazy-seq 
    (cons (dissoc qtree :children) 
          (mapcat node-seq (vals (:children qtree))))))

;(defn bfs-node-seq [qtree]
;  (lazy-seq 
;    (let [p (dissoc qtree :children)
;          cs ()]
;          (mapcat node-seq (vals (:children qtree))))))

(defn item-seq [qtree]
  (flatten (filter (complement empty?) (map get-items (node-seq qtree)))))

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
                              
    
;(defn hash-seq
;  "Return a set of DFS hashes to partitions in the tree.  "
;  [qtree]
;  ())


(defn box->bounds [qbox] ((juxt get-left get-right get-top get-bottom) qbox))

(defn find-enclosing-child [qtree qbox]
  (when (subdivided? qtree)
      (loop [remaining (keys (:children qtree))]
        (if (encloses? (get-bounds (get-child qtree (first remaining))) qbox)
          (first remaining)
          (when (seq (rest remaining))
            (recur (rest remaining)))))))

(defn bound-intersects? [b1 b2] (not (nil? (intersect-bounds b1 b2))))

(defn collision-seq 
  "Test bounds b aganst the spatial tree.  Returning a sequence of items the 
   bounds 'collide' with."
  [qtree b]
  (let [collision? (partial bound-intersects? b)]
    (loop [acc []
           remaining [qtree]]
      (if-let [tree (first remaining)] 	    
        (let [tb (get-bounds tree)]
          (if (collision? tb)
            (let [newitems (filter collision? (get-items tree))]
              (recur (concat acc newitems) 
                     (concat (rest remaining) (get-children tree))))                     
            (recur acc (rest remaining)))) 
        acc))))
                
;Could be a protocol member
(defn- merge-in-tree
  "Similar to assoc-in, but for trees.  given a path to a child node, merges 
   the subtree."
  [root path subtree]
  (if (empty? path) ;we're at root
    subtree
    (assoc-in root ;we're in a subtree
      (interleave (repeat :children) path) subtree))) ;interleave...is implementation detail.

(defn get-enclosing-path [root b]
  (loop [tree root
         path []]
    (if-let [child (find-enclosing-child tree b)]
      (recur (get-child tree child) (conj path child))
      path)))

;itm only has to support get-bounds.
(defn conj-spatial
  "Conjs the item onto the tree based on the item's bounds."
  [qtree itm] 
  (loop [root qtree ;root tree, entire tree
         subtree qtree ;subtree, the view
         path [] ;path from root to tree
         itms [itm]] ;pending insertions]
    (if-let [x (first itms)] ;items remain to conj
      (if (encloses? (get-bounds subtree) x) ;tree can contain the item
        (cond  
          (subdivided? subtree) ;tree has no capacity, has been divided.
            (if-let [k (find-enclosing-child  subtree x)]
              (recur root (get-child subtree k) (conj path k) itms) ;move focus
              ;item straddles quads, add it to internal node.
              (let [newtree (merge-in-tree root path 
                            (insert-item  subtree x))]
                (recur newtree newtree [] (rest itms))))   
          ;current tree has capacity or we can't subdivide further...
          (or (and (<= (inc (count (:items subtree))) (:capacity subtree)))
              (not (divisible? subtree)))
            (let [newtree (merge-in-tree root path 
                            (insert-item  subtree x))] ;modify current focus                  
              (recur newtree newtree [] (rest itms)))                
          :else ;need to sub-divide the current subtree, reinsert its items.            
            (let [divided  (drop-items (sub-divide subtree))
                  newtree  (merge-in-tree root path divided)
                  newitems (reduce conj (get-items subtree) itms)]
              (recur newtree divided path newitems)))
        root)
      root)))
   
(defn conj-spatials
  "Conj a bunch of volumes."
  [qtree itms]
  (reduce conj-spatial qtree itms))

(defn random-points
  "Generate a sequence of random points inside of the area around (x,y) +- hw."
  [x y hw]
  (let [w (* 2 hw)
        pointsize (/ 100 hw)
        [xmin xmax] [pointsize (- (+ x hw) pointsize)]
        [ymin ymax] [pointsize (- (+ y hw) pointsize)]        
        randbetween (fn [l u] (+ l (rand-int (- u l))))]                  
    (repeatedly
      (fn []
        (let [rx  (randbetween xmin xmax)
              ry  (randbetween ymin ymax)]
        (bbox-around  rx ry pointsize))))))

(comment
  (def testquad (make-quad-root 1 4 0 0 150 100))  
  (def testpoints (random-points 50 50 50))
  (def selection-box (bbox 25 25 30 30))
  (def point (bbox-around 3 14 2))
  (def invalidpoint (bbox-around 100 100 1))
)
;(defn conj-quad
;  "Add a bounded region of space to the quadtree.  Starting at the root, search
;   each child to find if the child's region of space contains the new bounds."
;  [q bounds shape]
;  (