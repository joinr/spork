(ns spork.data.quadtree
  (:require [spork.protocols.spatial :as spatial]
            [spork.data.bounds :refer :all]))

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
    (make-quad-root capacity (spatial/bbox x y width height) areafloor)))

(def directions [:topleft :topright :bottomleft :bottomright])

(defn bounded->quads
  "Compute the canonical directional quadrants, or sub quads, of bounding box 
   b."
  [b]
  (let [x  (spatial/get-left b)
        y  (spatial/get-bottom b)
        w  (spatial/get-width b)
        h  (spatial/get-height b)
        wnext (/ w 2.0)
        hnext (/ h 2.0)]
      {:topleft (spatial/bbox x y wnext hnext)
       :topright (spatial/bbox (+ x  wnext) y wnext hnext)
       :bottomleft (spatial/bbox x (+ y hnext) wnext hnext)
       :bottomright (spatial/bbox (+ x wnext) (+ y hnext) wnext hnext)}))

(defn equal-bounds?
  "Determine if the region bounded by b is square."
  [b] (= (spatial/get-width b) (spatial/get-height b)))

(defn bounded-area
  "Compute the area of the bounded region b."
  [b] (* (spatial/get-width b) (spatial/get-height b)))

(defn sub-divide
  "Divide an existing spatial leaf into 4 sub-regions, returning a tree with 
   the subdivided regions as child leaves."
  [{:keys [capacity bounds children floor] :as qtree}]
  (let [leaves (for [[d b] (bounded->quads bounds)] ;can probably make this a reduce
                 [d (make-quad-root capacity b floor)])]                                   
    (merge qtree {:children (into {} leaves)})))

;general to spatial trees -> these could form a protocol.
(defn quad-bounds [qtree] (:bounds qtree))
(defn get-floor [qtree] (:floor qtree))
(defn get-child [qtree dir] (get (:children qtree) dir))
(defn get-capacity [qtree] (:capacity qtree))
(defn get-items [qtree] (:items qtree))
(defn has-children? [qtree] (empty? (:children qtree)))
(defn get-children [qtree] (vals (:children qtree)))
(defn leaf? [qtree] (has-children? qtree))
(defn subdivided? [qtree] (not (leaf? qtree)))
(defn divisible? [qtree] (>= (/ (bounded-area (quad-bounds qtree)) 4) 
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

(defn find-enclosing-child [qtree qbox]
  (when (subdivided? qtree)
      (loop [remaining (keys (:children qtree))]
        (if (spatial/encloses? 
              (quad-bounds (get-child qtree (first remaining))) qbox)
          (first remaining)
          (when (seq (rest remaining))
            (recur (rest remaining)))))))

(defn collision-seq 
  "Test bounds b aganst the spatial tree.  Returning a sequence of items the 
   bounds 'collide' with."
  [qtree b]
  (let [collision? (partial spatial/bound-intersects? b)]
    (loop [acc []
           remaining [qtree]]
      (if-let [tree (first remaining)] 	    
        (let [tb (quad-bounds tree)]
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
      (interleave (repeat :children) path) subtree))) 
      ;interleave...is implementation detail.

(defn get-enclosing-path [root b]
  (loop [tree root
         path []]
    (if-let [child (find-enclosing-child tree b)]
      (recur (get-child tree child) (conj path child))
      path)))

;itm only has to support quad-bounds.
(defn conj-spatial
  "Conjs the item onto the tree based on the item's bounds."
  [qtree itm] 
  (loop [root qtree ;root tree, entire tree
         subtree qtree ;subtree, the view
         path [] ;path from root to tree
         itms [itm]] ;pending insertions]
    (if-let [x (first itms)] ;items remain to conj
      (if (spatial/encloses? (quad-bounds subtree) x) ;tree can contain the item
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
        (spatial/bbox-around  rx ry pointsize))))))

(comment
  (def testquad (make-quad-root 1 4 0 0 150 100))  
  (def testpoints (random-points 50 50 50))
  (def selection-box (spatial/bbox 25 25 30 30))
  (def point (spatial/bbox-around 3 14 2))
  (def invalidpoint (spatial/bbox-around 100 100 1))
)
