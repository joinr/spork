;;Various forms of clustering analysis, and vizualizations.
;;Adapted from Toby Segaran's excellent "Programming Collective Intelligence" 
;;book.
(ns spork.mining.clustering
  (:require [spork.mining [core :as core]]
            [spork.util   [table :as tbl]]
            [spork.data   [priorityq :as pq]]
            [spork.cljgui.components [swing :as gui]]
            [spork.graphics2d [canvas :as canvas]
                              [image  :as img]
                              [swing  :as swingcanvas]]
            [spork.geometry.shapes :refer :all]
            [spork.util [zip :as zip]] ;added zipper operations
            ))

(defn sum [xs]   
  (loop [acc 0.0
         idx 0]
    (if (= idx (count xs)) acc
      (recur (+ acc (nth xs idx)) (unchecked-inc idx)))))
(defn square [x] (* x x))
(defn sum-squares [xs] 
  (loop [acc 0.0
         remaining xs]
    (if (empty? remaining) acc
      (recur (+ acc (square (first remaining)))
             (rest remaining)))))
(defn dot [xs ys]
  (let [b (count xs)]
    (loop [acc (transient [])
           idx 0]
      (if (== idx b) (persistent! acc)
          (recur  (conj! acc (* (nth xs idx) (nth ys idx)))
                  (unchecked-inc idx))))))

(defn sum* [^doubles xs] (areduce xs idx acc 0.0 (+ acc (aget xs idx))))
(defn square* [^double x] (* x x))
(defn sum-squares* [^doubles xs] 
  (areduce xs idx acc 0.0 (+ acc (square* (aget xs idx)))))
(defn ^doubles dot* [^doubles v1 ^doubles v2]
  (amap v1 idx ^doubles acc 
        (aset acc idx (* (double (aget v1 idx)) (double (aget v2 idx))))))

;;probably core functionality....
;;Note! segaran injects 1-r at the end! 
(defn pearson
  "Pearson's correlation coeffecient, probably."
  [v1 v2]
  (let  [n           (count v1)
         sum1        (sum v1)
         sum2        (sum v2)
         ss1         (sum-squares v1)
         ss2         (sum-squares v2)
         psum        (sum (dot v1 v2))
         numer       (- psum (/ (* sum1 sum2) n))
         a           (- ss1 (/ (square sum1) n))
         b           (- ss2 (/ (square sum2) n))
         denom       (java.lang.Math/sqrt 
                       (* a b))]
    (if (== 0.0 denom) 0.0
        (/ numer denom))))

;;Faster version of pearson that uses primitive arrays 
(defn pearson* [^doubles v1 ^doubles v2]
    (let  [n     (alength v1)
           sum1  (sum* v1) 
           sum2  (sum* v2) 
           ss1   (sum-squares* v1) 
           ss2   (sum-squares* v2) 
           pdot  (dot* v1 v2)
           psum  (sum* pdot)
           s1    (* sum1  sum2)
           numer   (- psum (/  s1   n))
           denom  (java.lang.Math/sqrt 
                    (* (- ss1 (/ (square* sum1) n))
                       (- ss2 (/ (square* sum2) n))))]
    (if (== 0.0 denom) 0.0
        (/ numer denom))))

;;distance is closer = more correlation, hence the - 1 .
(defn max-correlation  [v1 v2] (- 1.0 (pearson v1 v2)))
(defn ^double max-correlation* [^doubles v1 ^doubles v2] (- 1.0 (pearson v1 v2)))

(defn euclidean
  "Computes the euclidean distance between a pair of vectors.  vector based."
  [v1 v2]
   (loop [idx 0
         acc 0.0]
     (if (= idx (count v1)) (Math/sqrt acc)
       (recur (unchecked-inc idx)
              (+ acc (square (- (nth v1 idx) (nth v2 idx))))))))

(defn ^double euclidean*
  "Computes the euclidean distance between a pair of vectors.  Array-based."
  [^doubles v1 ^doubles v2]
   (loop [idx 0
          acc 0.0]
     (if (= idx (alength v1)) (Math/sqrt acc)
       (recur (unchecked-inc idx)
              (+ acc (square* (- (aget v1 idx) (aget v2 idx))))))))

;;compute the element-wise average of two vectors.  This is a bit slow.
(defn avg-vec [xs ys] (vec (map (fn [x y] (/ (+  x y) 2.0)) xs ys)))
(defn ^doubles avg-vec* [^doubles xs ^doubles ys]
  (amap xs idx acc (* (* (aget xs idx) (aget ys idx)) 0.5)))  

;;General function;
(defn all-pairs [xs]  
  (let [xs (vec xs)  
        l  (count xs)]
    (for [i (range l) 
          j (range (inc i) l)]
      [(nth xs i) (nth xs j)])))

;;There's something more general here....
;;a simple node scheme.
(defrecord bicluster [^doubles vec left right ^double distance id])
(defn leaf? [^bicluster c] 
  (and (empty? (:left c)) (empty? (:right c))))

;;A more efficient algorithm for hierarchical clustering, based on the 
;;assumption that distance does not change.  We use a priority queue to manage
;;the fringe of clusters, and only add edges when new clusters are created, 
;;relative to the remaining clusters.
(defn hierarchical-cluster-any 
  [rows & {:keys [distance]  :or {distance max-correlation}}]
  (let [c-distance (memoize (fn [l r] (distance (:vec l) (:vec r))))
        valid?       (fn [m [l r]] (and (contains? m l) (contains? m r))) 
        distribute   (fn [x ys] (map #(vector x %) ys))
        weight-nodes  (fn [m pair] (c-distance (get m (first pair)) 
                                               (get m (second pair))))
        weighted-pair  (fn [m pair] [pair (weight-nodes m pair)])
        push-pairs     (fn [m fr xs] (reduce conj fr 
                                      (map (partial weighted-pair m) xs)))]   
	  (loop [nextid    -1
	         clust     (into {} (map-indexed 
                               (fn [i v] [i (->bicluster v nil nil 0.0 i)]) rows))
           fringe    (push-pairs clust pq/minq (all-pairs (range (count rows))))]
	    (cond (= (count clust) 1) (first (vals clust))
            ;process the fringe, using our minimum priority queue
            :else 
            (let [[popped-fringe [idx1 idx2]]  
                     (loop [remaining fringe]
                       (if-let [x (first remaining)]            
                         (if (valid? clust x) 
                           [(pop remaining) x]
                           (recur (pop remaining)))
                         (throw (Exception. "Empty Fringe!"))))
                     [l r]   [(get clust idx1) (get clust idx2)]
                  merged-vec  (avg-vec (:vec l) (:vec r))            
                  new-node    (->bicluster merged-vec l r 
                                           (c-distance  l r) nextid)
                  popped-clust   (-> (dissoc clust idx1) (dissoc idx2))
                  new-clust      (assoc popped-clust nextid new-node)]
              (recur (dec nextid)  new-clust 
                     (push-pairs  new-clust popped-fringe 
                                 (distribute nextid (keys popped-clust)))))))))

(defn hierarchical-cluster-numeric
  "A much faster implementation in practice, due to the use of double arrays 
   vs vectors.  Allows users to pass in distance functions that consume arrays."
  [rows & {:keys [distance] :or {distance max-correlation*}}]
  (let [rows (map double-array rows)
        c-distance (memoize (fn ^double c-distance [l r] (distance (:vec l) (:vec r))))
        valid?       (fn [m [l r]] (and (contains? m l) (contains? m r))) 
        distribute   (fn [x ys] (map #(vector x %) ys))
        weight-nodes (fn ^double weight-nodes [m pair] (c-distance (get m (first pair)) 
                                              (get m (second pair))))
        weighted-pair  (fn [m pair] [pair (weight-nodes m pair)])
        push-pairs     (fn [m fr xs] (reduce conj fr 
                                      (map (partial weighted-pair m) xs)))]   
	  (loop [nextid    -1
	         clust     (into {} (map-indexed 
                               (fn [i v] [i (->bicluster v nil nil 0.0 i)]) rows))
           fringe    (push-pairs clust pq/minq (all-pairs (range (count rows))))]
	    (cond (= (count clust) 1) (first (vals clust))
            ;process the fringe, using our minimum priority queue
            :else 
            (let [[popped-fringe [idx1 idx2]]  
                     (loop [remaining fringe]
                       (if-let [x (first remaining)]            
                         (if (valid? clust x) 
                           [(pop remaining) x]
                           (recur (pop remaining)))
                         (throw (Exception. "Empty Fringe!"))))
                     [l r]   [(get clust idx1) (get clust idx2)]
                  merged-vec  (avg-vec* (:vec l) (:vec r))            
                  new-node    (->bicluster merged-vec l r 
                                           (c-distance  l r) nextid)
                  popped-clust   (-> (dissoc clust idx1) (dissoc idx2))
                  new-clust      (assoc popped-clust nextid new-node)]
              (recur (dec nextid)  new-clust 
                     (push-pairs  new-clust popped-fringe 
                                 (distribute nextid (keys popped-clust)))))))))  

;;Primary entry point for hierarchical clustering...

(defn hierarchical-cluster
  "Computes a set of clusters, organized hierarchically, according to the 
   function distance, from a vector of vectors, rows.  If rows are inferred to 
   be numeric, uses a faster array-based representation.  User may supply a 
   custom distance function."
  [rows & {:keys [distance numeric?]
           :or {distance nil
                numeric? (every? number? (first rows))}}]
  (if numeric?
      (hierarchical-cluster-numeric rows 
         :distance (or distance #(- 1.0 (pearson* %1 %2))))
      (hierarchical-cluster-any     rows 
         :distance (or distance #(- 1.0 (pearson %1 %2))))))

(defn cluster->table
  "Converts the cluster into a tabular representation.
   At the end of the day, the cluster is just a database of arcs, the edges of 
   which describe the nearness of the cluster."
  [cluster])

(defn table->cluster 
  "We can define an operation that takes any table of data and performs 
   clustering on it." [tbl query])

(defn print-cluster [clust & {:keys [branch? get-label n] 
                              :or   {branch? (fn [c] (< (:id c) 0)) 
                                     get-label identity 
                                     n 0}}]  
  ;indentation 
  (do (dotimes [i n]
        (print \space))
      (if (branch? clust) 
        (println \-)
;        (println (str \- (:id clust) [(:id (:left clust)) (:id (:right clust))]))
        (println (get-label (:id clust))))
      (when (:left clust) 
        (print-cluster (:left clust) :branch? branch? :get-label get-label :n (inc n)))
      (when (:right clust) 
        (print-cluster (:right clust) :branch? branch? :get-label get-label :n (inc n)))))
        

;;visualization
;;At some point, I will factor this out into generic tree-drawing operations.
;;For now, we port straight from segaran.

(defn branch->shape [clust x y top bottom h1 h2 line-length]
  ;vertical line from this cluster to children  
  [(->line :black x (+ top (/ h1 2)) x (- bottom (/ h2 2)))
  ;horizontal line to the left item
   (->line :black x (+ top (/ h1 2)) (+ x line-length) (+ top (/ h1 2)))
  ;horizontal line to the right item
   (->line :black x (- bottom (/ h2 2)) (+ x line-length) (- bottom (/ h2 2)))])

(defn leaf->shape [x y top bottom h1 h2] 
  )

;;I broke out segaran's code into a draw-branch and draw-leaf function pair.
(defn draw-branch [canvas x y top bottom h1 h2 line-length]
  )

(defn get-height
  "Height of a cluster is a recursive function that determines the number of  
   levels in the tree."
  [c]
  (if (leaf? c) 1
    (+ (get-height (:left c)) 
       (get-height (:right c)))))

(defn error-depth
  "Computes the amount of error in each node, used to inform a scaling factor.
   Another recursive function.  This basically adds the distance in each 
   branch."
  [c]
  (if (leaf? c) 0
      (+ (:distance c) (max (error-depth (:left c)) (error-depth (:right c))))))

;;ABSTRACT TREE DRAWING
;;=====================
;;Note: these are really arbitrary tree-drawing functions....there's a good 
;;abstraction here.  If we replace distance with a uniform depth function, then 
;;it'll draw any binary tree.  Seems like a simple extension to n-ary trees.

;;Since we already have an abstract tree protocol, it would be nice to have 
;;an abstract tree shape, based on the templates implied below.

;;Turn a branch into a list of drawable shapes.  Forms the backing for drawing 
;;trees, i.e. draws the connectivity between nodes.
(defn branch->shape [x left-height right-height top bottom line-length]
  [(->line :black x (+ top (/ left-height 2)) 
                  x (- bottom (/ right-height 2)))
   ;horizontal line to the left item
   (->line :black x (+ top (/ left-height 2)) 
                  (+ x line-length) (+ top (/ left-height 2)))
   ;horizontal line to the right item
   (->line :black x (- bottom (/ right-height 2)) 
                  (+ x line-length) (- bottom (/ right-height 2)))])

;;This was originally a mutable drawing call.  Rather than do that, I'm just 
;;traversing over the cluster, computing a corresponding nested sequence 
;;of shapes for it.  Same difference, but more data-friendly and less 
;;side-effecty.
(defn node->shape 
  [clust x y scaling labels & {:keys [height] :or {height 20}}]
  (when clust ;cluster may be nil
    (if (leaf? clust) ;leaves are just text labels.
      (->plain-text :black (get labels (:id clust)) (+ x 5.0) (+ y 5.0))
      (let [left-height (get-height (:left  clust))
            right-height     (get-height (:right clust))
            top    (/ (- y (+ left-height right-height)) 2.0) ;top of the node
            bottom (/ (+ y (+ left-height right-height)) 2.0) ;bottom of the node
            ;line length, for the line that crosses over 
            line-length (* (:distance clust) scaling)
            child-length (+ x line-length)]
        ;;This is a little bit ugly, but I'll refactor it later.
        ;;These are the actual drawing calls...          
        ;vertical line from this cluster to children
        (reduce conj (branch->shape left-height right-height x 
                                    top bottom line-length) 
          ;;Recursive calls for each branch.
          [(node->shape (:left clust) child-length (+ top (/ left-height 2)) 
                        scaling labels :height height)
           (node->shape (:right clust) child-length (- bottom (/ left-height 2)) 
                        scaling labels :height height)])))))

;;An all-in-one wrapper to draw the clusters to the canvas as a dendrogram.
(defn dendrogram->image 
  [clust labels & {:keys [jpeg height width] :or {jpeg "clusters.jpg" 
                                                  height 20 
                                                  width 1200}}]
  (let [h       (* (get-height clust) height)
        depth   (error-depth clust)
        scaling (/ (- width 150) depth)
        img     (img/make-image width h)
        drawing (canvas/get-graphics img)
        background [(->rectangle :white 0 0 width h)
                    (->line      :black 0 (/ h 2) 10 (/ h 2))]]
    (canvas/draw-shapes img [background
                             (node->shape clust 10 (/ h 2) scaling labels)])))
;;Testing 
(comment 

 ;;Blog data comes in a matrix format, with blogname | word1  | word2 ...
 ;;                                         name1    | freq11 | freq12 ...
 ;;So I just use my table API to read it and dissect it.
 ;;If we didn't have the table lib, we could do it the way segaran does, with 
 ;;a file-scraper.
 
(defn blog-data [] (tbl/tabdelimited->table (core/get-dataset :blog)))
(defn read-blog-data [tbl & {:keys [blog-filter]}]  
  (let [valid?    (set blog-filter)
        tbl       (if blog-filter 
                    (tbl/select :from tbl :where #(valid? (:Blog %)))
                    tbl) 
        blognames (first (tbl/table-columns (tbl/select :fields [:Blog] :from tbl)))
        words     (subvec (tbl/table-fields tbl) 1) ;drop blog from the fields        
        labels    (zipmap (range (count blognames)) blognames)
        data      (tbl/table-rows (tbl/drop-field :Blog tbl))]
   ;Return a map of useful data
   {:names blognames :words words :data  data  :labels labels}))    
(def sample-blogs ["John Battelle's Searchblog"
                   "Search Engine Watch Blog"
                   "Read/WriteWeb"
                   "Official Google Blog"
                   "Search Engine Roundtable"
                   "Google Operating System"
                   "Google Blogoscoped"])
(def small-db (read-blog-data (blog-data) :blog-filter sample-blogs))  
(def db (read-blog-data (blog-data)))
(defn compute-cluster [f & [database]] (f (:data (or database db))))

(def clust (compute-cluster hierarchical-cluster))
(def small-clust (compute-cluster hierarchical-cluster small-db))

(defn display-clusters [the-cluster & [database]]
  (let [labels (:labels (or database db))]
    (print-cluster the-cluster :get-label labels)))
(defn display-sample-cluster []
  (display-clusters (compute-cluster hierarchical-cluster small-db) small-db))

)                         

;;Older Performance tweaks
(comment 
;;rows are vectors of numbers.
;;Note -> I changed his vector/array-based version to a map-based version.
;;It's more efficient.  Even still, this could be quite a bit faster.  If we 
;;used primitive arithmetic, and dropped seq usage.  There's room for 
;;optimization.
(defn hierarchical-cluster [rows & {:keys [distance] 
                                    :or {distance #(- 1.0 (pearson %1 %2))}}]
  (let [get-distance     (memoize (fn [l r] (distance (:vec l) (:vec r))))]
	  (loop [distances {} ;cache of distance calcs
	         nextid    -1
	         clust (into {} (map-indexed 
	                          (fn [i v] [i (->bicluster v nil nil 0.0 i)]) rows))]   
	    (if (= (count clust) 1) (first (vals clust))      
	          ;loop through all pairs looking for smallest distance.
	          (let [get-node          (fn [k] (get clust k))                
	                cluster-idx-pairs (all-pairs (keys clust))
	                lowest-pair0      (first cluster-idx-pairs)
	                closest0          (apply get-distance (map get-node lowest-pair0))
	                [closest lowest dists] 
	                (reduce (fn [[closest lowest-pair dists] pair]
	                          (let [[l r] (map get-node pair)
	                                dists (if (contains? dists pair) dists
	                                        (assoc dists pair (get-distance l r)))
	                                d     (get dists pair (get-distance l r))]
	                            (if (< d closest) 
	                              [d pair dists]
	                              [closest lowest-pair dists])))  
	                        [closest0 lowest-pair0 distances] (rest cluster-idx-pairs))                         
	                [l r]      (map get-node lowest)
	                merged-vec  (avg-vec (:vec l) (:vec r))            
	                new-cluster (->bicluster merged-vec l r closest nextid)]
	            (recur dists (dec nextid) (-> (dissoc clust (first lowest))
	                                          (dissoc (second lowest))
	                                          (assoc nextid new-cluster))))))))
(defn hierarchical-cluster* [rows & {:keys [distance] 
                                    :or {distance #(- 1.0 (pearson* %1 %2))}}]
  (let [rows       (map double-array rows)
        get-distance     (memoize (fn [l r] (distance (:vec l) (:vec r))))]
	  (loop [distances {} ;cache of distance calcs
	         nextid    -1
	         clust (into {} (map-indexed 
	                          (fn [i v] [i (->bicluster v nil nil 0.0 i)]) rows))]   
	    (if (= (count clust) 1) (first (vals clust))      
	          ;loop through all pairs looking for smallest distance.
	          (let [get-node          (fn [k] (get clust k))                
	                cluster-idx-pairs (all-pairs (keys clust))
	                lowest-pair0      (first cluster-idx-pairs)
	                closest0          (apply get-distance (map get-node lowest-pair0))
	                [closest lowest dists] 
	                (reduce (fn [[closest lowest-pair dists] pair]
	                          (let [[l r] (map get-node pair)
	                                dists (if (contains? dists pair) dists
	                                        (assoc dists pair (get-distance l r)))
	                                d     (get dists pair (get-distance l r))]
	                            (if (< d closest) 
	                              [d pair dists]
	                              [closest lowest-pair dists])))  
	                        [closest0 lowest-pair0 distances] (rest cluster-idx-pairs))                         
	                [l r]      (map get-node lowest)
	                merged-vec  (avg-vec* (:vec l) (:vec r))            
	                new-cluster (->bicluster merged-vec l r closest nextid)]
	            (recur dists (dec nextid) (-> (dissoc clust (first lowest))
	                                          (dissoc (second lowest))
	                                          (assoc nextid new-cluster))))))))
(defn h-cluster [] (compute-cluster hierarchical-cluster))
(defn h-cluster* [] (compute-cluster hierarchical-cluster*))
(defn hf-cluster [] (compute-cluster faster-hierarchical-cluster))
(defn rapid-cluster [] (compute-cluster fastest-hierarchical-cluster))

)
                   
;;Research (failed research) into zippers.  Note quite there.
(comment 
  
(defrecord btree [data left right])
(defn ^btree merge-btree [^btree l ^btree r]  (->btree nil l r ))
(defn get-bchildren [^btree c] (seq (remove nil? [(:left c) (:right c)])))
(defn ^btree append-bchild [^btree c child]
  (let [cs (get-children c)
        k  (count cs)]
    (case k 
      0 (assoc c :left child)  
      1 (assoc c :right child)
      2 (merge-btree c child))))

(extend-protocol zip/IZippable 
  btree   
  (-branch?       [x] (fn [_] true))
  (-get-children  [x] get-bchildren)
  (-append-child  [x] append-bchild))  
(def bsample (->btree 1
                      (->btree 2 (->btree 3 nil nil)
                                 (->btree 4 nil nil))
                      (->btree 5 nil nil)))
(def b (->btree 2 (->btree 3 nil nil)
                  (->btree 4 nil nil)))

;;bicluster zippable implementation.  ugh.
(def ^doubles empty-doubles  (double-array []))
(defn ^bicluster merge-cluster [^bicluster l ^bicluster r]
  (let [[vl vr] (map ^doubles (fn [m] (get m :vec empty-doubles)) [l r])]
    (->bicluster (avg-vec* vl vr) l r 0.0 (keyword (gensym "branch")))))
(defn get-children [^bicluster c]
    (seq (remove nil? [(:left c) (:right c)])))
(defn ^bicluster append-child [^bicluster c child]
  (let [cs (get-children c)
        k  (count cs)]
    (case k 
      0 (assoc c :left child)  
      1 (assoc c :right child)
      2 (merge-cluster c child))))

(defn ^bicluster append-children [^bicluster c xs] (reduce append-child c xs))  
(defn c-zipper [^bicluster root]
  (zip/zipper (fn [n] true) get-children append-children root))

(extend-protocol zip/IZippable 
  bicluster   
  (-branch?       [x] (fn [_] true))
  (-get-children  [x] get-children)
  (-append-child  [x] append-child))  

(defn walk-cluster [c] 
  (loop [z (zip/as-zipper c)]
    (if (zip/end? z) nil
      (if-let [nd (zip/node z)]
           (let [lid (:id (:left nd))
                 rid (:id (:right nd))
                 _   (println [(:id nd) lid rid])]
             (recur (zip/next z)))
          (do (println :bottom)
              nil)))))
;;biclusters now support generic zipping.
(defn relabel-cluster
  "Change the labels in a cluster to the values matching label-map.
   Leaves non-matches alone."
  [c label-map]
  (let [relabel (fn [k] (get label-map k k))]
	  (zip/map-zipper #(update-in % [:id] relabel) c)))     

(defn summarize-cluster
  "Useful for printing detailed information, drops the potentially large 
   vector weights from the clusters."
  [c]
  (zip/map-zipper #(dissoc % :vec) c))

)
          
        
;;testing, troubleshooting.  turns out python is jacked.
(comment 

;;my own simple samples..having trouble reproducing toby's stuff exactly.
(def sample-labels {0 "A" 1 "B" 2 "C" 3 "D" 4 "E"})
(def rows [[1 2 3] 
           [1 2 3]
           [1 2 5]
           [1 2 6]
           [2 3 9]])
(def rows2 [[1 2 3] ;[-1 [A, B]] [1 2 3] [1 2 3]
           [1 2 5]
           [1 2 6]
           [2 3 9]])
;;we're identical to this point
(def rows3 [[1 2 3]   ;[-1 [A, B]]
            [1 2 5.5] ;[-2 [C, D]] [1 2 5] [1 2 6]
            [2 3 9]]) ;E)
;;toby's algo chooses to merge E and -1 (a,b) into -3, where mine chooses 
;;to merge E with -2 (c,d) into -3.  Why?
;;logic dictates that the pairs with shortest distances should be merged...so..
(def rows4 [[1 2 3] ;[-1 [A, B]]
            [1.5 2.5 7.25];[-3 [-2, E]  [1 2 5.5] [2 3 9] ;[-2 [C,D]] [1 2 5] [1 2 6]
             ])
(def rows5 [1.25 2.25 5.125]) ;-4 [-1, -3]

(defn mindist [xs]
  (first (sort-by #(apply max-correlation %)))) 
    
)
        
             
