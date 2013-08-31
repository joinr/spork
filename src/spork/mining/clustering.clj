;;Various forms of clustering analysis, and vizualizations.
;;Adapted from Toby Segaran's excellent "Programming Collective Intelligence" 
;;book.
(ns spork.mining.clustering
  (:require [spork.mining [core :as core]]
            [spork.util   [table :as tbl]]
            [spork.data   [priorityq :as pq]]
            [spork.cljgui.components [swing :as gui]]))

(defn sum [xs]   (loop [acc 0.0
                        idx 0]
                   (if (= idx (count xs)) acc
                     (recur (+ acc (nth xs idx)) (unchecked-inc idx)))))
(defn square [x] (* x x))
(defn sum-squares [xs] 
  (loop [acc 0.0
         remaining xs]
    (if (empty? remaining) acc
      (recur (+ acc (square (first xs)))
             (rest remaining)))))
(defn dot [xs ys]
  (let [b (count xs)]
    (loop [acc (transient [])
           idx 0]
      (if (== idx b) (persistent! acc)
          (recur  (conj! acc (* (nth xs idx) (nth ys idx)))
                  (unchecked-inc idx))))))
         
         

(defn  sum* [^doubles xs] 
  (areduce xs idx acc 0.0 (+ acc (aget xs idx))))
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
         denom       (java.lang.Math/sqrt 
                       (*  (- ss1 (/ (square sum1) n))
                           (- ss2 (/ (square sum2) n))))]
    (if (== 0.0 denom) 0.0
        (/ numer denom))))

(defn pearson* [^doubles v1 ^doubles v2]
    (let  [n     (alength v1)
           sum1  (sum* v1) ;(areduce v1 idx acc 0.0 (+ acc (aget v1 idx)))
           sum2  (sum* v2) ;(areduce v2 idx acc 0.0 (+ acc (aget v2 idx)))
           ss1   (sum-squares* v1) ;(areduce v1 idx acc 0.0 (+ acc (square* (aget v1 idx))))
           ss2   (sum-squares* v2) ;(areduce v2 idx acc 0.0 (+ acc (square* (aget v2 idx))))
           pdot  (dot* v1 v2)
           psum  (sum* pdot);(areduce pdot idx acc 0.0 (+ acc (aget pdot idx)))
           s1    (* sum1  sum2)
           numer   (- psum (/  s1   n))
           denom  (java.lang.Math/sqrt 
                    (* (- ss1 (/ (square* sum1) n))
                       (- ss2 (/ (square* sum2) n))))]
    (if (== 0.0 denom) 0.0
        (/ numer denom))))

;;compute the element-wise average of two vectors.  This is a bit slow.
(defn avg-vec [xs ys] (vec (map (fn [x y] (/ (+  x y) 2.0)) xs ys)))
(defn ^doubles avg-vec* [^doubles xs ^doubles ys]
  (amap xs idx acc (* (* (aget xs idx) (aget ys idx)) 0.5)))  


;;a simple node scheme.
(defrecord bicluster [^doubles vec left right ^double distance id])
(defn all-pairs [xs]
  (let [xs (vec xs)
        l  (count xs)]
    (for [i (range l)
          j (range (inc i) l)]
      [(nth xs i) (nth xs j)])))


;;A more efficient algorithm for hierarchical clustering, based on the 
;;assumption that distance does not change.  We use a priority queue to manage
;;the fringe of clusters, and only add edges when new clusters are created, 
;;relative to the remaining clusters.
(defn hierarchical-cluster-any 
  [rows & {:keys [distance]  :or {distance #(- 1.0 (pearson %1 %2))}}]
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
  [rows & {:keys [distance] :or {distance #(- 1.0 (pearson* %1 %2))}}]
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

(defn print-cluster [clust & {:keys [branch? get-label n] 
                              :or   {branch? (fn [c] (< (:id c) 0)) 
                                     get-label identity 
                                     n 0}}]  
  ;indentation 
  (do (dotimes [i n]
        (print \space))
      (if (branch? clust) 
        (println \-)
        (println (get-label (:id clust))))
      (when (:left clust) 
        (print-cluster (:left clust) :branch? branch? :get-label get-label :n (inc n)))
      (when (:right clust) 
        (print-cluster (:right clust) :branch? branch? :get-label get-label :n (inc n)))))

        
;;testing 
(comment 
 ;;Blog data comes in a matrix format, with blogname | word1  | word2 ...
 ;;                                         name1    | freq11 | freq12 ...
 ;;So I just use my table API to read it and dissect it.
 ;;If we didn't have the table lib, we could do it the way segaran does, with 
 ;;a file-scraper.
(defn blog-data [] (tbl/tabdelimited->table (core/get-dataset :blog)))
(defn read-blog-data [tbl]
  (let [blognames (tbl/table-rows (tbl/select :fields [:Blog] :from tbl))
        words     (subvec (tbl/table-fields tbl) 1) ;drop blog from the fields        
        lookup    (fn [id] (nth blognames id))
        data      (tbl/table-rows (tbl/drop-field :Blog tbl))]
   ;Return a map of useful data
   {:names blognames :words words :data  data  :lookup lookup}))    

(def db (read-blog-data (blog-data)))
(defn compute-cluster [f]   (f (:data db)))

(defn display-clusters [& [limit]]
  (let [db (read-blog-data (blog-data))
        the-cluster (hierarchical-cluster
                      (#(if limit (take limit %) %) (:data db))
                      :distance pearson*)]
   (print-cluster the-cluster :get-label (:lookup db))))

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
                   
                  
          
        
        
             
