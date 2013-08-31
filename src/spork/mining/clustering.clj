;;Various forms of clustering analysis, and vizualizations.
;;Adapted from Toby Segaran's excellent "Programming Collective Intelligence" 
;;book.
(ns spork.mining.clustering
  (:require [spork.mining [core :as core]]
            [spork.util   [table :as tbl]]
            [spork.cljgui.components [swing :as gui]]))

(defn sum [xs] (reduce + xs))
(defn square [x] (* x x))
(defn sum-squares [xs] (sum (map square xs)))

;;probably core functionality....
(defn pearson
  "Pearson's correlation coeffecient, probably."
  [v1 v2]
  (let  [n           (count v1)
         [sum1 sum2] (map #(reduce + %) [v1 v2])
         [ss1 ss2]   (map sum-squares   [v1 v2])
         psum        (sum (map * v1 v2))
         numer       (- psum (/ (* sum1 sum2) n))
         denom       (java.lang.Math/sqrt 
                       (* (- ss1 (/ (square sum1) n))
                          (- ss2 (/ (square sum2) n))))]
    (if (== 0.0 denom) 0.0
        (dec (/ numer denom)))))

;;compute the element-wise average of two vectors.  This is a bit slow.
(defn avg-vec [xs ys] (vec (map (fn [x y] (/ (+  x y) 2.0)) xs ys))) 

;;a simple node scheme.
(defrecord bicluster [vec left right distance id])
;;rows are vectors of numbers.
(defn hierarchical-cluster [rows & {:keys [distance] :or {distance pearson}}]
  (loop [distances {} ;cache of distance calcs
         nextid    -1
         clust (vec (map-indexed 
                      (fn [i v] (->bicluster v nil nil 0.0 i)) rows))]   
    (if (= (count clust) 1) (first clust)
      ;loop through all pairs looking for smallest distance.
      (let [get-node          (fn [k] (nth clust k))
            get-distance      (fn [l r] (distance (:vec l) (:vec r)))
            cluster-idx-pairs (partition 2 1 (range (count clust)))
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
                          [closest0 lowest-pair0 distances] cluster-idx-pairs)                         
            [l r]      (map get-node lowest)
            merged-vec  (avg-vec (:vec l) (:vec r))            
            new-cluster (->bicluster merged-vec l r closest nextid)]
        (recur dists (dec nextid) (conj (subvec clust 2) new-cluster))))))

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

(defn display-clusters [& [limit]]
  (let [db (read-blog-data (blog-data))
        the-cluster (hierarchical-cluster
                      (#(if limit (take limit %) %) (:data db)))]
    (print-cluster the-cluster :get-label (:lookup db))))

)                         

                   
                  
          
        
        
             
