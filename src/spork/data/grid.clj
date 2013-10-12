(ns spork.data.grid
  (:require [spork.protocols.core :refer :all]))
(defn set-grid 
  [m k & [v]]
  (if (contains? m k) m (assoc m k v)))

;;This is way inefficient.  We want a numerical indexed way to do 
;;this, not using vector pairs.

(defn up2 [x y] [x (inc y)])
(defn up-right2 [x y] [(inc x) (inc y)])
(defn right2 [x y] [(inc x) y])
(defn down-right2 [x y] [(inc x) (dec y)])
(defn down2 [x y] [x (dec y)])
(defn down-left2 [x y] [(dec x) (dec y)])
(defn left2 [x y] [(dec x) y])
(defn up-left2 [x y] [(dec x) (inc y)])

(defn omni2 [[x y]] 
  (map (fn [f] (f x y))  
       [up2 up-right2 right2 down-right2 down2 down-left2 left2 up-left2]))

(defn manhattan2 [[x y]] 
  (map (fn [f] (f x y)) [up2 right2 down2 left2]))

(defn in-bounds2d [width height]
  (fn [[x y]]
    (and (and (>= x 0) (<= x width))
         (and (>= y 0) (<= y height)))))

(defn neighbors2 
  ([coord] (reduce conj #{} (omni2 coord)))
  ([f coord] (reduce conj #{} (filter f (omni2 coord)))))

(defn up3 [x y z] [x (inc y) z])
(defn up-right3 [x y z] [(inc x) (inc y) z])
(defn right3 [x y z] [(inc x) y z])
(defn down-right3 [x y z] [(inc x) (dec y) z])
(defn down3 [x y z] [x (dec y) z])
(defn down-left3 [x y z] [(dec x) (dec y) z])
(defn left3 [x y z] [(dec x) y z])
(defn up-left3 [x y z] [(dec x) (inc y) z])

(defn forward-up3 [x y z] [x (inc y) (inc z)])
(defn forward-up-right3 [x y z] [(inc x) (inc y) (inc z)])
(defn forward-right3 [x y z] [(inc x) y (inc z)])
(defn forward-down-right3 [x y z] [(inc x) (dec y) (inc z)])
(defn forward-down3 [x y z] [x (dec y) (inc z)])
(defn forward-down-left3 [x y z] [(dec x) (dec y) (inc z)])
(defn forward-left3 [x y z] [(dec x) y (inc z)])
(defn forward-up-left3 [x y z] [(dec x) (inc y) (inc z)])

(defn backward-up3 [x y z] [x (inc y) (dec z)])
(defn backward-up-right3 [x y z] [(inc x) (inc y) (dec z)])
(defn backward-right3 [x y z] [(inc x) y (dec z)])
(defn backward-down-right3 [x y z] [(inc x) (dec y) (dec z)])
(defn backward-down3 [x y z] [x (dec y) (dec z)])
(defn backward-down-left3 [x y z] [(dec x) (dec y) (dec z)])
(defn backward-left3 [x y z] [(dec x) y (dec z)])
(defn backward-up-left3 [x y z] [(dec x) (inc y) (dec z)])


(defn omni3 [[x y z]] 
  (map (fn [f] (f x y z)) (defn up3 [x y z] [x (inc y) z])
       [up3 
        up-right3
        right3 
        down-right3 
        down3 
        down-left3 
        left3 
        up-left3
        forward-up3 
        forward-up-right3
        forward-right3 
        forward-down-right3 
        forward-down3 
        forward-down-left3
        forward-left3 
        forward-up-left3 
        backward-up3 
        backward-up-right3 
        backward-right3 
        backward-down-right3 
        backward-down3 
        backward-down-left3 
        backward-left3 
        backward-up-left3])) 

(defn manhattan3 [[x y z]] 
  (map (fn [f] (f x y z)) (defn up3 [x y z] [x (inc y) z])
       [up3 
        right3 
        down3 
        left3 
        forward-up3 
        forward-right3 
        forward-down3 
        forward-left3 
        backward-up3 
        backward-right3 
        backward-down3 
        backward-left3])) 

(defn neighbors3 
  ([coord] (reduce conj #{} (omni3 coord)))
  ([f coord] (reduce conj #{} (filter f (omni3 coord)))))

(defn in-bounds3d [width height depth]
  (fn [[x y z]]
    (and (and (>= x 0) (<= x width))
         (and (>= y 0) (<= y height))
         (and (>= z 0) (<= z height)))))

;;in the grid, connectedness is implied. 
;;To keep the arc storage down, we compute connectedness, and 
;;retain only arcs that are explicitly blocked/dropped.  This is 
;;the opposite of the digraph implemention.

(deftype sparse-grid [coordinate-map source-drops sink-drops coord->neighbors dimensions]
  IGrid
  (grid-neighbors   [g coord] (coord->neighbors coord))
  (grid-assoc       [g coord v] 
    (sparse-grid. (assoc coordinate-map coord v) 
                  source-drops 
                  sink-drops 
                  coord->neighbors dimensions))
  (grid-dissoc      [g coord] 
    (sparse-grid. (dissoc coordinate-map coord)  
                  source-drops 
                  sink-drops 
                  coord->neighbors  dimensions))
  (grid-coords      [g] coordinate-map)
  (grid-dimensions  [g] dimensions)  
  ITopograph
  (-get-nodes [tg]     coordinate-map)
  (-set-nodes [tg m]   
    (sparse-grid. m source-drops sink-drops  coord->neighbors dimensions))
  (-conj-node [tg k v] (grid-assoc tg k v))
  (-disj-node [tg k]   (grid-dissoc tg k))
  (-has-node? [tg k]   (contains? coordinate-map k))
  (-conj-arc  [tg source sink w] 
    (-> coordinate-map 
      (set-grid source) 
      (set-grid sink)
      (sparse-grid. (disj source-drops source) 
                    (disj sink-drops sink) coord->neighbors dimensions)))                                                
  (-disj-arc  [tg source sink] 
    (sparse-grid. coordinate-map 
                  (conj source-drops source) 
                  (conj sink-drops sink) 
                  coord->neighbors dimensions))
  (-has-arc?  [tg source sink] 
    (and (not (and (contains? source-drops source) 
                   (contains? sink-drops sink))) 
         (contains? (coord->neighbors source) sink)))
  (-get-arc   [tg source sink] 
    (when (-has-arc? tg source sink) [source sink 1]))
  (-get-sources [tg k] 
    (when (not (contains? sink-drops k))
      (->> (coord->neighbors k)
           (filter (fn [v] (not (contains? source-drops v)))))))
  (-get-sinks [tg k]  
    (when (not (contains? source-drops k))
      (->> (coord->neighbors k)
           (filter (fn [v] (not (contains? sink-drops v))))))))

(defn ->grid2d [width height]         
  (sparse-grid. {} #{} #{} (partial neighbors2 (in-bounds2d width height)) 2))
(defn ->grid3d [width height depth]   
  (sparse-grid. {} #{} #{} (partial neighbors3 (in-bounds3d width height depth)) 3))

(def the-grid (->grid2d 10 10))