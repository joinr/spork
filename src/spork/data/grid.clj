;;Grids come up a lot in gaph algorithms, game development, and data structures.
;;You have some abstract notion of a set of containers, indexed by a coordinate,
;;that are fully connected to surrounding containers.
(ns spork.data.grid
  (:require [spork.protocols.core :refer :all]
            [spork.util [vectors :as v]
                        [vecmath :as vmath]]))
(defn set-grid 
  [m k & [v]]
  (if (contains? m k) m (assoc m k v)))

(defn shift [coord idx offset]
  (v/set-vec coord idx (long (+ offset (v/vec-nth coord idx)))))

(defn corner-nebs 
  [x-idx y-idx up right down left]  
  [(shift up x-idx 1) ;up-right
   (shift up x-idx -1) ;up-left 
   (shift down x-idx 1) ;down-right
   (shift down x-idx -1) ;down-left
   ])

(defn nebs2 
  [coord & {:keys [x-idx y-idx omni?] 
            :or {x-idx 0 y-idx 1 omni? true}}]
  (let [x (long (v/vec-nth coord x-idx))
        y (long (v/vec-nth coord y-idx))
        up    (v/set-vec coord y-idx (inc y))
        right (v/set-vec coord x-idx (inc x))
        down  (v/set-vec coord y-idx (dec y))
        left  (v/set-vec coord x-idx (dec x))]
    (into [up right down left]
          (when omni? 
              (corner-nebs x-idx y-idx up right down left)))))

(defn nebs [coord & {:keys [omni?] :or {omni? true}}]
  (let [bound (v/dimension coord)]
    (if (= bound 2)
        (nebs2 coord :x-idx 0 :y-idx 1 :omni? omni?)
        (let [base-nebs (nebs2 coord :x-idx 0 :y-idx 1 :omni? omni?)] 
          (loop [dim 2 
                 acc base-nebs]
            (if (= dim bound)   acc
              ;;grow the next dimension by expanding our accumulated 
              ;;dimension
              (recur (unchecked-inc dim)
                     (into acc
                           (concat (map #(shift % dim 1) acc)                    
                                   (map #(shift % dim -1) acc))))))))))

(defn in-bounds2d [width height & {:keys [left bottom] 
                                   :or {left 0 bottom 0}}]
  (let [right (+ left width)
        top   (+ bottom height)]
    (fn [coord]
      (let [x (v/vec-nth coord 0)
            y (v/vec-nth coord 1)]
        (and (and (>= x left) (<= x right))
             (and (>= y bottom) (<= y top)))))))
    
(defn neighbors 
  ([coord] (reduce conj #{} (nebs coord)))
  ([f coord] (reduce conj #{} (filter f (nebs coord)))))

(defn in-bounds3d 
  [width height depth 
   & {:keys [left bottom distance]  :or {left 0 bottom 0 distance 0}}]
  (let [right (+ left width)
        top   (+ bottom height)
        near  (+ depth distance)]
    (fn [coord]
      (let [x (v/vec-nth coord 0)
            y (v/vec-nth coord 1)
            z (v/vec-nth coord 2)]
        (and (and (>= x left) (<= x right))
             (and (>= y bottom) (<= y top))
             (and (>= z distance) (<= z near)))))))


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
  (-arc-weight [tg source sink] (when (-has-arc? tg source sink) 1))
  (-get-sources [tg k]   
    (->> (coord->neighbors k)
      (filter (fn [v] (not (contains? source-drops v))))))
  (-get-sinks [tg k]  
    (->> (coord->neighbors k)
      (filter (fn [v] (not (contains? sink-drops v)))))))

(defn ->grid2d [width height]         
  (sparse-grid. {} #{} #{} 
    (partial neighbors (in-bounds2d width height)) 2))
(defn ->grid3d [width height depth]   
  (sparse-grid. {} #{} #{} 
    (partial neighbors (in-bounds3d width height depth)) 3))
;;Note -> we could probably put in handy constructors for torroidal grids, since 
;;it's only a modification of the coord->neighbors input functions.


;;testing 
(comment 
(def the-grid (->grid2d 10 10))
)
