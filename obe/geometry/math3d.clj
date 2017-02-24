;;A library for math operations performed in 3dimensional real space, 
;;using homogeneous coordinates.
(ns spork.geometry.math3d
  (:require [spork.util.vectors :refer :all]
            [spork.util [vecmath :as math]]))


(defprotocol IMatrix3D)

(defprotocol I3D  (transform-3d [obj trans]))
;;patterned off of a xna-style math operations.


(defprotocol IMatrixOps
  (back    [x])
  (up      [x])
  (right   [x])
  (forward [x])
  (left    [x])
  (down    [x]))

(defrecord matrix4 [^vec4 x ^vec4 y ^vec4 z ^vec4 w]
  IMatrix3D
  (back  [x])
  (up    [x])
  (right [x])
  (front [x])
  (down  [x])
  (left  [x])
  (back  [x])
  (get-row [x n] (case row 0 x 1 y 2 z 3 w))
  (set-row [x v] (assoc matrix4 (case row 0 :x 1 :y 2 :z 3 :w) v))
  (get-col [x col])
  (set-col [x col])
  (transpose [x]))

(def id (->matrix4   (->vec4 1 0 0 0)
                     (->vec4 0 1 0 0)
                     (->vec4 0 0 1 0)
                     (->vec4 0 0 0 1)))
  
(defn reflect [v axis]
  (let [x (vec-nth v 0)
        y (vec-nth v 1)
        z (vec-nth v 2)]
    (case axis 
      :x (-> v (set-vec 1 (- y)) (set-vec 2 (- z)))
      :y (set-vec v 1 (- y)))
      :z (set-vec v 2 (- z))))

;;if we have homogenous coordinates, our basis changes.
;an orthonormal set of basis vectors.
(def x-axis (->vec4 1.0 0.0 0.0 0.0))
(def y-axis (->vec4 0.0 1.0 0.0 0.0))
(def z-axis (->vec4 0.0 0.0 1.0 0.0))
(def w-axis (->vec4 0.0 0.0 0.0 1.0))
