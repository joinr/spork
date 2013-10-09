(ns spork.geometry.math2d
    (:require [spork.util.vectors :refer :all]))

(defprotocol I2D  (transform-2d [obj trans]))
(defprotocol IMatrix2D
  (back    [x])
  (up      [x])
  (right   [x])
  (forward [x])
  (left    [x])
  (down    [x]))

(defrecord matrix3 [^vec3 x ^vec3 y ^vec3 z]
  IMatrix2D
  (back  [x])
  (up    [x])
  (right [x])
  (front [x])
  (down  [x])
  (left  [x])
  (back  [x]))

(def id (->matrix3   (->vec4 1 0 0 0)
                     (->vec4 0 1 0 0)
                     (->vec4 0 0 1 0)))


;;if we have homogenous coordinates, our basis changes.
;an orthonormal set of basis vectors.
(def x-axis (->vec3 1.0 0.0 0.0 0.0))
(def y-axis (->vec3 0.0 1.0 0.0 0.0))
(def z-axis (->vec3 0.0 0.0 1.0 0.0))
(def w-axis (->vec4 0.0 0.0 0.0 1.0))