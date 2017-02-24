;;TODO->port this to core.matrix or something official.
;;Note: hollowed out, most stuff is in vecmath.
;I've been getting some crazy ideas from computational geometry....namely the
;ability to unify 2d and 3d ops under a single set of rules and operations..
;It's pretty cool...In any case, this is currently a library based on the 
;canonical matrix-based linear algebra routines for defining transformations.
(ns spork.geometry.math
  (:require [spork.util.vectors :refer :all]
            [spork.util [vecmath :as math]]))

;nice macros to have...

;with-coordinate-system 
;with-basis [x y z] 
;with-projection 

;if I want to project a vec2 [x,y] onto a vec3, then...
(declare make-matrix)
  
(defprotocol IDimensioned
  (get-dimension  [x])
  (drop-dimension [x])
  (add-dimension  [x]))

(defn make-vector 
  "Creates 2D and 3D vectors."
  ([^double x ^double y] (->vec2 x y))
  ([^double x ^double y ^double z] (->vec3 x y z))
  ([^double x ^double y ^double z ^double w] (->vec4 x y z w)))

(defn ^double negate [^double x] (* -1.0 x))

;;if we have homogenous coordinates, our basis changes.
;an orthonormal set of basis vectors.
(def x-axis (->vec4 1.0 0.0 0.0 0.0))
(def y-axis (->vec4 0.0 1.0 0.0 0.0))
(def z-axis (->vec4 0.0 0.0 1.0 0.0))
(def w-axis (->vec4 0.0 0.0 0.0 1.0))
