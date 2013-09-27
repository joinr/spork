;;TODO -> unify this with the general vector implementation.
;I've been getting some crazy ideas from computational geometry....namely the
;ability to unify 2d and 3d ops under a single set of rules and operations..
;It's pretty cool...In any case, this is currently a library based on the 
;canonical matrix-based linear algebra routines for defining transformations.

(ns spork.geometry.math)

;why not define 2D AND 3D operations? 
(defprotocol I3D  (transform-3d [obj trans]))
(defprotocol I2D  (transform-2d [obj trans]))

;can we blend 2D and 3d? 
;yes...if we have a 2D object, we project it onto 3D coordinate

;nice macros to have...

;with-coordinate-system 
;with-basis [x y z] 
;with-projection 

;with-dimensions ? 

;if I want to project a vec2 [x,y] onto a vec3, then...
(declare make-vector make-matrix)

(defprotocol IDimensioned 
  (drop-dimension [x])
  (add-dimension  [x]))

(defrecord vec2 [^float x ^float y])
(defrecord vec3 [^float x ^float y ^float z])
(defrecord vec4 [^float x ^float y ^float z ^float w])
(defn make-vector 
  "Creates 2D and 3D vectors."
  ([^float x ^float y] (->vec2 x y))
  ([^float x ^float y ^float z] (->vec3 x y z))
  ([^float x ^float y ^float z ^float w] (->vec4 x y z w)))

(defn ^float negate [^float x] (* -1.0 x))

(defprotocol IMatrix
  (^vec3 back [x])
  (^vec3 up [x])
  (^vec3 right [x])
  (^vec3 forward [x])
  (^vec3 left [x])
  (^vec3 down [x]))

;(defn reflect [^vec3 v axis]
;  (let [[x y z] ((juxt :x :y :z) v)]
;    (case axis 
;      :x (make-vector x (negate y) (negate z))
;      :y (make-vector x (negate y) z)
;      :z (make-vector x y (negate z)))))

;we need a 4x4 matrix for homogenous coordinates in 3d.
(defrecord matrix2 [^vec2 x ^vec2 y])
(defrecord matrix3 [^vec3 x ^vec3 y ^vec3 z]
(defrecord matrix4 [^vec4 x ^vec4 y ^vec4 z ^vec4 w]
  IMatrix
  (back  [x])
  (up    [x])
  (right [x])
  (front [x])
  (down  [x])
  (left  [x])
  (back  [x])

(defn make-matrix 
  ([x y]     (->matrix2 x y))
  ([x y z]   (->matrix3 x y z))
  ([x y z w] (->matrix4 x y z w)))

;an orthonormal set of basis vectors. 
(def id (make-matrix (make-vector 1 0 0 0)
                     (make-vector 0 1 0 0)
                     (make-vector 0 0 1 0)
                     (make-vector 0 0 0 1)))

(def rotate-xy [
 
  
  
  