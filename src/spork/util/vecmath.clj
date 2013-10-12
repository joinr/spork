;;Companion to spork.util.vectors .  Provides common math operations for 
;;vectors and scalars.  This is not a linear algebra lib, but a small collection 
;;of useful routines.  On the other hand, it's entirely feasible to use for 
;;2D and 3D graphics (which we do), as well as anywhere else good old vector 
;;math may be useful.
(ns spork.util.vecmath
  (:require [spork.util.vectors :refer :all]))

;;add two equal length vectors together
(defn v+ [v1 v2] (vec-add v1 v2))
;;subtract two equal length vectors
(defn v- [v1 v2] (map-vec v1 v2 -))
;;computes the dot product of v1 and v2
(defn ^double dot [v1 v2] 
  (loop [idx 0 
         acc 0.0]
    (if (= idx (dimension v1)) acc
          (recur (unchecked-inc idx) 
                 (+ acc (* (vec-nth v1 idx) (vec-nth v2 idx)))))))

;;computes the squared normal (length) of v
(defn ^double v-norm-squared [v]    (dot v v))
;;computes the normal (length) of v.
(defn ^double v-norm [v] (Math/sqrt (v-norm-squared v)))
;;scale a vector, i.e. multiply a vector by a scalar
(defn v-scale [^double scalar v]  (map-vec v (fn [^double k] (* k scalar))))
;;convert a vector into a unit vector via normalization.
(defn v-unit [v] (v-scale (/ 1.0 ^double (v-norm v)) v)) 

;;returns the euclidean distance between two vectors
(defn ^double euclidean [v1 v2]
  (loop [idx 0
         acc 0.0]
    (if (= idx (dimension v1)) (Math/sqrt acc)
      (let [delta (- (vec-nth v1 idx) (vec-nth v2 idx))] 
        (recur (unchecked-inc idx)
               (+ acc (* delta delta)))))))

;;returns the manhattan distance between two vectors.
(defn ^double manhattan [v1 v2]
  (loop [idx 0
         acc 0.0]
    (if (= idx (dimension v1)) (Math/sqrt acc)
      (let [delta (- (vec-nth v1 idx) (vec-nth v2 idx))] 
        (recur (unchecked-inc idx)
               (+ acc (* delta delta)))))))

;;3D operations (and 2D pseudo ops)

;;computes the cross product of v1 and v2, where v1 and v2 are 3-dimensional 
;;vectors.  If v1 and v2 are 2dimensional vectors, treats the z coordinate 
;;as 0 (a psuedo vector).

;;computes the cross product of a pair of vectors.
;;components for u X v = ai,bj,ck 
;;a = u2*v3 - u3*v2
;;b = u3*v1 - u1*v3
;;c = u1*v2 - u2*v1
(defn  cross [u v]
  (let [u1 (vec-nth u 0) u2 (vec-nth u 1) u3 (vec-nth u 2)
        v1 (vec-nth v 0) v2 (vec-nth v 1) v3 (vec-nth v 2)]
    (->vec3 (- (* u2 v3)  (* u3 v2))
            (- (* u3 v1)  (* u1 v3))
            (- (* u1 v2)  (* u2 v1)))))
;;compute the pseudovector result of the cross product 
;;of vector u and v.  Treats u and v as if they have no third 
;;component, compatible with vectors of 2 components.
(defn  cross2 [u v]
  (let [u1 (vec-nth u 0) u2 (vec-nth u 1) u3 0.0
        v1 (vec-nth v 0) v2 (vec-nth v 1) v3 0.0]
    (->vec3 0.0 ;(- (* u2 v3)  (* u3 v2))
            0.0 ;(- (* u3 v1)  (* u1 v3))
            (- (* u1 v2) (* u2 v1)))))

;;yield the norm of the psuedovector resulting from a 2d cross product. 
(defn ^double pseudo-cross [u v]
  (let [u1 (vec-nth u 0) u2 (vec-nth u 1)
        v1 (vec-nth v 0) v2 (vec-nth v 1)]
    (- (* u1 v2) (* u2 v1))))

(defn unit-cross  [u v]  (cross  (v-unit u) (v-unit v)))
(defn unit-cross2 [u v]  (cross2 (v-unit u) (v-unit v)))

;;The dot product provides a useful identity that relates the sine of the angle
;;between two vectors.  The dot product generalizes, and is cheap, so we use 
;;it for angle checks.
(defn ^double vector-radians [u v] 
  (Math/acos (/ ^double (dot u v) (*  ^double (v-norm u) ^double (v-norm v)))))
(defn ^double vector-degrees [u v] 
  (* ^double (vector-radians u v)  180.0 (/ 1.0 Math/PI))) 


;;I think the preferred orthogonality check is to use the dot product, since 
;;you just add components.
(defn orthogonal? [u v] (zero? ^double (dot u v)))

;;We use our cross product (could do the same with the dot product) to determine
;;whether vectors are perpendicular or parallel.
(defn perpendicular?   [u v] (= 1.0 ^double (v-norm (unit-cross u v))))
(defn perpendicular-2? [u v] (= 1.0 ^double (v-norm (unit-cross2 u v))))

(defn parallel?        [u v] (= 0.0 ^double (v-norm (unit-cross u v))))
(defn parallel-2?      [u v] (= 0.0 ^double (v-norm (unit-cross2 u v))))

;;Quick test to determine if a candidate point is to the left or right or 
;;center (co-linear) with a line defined by an origin point and a destination
;;point.
(defn orientation-2d [origin dest candidate] 
  (let [r (v- candidate origin)
        l (v- dest origin)
        res ^double (pseudo-cross r l)]
    (cond (pos? res) :right 
          (neg? res) :left        
          :otherwise :center)))

;;maybe keep these guys out...too haskellish for me..
;(def || perpendicular?)
;(def |- parallel?)

;;Protocols for mucking with matrices.
;;These will definitely go if I swap out for a hardcore linear algebra lib,
;;but again, this is for kicks and lightweight practicality.
(defprotocol IMatrix
  (get-entry [x row col])
  (set-entry [x row col n])
  (get-row [x row])
  (set-row [x row v])
  (get-col [x col])
  (set-col [x col v])
  (transpose [x])
  (mat-mult  [x m])
  (inverse   [x]))

(defprotocol IMatrixStack
  (push-matrix [x m])
  (pop-matrix  [x])
  (get-matrix  [x]))

(defrecord matrix-stack [matrix stack]
  IMatrixStack
  (push-matrix [x m]
    (let [new-matrix (if matrix (mat-mult matrix m) m)]  
      (matrix-stack. new-matrix (conj stack new-matrix))))
  (pop-matrix  [x] (if (empty? stack)  x 
                     (matrix-stack. (first stack) (pop stack))))
  (get-matrix  [x] matrix)
  IMatrix
  (get-entry [x row col] (get-entry matrix row col))
  (set-entry [x row col n] (assoc x :matrix (set-entry matrix row col n)))
  (get-row [x row] (get-row matrix row))
  (set-row [x row v] (assoc x :matrix (set-row matrix row v)))
  (get-col [x col] (get-col matrix col))
  (set-col [x col v] (assoc x :matrix (set-col matrix col v)))
  (transpose [x] (let [t (transpose matrix)]
                   (matrix-stack. t (conj stack t))))
  (mat-mult  [x m] (mat-mult matrix m))
  (inverse   [x] (inverse matrix)))

(def empty-matrix-stack (->matrix-stack nil '()))
(defn stack-matrices [ms] (reduce push-matrix empty-matrix-stack ms))
    
;deprecated 
(comment 
  (defn ^double vector-radians [u v] (Math/asin (v-norm (unit-cross u v))))
)