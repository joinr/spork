;;An implementation of segmented paths and primitive verticee.  Used as a 
;;primitive facility for defining shapes and constructive geometry.
;;Work in progress, should free us from J2d entirely...
(ns spork.graphics2d.primitive
  (:require [spork.util [vectors :as v]])
  (:import  [spork.util.vectors vec2 vec3]))

(defn ->point [^double x ^double y] (v/->vec2 x y))
(defn ^double point-x [^vec2 v] (:x0 v))
(defn ^double point-y [^vec2 v] (:x1 v))

(def origin (->point 0.0 0.0))

;;adapted from j2d, not sure i'll need it though.
(defn segment-type [^long id] 
  (case id
    0 :close
    1 :cubic
    2 :line
    3 :quad
    4 :move 
    5 :even
    6 :nonzero))

(defrecord seg [^long id ^vec2 p1 ^vec2 p2  ^vec2 p3])
(defn line-to  [^double x ^double y] 
  (->segment 2 (->point x y) origin origin))
(defn curve-to [^double x ^double y ^double x2 ^double y2 ^double x3 ^double y3] 
  (->segment 1 (->point x y) (->point x2 y2) (->point x3 y3)))
(defn quad-to  [^double x ^double y ^double x2 ^double y2] 
  (->segment 3 (->point x y) (->point x2 y2) origin))

;;a path is defined as a sequence of segments. 
;;so...anything can be a path if it can provide a sequence of 
;;segments...

(defprotocol ISegmented
  (segment-path [s]   "Return a sequence of segments that define the shape."))

