(ns spork.graphics2d.stroke
  (:require [spork.graphics2d.canvas :as canvas])
  (:import [java.awt Stroke BasicStroke]))

(set! *warn-on-reflection* true)

(defn ^float stroke-width  [^BasicStroke strk]
  (.getLineWidth strk))

(defn stroke? [x] (instance? Stroke x))

(defn ^BasicStroke widen [^double amount ^BasicStroke strk]
  (BasicStroke. (float (* ^float (stroke-width strk) amount))
                (.getEndCap strk)
                (.getLineJoin strk)
                (.getMiterLimit strk)
                (.getDashArray strk)
                (.getDashPhase strk)))

(defn ^BasicStroke stroke-of-width [width ^BasicStroke strk]
  (BasicStroke. (float width)
                (.getEndCap strk)
                (.getLineJoin strk)
                (.getMiterLimit strk)
                (.getDashArray strk)
                (.getDashPhase strk)))
(defn stroke-by [{:keys [stroke width]}]
  (cond (and stroke width) (fn [s] (stroke-of-width width stroke))
        width (fn [s] (stroke-of-width width s))
        :else (fn [s] s)))

(set! *warn-on-reflection* false)
