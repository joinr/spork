(ns spork.graphics2d.stroke
  (:require [spork.graphics2d.canvas :as canvas])
  (:import [java.awt Stroke BasicStroke]))

(set! *warn-on-reflection* true)

(defn ^BasicStroke widen [amount ^BasicStroke strk]
  (BasicStroke. (float (* (.getLineWidth strk) amount))
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
(set! *warn-on-reflection* false)
