;;A simple handle for bindings to the underlying spork graphics
;;package.  This is a convenience namespace that pulls in all the
;;low-level dependencies, and provides a user-friendly API for newbs
;;to perform declarative rendering.  I really need to pull in the 
;;full scene graph library for this.  For now, it's being used as 
;;a sort of skeleton scene graph, for simple 2d diagrams and plotting.
(ns spork.sketch
  (:require [spork.graphics2d [canvas :as canvas]
                              [image :as image]
                              [swing :as provider]]
            [spork.geometry.shapes :refer :all]
            [spork.cljgui.components [swing :as gui]]))

(def ^:dynamic *current-sketch* nil)
      
(defn sketch-image [the-shapes]
  (->> the-shapes 
       (image/shape->img )
       (gui/view)))
