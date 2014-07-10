;;A namespace for wrapping font metrics and other fun operations.
;;This arose due to the need to establish bounding boxes for
;;strings for labeling purposes.  Ugh.
(ns spork.graphics2d.font
 (:import  [java.awt Font FontMetrics Graphics2D]
           [java.awt.image BufferedImage ImageObserver]
           [java.awt.geom  Rectangle2D]))

;;this is what we use by default.
(def default-font (Font. Font/DIALOG Font/PLAIN 12))
;;it'd be nice to import all the fonts...

(def font-graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_RGB)))
(defn ^Rectangle2D string-bounds 
  ([^Font f ^String txt]  
     (let [^FontMetrics fm (.getFontMetrics font-graphics f)]
       (.getStringBounds fm txt font-graphics)))
  ([^String txt] (string-bounds default-font txt)))
