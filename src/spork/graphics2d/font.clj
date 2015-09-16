;;A namespace for wrapping font metrics and other fun operations.
;;This arose due to the need to establish bounding boxes for
;;strings for labeling purposes.  Ugh.  Currently  a bit 
;;too dependent on Java's font class. Might wrap later.
(ns spork.graphics2d.font
 (:require [spork.protocols [spatial :as s]])
 (:import  [java.awt Font FontMetrics Graphics2D]
           [java.awt.image BufferedImage ImageObserver]
           [java.awt.geom  Rectangle2D]))

;;this is what we use by default.
(def default-font (Font. Font/DIALOG Font/PLAIN 12))
;;it'd be nice to import all the fonts...
(def ^Graphics2D font-graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_RGB)))
(def fonts (atom {:default default-font}))

(defn get-font [f] (if (instance? java.awt.Font f) f (get @fonts f)))

(defn  string-bounds 
  ([f ^String txt x y]   
     (let [^Font fnt (get-font f)
           ^FontMetrics fm (.getFontMetrics font-graphics fnt)
           ^Rectangle2D bv (.getStringBounds fm txt font-graphics)]
       (s/bbox x y (.getWidth bv) (- (.getY bv));(.getHeight bv)
               )))
  ([f ^String txt]   
     (let [^Font fnt (get-font f)
           ^FontMetrics fm (.getFontMetrics font-graphics fnt)
           ^Rectangle2D bv (.getStringBounds fm txt font-graphics)]
       (s/bbox (.getX bv) (.getY bv) (.getWidth bv) (.getHeight bv))))
  ([^String txt] (string-bounds default-font txt)))

(defn string-height
  ([f ^String txt]  
   (let [^Font fnt (get-font f)
         ^FontMetrics fm (.getFontMetrics font-graphics fnt)
         ^Rectangle2D bv (.getStringBounds fm txt font-graphics)]
       (.getHeight bv)))
  ([^String txt] (string-height default-font txt)))

(defn string-width
  ([f ^String txt]  
   (let [^Font fnt (get-font f)
         ^FontMetrics fm (.getFontMetrics font-graphics fnt)
         ^Rectangle2D bv (.getStringBounds fm txt font-graphics)]
       (.getWidth bv)))
  ([^String txt] (string-width default-font txt)))


(defn ^Font resize-font [^Font f ^double newsize]
  (.deriveFont f newsize))
;;lame.
(defn ^Font ->font [name style size] (Font.  name style  size))



;; (defn ^Font alter-font   [^Font f {:keys [styles size]
;;   (.deriveFont f newsize))
