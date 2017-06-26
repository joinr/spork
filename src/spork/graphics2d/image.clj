;A wrapper around various image formats.  
;I needed to abstract this away, since I'm using buffered images everywhere, 
;particularly for sprites, and I need to use a standard "image" format, 
;especially for rendering sprites and other goodies.
;So we define a simple class that can provide a default implementation for 
;make-bitmap, which "everyone" can use....
(ns spork.graphics2d.image
  (:require [spork.graphics2d.canvas :refer :all] 
            [spork.protocols [spatial :as spatial]]
            [spork.graphics2d.swing.shared :refer
             [null-observer get-transparency +clear+ opaque]]
            [spork.graphics2d.swing :as swing])
  (:import [java.awt GraphicsEnvironment  GraphicsDevice GraphicsConfiguration 
            Transparency Graphics2D AlphaComposite Color]
           [java.awt.image BufferedImage ImageObserver]
           [javax.imageio ImageIO]))

;;duplicated due to weird import statements...
(defn ->canvas-graphics [^Graphics2D g width height]
  (let [g (doto g (.translate 1.0 (double (dec height)))
                  (.scale   1.0 -1.0))]
    (spork.graphics2d.swing.CanvasGraphics. g width height)))

(defn  get-graphics-img [^BufferedImage img]
  (->canvas-graphics (.getGraphics img) (.getWidth img) (.getHeight img))) 

(defn ^BufferedImage make-imgbuffer 
  ([w h ^Transparency t]
	  (let [^GraphicsEnvironment ge 
	          (GraphicsEnvironment/getLocalGraphicsEnvironment)
	        ^GraphicsDevice gd (.getDefaultScreenDevice ge)
	        ^GraphicsConfiguration gc (.getDefaultConfiguration gd)]	  
	    (.createCompatibleImage gc w h t)))
  ([w h] (make-imgbuffer w h Transparency/TRANSLUCENT)))   

(defn save-image
  ([^BufferedImage img filepath postf]
   (let [tgt (clojure.java.io/file filepath)]
     (do 
       (if (not (.exists tgt)) 
         (.mkdirs (.getParentFile tgt)))
       (.createNewFile tgt)                                                                                 
       (ImageIO/write img "png" tgt)
       (postf (str "Buffer saved to:" filepath)))))
  ([img filepath] (save-image img filepath (fn [_] nil))))
  
(defn ^BufferedImage read-buffered-image [filepath]
  (if (string? filepath)
    (let [tgt (clojure.java.io/file filepath)]         
      (assert (.exists tgt)) 
      (ImageIO/read tgt))
    (ImageIO/read filepath)))

(defn clear-region [^Graphics2D g x y width height]
  (let [composite     (.getComposite g)]
        (doto g
          (.setComposite AlphaComposite/Clear)
          (.fillRect x y width height)
          (.setComposite composite))
        g))

(defn ^BufferedImage clear-buffered-image [^BufferedImage img]
  (let [^Graphics2D g (.getGraphics img)
        composite     (.getComposite g)]
    (do (clear-region g 0 0 (.getWidth img) (.getHeight img))
        img)))

(extend-type  BufferedImage
  IBitMap
  (bitmap-width [b]  (.getWidth b))
  (bitmap-height [b] (.getHeight b))
  (bitmap-graphics [b] (get-graphics-img b))
  (bitmap-format [b] :buffered-image)
  (as-buffered-image [b fmt] b)
  (write-image [b dest fmt] (save-image b dest (fn [_] nil)))
  IShape
  (draw-shape    [shp c] (draw-image c shp (.getTransparency shp) 0 0))
  (shape-bounds  [shp] (spork.protocols.spatial/bbox 0 0 (.getWidth shp) (.getHeight shp)))
  IShapeStack
  (push-shape [c shp] (do (draw-shape  shp (get-graphics-img c)) c))
  (pop-shape [c shp] c)
  IBoundedCanvas
  (canvas-width  [c] (.getWidth c))
  (canvas-height [c] (.getHeight c))
  IWipeable
  (wipe [obj] (clear-buffered-image obj)))

;;Images are mutable...It might be interesting to look at persistent images 
;;later (could certainly do it).
(defrecord image [image-format ^BufferedImage data width height transparency]
  IBitMap 
  (bitmap-width    [b]  width)
  (bitmap-height   [b] height)
  (bitmap-graphics [b] (get-graphics-img data))
  (bitmap-format   [b] image-format)
  (as-buffered-image [b fmt] (cond (= image-format  fmt :buffered-image) data 
                                   (= fmt image-format ) 
                                     (as-buffered-image data fmt)
                                     :else (throw (Exception. 
                                           "No other formats implemented."))))
  (write-image [b dest fmt] (save-image data dest (fn [_] nil)))
  IShape 
  (shape-bounds [s]   (spatial/bbox 0 0 width height))
  (draw-shape   [s c] (draw-image c data transparency 0 0))
  I2DGraphicsProvider
  (get-graphics [s] (get-graphics-img data))
  ICanvas2D
  (get-context    [canvas] 
    (get-context data)) 
  (set-context    [ig ctx]  
    (assoc ig :data (set-context data ctx))) 
  (draw-point     [ig color x y w] 
    (do (draw-point (get-graphics-img data) color x y w) ig))    
  (draw-line      [ig color x1 y1 x2 y2]
    (do (draw-line (get-graphics-img data) color x1 y1 x2 y2) ig))
  (draw-rectangle [ig color x y w h]  
    (do (draw-rectangle (get-graphics-img data) color x y w h) ig))
  (fill-rectangle [ig color x y w h]  
    (do (fill-rectangle (get-graphics-img data) color x y w h) ig))
  (draw-ellipse   [ig color x y w h]  
    (do (draw-ellipse (get-graphics-img data) color x y w h) ig))
  (fill-ellipse   [ig color x y w h]  
    (do (fill-ellipse (get-graphics-img data) color x y w h) ig))
  (draw-string    [ig color font s x y] 
    (do (draw-string (get-graphics-img data) color font s x y) ig))
  (draw-image     [ig img transparency x y] 
    (do (draw-image (get-graphics-img data) img transparency x y) ig))
  ICanvas2DExtended
  (draw-polygon   [ig color  points] (do (draw-polygon (get-graphics-img data) color points) ig))
  (fill-polygon   [ig color  points] (do (fill-polygon (get-graphics-img data) color points) ig))
  (draw-path      [ig points] (do (draw-path (get-graphics-img data) points) ig))
  (draw-poly-line [ig pline]  (do (draw-poly-line (get-graphics-img data) pline) ig))
  (draw-quad      [ig tri]    (do (draw-quad (get-graphics-img data) tri) ig))
  IInternalDraw
  (-draw-shapes [ig xs] (let [g (get-graphics-img data)]
                          (do (draw-shapes g xs)
                            ig))))
  
(defn make-image
  "Creates a portable image-buffer, which allows us to abstract away the java
   specific implementation....while benefitting from the existence of 
   BufferedImage"
  [w h & {:keys [transp image-format ] 
          :or {transp :translucent
               image-format  :buffered-image}}]
  (->image image-format (make-imgbuffer w h (get-transparency transp))
           w h transp))

(defn shape->img
  "Takes any IShape s, and draws it to a buffered image sized according to its 
   bounds."
  ([transparency s] 
     (let [{:keys [x y width height]} (shape-bounds s)
           b (make-imgbuffer  (inc width)
                              (inc height)
                             (get-transparency 
                              transparency))]
       (->image :buffered-image 
                (do (draw-shape s 
                                (translate-2d
                                    (bitmap-graphics b) 
                                          (* -1 x)  (* -1 y)
                                        ;x
                                        ;y
                                              ))  b)
                (inc width)
                (inc height)
                transparency)))
  ([s] (shape->img :translucent s)))

(defn read-image [filepath]
  (let [buf (read-buffered-image filepath)]
    (->image :buffered-image buf  (bitmap-width buf) (bitmap-height buf) :translucent)))

(defn shape->png
  "Saves a component/shape to an image (even an offscreen component), per the 
   implementation's method of drawing as a shape onto a canvas.
   Creates a temporary, stand-alone image buffer.  Caller may supply a 
   function to process the file-path, i.e. printing out to the console, 
   or not."
  [c path & {:keys [on-save] :or {on-save (fn [_] nil)}}]
  (-> (shape->img c)
      (spork.graphics2d.canvas/as-buffered-image :buffered-image)
      (save-image  path on-save)))
