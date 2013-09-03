;A wrapper around various image formats.  
;I needed to abstract this away, since I'm using buffered images everywhere, 
;particularly for sprites, and I need to use a standard "image" format, 
;especially for rendering sprites and other goodies.
;So we define a simple class that can provide a default implementation for 
;make-bitmap, which "everyone" can use....
(ns spork.graphics2d.image
  (:use [spork.graphics2d.canvas])
  (:import [java.awt GraphicsEnvironment  GraphicsDevice GraphicsConfiguration 
            Transparency Graphics2D]
           [java.awt.image BufferedImage ImageObserver]
           [javax.imageio ImageIO]))

(def opaqe Transparency/OPAQUE)

(def null-observer 
  (reify ImageObserver 
     (imageUpdate [this img infoflags x y width height]
        false)))

(defn ^Graphics2D get-graphics-img [^BufferedImage img] (.getGraphics img))

(defn ^BufferedImage make-imgbuffer 
  ([w h ^Transparency t]
	  (let [^GraphicsEnvironment ge 
	          (GraphicsEnvironment/getLocalGraphicsEnvironment)
	        ^GraphicsDevice gd (.getDefaultScreenDevice ge)
	        ^GraphicsConfiguration gc (.getDefaultConfiguration gd)]	  
	    (.createCompatibleImage gc w h t)))
  ([w h] (make-imgbuffer w h Transparency/TRANSLUCENT)))   

(defn save-image [^BufferedImage img filepath postf]
   (let [tgt (clojure.java.io/file filepath)]
         (do 
           (if (not (.exists tgt)) 
              (.mkdirs (.getParentFile tgt)))
           (.createNewFile tgt)                                                                                 
           (ImageIO/write img "png" tgt)
           (postf (str "Buffer saved to:" filepath)))))

(defn ^BufferedImage read-image [filepath]
   (let [tgt (clojure.java.io/file filepath)]         
     (assert (.exists tgt)) 
     (ImageIO/read tgt)))

(def get-transparency 
  (let [transmap {:opaque Transparency/OPAQUE
                  :translucent Transparency/TRANSLUCENT
                  :bitmask Transparency/BITMASK
                  Transparency/OPAQUE Transparency/OPAQUE
                  Transparency/TRANSLUCENT Transparency/TRANSLUCENT
                  Transparency/BITMASK Transparency/BITMASK}]
    (fn [t]
      (get transmap t Transparency/OPAQUE)))) 

(extend-protocol IBitMap 
  BufferedImage 
  (bitmap-width [b]  (.getWidth b))
  (bitmap-height [b] (.getHeight b))
  (bitmap-graphics [b] (get-graphics-img b))
  (bitmap-format [b] :buffered-image)
  (as-buffered-image [b fmt] b)
  (write-image [b dest fmt] (save-image b dest (fn [_] nil))))


(defrecord image [image-format data width height transparency]
  IBitMap 
  (bitmap-width [b]  width)
  (bitmap-height [b] height)
  (bitmap-graphics [b] (get-graphics-img data))
  (bitmap-format [b] image-format)
  (as-buffered-image [b fmt] (cond (= image-format  fmt :buffered-image) data 
                                   (= fmt image-format ) 
                                     (as-buffered-image data fmt)
                                     :else (throw (Exception. 
                                           "No other formats implemented."))))
  (write-image [b dest fmt] (save-image data dest (fn [_] nil))))
  
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
  [s transparency] 
  (let [{:keys [x y width height]} (shape-bounds s)
        b (make-imgbuffer (inc width) (inc height) (get-transparency 
                                                     transparency))]
    (->image :buffered-image 
             (do (draw-shape s 
                     (translate-2d (bitmap-graphics b) 
                                   (* -1 x) (* -1 y)))  b)
             (inc width) (inc height) transparency)))


