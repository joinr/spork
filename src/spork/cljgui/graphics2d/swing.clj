;A swing adapter for platforms in which cljgui can use swing.
;Currently the default, although we should have plenty of other pluggable
;backends. 
(ns spork.cljgui.graphics2d.swing
  (:use     [spork.cljgui.graphics2d.canvas])
  (:require [spork.cljgui.graphics2d [image :as image]])
  (:import [java.awt AlphaComposite Graphics Graphics2D GraphicsEnvironment 
                     GraphicsDevice GraphicsConfiguration Polygon Point 
                     Rectangle Shape Dimension Color Transparency
                     Component]
           [java.awt.geom AffineTransform Point2D Rectangle2D Line2D]
           [java.awt.image BufferedImage ImageObserver]
           [javax.swing JFrame JComponent JPanel]
           [javax.imageio ImageIO]))

;Java2D wrappers....

(extend-protocol IColor
  Color 
  (get-rgb [c] (.getRGB c))
  (get-r   [c] (.getRed c))
  (get-g   [c] (.getGreen c))
  (get-b   [c] (.getBlue c))
  (get-a   [c] (.getAlpha c)))
  
(defn draw-line*
  ([^Graphics2D g x1 y1 x2 y2]
    (do (.. g (drawLine x1 y1 x2 y2)) g))
  ([^Graphics2D g [x1 y1 x2 y2]]
    (draw-line* g x1 y1 x2 y2)))

(defn make-rectangle [x1 y1 x2 y2] 
  (java.awt.geom.Rectangle2D$Double. x1 y1 x2 y2))

(defn draw-rectangle* [^Graphics2D g x1 y1 x2 y2]
  (do (.drawRect g x1 y1 x2 y2) g))

(defn fill-rectangle* [^Graphics2D g x1 y1 x2 y2]
  (do (.fillRect g x1 y1 x2 y2) g))
  
(defn draw-circle [^Graphics2D g x1 y1 w h]
  (do (.drawOval g x1 y1 w h) g))

(defn fill-circle [^Graphics2D g x1 y1 w h]
  (do (.fillOval g x1 y1 w h) g))  

(defn clear-background [^Graphics2D g ^Color color width height]
  (.setColor g color)
  (fill-rectangle* g 0 0 width height))

(defn get-transform* [^Graphics2D g]  
  (.getTransform g))
(defn set-transform* [^Graphics2D g ^AffineTransform affine] 
  (.setTransform g affine))

(defn make-translation [x y]
  (doto (AffineTransform.)
    (.translate x y)))

(defn make-rotation [theta]
  (doto (AffineTransform.)
    (.rotate (Math/toRadians theta))))

(defn translate-xy [^Graphics2D g ^Integer x ^Integer y]
  (.translate g x y))

(defn rotate-xy [^Graphics2D g theta]
  (.rotate g (double theta)))

(defn scale-xy [^Graphics2D g scalex scaley]
  (.scale g (double scalex) (double scaley)))

(defn compose-transforms [coll]
  (let [t (AffineTransform.)
        matmult (fn [^AffineTransform t1 ^AffineTransform t2] 
                  (do (.concatenate t1 t2) t1))]
    (do (reduce matmult t coll)))) 

(defn make-alphacomposite [alpha]
    (if (= (class alpha) AlphaComposite) 
      alpha
      (AlphaComposite/getInstance AlphaComposite/SRC_OVER  (float alpha))))

(defn set-composite [^Graphics2D g ^AlphaComposite ac] 
  (.setComposite g ac))

(defn ^AlphaComposite get-composite [^Graphics2D g]
  (.getComposite g))



;BufferedImage compatibleImage = gc.createCompatibleImage(width,height,transparency);
;//for the transparency you can use either Transparency.OPAQUE, Transparency.BITMASK, or Transparency.TRANSLUCENT

(def opaqe Transparency/OPAQUE)

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

;Color wrappers.
;(defn color->rgb [^Color c]  [(.getRed c) (.getGreen c) (.getBlue c)])

(def colormap     
  {:black Color/BLACK :blue Color/BLUE :cyan Color/CYAN
   :darkgray Color/DARK_GRAY :green Color/GREEN :lightgray Color/LIGHT_GRAY
   :magenta Color/MAGENTA :orange Color/ORANGE :pink Color/PINK
   :red Color/RED :white Color/WHITE :yellow Color/YELLOW}) 

;this is the dumb way to do it.
(defn get-gui-color 
  ([colorkey] (cond (keyword? colorkey) 
                    (if-let [clr (get colormap colorkey)]
                      clr 
                      (throw (Exception. 
                         (str "Color " colorkey " does not exist!"))))
                    (= (class colorkey) Color) 
                       colorkey                     
                    (satisfies? IColor colorkey)
                      (get-gui-color (get-r colorkey)
                                     (get-g colorkey)
                                     (get-b colorkey)
                                     (get-a colorkey))
                      :else (throw (Exception. 
                                     (str "Invalid color key: " colorkey)))))                    
  ([colorkey alpha] 
    (let [[r g b] (color->rgb  (get-gui-color colorkey))]
      (Color. (float r) (float g) (float b) (float alpha))))          
  ([^Float r ^Float g ^Float b] (Color. r g b))
  ([^Float r ^Float g ^Float b ^Float alpha] (Color. r g b alpha)))                  
       
(defn set-gui-color [^Graphics2D g color]
  (do
	  (cond (keyword? color) 
         (.setColor g (get-gui-color color))))
      (= (class? color) Color)
          (.setColor g color))

(defn get-current-color [^Graphics2D g] (.getColor g))
;(defn ^Graphics2D get-graphics-img [^BufferedImage img] (.getGraphics img))

(defn draw-image* [^Graphics g ^BufferedImage img  x y & rest] 
  (do (.drawImage g img x y image/null-observer)
    g))


(def get-transparency image/get-transparency) 
 

(defn unsupported
  ([msg] (throw (Exception. (str msg))))
  ([] (unsupported (str "Unsupported operation"))))
 


(extend-type  Graphics2D 
  ICanvas2D
  (get-context    [^Graphics2D g]  g)
  (set-context    [^Graphics2D g ctx] (throw (Exception. "not implemented")))  
  (draw-point     [^Graphics2D g color x1 y1 w]
    (with-color (get-gui-color color) g 
      #(draw-rectangle* % color x1 y1 10)))     
  
  (draw-line      [^Graphics2D g color x1 y1 x2 y2]
    (with-color (get-gui-color color) g 
      #(draw-line* % x1 y1 x2 y2)))
 
  (draw-rectangle [^Graphics2D g color x y w h]
    (let [c  (if (nil? color) :black color)]
      (with-color (get-gui-color c) g 
        #(draw-rectangle* % x y w h))))

  (fill-rectangle [^Graphics2D g color x y w h]
    (let [c  (if (nil? color) :black color)]
      (with-color (get-gui-color c) g 
        #(fill-rectangle* % x y w h))))
  
  (draw-ellipse   [^Graphics2D g color x y w h]
    (with-color (get-gui-color color) g 
      #(draw-circle % (inc x) (inc y) (dec w) (dec h))))
  
  (fill-ellipse   [^Graphics2D g color x y w h]
    (with-color (get-gui-color color) g 
      #(fill-circle % x y w h)))
  
  (draw-string    [^Graphics2D g color font s x y]
    (with-color (get-gui-color color) g 
      #(do (.drawString % (str s) (float x) (float y)) %)))
  
  (draw-image [^Graphics2D g img transparency x y]
    (draw-image* g (as-buffered-image img (bitmap-format img)) x y nil)))
  
;  ICanvas2DExtended
;  (-draw-polygon   [^Graphics2D g color points] )
;  (-fill-polygon   [^Graphics2D g color points] )
;  (-draw-path      [^Graphics2D g points]   )
;  (-draw-poly-line [^Graphics2D g pline]  )
;  (-draw-quad      [^Graphics2D g tri]  ))


;a set of rendering options specific to the j2d context.
;We can expose these options for low-level stuff later.
;Right now, they don't do anything...
(def rendering-options {:translate identity
                        :alpha     identity
                        :color     identity})
  
(extend-protocol IGraphicsContext  
  Graphics2D 
  (get-alpha      [ctx] (get-composite ctx))
  (get-transform  [ctx] (get-transform* ctx))
  (get-color      [ctx] (get-current-color ctx))     
  (set-color      [ctx c] (do (set-gui-color ctx 
                                (get-gui-color c)) ctx))
  (set-alpha      [ctx a] (do (set-composite ctx (make-alphacomposite a))
                            ctx))
  (set-transform  [ctx t] (do (set-transform* ctx t) ctx))
  
  (translate-2d   [ctx x y] (doto ctx (.translate (int x) (int y))))
  (scale-2d       [ctx x y] (doto ctx (.scale (int x) (int y))))
  (rotate-2d      [ctx theta] (doto ctx (.rotate (float theta))))
  
  (set-state      [ctx state] ctx)
  (make-bitmap    [ctx w h transp] (make-imgbuffer w h transp)))

(defrecord swing-graphics [^Graphics2D g options]
  IGraphicsContext 
  (get-alpha      [ctx]  (get-composite g))
  (get-transform  [ctx]  (get-transform g))
  (get-color      [ctx]  (get-current-color g))     
  (set-color      [ctx c] (swing-graphics. 
                            (do (set-gui-color g 
                                (get-gui-color c)) g)
                            options))
  (set-alpha      [ctx a] (swing-graphics. 
                            (do (set-composite g (make-alphacomposite a)) g)
                            options))
  (set-transform  [ctx t] (swing-graphics. 
                            (do (set-transform* g t) g)
                            options))  
  (translate-2d   [ctx x y] (swing-graphics. 
                              (doto ctx (.translate (int x) (int y)))
                              options))
  (scale-2d       [ctx x y] (swing-graphics. 
                              (doto ctx (.scale (int x) (int y))) 
                              options))                 
  (rotate-2d      [ctx theta] (swing-graphics. 
                                (doto ctx (.rotate (float theta)))
                               options))  
  (set-state      [ctx state] ctx)
  (make-bitmap    [ctx w h transp] (make-imgbuffer w h 
                                       (get-transparency transp))))

(def empty-swing-context (->swing-graphics nil nil))

(extend-protocol I2DGraphicsProvider
  BufferedImage 
  (get-graphics [source] (->swing-graphics (.getGraphics source) nil))
  Component 
  (get-graphics [source] (->swing-graphics (.getGraphics source) nil))
  JFrame 
  (get-graphics [source] (->swing-graphics (.getGraphics source) nil))
  JComponent
  (get-graphics [source] (->swing-graphics (.getGraphics source) nil))
  JPanel
  (get-graphics [source] (->swing-graphics (.getGraphics source) nil)))

;(defn ^Graphics2D get-graphics 
;  ([obj] (cond (satisfies? IBitMap obj) (bitmap-graphics obj)
;               (satisfies? IJ2dProvider obj) (get-graphics-j2d obj)
;               :else (throw (Exception. 
;                              (str "Cannot derive a j2d graphics from obj: "
;                                   (class obj)))))))
;(add-device :java-2d get-graphics-j2d)
