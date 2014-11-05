;A swing adapter for platforms in which cljgui can use swing.
;Currently the default, although we should have plenty of other pluggable
;backends. 
(ns spork.graphics2d.swing
  (:use     [spork.graphics2d.canvas])
  (:require [spork.graphics2d [image :as image] [font :as f]]
            [spork.protocols [spatial :as s]])
  (:import  [java.awt AlphaComposite Graphics Graphics2D GraphicsEnvironment 
             FontMetrics GraphicsDevice GraphicsConfiguration Polygon Point 
             Rectangle Shape Dimension Color Transparency Component Stroke GradientPaint]
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

(extend-type Rectangle2D 
  s/IBounded 
  (get-width  [b]  (.getWidth b))
  (get-height [b]  (.getHeight b))
  (get-left   [b]  (.getMinX b))
  (get-right  [b]  (.getMaxX b))
  (get-top    [b]  (.getMaxY b))
  (get-bottom [b]  (.getMinY b))
  s/IBoundingBox
  (get-bounding-box [bv] (s/->boundingbox (.getX bv) (.getY bv) (.getWidth bv) (.getHeight bv))))
  
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

;;A better way is to exploit the paint interface, wrap our own
;;protocol....

(defprotocol IPaintable
  (as-paint [p]))


(defn get-gui-color 
  ([colorkey] (if (satisfies? IPaintable colorkey)
                (as-paint colorkey)
                (throw (Exception. 
                        (str "Not IPaintable :" colorkey)))))                    
  ([colorkey alpha] 
    (let [[r g b] (color->rgb  (get-gui-color colorkey))]
      (Color. (float r) (float g) (float b) (float alpha))))          
  ([^Float r ^Float g ^Float b] (Color. r g b))
  ([^Float r ^Float g ^Float b ^Float alpha] (Color. r g b alpha)))

;Color wrappers.
;(defn color->rgb [^Color c]  [(.getRed c) (.getGreen c) (.getBlue c)])

(def colormap     
  (into (reduce-kv (fn [m clr [r g b]]
                     (assoc m clr (Color. (int r) (int g) (int b))))
                   {}
                   spork.graphics2d.canvas/color-specs)
       {:black Color/BLACK :blue Color/BLUE :cyan Color/CYAN
        :darkgray Color/DARK_GRAY :green Color/GREEN :lightgray Color/LIGHT_GRAY
        :magenta Color/MAGENTA :orange Color/ORANGE :pink Color/PINK
        :red Color/RED :white Color/WHITE :yellow Color/YELLOW}))

(extend-type  GradientPaint
  IGradient
  (left-color  [g] (.getColor1 g))
  (right-color [g] (.getColor2 g))
  (left-point  [g] (.getPoint1 g))
  (right-point [g] (.getPoint2 g)))

(defn ^GradientPaint gradient-across 
  ([x1 y1 clr1 x2 y2 clr2 cyclic?] (GradientPaint. x1 y1 (get-gui-color clr1) x2 y2 (get-gui-color clr2) cyclic?))
  ([x1 y1 clr1 x2 y2 clr2] (GradientPaint. x1 y1 (get-gui-color clr1) x2 y2 (get-gui-color clr2))))


(extend-protocol IPaintable 
  Color 
  (as-paint [c] c)
  GradientPaint 
  (as-paint [c] c)
  spork.graphics2d.canvas.gradient 
  (as-paint [c] (let [[x1 y1] (left-point c)
                      [x2 y2] (right-point c)]
                  (gradient-across x1 y1 (left-color c) x2 y2 (right-color c))))
  clojure.lang.Keyword
  (as-paint [k] (if-let [clr (get colormap k)]
                  clr 
                  (throw (Exception. 
                          (str "Color " k " does not exist!")))))
  spork.graphics2d.canvas.color-rgba
  (as-paint [c] (Color. (.r c) (.g c) (.g c) (.a c))))  



;this is the dumb way to do it.
;; (defn get-gui-color 
;;   ([colorkey] (cond (keyword? colorkey) 
;;                     (if-let [clr (get colormap colorkey)]
;;                       clr 
;;                       (throw (Exception. 
;;                          (str "Color " colorkey " does not exist!"))))
;;                     (identical? (class colorkey) Color) 
;;                        colorkey                     
;;                     (satisfies? IColor colorkey)
;;                       (get-gui-color (get-r colorkey)
;;                                      (get-g colorkey)
;;                                      (get-b colorkey)
;;                                      (get-a colorkey))
;;                      (satisfies? IGradient colorkey) colorkey
;;                       :else (throw (Exception. 
;;                                      (str "Invalid color key: " colorkey)))))                    
;;   ([colorkey alpha] 
;;     (let [[r g b] (color->rgb  (get-gui-color colorkey))]
;;       (Color. (float r) (float g) (float b) (float alpha))))          
;;   ([^Float r ^Float g ^Float b] (Color. r g b))
;;   ([^Float r ^Float g ^Float b ^Float alpha] (Color. r g b alpha)))                  


       
(defn set-gui-color [^Graphics2D g color]
  (do (.setPaint g (get-gui-color color)) g))


(defn get-current-color [^Graphics2D g] (.getColor g))
;(defn ^Graphics2D get-graphics-img [^BufferedImage img] (.getGraphics img))

(defn draw-image* [^Graphics g ^BufferedImage img  x y & rest] 
  (do (.drawImage g img x y image/null-observer)
    g))


(def get-transparency image/get-transparency) 
 

(defn unsupported
  ([msg] (throw (Exception. (str msg))))
  ([] (unsupported (str "Unsupported operation"))))

(def font-graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB)))
(defn ^Rectangle2D string-bounds 
  ([^Graphics2D g ^String txt]  
     (let [^FontMetrics fm (.getFontMetrics g)]
       (.getStringBounds fm txt g)))
  ([^String txt] (string-bounds font-graphics txt)))

;; Font font = ... ;
;; BufferedImage img = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
;; FontMetrics fm = img.getGraphics().getFontMetrics(font);
;; int width = fm.stringWidth("Your string");

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
      #(do (.drawString ^Graphics2D % (str s) (float x) (float y)) %)))  
  (draw-image [^Graphics2D g img transparency x y]
    (draw-image* g (as-buffered-image img (bitmap-format img)) x y nil))
  IStroked
   (get-stroke [^Graphics2D ctx] (.getStroke ctx))
   (set-stroke [^Graphics2D ctx ^Stroke s] (doto ctx (.setStroke s)))
  ITextRenderer
  (text-width     [^Graphics2D canvas ^String txt] (.getWidth  (string-bounds canvas txt)))
  (text-height    [^Graphics2D canvas ^String txt] (.getHeight (string-bounds canvas txt))))

  
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
  (scale-2d       [ctx x y] (doto ctx (.scale  x  y)))
  (rotate-2d      [ctx theta] (doto ctx (.rotate (float theta))))
  
  (set-state      [ctx state] ctx)
  (make-bitmap    [ctx w h transp] (make-imgbuffer w h transp)))

;encapsulation for everything swing...plumbing mostly.
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
                              (doto g (.translate (int x) (int y)))
                              options))
  (scale-2d       [ctx x y] (swing-graphics. 
                              (doto g (.scale  x  y)) 
                              options))                
  (rotate-2d      [ctx theta] (swing-graphics. 
                                (doto g (.rotate (float theta)))
                               options))  
  (set-state      [ctx state] ctx)
  (make-bitmap    [ctx w h transp] (make-imgbuffer w h 
                                       (get-transparency transp)))
  ICanvas2D
  (get-context    [sg]  g)
  (set-context    [sg ctx] (swing-graphics. ctx options))  
  (draw-point     [sg color x1 y1 w] 
    (swing-graphics. (draw-point g color x1 y1 w)  options))  
  (draw-line      [sg color x1 y1 x2 y2] 
    (swing-graphics. (draw-line g color x1 y1 x2 y2) options)) 
  (draw-rectangle [sg color x y w h]
    (swing-graphics. (draw-rectangle g color x y w h) options))
  (fill-rectangle [sg color x y w h]
    (swing-graphics. (fill-rectangle g color x y w h) options))   
  (draw-ellipse   [sg color x y w h]
    (swing-graphics. (draw-ellipse g color x y w h) options))  
  (fill-ellipse   [sg color x y w h]
    (swing-graphics. (fill-ellipse g color x y w h) options))  
  (draw-string    [sg color font s x y]
    (swing-graphics. (draw-string g color font s x y) options))  
  (draw-image     [sg img transparency x y]
    (swing-graphics. 
      (draw-image* g (as-buffered-image img (bitmap-format img)) x y nil)
      options))
  ITextRenderer
  (text-width     [sg  txt] (.getWidth  (string-bounds g txt)))
  (text-height    [sg  txt] (.getHeight (string-bounds g txt)))
  IStroked
  (get-stroke [sg]   (.getStroke g))
  (set-stroke [sg s] (swing-graphics. (doto g (.setStroke s)) options))
  IInternalDraw
  (-draw-shapes [sg xs] (assoc sg :g (draw-shapes g xs))))

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
