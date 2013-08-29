(ns spork.cljgui.graphics2d.canvas)

;We can actually do some cool stuff here...
;The absolute minimum primitives we need are...
;draw-point.

(defprotocol IColor 
  (get-rgb   [c] "Returns an int encoding of the RGB values.")
  (get-r     [c] "Returns an int encoding Red, between 0-255.")
  (get-g     [c] "Returns an int encoding Green, between 0-255.")
  (get-b     [c] "Returns an int encoding Blue, between 0-255.")
  (get-a     [c] "Returns an int encoding of alpha, between 0-255."))

(defrecord color-rgba [^int r ^int g ^int b ^int a]
  IColor 
  (get-rgb [c] (+ (bit-shift-left r 16)
                  (bit-shift-left g 8)
                  b))  
  (get-r [c] r)
  (get-g [c] g)
  (get-b [c] b)
  (get-a [c] a))

(defn- get-byte [n idx] (bit-and (bit-shift-right n (* idx 8)) 255))
(defn get-rgba [c] (+ (get-rgb c) (bit-shift-left (get-a c) 24))) 
(defn color->alpha [c] (float (/ (get-a c) 255)))
(defn color->rgb [c]   (juxt [get-r get-g get-b]))
(defn color->rgba [c]  (juxt [get-r get-g get-b get-a]))
(defn color->rgb-alpha [c]  (juxt [get-r get-g get-b color->alpha]))
(defn rgba->color-rgba [rgba]
  (let [byte-at (partial get-byte rgba)] 
    (->color-rgba (byte-at 2)
            (byte-at 1)
            (byte-at 0)
            (byte-at 3))))

(defn rgb-equal
  "Determines if two IColors are in fact equal across red, green, and blue."
  [c1 c2] (= (get-rgb c1) (get-rgb c2)))

(defn rgba-equal
  "Determines if two IColors are in fact equal across all four channels."
  [c1 c2]
  (and (= (get-a c1) (get-a c2))
       (rgb-equal c1 c2)))

(def colors     
  #{:black :magenta :white 
    :orange :red :lightgray 
    :blue :darkgray :green
    :pink :yellow :cyan})

;(def colormap     
;  {:black -16777216,
;   :magenta -65281,
;   :white -1,
;   :orange -14336,
;   :red -65536,
;   :lightgray -4144960,
;   :blue -16776961,
;   :darkgray -12566464,
;   :green -16711936,
;   :pink -20561,
;   :yellow -256,
;   :cyan -16711681})

;(def rgbmap 
;  {-1 :white,
;   -16711681 :cyan,
;   -65281 :magenta,
;   -16776961 :blue,
;   -20561 :pink,
;   -256 :yellow,
;   -16711936 :green,
;   -4144960 :lightgray,
;   -12566464 :darkgray,
;   -14336 :orange,
;   -65536 :red,
;   -16777216 :black})


(defprotocol IBitMap
  (bitmap-width [b])
  (bitmap-height [b])
  (bitmap-graphics [b])
  (bitmap-format [b])
  (write-image [b dest fmt])
  (as-buffered-image [b fmt]))
    
(defprotocol I2DGraphicsProvider 
  (get-graphics [obj]))

(defprotocol IGraphicsContext     
    (get-alpha      [ctx])
    (get-transform  [ctx])
    (get-color      [ctx])    
    (set-color      [ctx color])
    (set-alpha      [ctx a])
    (set-transform  [ctx t])    
    (translate-2d   [ctx x y])
    (scale-2d       [ctx x y])
    (rotate-2d      [ctx theta])
    (set-state      [ctx state])
    (make-bitmap    [ctx w h transp]))    

(defn with-color
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the color of the context if necessary, then reverts to
   the original color."
  [color ctx f]
  (let [c (get-color ctx)]
;    (if (rgba-equal c color)
;      (f ctx)
      (-> (f (set-color ctx color))
        (set-color c))))

(defn with-translation
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the translation of the context, applies f, then undoes
   the translation."
  [x y ctx f] 
  (-> (f (translate-2d ctx x y))
    (translate-2d (* -1 x) (* -1 y))))

(defn with-rotation
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the rotation of the context, applies f, then undoes
   the rotation."
  [theta ctx f] 
  (-> (f (rotate-2d ctx theta))
    (rotate-2d (* -1 theta))))

(defn with-scale
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the translation of the context, applies f, then undoes
   the translation."
  [xscale yscale ctx f] 
  (-> (f (scale-2d ctx (double xscale)  (double yscale)))
    (scale-2d (double (/ 1  xscale)) (double (/ 1  yscale)))))

(defn with-transform
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the rotation of the context, applies f, then undoes
   the rotation."
  [xform ctx f]
  (let [xform0 (get-transform ctx)]
    (-> (f (set-transform ctx xform))
      (set-transform xform0))))

(defn with-alpha
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the alpha blending of the context, applies f, then undoes
   the blend."
  [alpha ctx f]
  (let [alpha0 (get-alpha ctx)]
    (-> (f (set-alpha ctx alpha))
      (set-alpha alpha0))))

;note - there are different ways to blend and composite two images...
;OpenGL uses a slew of blending rules and a blendfunction (these are built-in).
;Java2D does something similar....

;we'd like to have more....so we can define protocols 
;primitive operations for drawing....

(def transparencies [:opaque :translucent :bitmask])
(def compositing    [:source-over-destination])

(defn ->blend [transparency compositing])

;a list of 2d rendering devices.
(def devices (atom {}))
(defn get-device [name] (get @devices name))
(defn add-device [name make-context]
  (swap! (fn [s] (assoc s name make-context)) devices))

          
(def canvases (atom {}))
(defn get-canvas [name] (get @devices name))
(defn add-canvas [name make-canvas]
  (swap! (fn [s] (assoc s name make-canvas)) devices))

;these are the bare bones rendering procedures we must have for 2 dimensional
;rendering.  They're copped from a couple of different APIs.
;Note -> these aren't even "that" primitive....we could have a rasterization 
;engine under the hood that does this for us....but that's another layer of 
;abstraction.  
(defprotocol ICanvas2D
  (get-context    [canvas] "gets the IGraphicsContext behind the canvas") 
  (set-context    [canvas ctx] "Sets the IGraphicsContext for the canvas.") 
  (draw-point     [canvas color x y w] "Draws a 2d point at x/y.")    
  (draw-line      [canvas color x1 y1 x2 y2] "Draws a 2d line between points.")
  (draw-rectangle [canvas color x y w h]     "Outlines a rectangle at x,y.")
  (fill-rectangle [canvas color x y w h]     "Draws a filled rectangle at x,y.")
  (draw-ellipse   [canvas color x y w h]     "Outlines a an ellipse at x,y.")
  (fill-ellipse   [canvas color x y w h]     "Draws a filled elipse at x,y.")
  (draw-string    [canvas color font s x y]  "Draws a string at x,y.")
  (draw-image     [canvas img transparency x y] "Draws an IBitmap at x,y."))

;these are fancy rendering options.  We prefer them if supported.
;if the device supports them, then we'll use them.  Otherwise, we use the 
;primtive drawing functions above to draw.
(defprotocol ICanvas2DExtended
  (-draw-polygon   [canvas color points] "Outlines a polygon defined by points.")
  (-fill-polygon   [canvas color points] "Fills a polygon defined by points.")
  (-draw-path      [canvas points]   "Draws a general path defined by points.")
  (-draw-poly-line [canvas pline]  "Draws adjacent segments defined by points.")
  (-draw-quad      [canvas quad]    "Draws a quadrilateral defined by points.")
  (-draw-curve     [canvas curve] "Working on curves later..."))

;(defn draw-path [canvas points])
;(draw-poly-line [canvas points]

;A general protocol for drawing arbitrary shapes.  Specifically, we let the 
;shape, using primitive canvas operations, dictate how it's drawn.  I elevated
;this protocol (used to be locked up in shapes), because it's important enough
;to have here...  That way, a namespace and just 
(defprotocol IShape
  (shape-bounds [s] "Get a bounding box for the shape.")
  (draw-shape [s c] "Draw the shape onto a canvas."))

