(ns spork.graphics2d.canvas
  (:require [spork.protocols  [spatial :as spatial]]
            [spork.graphics2d [primitive :as prim]]))

;We can actually do some cool stuff here...
;The absolute minimum primitives we need are...
;draw-point.

(defprotocol IColor 
  (get-rgb   [c] "Returns an int encoding of the RGB values.")
  (get-r     [c] "Returns an int encoding Red, between 0-255.")
  (get-g     [c] "Returns an int encoding Green, between 0-255.")
  (get-b     [c] "Returns an int encoding Blue, between 0-255.")
  (get-a     [c] "Returns an int encoding of alpha, between 0-255."))

(defprotocol IGradient 
  (left-color  [g])
  (right-color [g])
  (left-point  [g])   
  (right-point [g]))

(defrecord gradient [lcolor rcolor lpoint rpoint]
  IGradient 
  (left-color  [g] lcolor)
  (right-color [g] rcolor)
  (left-point  [g] lpoint)    
  (right-point [g] rpoint))

(declare shape-bounds)

(defn gradient-right [c1 c2]
  (fn [shp]
    (let [{:keys [x y width height]} (shape-bounds shp)]
      (->gradient c1 c2 [x y] [(+ x width) y]))))

(defn gradient-left [c1 c2]
  (fn [shp]
    (let [{:keys [x y width height]} (shape-bounds shp)]
      (->gradient c2 c1 [x y] [(+ x width) y]))))

(defn gradient-down [c1 c2]
  (fn [shp]
    (let [{:keys [x y width height]} (shape-bounds shp)]
      (->gradient c1 c2 [x y] [x (+ y height)]))))

(defn gradient-up [c1 c2]
  (fn [shp]
    (let [{:keys [x y width height]} (shape-bounds shp)]
      (->gradient c2 c1 [x y] [x (+ y height)]))))

(defn color-by [f shp]
  (let [grad (f shp)]
    (assoc shp :color grad)))


;;there are occasions where we want to specify a gradient based on the 
;;shape bounds

(defrecord color-rgba [^int r ^int g ^int b ^int a]
  IColor 
  (get-rgb [c] (+ (bit-shift-left r 16)
                  (bit-shift-left g 8)
                  b))  
  (get-r [c] r)
  (get-g [c] g)
  (get-b [c] b)
  (get-a [c] a))

;;vectors are simple color containers.
(extend-protocol IColor 
  clojure.lang.PersistentVector 
  (get-rgb [c] (+ (bit-shift-left (nth c 0) 16)
                  (bit-shift-left (nth c 1) 8)
                  (nth c 2)))
  (get-r [c] (nth c 0))
  (get-g [c] (nth c 1))
  (get-b [c] (nth c 2))
  (get-a [c] (nth c 3)))

(defn random-color []
  [(rand-int 256)
   (rand-int 256)
   (rand-int 256)
   255])

(defn- get-byte [n idx] (bit-and (bit-shift-right n (* idx 8)) 255))
(defn  get-rgba     [c] (+ (get-rgb c) (bit-shift-left (get-a c) 24))) 
(defn  color->alpha [c] (float (/ (get-a c) 255)))
(defn  color->rgb   [c]   (juxt [get-r get-g get-b]))
(defn  color->rgba  [c]  (juxt [get-r get-g get-b get-a]))
(defn  color->rgb-alpha [c]  (juxt [get-r get-g get-b color->alpha]))
(defn  rgba->color-rgba [rgba]
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

;;The clear color, used for "wiping" out existing colors.

(def colors     
  #{:black :magenta :white 
    :orange :red :lightgray 
    :blue :darkgray :green
    :pink :yellow :cyan})

;;a set of colors from
;;http://www.rapidtables.com/web/color/RGB_Color.htm
(def color-specs
  (into {}
        [[:clear [255 255 255 0]]
         [:maroon [128 0 0]]
         [:dark-red [139 0 0]]
         [:brown [165 42 42]]
         [:firebrick [178 34 34]]
         [:crimson [220 20 60]]
         [:red [255 0 0]]
         [:tomato [255 99 71]]
         [:coral [255 127 80]]
         [:light-red [255 127 80]] ;added
         [:indian-red [205 92 92]]
         [:light-coral [240 128 128]]
         [:dark-salmon [233 150 122]]
         [:salmon [250 128 114]]
         [:light-salmon [255 160 122]]
         [:orange-red [255 69 0]]
         [:dark-orange [255 140 0]]
         [:orange [255 165 0]]
         [:gold [255 215 0]]
         [:dark-golden-rod [184 134 11]]
         [:golden-rod [218 165 32]]
         [:pale-golden-rod [238 232 170]]
         [:dark-khaki [189 183 107]]
         [:khaki [240 230 140]]
         [:olive [128 128 0]]
         [:yellow [255 255 0]]
         [:yellow-green [154 205 50]]
         [:dark-olive-green [85 107 47]]
         [:olive-drab [107 142 35]]
         [:lawn-green [124 252 0]]
         [:chart-reuse [127 255 0]]
         [:green-yellow [173 255 47]]
         [:dark-green [0 100 0]]
         [:green [0 128 0]]
         [:forest-green [34 139 34]]
         [:lime [0 255 0]]
         [:lime-green [50 205 50]]
         [:light-green [144 238 144]]
         [:pale-green [152 251 152]]
         [:dark-sea-green [143 188 143]]
         [:medium-spring-green [0 250 154]]
         [:spring-green [0 255 127]]
         [:sea-green [46 139 87]]
         [:medium-aqua-marine [102 205 170]]
         [:medium-sea-green [60 179 113]]
         [:light-sea-green [32 178 170]]
         [:dark-slate-gray [47 79 79]]
         [:teal [0 128 128]]
         [:dark-cyan [0 139 139]]
         [:aqua [0 255 255]]
         [:cyan [0 255 255]]
         [:light-cyan [224 255 255]]
         [:dark-turquoise [0 206 209]]
         [:turquoise [64 224 208]]
         [:medium-turquoise [72 209 204]]
         [:pale-turquoise [175 238 238]]
         [:aqua-marine [127 255 212]]
         [:powder-blue [176 224 230]]
         [:cadet-blue [95 158 160]]
         [:steel-blue [70 130 180]]
         [:corn-flower-blue [100 149 237]]
         [:deep-sky-blue [0 191 255]]
         [:dodger-blue [30 144 255]]
         [:light-blue [173 216 230]]
         [:sky-blue [135 206 235]]
         [:light-sky-blue [135 206 250]]
         [:midnight-blue [25 25 112]]
         [:navy [0 0 128]]
         [:dark-blue [0 0 139]]
         [:medium-blue [0 0 205]]
         [:blue [0 0 255]]
         [:royal-blue [65 105 225]]
         [:blue-violet [138 43 226]]
         [:indigo [75 0 130]]
         [:dark-slate-blue [72 61 139]]
         [:slate-blue [106 90 205]]
         [:medium-slate-blue [123 104 238]]
         [:medium-purple [147 112 219]]
         [:dark-magenta [139 0 139]]
         [:dark-violet [148 0 211]]
         [:dark-orchid [153 50 204]]
         [:medium-orchid [186 85 211]]
         [:purple [128 0 128]]
         [:thistle [216 191 216]]
         [:plum [221 160 221]]
         [:violet [238 130 238]]
         [:magenta [255 0 255]]
         [:fuchsia [255 0 255]]
         [:orchid [218 112 214]]
         [:medium-violet-red [199 21 133]]
         [:pale-violet-red [219 112 147]]
         [:deep-pink [255 20 147]]
         [:hot-pink [255 105 180]]
         [:light-pink [255 182 193]]
         [:pink [255 192 203]]
         [:antique-white [250 235 215]]
         [:beige [245 245 220]]
         [:bisque [255 228 196]]
         [:blanched-almond [255 235 205]]
         [:wheat [245 222 179]]
         [:corn-silk [255 248 220]]
         [:lemon-chiffon [255 250 205]]
         [:light-golden-rod-yellow [250 250 210]]
         [:light-yellow [255 255 224]]
         [:saddle-brown [139 69 19]]
         [:sienna [160 82 45]]
         [:chocolate [210 105 30]]
         [:peru [205 133 63]]
         [:sandy-brown [244 164 96]]
         [:burly-wood [222 184 135]]
         [:tan [210 180 140]]
         [:rosy-brown [188 143 143]]
         [:moccasin [255 228 181]]
         [:navajo-white [255 222 173]]
         [:peach-puff [255 218 185]]
         [:misty-rose [255 228 225]]
         [:lavender-blush [255 240 245]]
         [:linen [250 240 230]]
         [:old-lace [253 245 230]]
         [:papaya-whip [255 239 213]]
         [:sea-shell [255 245 238]]
         [:mint-cream [245 255 250]]
         [:slate-gray [112 128 144]]
         [:light-slate-gray [119 136 153]]
         [:light-steel-blue [176 196 222]]
         [:lavender [230 230 250]]
         [:floral-white [255 250 240]]
         [:alice-blue [240 248 255]]
         [:ghost-white [248 248 255]]
         [:honeydew [240 255 240]]
         [:ivory [255 255 240]]
         [:azure [240 255 255]]
         [:snow [255 250 250]]
         [:black [0 0 0]]
         [:dim-gray [105 105 105]] 
         [:dim-grey [105 105 105]]
         [:gray [128 128 128]]
         [:grey [128 128 128]]
         [:dark-gray [169 169 169]]
         [:dark-grey [169 169 169]]
         [:silver [192 192 192]]
         [:light-gray [211 211 211]]
         [:light-grey [211 211 211]]
         [:gainsboro [220 220 220]]
         [:white-smoke [245 245 245]]
         [:white [255 255 255]]
         ;;added due to conveneience
         [:amber [178 140 0]]]))

;; var RColor = function() {
;; 		this.hue			= Math.random(),
;; 		this.goldenRatio 	= 0.618033988749895;
;; 		this.hexwidth		= 2;
;; 	};

(defn rgb-floor [xs]  [(int  (Math/floor (*  (nth xs 0) 255.0)))
                       (int  (Math/floor (*  (nth xs 1) 255.0)))
                       (int  (Math/floor (*  (nth xs 2) 255.0)))])

(defn hsv->rgb [h s v]
  (let [h_i (long (Math/floor (* h 6)))
        f   (- (* h 6) h_i)
        p   (* v (- 1.0 s))
        q   (* v (- 1.0 (* f s)))
        t   (* v (- 1.0 (*  (- 1.0 f) s) ))]
    (rgb-floor  (case h_i
                  0 [v t p]
                  1 [q v p]
                  2 [p v t]
                  3 [p q v]
                  4 [t p v]
                  5 [v p q]))))

(defn rgb->hsv [r g b]
  (let [r (/ r 255.0)
        g (/ g 255.0)
        b (/ b 255.0)
        cmax (max r g b)
        cmin (min r g b)
        delta  (- cmax cmin)
        d  (if (zero? delta) 1.0 delta)
        v cmax
        ;_ (println [r g b cmax cmin delta v])
        s (if (zero? delta) 0 (/ delta cmax))]
    [(/  (cond (= r cmax)  (mod (/ (- g b) d) 6)
                  (= g cmax)  (+ (/ (- b r) d) 2)
                  :else       (+ (/ (- r g) d) 4))
         6)
     s
     v]    
    ))

;;Thanks to Martin Ankerl at
;; http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/: 


;; golden_ratio_conjugate = 0.618033988749895
;; h = rand # use random start value
;; gen_html {
;;   h += golden_ratio_conjugate
;;   h %= 1
;;   hsv_to_rgb(h, 0.5, 0.95)
;; }

(def ^:constant +golden-ratio+ 0.618033988749895)

(let [hue         (double-array [(rand)])]
  (defn random-golden-hue []
    (let [h (double (mod (+ (aget hue 0) +golden-ratio+) 1))]
      (do (aset hue 0 h)
          h))))   

(defn saturations 
  ([step r g b]
     (let [[h s v] (rgb->hsv r g b)]
       (map (fn [sat]
              (hsv->rgb h sat v)) (take-while #(<= % 1.0) (iterate (fn [x] (+ x step)) 0.0)))))
  ([step color] (saturations step (get-r color) (get-g color) (get-b color))))

(defn mono-color-palette
  ([step color] (saturations step   (get-r color) (get-g color) (get-b color)))
  ([step]       (mono-color-palette step (random-color))))

(defn random-color-palette 
  ([s v] (map (fn [h] (hsv->rgb h s v)) (repeatedly random-golden-hue)))
  ([] (random-color-palette 0.5 0.95)))

;;source:https://github.com/sterlingwes/RandomColor/blob/master/rcolor.js

;; 	RColor.prototype.hsvToRgb = function (h,s,v) {
;; 		var	h_i	= Math.floor(h*6),
;; 			f 	= h*6 - h_i,
;; 			p	= v * (1-s),
;; 			q	= v * (1-f*s),
;; 			t	= v * (1-(1-f)*s),
;; 			r	= 255,
;; 			g	= 255,
;; 			b	= 255;
;; 		switch(h_i) {
;; 			case 0:	r = v, g = t, b = p;	break;
;; 			case 1:	r = q, g = v, b = p;	break;
;; 			case 2:	r = p, g = v, b = t;	break;
;; 			case 3:	r = p, g = q, b = v;	break;
;; 			case 4: r = t, g = p, b = v;	break;
;; 			case 5: r = v, g = p, b = q;	break;
;; 		}
;; 		return [Math.floor(r*256),Math.floor(g*256),Math.floor(b*256)];
;; 	};

(defprotocol IBitMap
  (bitmap-width      [b])
  (bitmap-height     [b])
  (bitmap-graphics   [b])
  (bitmap-format     [b])
  (write-image       [b dest fmt])
  (as-buffered-image [b fmt]))

(defprotocol IWipeable
  (wipe [obj]))

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

;;slight hack to enable some visual trickery.
(defprotocol IStroked
   (get-stroke [ctx])
   (set-stroke [ctx s]))

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

(defn with-stroke
  "Given a drawing function f, where f::IStroked->IStroked, 
   temporarily changes the stroke of the context, applies f, then undoes
   the blend."
  [stroke ctx f]
  (let [stroke0 (get-stroke ctx)]
    (-> (f (set-stroke ctx stroke))
      (set-stroke stroke0))))

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

;;I'll probably revist this, and, instead of a loose wrapper around the 
;;graphics device, build it up from real primitives like paths and regions.
;;Then define everything on top of that.  

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

(defprotocol ITextRenderer
  (text-width     [canvas txt] "Gets the weidth of txt on canvas, in pixels.")
  (text-height    [canvas txt] "Gets the height of txt on canvas, in pixels."))


;;__TODO__ Turn these stubs into protocol members.
;; (defn text-width
;;   "Currently a stub.
;;    Returns the width, in pixels, of some text."  
;;   ^long [canvas txt] (count txt))
;; (defn text-height
;;   "Currently a stub.
;;    Returns the height, in pixels, of some text."
;;   ^long [canvas txt] 1)

;these are fancy rendering options.  We prefer them if supported.
;if the device supports them, then we'll use them.  Otherwise, we use the 
;primtive drawing functions above to draw.  ;;These are actually our primitive 
;drawing operations...
(defprotocol ICanvas2DExtended
  (draw-polygon   [canvas color points] "Outlines a polygon defined by points.")
  (fill-polygon   [canvas color points] "Fills a polygon defined by points.")
  (draw-path      [canvas points]       "Draws a general path defined by points.")
  (draw-poly-line [canvas pline]        "Draws adjacent segments defined by points.")
  (draw-quad      [canvas quad]         "Draws a quadrilateral defined by points.")
  (draw-curve     [canvas curve]        "Working on curves later..."))

;(defn draw-path [canvas points])
;(draw-poly-line [canvas points]


;A general protocol for drawing arbitrary shapes.  Specifically, we let the 
;shape, using primitive canvas operations, dictate how it's drawn.  I elevated
;this protocol (used to be locked up in shapes), because it's important enough
;to have here...That way, a namespace and just 
(defprotocol IShape
  (shape-bounds [s]   "Get a bounding box for the shape.")
  (draw-shape   [s c] "Draw the shape onto a canvas."))

(defn shape-seq?
  "Function that determines if the collection is a simple sequence of things 
   that can be drawn using IShape, or if, despite being a sequence, already 
   directly support IShape and knows how to draw itself."
  [x]  (and (sequential? x) (not (satisfies? IShape x))))  

;;Protocol for more efficient drawing operations, if a canvas supports them.
(defprotocol IInternalDraw
  (-draw-shapes [canvas xs]))

(defn draw-shapes
  "Draws a sequence of shapes, xs, to canvas c.  Expands collections into a 
   sequence of shapes, if and only is the collection is not a shape collection."
  [c xs]
  (if (satisfies? IInternalDraw c) (-draw-shapes c xs) 
    (reduce 
      (fn [c s] (if (not (shape-seq? s)) (draw-shape s c) (draw-shapes c s))) 
      c xs)))

(extend-protocol IShape 
  clojure.lang.PersistentVector
  (shape-bounds [xs] (when xs (spatial/group-bounds (map shape-bounds xs))))
  (draw-shape   [xs c] (draw-shapes c xs)))

;;maybe....
(defn draw [s c]
  (let [g  (if (satisfies? ICanvas2D c) c (get-graphics c))]
    (draw-shape s g)))


(defprotocol IShapeStack
  (push-shape [s shp])
  (pop-shape  [s]))

;;Allow mutable shapes. 
(extend-type clojure.lang.Atom
  IShape
  (draw-shape [shp c] (draw-shape (deref shp) c))
  (shape-bounds [shp] (shape-bounds (deref shp))))
