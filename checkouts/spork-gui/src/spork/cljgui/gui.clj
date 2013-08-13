(ns spork.cljgui.gui
  (:require [spork.cljgui [spatial :as spatial]])
  (:import [javax.swing JFrame JPanel Box BoxLayout JTextField JSplitPane
            JLabel JButton JOptionPane JScrollPane Timer SwingUtilities]
           [javax.swing.event ChangeListener]
           [java.awt AlphaComposite BorderLayout Container Component GridLayout 
                     FlowLayout Graphics Graphics2D GraphicsEnvironment 
                     GraphicsDevice GraphicsConfiguration Polygon Point 
                     Rectangle Shape Dimension Color GridBagConstraints 
                     GridBagLayout Insets Transparency]
           [java.awt.geom AffineTransform Point2D Rectangle2D Line2D]
           [java.awt.event ActionListener MouseListener ComponentListener 
                           MouseAdapter MouseEvent]
           [java.awt.image BufferedImage ImageObserver]
           [javax.imageio ImageIO])
  (:gen-class))

(defn import-swing []
  (import '[javax.swing JFrame JPanel Box BoxLayout JTextField JSplitPane
             JLabel JButton JOptionPane Timer SwingUtilities]
           '[javax.swing.event ChangeListener]
           '[java.awt BorderLayout Container Component GridLayout FlowLayout
                     Graphics Graphics2D Polygon Point Rectangle Shape 
                     Dimension]
           '[java.awt.geom AffineTransform Point2D Rectangle2D]
           '[java.awt.event ActionListener MouseListener ComponentListener 
                           MouseAdapter]))


;GUIComponent defines an abstract interface for us to interrogate some data
;and learn about useful bits relative to the Model-View-Controller paradigm.
;Specifically, we can derive events associated with the component (events 
;are actually IObservable instances, even for wrapped GUI components.), the 
;model associated with the component, a view of the component (usually a 
;Swing Panel, but not always!), and any extra state associated with the 
;component.  State is usually a dumping ground for - useful - metadata.  
(defprotocol IGUIComponent
  (get-events [g])
  (get-model [g])
  (get-view [g])
  (get-state [g]))

;A common containter for handling MVC relations.  I found that returning only 
;a swing panel was rather limiting, especially since I'm capturing individual 
;events, as well as building new events for certain widgets. 
;Model is generally some state (usually in an agent) associated with 
;a view.  For basic swing panels, the model IS the panel.  Typical swing 
;development encapsulates important state inside of a class that extends 
;a basic Swing interface, like JFrame, JPanel, etc.  You then interact with
;the model through public methods on the class.  To facilitate clojurian 
;development, I keep access to the underlying model, including its 
;data.  We typically use asynchronous workers (agents) to deal with maintaining
;the model and its data, which also allows us to ignore the single-threaded 
;issues relative to Swing.  A view is usually a swing-panel associated with the 
;underlying model. 

(defrecord modelview [model view control state]
  IGUIComponent
  (get-events [g] control)
  (get-model [g] model)
  (get-view [g] view)
  (get-state [g] state))
  
(defn make-modelview
  "Multi-arity constructor for new modelview records.  Some MVCs will not 
   have any extra state associated with them, so we provide a default 
   empty map."
  ([model view control] (->modelview model view control {}))
  ([model view control state] 
    (->modelview model view control state)))

(defn add-state 
  "Add meta data to the state map in the MVC."
  [mvc k v]
  (assoc-in mvc [:state k] v))

;Java2D wrappers....
(defn get-transform [^Graphics2D g]  (.getTransform g))
(defn set-transform [^Graphics2D g affine] (.setTransform g affine))

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
  (AlphaComposite/getInstance AlphaComposite/SRC_OVER  (float alpha)))

(defn set-composite [^Graphics2D g ^AlphaComposite ac] 
  (.setComposite g ac))

(defn ^AlphaComposite get-composite [^Graphics2D g]
  (.getComposite g))


(def bbox spatial/bbox)

(defprotocol IShape
  (draw-shape [s g])
  (shape-bounds [s]))

(defn shape? [shp] (satisfies? IShape shp))
(defn shape-renderer [s] (partial draw-shape s))

(def null-observer 
  (reify ImageObserver 
     (imageUpdate [this img infoflags x y width height]
        false)))


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
           (postf (str "Buffer saved to: " filepath)))))


;Color wrappers.
(defn color->rgb [^Color c]  [(.getRed c) (.getGreen c) (.getBlue c)])
(def colormap     
  {:black Color/BLACK :blue Color/BLUE :cyan Color/CYAN
   :darkgray Color/DARK_GRAY :green Color/GREEN :lightgray Color/LIGHT_GRAY
   :magenta Color/MAGENTA :orange Color/ORANGE :pink Color/PINK
   :red Color/RED :white Color/WHITE :yellow Color/YELLOW})

;this is the dumb way to do it.
(defn get-gui-color 
  ([colorkey] (if-let [clr (get colormap colorkey)]
                clr 
               (throw (Exception. (str "Color " colorkey " does not exist!")))))
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
(defn ^Graphics2D get-graphics-img [^BufferedImage img] (.getGraphics img))


(defn draw-image [^Graphics g ^BufferedImage img  x y & rest] 
  (.drawImage g img x y null-observer))


(def get-transparency 
  (let [transmap {:opaque Transparency/OPAQUE
                  :translucent Transparency/TRANSLUCENT
                  :bitmask Transparency/BITMASK
                  Transparency/OPAQUE Transparency/OPAQUE
                  Transparency/TRANSLUCENT Transparency/TRANSLUCENT
                  Transparency/BITMASK Transparency/BITMASK}]
    (fn [t]
      (get transmap t Transparency/OPAQUE))))  

;a structure to hold a sprite.  A sprite is just a raster image, built from 
;a source.  Source must be a buffered image.
(defrecord sprite [^BufferedImage source transparency x y]
  IShape 
  (draw-shape [s g]
     (draw-image g source x y nil))
   (shape-bounds [s] (bbox x y (.getWidth source) (.getHeight source)))
   spatial/IBoundingBox 
   (get-bounding-box [b] (shape-bounds b)))

(defn shape->img
  "Takes any IShape s, and draws it to a buffered image sized according to its 
   bounds."
  [s t] 
  (let [{:keys [x y width height]} (shape-bounds s)
        ^BufferedImage b (make-imgbuffer (inc width) (inc height) (get-transparency t))
        ^Graphics2D g (doto (.getGraphics b)
                            (.translate (* -1 x) (* -1 y)))]
    (do (draw-shape s g)
      b)))   

(defn with-color [color ^Graphics2D g  f]
  (let [c (.getColor g)]
    (do (set-gui-color g color)
        (f g)
        (set-gui-color g c))))          

(defn draw-line
  ([^Graphics2D g x1 y1 x2 y2]
    (.. g (drawLine x1 y1 x2 y2)))
  ([^Graphics2D g [x1 y1 x2 y2]]
    (draw-line g x1 y1 x2 y2)))

(defrecord line [color x1 y1 x2 y2]
  IShape 
  (draw-shape [s g]
    (let [original (get-current-color g)]
       (do (set-gui-color g (get-gui-color color)) 
           (draw-line g x1 y1 x2 y2)
           (set-gui-color g original))))
  (shape-bounds [s] (bbox  x1 y1 (- x2 x1) (- y2 y1)))
  spatial/IBoundingBox 
   (get-bounding-box [b] (shape-bounds b)))

(defn make-rectangle [x1 y1 x2 y2] 
  (java.awt.geom.Rectangle2D$Double. x1 y1 x2 y2))

(defn draw-rectangle [^Graphics2D g x1 y1 x2 y2]
  (.drawRect g x1 y1 x2 y2))

(defn fill-rectangle [^Graphics2D g x1 y1 x2 y2]
  (.fillRect g x1 y1 x2 y2))

(defrecord rectangle [color x y w h]
  IShape
  (draw-shape [s g]
     (let [paintf (if (nil? color) draw-rectangle fill-rectangle)                                         
           c  (if (nil? color) :black color)
           original (get-current-color g)]                   
        (do 
          (set-gui-color g (get-gui-color c))
          (paintf g x y w h)
          (set-gui-color g original))))
  (shape-bounds [s] (bbox x y w h))
  spatial/IBoundingBox 
  (get-bounding-box [b] (shape-bounds b)))

(defn negate [n] (* -1 n))
(defn halve [n] (/ n 2))

(defn relative-coords 
  ([size]
		  [((comp negate halve) size) 
		   ((comp negate halve) size) 
		    size 
		    size])
  ([x1 y1 x2 y2] [x1 y1 x2 y2]))

(defn ->relative-rectangle [color size]
  (let [[x1 y1 x2 y2] (relative-coords size)]
  (->rectangle color x1 y1 x2 y2)))

(defn circle->fillcoords 
  ([size] 
    (let [[x1 y1 x2 y2] (relative-coords size)]
      [(inc x1) (inc y1) (dec x2) (dec y2)]))
  ([x1 y1 x2 y2] [(inc x1) (inc y1) (dec x2) (dec y2)]))
  
(defn draw-circle [^Graphics2D g [x1 y1 w h]]
  (.drawOval g x1 y1 w h))

(defn fill-circle [^Graphics2D g [x1 y1 w h]]
  (.fillOval g x1 y1 w h))

(defrecord circle [color x y w h]
  IShape 
  (draw-shape [s g] 
    (do (set-gui-color g (get-gui-color :black))
        (draw-circle g [x y w h])
        (set-gui-color g (get-gui-color color))
        (fill-circle g [(inc x) (inc y) (dec w) (dec h)])))
  (shape-bounds [s] (bbox x y w h))
  spatial/IBoundingBox 
  (get-bounding-box [b] (shape-bounds b)))

(defn ->relative-circle [color size]
  (apply ->circle color (relative-coords size)))

(defn draw-rectangles [g rects]
  (doseq [[x1 y1 x2 y2] rects] 
    (draw-rectangle g x1 y1 x2 y2)))

(defn draw-lines [g lines]
  (doseq [[x1 y1 x2 y2] lines]
    (draw-line g x1 y1 x2 y2)))

(defn fill-rectangles [g rects] 
    (doseq [[x1 y1 x2 y2] rects] 
    (fill-rectangle g x1 y1 x2 y2)))

(defn clear-background [^Graphics2D g ^Color color width height]
  (.setColor g color)
  (fill-rectangle g 0 0 width height))

;generic drawing functions....unify with drawshape
(defn drawing [f]
  (if (satisfies? IShape f)
    (fn [^Graphics2D g] (draw-shape f g))
  (fn [^Graphics2D g] (f g))))

;circle drawing functions....
(defn make-circle [color size]
	  (drawing 
       #(do (set-gui-color % :black)
         (draw-circle % (map int (relative-coords size)))
         (set-gui-color % color)
         (fill-circle % (map int (circle->fillcoords size))))))

;generic rasterization
(doseq [k [sprite line circle rectangle]]
  (derive k ::simpleraster))


(defmulti rasterize (fn [s t] (class s)))
(defmethod rasterize ::simpleraster [r t] 
  (shape->img r t))

;(defn rasterize [r t]
;  (shape->img r t))

(defn make-sprite [s trans x y]
  (->sprite (rasterize s trans) trans x y))


(defn boundingbox->rectangle [b]
  (->rectangle nil (spatial/get-left b) (spatial/get-bottom b) 
               (spatial/get-width b) (spatial/get-height b)))

;extend IShape to nil and bounds.  Bounds are drawn as 
(extend-protocol IShape 
  nil 
  (draw-shape [s g] g)
  (shape-bounds [s] (bbox 0 0))
  
  cljgui.spatial.boundingbox
  (draw-shape [s g]  (draw-shape (boundingbox->rectangle s) g))
  (shape-bounds [s] (spatial/get-bounding-box s)))
             
(defn compose-drawing [drawf1 drawf2]
  "Returns a function that is the composition of two drawings, rendered in 
   the order of the args.  Drawf1 will be draw first, then drawf1.  This means 
   that in alpha-blended images, or images with 'depth', drawf1 will be lower 
   in the z-order, and thus 'benath' drawf2 if any overlap occurs."
  (drawing 
    #(do (drawf1 %)
         (drawf2 %))))

(defn scale-drawing
  "Scales the drawing function drawf by composing a new drawing that first 
   scales the canvas (the graphics context) by x y, then invokes the original
   drawing function "
  [x y drawf]
  (drawing
    (fn [^Graphics2D g]
      (do (.scale g (double x) (double y))
          (drawf g)
          (.scale g (double (/ 1  x)) (double (/ 1  y)))))))

(defn fade-drawing
  "Creates a fading effect by modifying the alpha value of the graphics 
   context, prior to invoking the drawing function drawf.  Returns a function 
   that changes the alpha, invokes drawf, and resets the alpha to its original 
   state."
  [alpha drawf]
  (let [^AlphaComposite composite (make-alphacomposite alpha)] 
	  (drawing 
	    (fn [^Graphics2D g]
       (let [^AlphaComposite ac (get-composite g)]
	       (do (set-composite g composite)
             (drawf g)
             (set-composite g ac)))))))          

(defn rotate-drawing
  "Basic rotation.  Applies a rotation to the graphics context, specified by 
   theta (radians), invokes drawf, and inverts the rotation, returning the 
   original graphics context."
  [theta drawf]
  (drawing
    (fn [^Graphics2D g]
      (do (.rotate g (double theta))
          (drawf g)
          (.rotate g (double (negate theta)))))))

(defn translate-drawing
  "Translates the drawing function drawf by composing a new drawing that 
   first translates the canvas (the graphics context) by the amounts 
   specified in x y, then invokes the original drawing function drawf, then 
   cleans up by inverting the translation."
  [x y drawf]
  (drawing 
    (fn [^Graphics2D g]
      (do (.translate g (int x) (int y))
          (drawf g)
          (.translate g (int (negate x)) (int (negate y)))))))

;;Composable gui components....


(defn ^JFrame empty-frame [] (JFrame.))
(defn ^JFrame make-visible [^JFrame frm]     
  (doto frm 
    (.pack) 
    (.setVisible true)))

(defn ^JFrame add-view [^JFrame frm ^JPanel view] 
 (do (.add (.getContentPane frm) view) 
         frm))

(defn visible? [^JFrame frm] (.isVisible frm))
(defn ^JFrame toggle-top [^JFrame frm]
  (doto frm (.setAlwaysOnTop (not (.isAlwaysOnTop frm))))) 

(defn clear-contents [^JFrame frm]
  (let [cp (.removeAll (.getContentPane frm))]
  (doto frm 
    (.repaint))))

(defn display-stretched [^JFrame frm ^JPanel pane]
  (doto frm 
     (add-view pane)
     (make-visible)))

(defn- guitype [c]
  (cond 
    (= (class c) cljgui.gui.modelview) :modelview
    (or (instance? javax.swing.JComponent c)
        (instance? java.awt.Component c) )   :component
    (satisfies? IShape c) :shape
    :else nil))
    
;define a multimethod for displaying things....this stemmed from an original 
;lone function called display, that was centered around viewing a JPanel in 
;a JFrame.  Now that we have modelviews, I'd like to be able to display them 
;as well.  In essence, we want to keep the structure of modelviews accessible, 
;and facilitate interop.
(defmulti display (fn [frame contents] (guitype contents)))

;Display any generic swing component.  All implementations support JComponent.
(defmethod  display :component [^JFrame frm ^JPanel pane]
	  (doto frm
	    (clear-contents)
	    (.setContentPane (doto (JPanel.) 
	                       (.add pane BorderLayout/CENTER)))
	    (make-visible)))
  
(defmethod display :modelview [^JFrame frm mvc]
  (display frm (get-view mvc)))

(defn alert 
  ([msg] (alert nil msg))
  ([frame msg] 
    (javax.swing.JOptionPane/showMessageDialog frame msg)))

;rather than dealing with all of the linear operations using the 
;internal state of the graphics object....why don't we just provide 
;functions for handling transforms?  Seems like it'd be easier...

;That way, we can define a coordinate system.  Usually, when we create a panel
;the starting width/height will define a coordinate system.
;We can then  draw (relative) points to this coordinate system using the 
;identity transform.  

;What happens when we scale the component?  We're still drawing to the 
;coordinate system....

(defn paintpanel
  "Create a JPanel with its paint method overriden by paintf, which will be 
   called using g.  We can get mutable behavior by passing a function that 
   evals and applies a ref'd function for paintf, or we can keep the painting 
   static.  Note, paintpanel is static, in that resize behavior will not scale
   the original coordinate system.  It will NOT stretch.  Use painpanel for 
   simple situations where you have fixed dimensions."
  ([width height paintf]
     (let [^BufferedImage buffer  (make-imgbuffer  width height)
           ^Graphics2D bg         (.getGraphics buffer)
           
           p (fn [^Graphics2D g]
               (do                   
                 (paintf bg) 
                 (draw-image g buffer 0 0 nil)))
           panel  (proxy [JPanel] [] 
                    (paint [g]  (p g)))                    
           savelistener (proxy [MouseAdapter] []
                          (mouseClicked [^MouseEvent e]
                             (if (= (.getButton e) MouseEvent/BUTTON3)
                               (save-image buffer 
                                 (str (str (System/getProperty "user.home") 
                                    "\\" "SavedBuffer.png")) alert))))]                               
       (doto panel                                                     
         (.setPreferredSize (Dimension. width height))
         (.addMouseListener savelistener))))
  ([paintf] (paintpanel 250 250 paintf)))


(defmethod display :shape [^JFrame frm s]
  (let [{:keys [x y width height]} (shape-bounds s)]
    (display frm (paintpanel 
                   (inc (+ width x)) 
                   (inc (+ height y)) #(draw-shape s %)))))

(defn repaint [^JPanel p]
  (.repaint p))

;Code adapted from Staurt Sierra's excellent blog post.
(def c (GridBagConstraints.))

(defmacro set-grid! [constraints field value]
  `(set! (. ~constraints ~(symbol (name field)))
         ~(if (keyword? value)
            `(. java.awt.GridBagConstraints
                ~(symbol (name value)))
            value)))


(defmacro grid-bag-layout [container & body]
  (let [c (gensym "c")
        cntr (gensym "cntr")]
    `(let [~c (new java.awt.GridBagConstraints)
           ~cntr ~container]
       ~@(loop [result '() body body]
           (if (empty? body)
             (reverse result)
             (let [expr (first body)]
               (if (keyword? expr)
                 (recur (cons `(set-grid! ~c ~expr
                                          ~(second body))
                              result)
                        (next (next body)))
                 (recur (cons `(.add ~cntr ~expr ~c)
                              result)
                        (next body)))))))))

                   
(defn stretchable-panel
  "Create a JPanel with its paint method overriden by paintf, which will be 
   called using g.  We can get mutable behavior by passing a function that 
   evals and applies a ref'd function for paintf, or we can keep the painting 
   static.  Note, paintpanel is static, in that resize behavior will not scale
   the original coordinate system.  It will stretch.  Use stretchpanel for 
   situations where you have user resizing.  Note, the behavior will only take
   effect when Resize events are generated.  This means that you must use 
   stretchpanel in a layout manager that supports it."
  ([iwidth iheight paintf]
     (let [buffer (ref (make-imgbuffer iwidth iheight))
           bg  (ref (get-graphics-img @buffer)) ;this is really our drawing surface...
           
           aspect  (/ iheight iwidth )
           w (ref iwidth) ;our nominal coordinate width
           h (ref iheight) ;our nominal coordinate height           
           offset (ref [0 0]) ;no offset to begin with applied.           
           p (fn [^Graphics2D g]
               (do                   
                 (paintf @bg) ;paint to the buffer
                 (draw-image g @buffer 0 0 nil))) ;draw buffer to any graphics.
           panel (doto 
                   (proxy [JPanel] [] (paint [g]  (p g)))
                   (.setPreferredSize (Dimension. iwidth iheight)))
           resize (fn [wnew hnew] ;if w and h have changed, need to change the buffer
                    (let [wprev @w
                          hprev @h
                          ^BufferedImage oldbuff @buffer]
                          
	                    (if (or (not= wnew wprev) (not= hnew hprev))
                       (dosync                         
                         (let [[xoff yoff] [(- wprev wnew) (- hprev hnew)]
                               newbuff (make-imgbuffer wnew hnew)
                               ^Graphics2D bgraph (.getGraphics newbuff)]                               
                           (dosync 
                             (ref-set offset [xoff yoff])
                             (ref-set buffer newbuff)
                             (ref-set bg (doto bgraph                                                                                     
                                           (.scale (double (/ wnew wprev)) 
                                                   (double (/ hnew hprev))
                                           )))
                             (.setPreferredSize panel (Dimension. wnew hnew))
                             ))))))]
       (doto (JPanel. (GridBagLayout.))
                                                                                                         
         (grid-bag-layout (JPanel. (GridBagLayout.)) 
                      :fill :BOTH, :insets (Insets. 0 0 0 0)
                      :gridx 0, :gridy 0, :weightx 1 :weighty 1 
                      
                     (doto panel 
                            (.setPreferredSize (Dimension. iwidth iheight))
                            (.addComponentListener 
                              (proxy [ComponentListener] []
                                (componentMoved [e] nil)
                                (componentHidden [e] nil)
                                (componentResized [e]
                                  (let [w (.getWidth panel)
                                        h (.getHeight panel)]                                    
                                    (if (and (> w 0) (> h 0))
                                      (let [areaw (/ (* w w ) aspect)
                                            areah (* h h aspect)]
                                        (if (<= areaw areah) ;minimize 
                                          (resize w
                                              (* aspect w))
                                          (resize h
                                              (/ h aspect)))))))                                   
                                (componentShown [e] nil))))))))
                       
     

  ([paintf] (stretchable-panel 250 250 paintf)))

(defn view 
  "Bread and butter function to quickly visualize anything that can be 
   painted into a frame.  Uses the implementation of display for s.  If 
   no frame is provided, an empty one is created with a simple paint panel."
  ([^JFrame f s]
    (let [mvc (case (guitype s)
                :modelview s
                :shape (let [{:keys [width height]} (shape-bounds s)]
                         (make-modelview s 
                             (paintpanel width height #(draw-shape s %)) {}))
                :component (make-modelview s s {}))]                
      [(display f s)  mvc]))
  ([s] (view (empty-frame) s)))

;(defn stretched [panel]
;  (doto (JPanel. (GridBagLayout.))
;    (grid-bag-layout 
;                   :fill :BOTH, :insets (Insets. 5 5 5 5)
;                   :gridx 0, :gridy 0, :weightx 1 :weighty 1 
;                   panel)))
;                   

;(def b (stretchpanel 500 500 
;         (fn [g] (draw-shape (->rectangle 50 50 200 200 :black) g))))

(defn shelf [& components]
  (let [shelf (JPanel.) ]
    (.setLayout shelf (FlowLayout.))
    (doseq [c components] (.add shelf c))
    shelf))

(defn stack [& components]
  (let [stack (Box. BoxLayout/PAGE_AXIS)]
    (doseq [c components]
      (.setAlignmentX c Component/CENTER_ALIGNMENT)
      (.add stack c))
    stack))

(defn splitter [top bottom]
  (doto (JSplitPane.)
    (.setOrientation JSplitPane/VERTICAL_SPLIT)
    (.setLeftComponent top)
    (.setRightComponent bottom)))

(defn button 
  ([text] (JButton. text))
  ([text f]
	  (doto (JButton. text)
		    (.addActionListener 
		      (proxy [ActionListener] []
		        (actionPerformed [e] (f e)))))))

;implement scroll pane? 

(defn label [txt] (JLabel. txt))

(defn txt [cols t]
  (doto (JTextField.)
    (.setColumns cols)
    (.setText t)))

(defn grid-container [x y f]
  (let [g (doto (JPanel.)
            (.setLayout (GridLayout. x y)))]
    (dotimes [i x]
      (dotimes [j y]
        (.add g (f))))
    g))


(defn text-field [value]
  (doto (JTextField. value 15)
    (.setEnabled false)
    (.setHorizontalAlignment JTextField/RIGHT)))

(defn change-label
  "Changes the text value of label L."
  [^JLabel lab f e] 
  (.setText lab (f e)))

;Courtesy of Stuart Sierra.
(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

(defmacro with-resize [component event & body]
  `(. ~component addComponentListener
      (proxy [java.awt.event.ComponentListener] []
        (componentResized [~event] ~@body))))

(defmacro with-moved [component event & body]
	`(. ~component addComponentListener
	      (proxy [java.awt.event.ComponentListener] []
	        (componentResized [~event] ~@body))))

 (defn run-app [app] 
   (if (javax.swing.SwingUtilities/isEventDispatchThread)
       (app)
     (SwingUtilities/invokeLater app)))
 
 (defn run-now [app]
   (if (javax.swing.SwingUtilities/isEventDispatchThread)
       (app)
     (SwingUtilities/invokeAndWait app)))
 


 

 
