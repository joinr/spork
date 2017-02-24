;;A class for extending our host Graphics2D with a handy
;;getClientRect property, which is a given, and a
;;default to a cartesian coordinate system with 0,0 at
;;the bottom left of the CanvasGraphics.
(ns spork.cljgui.components.CanvasGraphics
  (:import [java.awt.image  BufferedImage]
           [java.awt Graphics2D Graphics]
           [javax.swing JComponent ImageIcon]
           [spork.graphics2d.canvas
            ICanvas2D
            IStroked
            ITextRenderer
            ])
  (:require [spork.graphics2d.canvas]
            [spork.graphics2d.swing :as jgraphics]))


  
(gen-class
   :extends javax.swing.JPanel
   :implements [clojure.lang.IMeta
                clojure.lang.IObj
                clojure.lang.IDeref
                ]
   :name    spork.cljgui.components.PaintPanel
   :state   state
   :init    init
   :constructors {[long long clojure.lang.IFn] []}
   :exposes-methods {;paintComponent parentPaintComponent
                     paint          parentPaint
                     removeAll      parentRemoveAll
                     removeNotify   parentRemoveNotify})

;;The buffer is the canvas.  The label is just a display.  We'd "like"
;;to treat it as an object that compatible with clojure's system.

;;this is somewhat hackish at the moment...
(defn -init [width height painter]
  (let [^BufferedImage buffer (jgraphics/make-imgbuffer  width height)
        ^Graphics2D bg (.getGraphics buffer)]
    [[]
     (->ppdata  width height  painter
                bg  buffer nil)]))


(defn -deref    [this]    (.state this))
(defn -withMeta [this m]  this)
(defn -meta     [this]    nil)
  
;;This is our primary wrapper.  Allows us to avoid reflection warnings
;;(and the creation of tons of java.lang.reflector objects when we're calling
;;paintComponent (as we would via proxy).
(defn -paint [this ^Graphics g]
  (let [^javax.swing.JComponent that this]
    (do ; (.parentPaint that g)
      ((.getPaintf ^spork.cljgui.components.PaintPanel.ICanvasPanel
                   (.state this))
       g))))

(defn -removeNotify [this]
  (do (println "removing!")      
      (.parentRemoveAll    this)
      (.parentRemoveNotify this)
      (when-let [buffer (get (.state this) :buffer)]
        (do (.flush ^BufferedImage buffer)
            (.dispose ^Graphics2D (get (.state this) :bg))))))

;; (defn ^Graphics2D -get_graphics [this]
;;   (when-let [b (get @(.state this) :buffer)]
;;     (.getGraphics ^BufferedImage b)))
    
  
