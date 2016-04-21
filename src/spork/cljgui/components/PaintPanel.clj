;;Init is still jacked up....trying to figure out constructors, plus we get reflection warnings, blah.
(ns spork.cljgui.components.PaintPanel
  (:import [java.awt.image  BufferedImage]
           [java.awt Graphics2D Graphics]
           [javax.swing JComponent ImageIcon]
         )
  (:require
   [spork.graphics2d.canvas]
   [spork.graphics2d.swing :as jgraphics]))

(defprotocol ICanvasPanel
  (^clojure.lang.IFn getPaintf   [obj])
  (setPainter                    [obj atm])
  (^spork.graphics2d.swing.CanvasGraphics getBufferGraphics [obj])
  (^BufferedImage getBuffer      [obj]))

(defrecord ppdata [width height painter bg ^BufferedImage buffer  metadata]
  ICanvasPanel
  (getPaintf         [obj]      painter)
  (setPainter        [obj atm] (reset! painter @atm))
  (getBufferGraphics [obj]      bg)
  (getBuffer         [obj]      buffer))
    
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
        bg (jgraphics/->canvas-graphics (.getGraphics buffer) width height)]
    [[]
     (->ppdata  width height  painter
                bg  buffer (atom {}))]))

(defn -deref    [^spork.cljgui.components.PaintPanel this]    (.state this))
(defn -withMeta [^spork.cljgui.components.PaintPanel this m]
  (do (when-let [oldm  (.metadata (.state this))]
        (reset!  oldm m))
      this))
(defn -meta     [^spork.cljgui.components.PaintPanel this]
  (when-let [m (.metadata (.state this))]
    @m))
  
;;This is our primary wrapper.  Allows us to avoid reflection warnings
;;(and the creation of tons of java.lang.reflector objects when we're calling
;;paintComponent (as we would via proxy).
(defn -paint [this ^Graphics g]
  (let [^spork.cljgui.components.PaintPanel.ICanvasPanel s (.state ^spork.cljgui.components.PaintPanel this)
        ;_ (.parentPaint this g)
        ^javax.swing.JComponent this this]
    (do  
         ((.getPaintf s)
          (.getBufferGraphics s))
         (jgraphics/draw-image* g (.getBuffer s) 0 0))))

(defn -removeNotify [^spork.cljgui.components.PaintPanel this]
  (do (println "removing!")      
      (.parentRemoveAll    this)
      (.parentRemoveNotify this)
      (when-let [buffer (get (.state this) :buffer)]
        (do (.flush ^BufferedImage buffer)
            (.dispose @(get (.state this) :bg))))))

;; (defn ^Graphics2D -get_graphics [this]
;;   (when-let [b (get @(.state this) :buffer)]
;;     (.getGraphics ^BufferedImage b)))
    
  
