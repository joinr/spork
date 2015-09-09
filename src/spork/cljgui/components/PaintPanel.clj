;;Init is still jacked up....trying to figure out constructors, plus we get reflection warnings, blah.
(ns spork.cljgui.components.PaintPanel
  (:import [java.awt.image  BufferedImage]
         [java.awt Graphics2D Graphics]
         [javax.swing JComponent ImageIcon]
         )
  (:require [spork.graphics2d.canvas]
            [spork.graphics2d.swing :as jgraphics]))

(defprotocol ICanvasPanel
  (^clojure.lang.IFn getPaintf   [obj])
  (setPainter                    [obj atm])
  (^Graphics2D getBufferGraphics [obj])
  (^BufferedImage getBuffer      [obj]))

(defrecord ppdata [width height painter ^Graphics2D bg ^BufferedImage buffer  meta]
  ICanvasPanel
  (getPaintf [obj] painter)
  (setPainter [obj atm] (reset! painter @atm))
  (getBufferGraphics [obj] bg)
  (getBuffer [obj] buffer))
    
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
         ((.getPaintf ^spork.cljgui.components.PaintPanel.ICanvasPanel (.state this)) g))))

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
    
  
