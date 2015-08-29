(ns spork.cljgui.components.PaintPanel
  (:gen-class
   :extends javax.swing.JPanel
   :implements [clojure.lang.IMeta clojure.lang.IObj]
   :name    spork.cljgui.components.PaintPanel
   :exposes-methods {paintComponent parentPaintComponent
                     removeAll      parentRemoveAll
                     removeNotify   parentRemoveNotify}))

(def painter (atom identity))
(def metamap (atom {:paintf painter}))


(defn -paintComponent [this ^Graphics g]
  (do  (.parentPaintComponent this g)
       (@painter g)))

(defn -removeNotify []
  (do (println "removing!")
      (parentRemoveAll)
      (parentRemoveNotify)))

(defn -withMeta [m] (reset! meta-map m))
(defn -meta [] @meta-map)       

;;Note -> if we don't call the parent's paintcomponent, swing will not 
;;repaint the component like it should, which messes up scrollpanes
;;and introduces graphical artifacts.
(defn paintpanel
  "Create a JPanel with its paint method overriden by paintf, which will be 
   called using g.  We can get mutable behavior by passing a function that 
   evals and applies a ref'd function for paintf, or we can keep the painting 
   static.  Note, paintpanel is static, in that resize behavior will not scale
   the original coordinate system.  It will NOT stretch.  Use paintpanel for 
   simple situations where you have fixed dimensions."
  ([width height paintf]
   (let [painter  (atom paintf)
         meta-map (atom {:paintf painter})
         panel    (proxy [JPanel clojure.lang.IMeta clojure.lang.IObj] []
                    (paintComponent [^Graphics g]
                      (let [^JComponent this this
                            _  (proxy-super paintComponent g)]
                        (paintf g)))
                    (removeNotify [] (do (println "removing!")
                                         (proxy-super removeAll)
                                         (proxy-super removeNotify)))
                    (withMeta [m] (reset! meta-map m))
                    (meta [] @meta-map))
           savelistener (proxy [MouseAdapter] []
                          (mouseClicked [^MouseEvent e]
                             (if (= (.getButton e) MouseEvent/BUTTON3)
                               (let [savepath 
                                     (str (str (System/getProperty "user.home") 
                                               "\\" "SavedBuffer.png")) ]
                                 (do (let [buffer (jgraphics/make-imgbuffer width height)
                                           bg     (j2d/bitmap-graphics buffer)
                                           _      (paintf bg)]
                                       (j2d/write-image buffer savepath nil))
                                     (alert (str "Saved image to " savepath)))))))]                                      
          (doto panel                                                     
            (.setPreferredSize (Dimension. width height))
            (.addMouseListener  savelistener)
            (.setName  (str (gensym "Canvas"))))))
  ([paintf] (paintpanel 250 250 paintf)))
