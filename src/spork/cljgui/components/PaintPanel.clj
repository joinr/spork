;;Init is still jacked up....trying to figure out constructors, plus we get reflection warnings, blah.
(ns spork.cljgui.components.PaintPanel
  (:gen-class
   :extends javax.swing.JPanel
   :implements [clojure.lang.IMeta clojure.lang.IObj clojure.lang.IDeref]
   :name    spork.cljgui.components.PaintPanel
   :state   state
   :init    init
   :constructors {[] []}
   :exposes-methods {paintComponent parentPaintComponent
                     removeAll      parentRemoveAll
                     removeNotify   parentRemoveNotify})
  (:import [java.awt Graphics Component]
           [javax.swing JComponent]
          ))

;;this is somewhat hackish at the moment...
(def painter (atom identity))
(def remover (atom identity))
(def metamap (atom {:paintf  painter
                    :remover remover}))

(defn -init [msg]  [[] (atom msg)])
(defn -deref [this] @(.state this))

;;This is our primary wrapper.  Allows us to avoid reflection warnings
;;(and the creation of tons of java.lang.reflector objects when we're calling
;;paintComponent (as we would via proxy).
(defn -paintComponent [this ^Graphics g]
    (do  (.parentPaintComponent this g)
         (@painter g)))

(defn -removeNotify [this]
  (do (println "removing!")
      (@remover this)
      (.parentRemoveAll    this)
      (.parentRemoveNotify this)))

(defn -withMeta [m] (reset! metamap m))
(defn -meta     [this] @metamap)       


