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
  (:import [java.awt Graphics]))

(def painter (atom identity))
(def metamap (atom {:paintf painter}))


(defn -init [msg]  [[] (atom msg)])
(defn -deref [this] @(.state this))

(defn -paintComponent [this ^Graphics g]
  (do  (.parentPaintComponent this g)
       (@painter g)))

(defn -removeNotify [this]
  (do (println "removing!")
      (.parentRemoveAll this)
      (.parentRemoveNotify this)))

(defn -withMeta [m] (reset! metamap m))
(defn -meta [this] @metamap)       


