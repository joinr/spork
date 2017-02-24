;Code adapted from Staurt Sierra's excellent blog post.
(ns spork.cljgui.gridbag
  (:use [spork.cljgui gui])
  (:import [java.awt GridBagConstraints
                     GridBagLayout Insets]
           [javax.swing JPanel JFrame]))

;(def c (GridBagConstraints.))

;(defmacro set-grid! [constraints field value]
;  `(set! (. ~constraints ~(symbol (name field)))
;         ~(if (keyword? value)
;            `(. java.awt.GridBagConstraints
;                ~(symbol (name value)))
;            value)))
;
;
;(defmacro grid-bag-layout [container & body]
;  (let [c (gensym "c")
;        cntr (gensym "cntr")]
;    `(let [~c (new java.awt.GridBagConstraints)
;           ~cntr ~container]
;       ~@(loop [result '() body body]
;           (if (empty? body)
;             (reverse result)
;             (let [expr (first body)]
;               (if (keyword? expr)
;                 (recur (cons `(set-grid! ~c ~expr
;                                          ~(second body))
;                              result)
;                        (next (next body)))
;                 (recur (cons `(.add ~cntr ~expr ~c)
;                              result)
;                        (next body)))))))))


(def b (stretchpanel 500 500 
         (fn [g] (draw-shape (->rectangle 50 50 200 200 :black) g))))

(def panel
     (doto (JPanel. (GridBagLayout.))
       (grid-bag-layout
        :fill :BOTH, :insets (Insets. 5 5 5 5)
        :gridx 0, :gridy 0, :weightx 1 :weighty 1
        b)))
             
        
(def frame
     (doto (JFrame. "GridBagLayout Test")
       (.setContentPane panel)
       (.pack)
       (.setVisible true)))