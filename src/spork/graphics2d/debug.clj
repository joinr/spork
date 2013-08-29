(ns spork.graphics2d.debug
  (:require [spork.graphics2d.canvas]))

(def ^:dynamic *debug-graphics* false)
(def commands (atom []))
(defn log-command! [name & data] 
  (if *debug-graphics* (println [:graphics [name data]])
    (swap! commands conj [name data])))

(defn flush-commands! [] (swap! commands (fn [_] [])))
(defmacro arg-map [args]
  (let [ks (map keyword args)]
    `(hash-map ~@(interleave ks args)))) 

(defmacro debug-command [name args return]
  `(do (log-command! ~(keyword name) (arg-map ~args))
       ~return))

(defn emit-debug-fn [[name args & rest]]
  `(debug-command (keyword ~name) ~args ~(first args)))

;;working on a debug stub macro that will implement dummy protocols.

;(def debug-canvas 
;  (reify
;    cljgui.graphics2d.canvas/IGraphicsContext    
;    (get-alpha      [ctx] (debug-command get-alpha [ctx]))
;    (get-transform  [ctx] (debug-command get-transform  [ctx]) 
;    (get-color      [ctx] (log-command! :get-color))    
;    (set-color      [ctx color] (log-command! :set-color (arg-map [color])))
;    (set-alpha      [ctx a] (log-command! :set-alpha (arg-map [a])))
;    (set-transform  [ctx t] (log-command! :set-transform (arg-map [t])))    
;    (translate-2d   [ctx x y] (log-command! :translate-2d (arg-map [x y])))
;    (scale-2d       [ctx x y] (log-command! :scale-2d (arg-map [x y])))
;    (rotate-2d      [ctx theta] (log-command! :rotate-2d (arg-map [theta])))
;    (set-state      [ctx state] (log-command! :set-state (arg-map [state])))
;    (make-bitmap    [ctx w h transp] 
;      (log-command! :make-bitmap (arg-map [w h transp])))
;    
;    cljgui.graphics2d.canvas/ICanvas2D
;    (get-context    [canvas]  (log-command!  :get-context)) 
;    (set-context    [canvas ctx] 
;      (log-command! :set-context (arg-map [ctx]))) 
;    (draw-point     [canvas color x y w] 
;      (log-command! :draw-point (arg-map [color x y w])))    
;    (draw-line      [canvas color x1 y1 x2 y2] 
;      (log-command! :draw-line (arg-map [color x1 y1 x2 y2])))
;    (draw-rectangle [canvas color x y w h] 
;      (log-command! :draw-rectangle (arg-map [color x y w h])))
;    (fill-rectangle [canvas color x y w h]    
;      (log-command! :fill-rectangle (arg-map [color x y w h])))
;    (draw-ellipse   [canvas color x y w h] 
;      (log-command! :draw-ellipse (arg-map [color x y w h])))
;    (fill-ellipse   [canvas color x y w h]   
;      (log-command! :fill-ellipse (arg-map [color x y w h])))
;    (draw-string    [canvas color font s x y] 
;      (log-command! :draw-string (arg-map [color font s x y])))
;    (draw-image     [canvas img transparency x y]
;      (log-command! :draw-image (arg-map [img transparency x y])))))

(defmacro with-debug-graphics [body]
  (binding [*debug-graphics* true]
    ~body))