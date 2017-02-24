;;__TODO__ Currently the scenegraph dependency will cause this to fail.
;;Everything else is okay.  Probably revisit this guy later though.  There is 
;;likely a more elegant way, since this was an early attempt from a couple of 
;;years ago...

;Tom Spoon 15 Feb 2012 
;This is an asynchronous drawing/state framework for implementing 
;model/view/controller schemes in Clojure, with Swing.  This library 
;provides a generalized asynchronous renderer, the paintbot, which is 
;simply a Clojure agent with special functionality.  The libray also provides
;the async-paintpanel, which is an extension of the normal paintpanel gui 
;component from cljgui.gui, with the ability to modify the rendering of the 
;form by interacting with a paintbot.  Several functions are included to allow 
;clients to define and access the paint bot, attach views to it, etc.
(ns spork.cljgui.asyncdrawing
  (:use [spork.cljgui.components.swing]
        [spork.cljgui.events observe native]
        [spork.graphics2d.canvas])
  (:require [spork.graphics2d [scenegraph :as sg]]))

(def drawfuncs {:shape draw-shape 
                :scene sg/render})

(defn get-drawtype [s] 
  (cond (satisfies? IShape s) :shape 
        (satisfies? sg/IScene s) :scene))
        
(defn get-renderer
  "Selects between a primitive shape renderer and a complex scene renderer."
  ([draw-type] (get drawfuncs draw-type identity)))
	  
(defn direct-render
  "Render that doesn't need to know about the bot's state."
  [f] 
  (fn [g & s] (f g)))

(defn default-prerender [g {:keys [fillcolor width height] :as state}]
    (clear-background g (get-gui-color fillcolor) width height))

(defn shape-painter
  "Default painting function for our asynchronous painter.  Calls the draw-shape
   protocol function from cljgui.gui/IShape."
  ([g {:keys [fillc paintc paintfs w h shapes prerenders] :as state}]
    (when (seq shapes)       
      (with-color paintc g 
       #(doseq [[drawtype shp] shapes]
           ((get-renderer drawtype) shp %))))))
                
;container for our drawing state.
;Contains a set of prerenders and paintfunctions.  We add a default 
;prerender, which clears the background, and a default paintfunction, which 
;paints all of the shapes in the draw state.  We later provide functions to 
;add rendering steps to the prerender and the paintfunctions, in case of 
;more complex painting (i.e. static scenes are more complicated, or we 
;want to replace them with a buffered image to avoid redrawing.)  Notice 
;that draw-state is a generic IShape, and it gets drawn by calling each of 
;it prerenderers on a graphics context and with its state, then following 
;through with its paintfunctions. 
(defrecord draw-state 
  [fillcolor paintcolor paintfunctions width height shapes prerenders]
  IShape 
  (draw-shape [s g]
              
              (do                
                (doseq [r prerenders] (r g s))
                (doseq [p paintfunctions] (p g s)))))


;default color scheme, size, drawing function.
(def init-state
  (draw-state. :white :black [shape-painter] 500 500  [] [default-prerender]))

(defn size-state [state width height]
  (merge state {:width width :height height}))

;constructor to make a paintbot!
(defn make-paintbot
  ([] (make-paintbot init-state))
  ([w h] (make-paintbot (size-state init-state w h)))
  ([state] (agent state)))

;get properties from the paintbot!
(defn get-bot [bot k]
  (-> @bot(get k)))

(defn get-shapes
  "Retrieve a collection of shapes.  Currently, it's just a vector of 
   coordinate vectors, which are interpreted to be rectangles."
  [bot]
  (get-bot bot :shapes))  

(defn get-color
  "Get the color the bot is using to paint its shapes with."
  [bot]
  (get-bot bot :paintcolor))

(defn get-fill
  "Get the color the bot us using to clear the screen with."
  [bot]
  (get-bot bot :fillcolor))

(defn set-color
  "Change the paint color of the bot."
  [bot color]
    (send-off bot assoc :paintcolor color))

(defn clear-shapes
  "Erase all the shapes from the bot."
  [bot]
  (send-off bot assoc  :shapes []))
                   
(defn- shape-entry [shp]
  (if-let [drawtype (get-drawtype shp)]
    [drawtype shp]
    (throw (Exception. (str "Unknown shape" shp)))))

(defn add-shape
  "Add a shape to the bot's collection.  Currently, the shape should be 
   a [[x1 x2 x3 x4] [x1 x2 x3 x4]] nested vector."
  [bot shp]  
    (send-off bot #(assoc % :shapes (conj (:shapes %) 
                      (shape-entry shp)))))

(defn add-shapes
  "Add a shape to the bot's collection.  Currently, the shape should be 
   a [[x1 x2 x3 x4] [x1 x2 x3 x4]] nested vector."
  [bot coll]  
    (send-off bot #(assoc % :shapes 
                     (reduce conj (:shapes %) (map shape-entry coll)))))

(defn set-width
  "Change the width of the bot's rendering surface."
  [bot w]
  (send-off bot assoc :width w))

(defn set-height
  "Change the height of the bot's rendering surface."
  [bot h]
  (send-off bot assoc :height h))

(defn set-paintfunction
  "Change the function the bot uses to paint with.  Note, new functions should 
   have args corresponding to [graphics botstate], since the bot will get a 
   current value of its state before rendering.  No value is returned."
  [bot f]
  (send-off bot assoc :paintfunctions f))

(defn add-paintfunction 
  "Conjoin a paintfunction to the vector of paint functions in the bot.  The bot
   will compose each paint function sequentially."
  [bot f]
  (send-off bot #(assoc % :paintfunctions (conj (:paintfunctions %)  f))))

(defn set-prerender
  "Set the pre-rendering function of the bot.  This is typically something that 
   remains static (like a buffered image for instance)."
  [bot f]
  (send-off bot assoc :prerenders f))

(defn add-prerender
  "Like add-paintfunction.  This adds a pre-rendering function to be composed 
   in order with earlier pre-rendering functions."
  [bot f]   
  (send-off bot #(assoc % :prerenders (conj (:prerenders %) f))))

(defn async-paintfunction
  "Return a function - likely used for paintpanels - that uses a drawing agent's
   state to allow changes in the rendered image.  We wrap this in an agent 
   because of the threading.  Calls are made, by Swing, from the event dispatch
   thread."
  [agt]
  (let [get-state (fn [] @agt)]	       
    (fn [g] (draw-shape (get-state) g))))

(defn get-panel
  "Build a paintpanel, either from an existing paintbot's specs, or a new 
   paintbot, which will allow us to mutate the drawing of the paintpanel.
   Returns a map of {:panel xxxx :events {:painterchanged event}}"
  ([paintbot]  
    (let [[w h] ((juxt :width :height) @paintbot)
          panel-id (keyword (gensym "paint"))
          panel (paintpanel w h (async-paintfunction paintbot))]
      do (add-watch paintbot panel-id 
                    (fn [watchkey bot oldstate newstate] 
                      (if-not (or (nil? panel) 
                                  (not (.isVisible panel)))
                        (.repaint panel)
                        (remove-watch bot watchkey))))
      panel))
  ([w h] (get-panel (make-paintbot w h))) 
  ([] (get-panel (make-paintbot 500 500))))

(defn get-stretched-panel 
  "Build a stretch-panel, either from an existing paintbot's specs, or a new 
   paintbot, which will allow us to mutate the drawing of the paintpanel.
   Returns a map of {:panel xxxx :events {:painterchanged event}}"
  ([paintbot]  
    (let [[w h] ((juxt :width :height) @paintbot)
          panel-id (keyword (gensym "paint"))
          panel (stretchable-panel w h (async-paintfunction paintbot))]
      do (add-watch paintbot panel-id 
                    (fn [watchkey bot oldstate newstate] 
                      (if-not (or (nil? panel) 
                                  (not (.isVisible panel)))
                        (.repaint panel)
                        (remove-watch bot watchkey))))
      panel))
  ([w h] (get-stretched-panel (make-paintbot w h))) 
  ([] (get-stretched-panel  (make-paintbot 500 500))))
   
(comment
(def canvas (make-paintbot))

(defn view-builder 
  [pb & {:keys [stretched?] :or {stretched? true}}]
	  (let [panelfunc (if stretched? get-stretched-panel get-panel)
          resetbtn (button "Click to clear lines!")
	        lbl (label "These two are mirrored")
	        addline (fn [e] 
	                  (add-shape pb (->line :black (rand 500) (rand 500) 
				                        (rand 500) (rand 500) )))
	        change-label (fn [lab f e] (.setText lab (f e)))  
	        counter (label "0")
	        zero-counter (fn [c e] (change-label c (fn [_] "0") e))
	        shapecounter (shelf (label "Shape Count: ")
	                         counter)                      
	
	        clearclick (->> (get-observer resetbtn :action)
	                     (:actionPerformed)
	                     (subscribe 
	                       (fn [e]
	                         (zero-counter counter e)
	                         (clear-shapes pb))))
	
	        drag-lines (fn [panel]  
	                     (do (->> (get-observer panel :mouse)
	                              (:dragged)
	                              (map-obs #(doto % (addline)))
	                              (subscribe 
	                                (partial change-label counter 
	                                    (fn [_] (str (count (:shapes @pb))))))) 
	                       panel))] 
	     (make-modelview 
			    pb 
					(stack shapecounter
			        (drag-lines (panelfunc pb))                
			        resetbtn
			        (drag-lines (panelfunc pb)))
			     {:clearclick clearclick 
			      :drag-lines drag-lines})))

;(def static-test (view-test canvas get-view))
(defn stretched-view-test [pb]
  (let [resetbtn (button "Click to clear lines!")
        lbl (label "These two are mirrored")
        addline (fn [e] 
                  (add-shape pb (->line :black (rand 500) (rand 500) 
			                        (rand 500) (rand 500) )))
        change-label (fn [lab f e] (.setText lab (f e)))  
        counter (label "0")
        zero-counter (fn [c e] (change-label c (fn [_] "0") e))
        shapecounter (shelf (label "Shape Count: ")
                         counter)                      

        clearclick (->> (get-observer resetbtn :action)
                     (:actionPerformed)
                     (subscribe 
                       (fn [e]
                         (zero-counter counter e)
                         (clear-shapes pb))))

        drag-lines (fn [panel]  
                     (do (->> (get-observer panel :mouse)
                              (:dragged)
                              (map-obs #(doto % (addline)))
                              (subscribe 
                                (partial change-label counter 
                                    (fn [_] (str (count (:shapes @pb))))))) 
                       panel))] 
    (make-modelview 
	     pb
	     (stack 
	        (shelf shapecounter resetbtn)
	     	  (drag-lines (get-stretched-panel  pb)))
	     {:clearclick clearclick 
		    :drag-lines drag-lines})))

)
                                
                                
                                