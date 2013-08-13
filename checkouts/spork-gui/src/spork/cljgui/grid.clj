;Tom Spoon Feb 13 2012
;This is a simple library for drawing and interacting with grids.  It's meant to
;serve as the basis for a visualization testbed for various graph searches, but 
;it's got a bunch of uses beyond that.  One of them is to get some practical 
;experience with Swing, and in particular, to test out the Reactive extensions 
;I ported/added via Clojure.

(ns spork.cljgui.grid
  (:use [spork.cljgui.gui]
        [spork.cljgui.scenegraph] 
        [spork.cljgui.asyncdrawing]
        [spork.cljgui.events base observe native]))
  

;This serves as a testbed for visualizing various graph algorithms, specifically
;shortest paths, etc. 

;What I'd like to do is steal some inspiration from Matt Buckland, and use his 
;excellent examples in his Game AI book.

;Matt's world exists largely in a grid. 
;He breaks up a view into nxn tiles.
;He then defines sets of polygons that fill said world, and act as boundaries
;for any path-finding algorithm. 

;Coordinates in the grid-world are actually centered on a tile.
;This only really matters for drawing purposes.
(set! *warn-on-reflection* true)

(defn get-gridlines [width height n color]
  (let [vs (for [i (range (inc n))] 
             (->line color 
                     (* i (int (/ width n))) 0 
                     (* i (int (/ width n))) height))
        hs (for [i (range (inc n))] 
             (->line color 
                     0     (* i (int (/ height n))) 
                     width (* i (int (/ height n))) ))]
    (concat vs hs)))                         

(defn draw-gridlines [g width height n]
  (let [w (min width height)]
      (set-gui-color g :black)
      (doseq [[x1 y1 x2 y2] (get-gridlines w w n)]
        (draw-line  g x1 y1 x2 y2))))

;we can change the paintf by supplying a ref'd paint operations function.
;and maintaining access to it....
(defrecord grid [width steps]
  IShape 
  (draw-shape [s g] 
      (doseq [l (get-gridlines width width steps :black)]
        (draw-shape l g)))
  (get-bounds [s] (bbox 0 0 width width)))

(defn make-grid
  "Auxillary function to ensure we produce clean grids, by clean, we mean 
   (= 0 (mod width n)).  This is to ensure even integral spacing for pixels.
   In reality, it doesn't matter 'that' much, but for viewing, it's easier to
   render.  We adjust the grid if the user supplies parameters with an 
   irrational step size, by growing the step size and parameterizing the width
   based on the new stepsize."
  [width n] 
  (if (zero? (mod width n))
    (->grid width n)
    (let [stepsize (int (/ width n))]
      (->grid (* n (inc stepsize)) n))))

(defn async-gridpanel 
  ([grid stretched]
    (let [w (:width grid)
          gridsprite (make-sprite grid :bitmask 0 0)
          bot (-> (make-paintbot w w)
                  (add-prerender 
                    (direct-render (shape-renderer gridsprite))))
          viewf (if stretched get-stretched-panel get-panel)]
      (make-modelview bot (viewf bot) {})))
  ([grid] (async-gridpanel grid false)))

(defn point-set
  "Returns a map, wrapping a closure, that records a set of tiles."
  []
  (let [coords (atom #{})]            
    {:coords coords}))

(defn get-points! [t] @(:coords t))
(defn conj-point! [t x y] (do (if-not (contains? @(:coords t) [x y]) 
	                            (swap! (:coords t) conj [x y]))) t)


(defn drop-point! [t x y] (do (swap! (:coords t) disj [x y])) t)
(defn clear-points! [t] (do (swap! (:coords t) (fn [s] #{})) t))


;an implementation of Bresenham's Line algorithm in clojure.
;In Swing, the mouseDragged event is not polled with high frequency (probably a
;good thing), in that we're not getting per-pixel events....it's based on a 
;time step, and if the mouse moves fast enough, the distance covered in the 
;time step may cause "skipping" to occur, i.e. the next sampled mousedrag may 
;not be adjacent to the previous drag.  For tile-based controls like the Grid, 
;this is pretty important.  As such, we develop an interpolation scheme for 
;quickly generating the "missed" adjacent drags.  Note, this only really matters
;for my grid app, when we're doing things like a "drawing" app.  


;void plotLine(int x0, int y0, int x1, int y1)
;{
;  int dx =  abs(x1-x0), sx = x0<x1 ? 1 : -1;
;  int dy = -abs(y1-y0), sy = y0<y1 ? 1 : -1; 
;  int err = dx+dy, e2; /* error value e_xy */
; 
;  for(;;){  /* loop */
;    setPixel(x0,y0);
;    if (x0==x1 && y0==y1) break;
;    e2 = 2*err; <-this is invariant, should be outside the loop.
;    if (e2 >= dy) { err += dy; x0 += sx; } /* e_xy+e_x > 0 */
;    if (e2 <= dx) { err += dx; y0 += sy; } /* e_xy+e_y < 0 */
;  }
;}

(defn dist [x0 x1]
  (Math/abs (- x0 x1)))

(defn bres-line2 [x0 y0 x1 y1] ;0 0 50 40
  (let [dx (dist x1 x0)  ;50
        sx (if (< x0 x1) 1 -1) ;1       
        dy (negate (dist y1 y0)) ;-50 
        sy (if (< y0 y1) 1 -1)] ;1
    (loop [x x0 ;0
           y y0 ;0
           err (+ dx dy)
           pts []]
      (if (and (= x x1) (= y y1)) 
        (conj pts [x1 y1]) ;base case
        (if (>= (* 2 err) dy)
          (recur (+ x sx) y (+ err dy) (conj pts [x y]))
          (recur x (+ y sy) (+ err dx) (conj pts [x y]))))))) 

(defn ->tile [x y size]
  (->translation x y 
    (->scale size size
     (->rectangle :black 0 0 1 1))))

(defn ->tile-path [size coords]
  (map (fn [[x y]] (->tile (* size x) (* size y) size)) coords))

(defn bres-test [x0 y0 x1 y1 & {:keys [size] :or {size 10}}]
  (paint-scene 
    [(->tile-path size (bres-line2 x0 y0 x1 y1))
     (->line :red (* x0 size) (* y0 size)  (* x1 size) (* y1 size))]))


(defn bres-lerp
  "Given two mouse coordinates, linearly interpolate the coords based on 
   bresenham's line algorithm.  Returns a vector of coordinates."
  [[x y] [x2 y2]]
  (if (and (<= (dist x x2) 1) (<= (dist y y2) 1)) 
    [[x2 y2]]
    (bres-line2 x y x2 y2)))
          

(defn mouse-lerp
  "An observer that buffers mouse coordinates, and linearly interpolates them 
   using bresenham's line algorithm.  This is specifically to handle 'missing' 
   mouse drag events in swing.  It'll return a stream of update! calls to 
   subscribers, interweaving interpolated mousedrag events as necessary."
  [releaseobs dragobs]
  (let [contiguous-coords (fn [[xyold xynew]] (bres-lerp xyold xynew))]                            
    (->> (cyclical-obs releaseobs dragobs) ;allows us to inject new coordinates
         ;(pairwise-obs dragobs)
         (map-obs contiguous-coords) ;returns a seq of [x y]
         (sequential-obs))))
            

(comment
  (defn simple-lerp  
    [releaseobs dragobs]
      (let [contiguous-coords (fn [[xyold xynew]] (bres-lerp xyold xynew))]                            
        (->> (cyclical-obs (make-observable) dragobs) ;allows us to inject new coordinates
         ;(pairwise-obs dragobs)
             (map-obs contiguous-coords))))
  (def fakemouse (make-observable))
  (def fakerelease (make-observable))
  (defn ui [simevents t] (doseq [e simevents] (notify! t e))) 
  
  (def lerp-mouse (simple-lerp fakerelease fakemouse))
  (def seq-mouse (sequential-obs lerp-mouse))
;  (->> lerp-mouse
;       (map-obs (fn [arg] (str "Lerped:" arg)))
;       (subscribe println))
  (notify! fakemouse [0 1])
  (defn mouse-walk [x y]
    (let [rand-dir (fn [[x y]] (case (rand-int 4)
                                 0 [(inc x) y]
                                 1 [x (inc y)]
                                 2 [(dec x) y]
                                 3 [x (dec y)]))]
    (iterate rand-dir [x y])))  
  (defn mouse-simulator [n] 
    (take n (filter (fn [_] (>= (rand) 0.75)) (mouse-walk 0 0))))   
)


;need to account for scale changing inside....
(defn grid->mvc 
  "Creates a simple, stand-alone grid modelview that is based on a grid.
   Given a grid, we create an asynchronous state-handler (a paint bot from 
   cljgui.asyncdrawing) that will maintain a canvas for us.  This canvas 
   has a default pre-renderer built in that draws the grid a graphics arg.  
   This allows us to add shapes to the canvas, while ensuring that the grid 
   is always drawn (i.e. static).  We could move this to a buffered image later,
   but for now it's fast enough!  We also define some useful observable events 
   for the modelview: 
      gridevents - any mousevent that happens over the grid view
      mousedown-xy - a stream of [x y] vectors of mouse panel coordinates 
                     recorded during mouse-dragging (i.e. clicking and holding).
      gridmouse-xy - a stream of [x y] vectors of mouse GRID coordinates  
                     recorded during mouse-dragging.
      panel-click - any click event generated by the left button.  Allows us to 
                    capture use clicks on the grid, as opposed to drags.  

      grid-click - any click event generated by the left button, with x y 
                   properties translated to GRID relative coordinates."
  ([grid] (grid->mvc grid false))
  ([grid stretched]
    (let [[panel bot] ((juxt get-view get-model) 
                       (async-gridpanel grid stretched))                                                      
          gridevents (get-observer panel :mouse)        
          gridscale  (/ (:steps grid) (:width grid))
          gridwidth  (ref (:width grid))                
          origin     (ref :upper-left)                           
          
          panel->grid (fn [[x y]]
                        (let [yoffset (if (= @origin :upper-left) 
                                        y
                                        (- @gridwidth y))]                                       
                          [(int (* x  gridscale)) 
                           (int (* yoffset gridscale))]))        
          get-xy (fn [{:keys [X Y]}] [X Y])        
          in-bounds? (fn [width height [x y]] 
                       (and (>= x 0 ) (>= y 0 ) (<= x width) (<= y height)))                
          mousedown-xy (->> (-> gridevents :dragged)                       
                         (map-obs (comp get-xy event-data)))               
          gridmouse-xy (->> mousedown-xy 
                         (filter-obs  #(in-bounds? @gridwidth @gridwidth %))                       
                         (map-obs panel->grid)
                         (mouse-lerp (-> gridevents :released)))
          panelclick (->> (-> gridevents :clicked)                    
                       (filter-obs left-button?)
                       (map-obs event-data))          
          gridclick (->> (-> gridevents :clicked)                    
                         (filter-obs left-button?)
                         (map-obs 
		                        (comp 
		                          (fn [{:keys [X Y] :as data}] 
		                            (merge data (zipmap [:X :Y] 
                                                    (panel->grid [X Y]))))                          
		                          event-data 
		                          )))]
    ;define the model, the view, and the controls             
    (make-modelview 
      bot  
      panel  
      (merge gridevents {:mousedown-xy mousedown-xy
                         :gridmouse-xy gridmouse-xy
                         :gridclick gridclick
                         :panelclick panelclick})
      {:gridscale gridscale  ;<- package some extra state with the modelview
       :origin origin}))))                                    
(defn grid-app2 []
  (display-stretched (empty-frame) 
                     (get-view (grid->mvc (make-grid 500 10) true))))

(defn grid-app
  "A small application that shows how we can interact with a gridcomponent.  
   Given a width and the number of cells we want, we create a grid model, and 
   bolt on some more sophisticated event handling functionality using 
   observers, and the events housed in the gridmodel's controller."
  ([width step] 
    (let [gridmvc (grid->mvc (make-grid width step))
          gridevents (get-events gridmvc)
          origin (-> (get-state gridmvc) :origin )
          clickhandler (->> (gridevents :gridclick)
                            (subscribe 
                              (fn [{:keys [X Y Button]}]
                                (alert 
                                  (format "Clicked at X:%s Y:%s with btn:%s" 
                                    (str X) (str Y) (str Button))))))          
          panelxy (label "Current Coordinate: 0 0")
          gridxy (label "Current Grid Cell: 0 0")
                    
          panel-delta (->> (gridevents :mousedown-xy)
                           (map-obs 
                             (fn [[x y]] (format "Current Coordinate: %s %s" 
                               (str x) (str y))))
                           (subscribe 
                             (fn [newlabel] (.setText panelxy newlabel))))
          grid-delta  (->> (gridevents :gridmouse-xy)                         
                           (subscribe 
                             (fn [[x y]] (.setText gridxy 
                                (format "Current Grid Coordinate: %s %s"
                                  (str x) (str y))))))
          flip-coordinates (fn [e] (dosync 
                                     (alter origin 
                                        #(case %  :upper-left :bottom-left
                                                  :bottom-left :upper-left))))
          coordbtn (button "Invert Coordinates")
          coordclick (->> (action-observer coordbtn)
                       (:actionPerformed)
                       (subscribe flip-coordinates))]
      (display-stretched 
        (empty-frame)  
               (stack
                 (stack (label "This is a Grid !")
                        coordbtn)
                 (stack (get-view gridmvc)
                         panelxy 
                         gridxy)))))
  ([] (grid-app 500 50)))

(defn drawing-app
  "A small application that allows the user to 'draw' on the grid, showing tiles 
   when the mouse is clicked.  I spent a chunk of time solving mouse-drag event 
   latency via linear interpolation.  Built a few new observer combinators as 
   a result!"
    ([width step] 
    (let [gridmvc    (grid->mvc (make-grid width step))          
;          showngrid (:prerenders @(get-model gridmvc))
;          hiddengrid (subvec showngrid 0 1)           
;          
;          hidegrid (button "Hide Grid")          
;          hideclick (->> (action-observer hidegrid)
;                      (:actionPerformed)
;                      (subscribe 
;                        (fn [_] (set-prerender 
          
          ;properties extracted from the state of the grid modelview
          gridscale  (-> (get-state gridmvc) :gridscale)
          invscale   (/ 1 gridscale) 
          origin     (-> (get-state gridmvc) :origin )
          ;the set of events published by the grid modelview
          gridevents (get-events gridmvc)
          
          panelxy    (label "Current Coordinate: 0 0")
          gridxy     (label "Current Grid Cell: 0 0")
          
          ;a mutable set of points, set [x y]           
          *points*   (point-set)
          ;an observer that notifies when the pointset mutates.
          points-delta (observe-mutation (:coords *points*))
          ;a tile scene constructor.  Translates [x y] grid coordinates into
          ;a black tile on the grid.
;          ->tile  (let [base   
;                                  (->scale invscale invscale
;                                    (->rectangle :black 0 0 1 1))]
;                        (fn [[x y]] (->translation (* invscale x) (* invscale y)
;                                      base)))
          ->tile  (let [base (make-sprite 
                                (->scale invscale invscale 
                                   (->rectangle :black 0 0 1 1)) :bitmask 0 0)]
                        (fn [[x y]] (->translation (* invscale x) (* invscale y)
                                      base)))
          draw-points! (fn [g s] 
                         (render (map ->tile (get-points! *points*))  g))
          ;buffer-point! (fn [pt] (->tile pt)) 
                            
          _ (add-paintfunction (get-model gridmvc) draw-points!)
          _ (->> points-delta 
              (subscribe (fn [_]
                           (repaint (get-view gridmvc)))))


          pointbtn   (button "View Points")
          viewpointclick (->> (action-observer pointbtn)
                              (:actionPerformed)
                              (subscribe 
                                (fn [_]
                                  (alert (str "Scale:" invscale \n 
                                              "Points:"
                                              (get-points! *points*))))))

          clearbtn   (button "Clear Screen")
          clearclick (->> (action-observer clearbtn)
                       (:actionPerformed)
                       (subscribe (fn [_] (clear-points! *points*))))
          
                    
          panel-delta (->> (gridevents :mousedown-xy)
                           (map-obs 
                             (fn [[x y]] (format "Current Coordinate: %s %s" 
                               (str x) (str y))))
                           (subscribe 
                             (fn [newlabel] (.setText panelxy newlabel))))
          
          grid-delta  (->> (gridevents :gridmouse-xy)                         
                           (subscribe 
                             (fn [[x y]] 
                               (do (.setText gridxy 
                                     (format "Current Grid Coordinate: %s %s"
                                             (str x) (str y)))
                                   ))))          
          ;an event that conjoins new points onto the set of visited points.
          new-point (->> (gridevents :gridmouse-xy)
                          (filter-obs 
                            #(not (contains? (get-points! *points*) %)))
                          (subscribe (fn [[x y]] (conj-point! *points* x y))))]
      (make-modelview 
        gridmvc 
       (stack
         (stack (label "Move while left-clicking to draw!")
                pointbtn
                clearbtn
                (button "Point Count" 
                  (fn [_] (alert (count (deref (:coords *points*)))))))
         (stack (get-view gridmvc)
                 panelxy 
                 gridxy))
        (merge gridevents {:viewpointclick viewpointclick
                           :clearclick clearclick
                           :new-point new-point
                           :grid-delta grid-delta 
                           :panel-delta panel-delta})
        {:points *points*})))
  ([] (drawing-app 500 50)))

(defn draw! 
  ([]   (let [app (drawing-app)]
          (do (display (empty-frame) (get-view app))
            app))) 
  ([width step]  (let [app (drawing-app width step)]
                   (do (display (empty-frame) (get-view app))
                     app))))

(defn resizable-grid-app 
  ([width step] 
    (display-stretched (empty-frame)  
                       (stack
                           (stack (label "This is a Grid !"))
                           (stack (get-view (grid->mvc  
                                              (make-grid width step) true))))))
                                              
  ([] (grid-app 500 50)))
      
        
        
                

  


 
