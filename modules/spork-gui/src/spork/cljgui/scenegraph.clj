;Extends the rendering model for drawing shapes into something a bit more 
;robust.  The idea is to allow declarative rendering.  Note, there is quite 
;a substantial library in cljgui.gui, which contains most of the primitive 
;functions used to interact with Java2D, wrapped with Clojure.

(ns spork.cljgui.scenegraph
  (:use [cljgui.gui :only [IShape get-current-color get-gui-color  set-gui-color 
                           translate-xy rotate-xy  halve negate make-sprite 
                           get-composite make-alphacomposite set-composite 
                           scale-xy empty-frame paintpanel display ->line 
                           ->circle ->rectangle draw-shape shape-bounds 
                           make-imgbuffer get-transparency rasterize]]
        [cljgui.spatial :only [bbox scale-bounds translate-bounds 
                               rotate-bounds group-bounds get-bounding-box]])
  (:require [clojure [zip :as zip]])
  (:import [java.awt AlphaComposite Graphics2D] ;these need to be wrapped...
           [java.awt.image BufferedImage]))

(defprotocol IScene
  (draw [s g] "Draws the scene onto graphics context g.")
  (child-scenes [s] "Returns nested child scenes, if any exist.")
  (conj-scene [s child]   
   "Conjoins child to parent, so that when more-scenes is called on parent, 
    child will return as the last element.")
  (scene-bounds [s] "Returns a rectangular bounding box that contains the scene
                     and its children."))

(def unbounded (bbox 0 0 0 0))

(defn scene?     [s] (satisfies? IScene s))
(defn composite? [s] (> (count (child-scenes s)) 0))       

(defn get-scene-bounds [s] 
  (cond (scene? s) (scene-bounds s)
        (satisfies? IShape s) (shape-bounds s)
        :else unbounded))

(defn conj-scenes
  "Conjoins child scenes onto the parent scene, returning a composite scene."
  [parent scenes]
  (reduce conj-scene parent scenes))  
    
(defn as-vec [itm] (cond (vector? itm) itm
                         (map? itm) [itm]
                         (coll? itm) itm
                         :else (vector itm)))
(defn scene-zipper
  "Creates a zipper structure, from clojure.zip, which allows traversal, editing
   and updating of the scene in a purely functional manner."
  [s & {:keys [branchf] 
        :or   {branchf composite?}}]
  (zip/zipper scene? (comp as-vec child-scenes) conj-scenes s))  

(defn scene-seq 
  [s]
  (tree-seq scene? (comp as-vec child-scenes) s))

(defn scene-paths
  "Extracts paths to leaf nodes in the scene, where the path is a series of 
   transforms required to render the leaf node."
  [s]
   (->> (scene-zipper s)
     (iterate zip/next)
     (take-while #(not (zip/end? %)))
     (filter #(not (scene? %)))
     (map (fn [loc] (zip/node loc))))) 
    

(def empty-scene  {:bounds unbounded :scene nil})

(defn make-scene
  "Creates a nested scene from one or more child scenes, with scene bounds 
   derived from child bounds.  Optionally, we can pass in the bounds,  
   if we already know ahead of time what they may be."
  ([]          empty-scene)  
  ([xs] (make-scene (group-bounds (map get-scene-bounds xs)) xs))
  ([bounds xs] {:bounds bounds :scene  xs}))

(defn ->group 
  [& xs]
  (make-scene xs))

(defn render
  "Render scene onto graphics context g.  If scene is a primitive IShape, 
   draw-shape from the IShape protocol is called, else draw from the IScene 
   interface is called.  We use render as an auxillary function to allow us 
   to differentiate between primitive shapes and composite scenes.  You will
   not see recursive calls to draw in any IScene's implementation of draw, but 
   calls to render instead, which allows us to invoke draw-shape."
  [scene ^Graphics2D g]
  (if (satisfies? IShape scene)
    (draw-shape scene g)
    (draw scene g)))

(defn- sequential-draw
  "Draw all scenes in s sequentially, onto g."
  [s ^Graphics2D g]
  (loop [coll s]
    (if-let [scn (first coll)]
      (do 
        (render (first coll) g)
        (recur (rest coll))))))

;note, we're currently using bounding volume hierarchies for the scenegraph.
;we could just as easily create a quad-scene, which uses quadtrees to store 
;its scenes.  When adding a child scene, we simply modify the conj-scene 
;behavior to consult the quadtree as to where the child should go. 
;still thinking on this one.

(extend-protocol IScene
  nil
  (draw [s g] nil)
  (child-scenes [s] nil)
  (conj-scene [parent child] (make-scene [child]))
  (scene-bounds [s] unbounded)  
  
  clojure.lang.PersistentArrayMap
  (draw [s g] (sequential-draw (:scene s) g))
  (child-scenes [s] (get s :scene))
  (conj-scene [parent child] (make-scene (conj-scene (:scene parent) child) 
                                         (group-bounds 
                                           (map get-scene-bounds 
                                                [parent child]))))
  (scene-bounds [s] (:bounds s)))
  
;We make vectors, lists, and sequences look like composite scenes by extending 
;the IScene protocol to them, and using sequential draw for their implementation
;of draw.  This really cleans up the calling code and makes the scenegraphs 
;look cleaner.  
;(extend-protocol IScene
;  nil
;  (draw [s g] nil)
;  (more-scenes [s] nil)
;  (conj-scene [parent child] [child])
;  (scene-bounds [s] unbounded)
;  
;  clojure.lang.PersistentVector
;  (draw [s g] (sequential-draw s g))
;  (more-scenes [s] (when (seq (rest s)) (rest s)))
;  (conj-scene [parent child] (make-scene (conj parent child))
;  (scene-bounds [s] (or (:bounds (meta s)) unbounded)))) 



;We represent a scene as a nested datastructure (duh, this is lisp after all), 
;using clojure records.  Records were chosed due to their speed, nice features,
;and the automatic 'typing' and inline protocol implementation.  Each element
;of the scene graph will have one or more unique arguments for its specific 
;role, with a final argument that consists of the "rest" of the scene, i.e. 
;nil, or some other IScene.  

;Each structure corresponds to an operation on the scene context, which under 
;the hood, is a transformation applied to the mutable Java2D graphics object 
;that the scene is being rendered to.  The scene itself is merely a structure, 
;that is traversed depth-first when called by render with a graphics object 
;as an arg.  Since we're dealing with a mutable Java2D graphics context under 
;the hood, which is essentially a state machine, the rendering implementations 
;for each element of the scene graph are careful to 'undo' their operations 
;on the graphics object after rendering themselves and their children.  This 
;keeps the rendering context effectively immutable, and keps the declared 
;structure of the scene consistent.  

;As we traverse the directed acyclic graph that describes the scene, any path 
;in the graph describes an ordered list of transforms to apply to any graphics 
;context, with leaf or terminal nodes resulting in actual rendered images or 
;shapes.  Since each node in the graph is an IScene, all nodes have a unified
;interpretation of draw from the IScene protocol, and we can draw the entire 
;scene (traverse the graph in order, drawing each node as we go) by calling 
;our render function on the root node of the scene.

;Note - this implementation is somewhat lacking for high-performance apps like 
;2D games or million-point animated data plots, because it's NOT leveraging all
;of the effeciencies of scene-graphs.  Future versions will include spatial 
;information in the scene graph to allow us to query and update (re-render) only
;portions of the scene that have changed.  This ends up being drastically more 
;efficient and scaleable.  For our purposes, on modern hardware the current 
;implementation is fast enough and can handle fairly complex scenes 
;instantaneously.  For games or demand scientific visualization (particularly 
;with Java2D's slooow alpha blending), we'd need to render the scene graph to 
;alternative graphics contexts as well, basically OpenGL.  There are several 
;nice libraries for this, including PulpCore, Slick2D, JOGL, LWJGL, and LibGDX.

(defn scene->img
  "Takes any IScene s, and draws it to a buffered image sized according to its 
   bounds."
  [s & [t]]  
  (let [{:keys [x y width height]} (scene-bounds s)
        trans (if t (get-transparency t) (get-transparency :bitmask))
        ^BufferedImage b (make-imgbuffer (inc width) (inc height)  trans)
        ^Graphics2D g (doto (.getGraphics b)
                            (.translate (* -1 x) (* -1 y)))]
    (do (draw s g)
      b)))   

(defn scene-node [s]
  (if (map? s) (dissoc :scene s)
      s))
;Color allows us to change the color of the scene, which modifies the color 
;of any shapes being drawn.  After the child scenes are drawn, the color context
;reverts to its previous state.
(defrecord color [colorkey scene]
  IScene
  (draw [s g]
        (let [clr (get-current-color g)]
    (do (set-gui-color g (get-gui-color colorkey))
        (render scene g)
        (set-gui-color g clr))))
  (child-scenes [s] scene)
  (conj-scene [s child] (->color colorkey (conj-scene scene child)))
  (scene-bounds [s] (get-scene-bounds scene))
  IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] (get-scene-bounds scene)))

;Translation defines a relative 2D translation.  All children will be affected 
;by the x/y coordinate shift.  As with color, once child scenes are rendered, 
;the graphics context is reset to its pre-translated state by applying an 
;inverse translation to the graphics context. 
(defrecord translation [x y scene]
  IScene 
  (draw [s g] 
      (do (translate-xy g (int x) (int y))
          (render scene g)
          (translate-xy g (int (negate x)) (int (negate y)))))
  (child-scenes [s] scene)
  (conj-scene [s child] (->translation x y (conj-scene scene child)))
  (scene-bounds [s] (shape-bounds s))
    IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] (translate-bounds x y (get-scene-bounds scene))))

;Rotation defines a relative 2D rotation to the scene.  It has a single arg, 
;theta, which defines, in radians, the angle of rotation.  All child scenes are 
;affected by the rotated context of the scene, and the rotation is inverted 
;after children are rendered.
(defrecord rotation [theta scene] 
  IScene 
  (draw [s g]     
      (do (rotate-xy g theta)
          (render scene g) 
          (rotate-xy g (* -1 theta))))
  (child-scenes [s] scene)
  (conj-scene [s child] (->rotation theta (conj-scene scene child)))
  (scene-bounds [s] (shape-bounds s))
    IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] (rotate-bounds theta (get-scene-bounds scene))))

;Fade changes the alpha blending of the scene, defined by the argument alpha.
;Alpha values range between 0.0 (transparent) and 1.0 (opaque).
(defrecord fade [alpha scene]
  IScene 
  (draw [s g] 
	    (let [^AlphaComposite ac (get-composite g)]
	       (do (set-composite g (make-alphacomposite alpha))
             (render scene g)
             (set-composite g ac))))
  (child-scenes [s] scene)
  (conj-scene [s child] (->fade alpha (conj-scene scene child)))
  (scene-bounds [s] (get-scene-bounds scene))
  IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] (get-scene-bounds scene)))

;Scales the scene in two dimensions.  xscale and yscale are typical 
;linear scaling factors (i.e. numbers that coordinates get multiplied by).
(defrecord scale [xscale yscale scene]
  IScene
  (draw [s g]
      (do (scale-xy g (double xscale) (double yscale))
        (render scene g)
        (scale-xy g (double (/ 1  xscale)) (double (/ 1  yscale)))))
  (child-scenes [s] scene)
  (conj-scene [s child] (->scale xscale yscale (conj-scene scene child)))
  (scene-bounds [s] (shape-bounds s))
  IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] (scale-bounds xscale yscale (get-scene-bounds scene))))

(defrecord compound-transform [xt yt xscale yscale alpha theta color scene]
  IScene
  (draw [s g] (draw (->scale xscale yscale 
                     (->fade alpha
                       (->rotation theta
                         (->translation xt yt scene)))) g))              
  (child-scenes [s] scene)
  (conj-scene [s child] (->compound-transform xt yt xscale yscale alpha theta 
                                              color (conj-scene scene child)))
  (scene-bounds [s] (shape-bounds s))
  IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] (scale-bounds xscale yscale 
                      (rotate-bounds theta (get-scene-bounds scene)))))
  
(defn ->transform [scene & {:keys [xt yt xscale yscale alpha theta color]
                            :or {xt 0
                                 yt 0
                                 xscale 1.0 
                                 yscale 1.0
                                 alpha 1.0
                                 theta 0.0 
                                 color :black}}]
  (->compound-transform xt yt xscale yscale alpha theta color scene))

;Masks the scene and any children.  Prevents them from being traversed.
(defrecord hide [scene]
	IScene
	  (draw [s g])
    (child-scenes [s] nil)
    (conj-scene [s child] (->hide (conj-scene scene child)))
    (scene-bounds [s] (shape-bounds s))
  IShape 
	  (draw-shape [s g])
	  (shape-bounds [s] unbounded))

(doseq [k [color translation rotation fade scale compound-transform 
           clojure.lang.PersistentArrayMap]]
  (derive k ::complexraster))

(defmethod rasterize ::complexraster [r t] 
  (scene->img r t))

;We define several combinators for IScenes.  This allows us to define scenes 
;programmatically pretty easily.  It allows for another calling convention that
;implies the 'scene' nature of the operations.  We can just as easily define 
;scenes using the ->[recordname] constructor functions, and nest them together.
;That is, in effect, what these functions are doing.  They provide a nice 
;API for the construction of scenes, which becomes useful in things like 
;functional reactive programs, that can lift functions to perform complicated
;transforms to create new scenes compositionally and purely.

;Note - the end result is homoiconic, in that the data structure produced by 
;the function can be "read" by the reader, and turned right back into a 
;clojure record.  This means that we can trivially serialize our scenes as 
;strings, which are then de-serialized by the reader into lisp structures.  
;We can also apply a transform to any scene, to trivially convert it to 
;XML or JSON output.  Score one for nested records.....

;;NEED TO LOOK AT THESE GUYS AGAIN!
;(defn compose-scene
;  "Return a scene that is the result of composing s1 with s2.  
;   Composition implies that s2 is subordinated, either nested or 
;   conjd, to s1.  If s1 is a sequence (usually a vector in calling code)
;   we just conj s2 onto it.  If it is a map (which all of our scene primitives 
;   are, we nest s2 within s1 by associng s2 to s1's :scene key."
;  [scene1 scene2] 
;    (cond (seq? scene1) (conj scene1 scene2)
;          :else [scene1 scene2]))
;
;(defn push-scene
;  "Return a scene that composes like a stack...caches a sprite of oldscene, 
;   drawing newscene on top of it."
;  ([oldscene newscene trans]
;    (let [composite [oldscene newscene]]       
;      [(->hide oldscene)
;       (make-sprite composite trans 0 0)]))
;  ([oldscene newscene] (push-scene oldscene newscene :bitmask)))

(defn dummy []
  (->hide nil))
  
(defn color-scene
  "Compose a color change with IScene scene."
  [color scene]
  (->color color scene))

(defn translate-scene
  "Compose a translation with IScene scene."
  [x y scene]
  (->translation x y scene))

(defn rotate-scene
  "Compose a rotation with IScene scene."
  [theta scene]
  (->rotation theta scene))

(defn fade-scene
  "Compose a fade with IScene scene."
  [alpha scene]
  (->fade alpha scene)) 

(defn scale-scene
  "Compose a scale effect with IScene scene."
  [xscale yscale scene]
  (->scale xscale yscale scene)) 

;(doseq [k [clojure.lang.PersistentVector  
;           clojure.lang.PersistentList
;           clojure.lang.LazySeq]]
;  (derive k ::sceneraster))
;
;
;(defn scene->img
;  "Takes any IScene s, and draws it to a buffered image sized according to its 
;   bounds."
;  [s t] 
;  (let [{:keys [width height]} (shape-bounds s)
;        ^BufferedImage b (make-imgbuffer width height (get-transparency t))
;        ^Graphics2D g (.getGraphics b)]
;    (do (render s g)
;      b)))   
;
;(defmethod rasterize ::sceneraster [s t] (scene->img s t))
                                     
(defn paint-scene
  "Passes the scene's renderer as the drawing function for a paintpanel from 
   cljgui.gui, and displays the paintpanel on an empty frame."
  ([height width scene]
    (display (empty-frame) (paintpanel height width #(render scene %))))
  ([scene] (paint-scene 500 500 scene)))

(defn simplescene
  "Draws a rudimentary scene across a canvas dimensioned by width and height.
   The scene has a white background, with a red circle drawn in the center, 
   and a black line drawn from the circle radiating out about 20 units length."
  [width height] 
  (let [lineball
        (->color :white
           (->group
             (->rectangle :white 0 0 width height) 
             (->translation (halve width) (halve height) 
                (->group (->circle :red -20 -20 20 20)
                         (->translation 12 12
                            (->rotation (* 3 (/ Math/PI 4))                                 
                               (->line  :black 0 0 0 20)))))))]
    (paint-scene width height lineball)))

(defn ->local-point
  "Create a small circular 'point' of radius = size, relative to an origin at 
   (0,0)"
  [color size] 
  (->circle color (negate size) (negate size) size size))

(defn ->faded-plot
  "Creates a local-point, faded by alpha, and translated 
   to an absolute position at (x,y)."
  [x y alpha shp]
     (->fade alpha
       (->translation x y
          shp)))

(defn fast-plot [color] 
  (let [img (make-sprite (->local-point color 20) :bitmask 0 0)]
    (fn [_ x y alpha] (->faded-plot x y alpha img))))   

(defn spirograph
  "Creates a scene of n circular points plotted in what appears to be 
   a spiraling trajectory.  color determines the points' color, stepsize 
   determines how the distance between steps in the trajectory function, which 
   either contracts [for small sizes] or expands [for larger sizes] the spread
   of the points.  growthf determines how fast the radius of the spiral grows 
   as a function of time (if user supplies (fn [t] 1.0) then only a circle is 
   rendered).  direction describes whether the spiral appears to originate from 
   the edge of the screen, zooming toward the center, or vice versa.  plotf 
   is the renderer for each coordinate of the spiral, typically faded points."
  [n width & {:keys [color stepsize growthf direction plotf] 
                             :or {color :red
                                  stepsize (/ Math/PI 4)
                                  growthf (fn [t] (* 1.10 t))
                                  direction :positive
                                  plotf (fast-plot :red)}}]
  (let [halfwidth (halve width)
        get-step (fn [t] (if (= 0 t) 0
                           (/ t stepsize)))
        scale-factor (if (> (growthf n) halfwidth)
                       (/  halfwidth (growthf n))
                       1.0)
        samples (let [s (take n (iterate (fn [t] (+ stepsize t)) 0))] ;weak
                  (if (= direction :positive) 
                    s
                    (reverse s)))
        alpha  (fn [t] (- 1 (/ (get-step t) n)))
        radius (fn [t] (growthf (/ t stepsize)))
        coords (fn [xorigin yorigin]
                 (let [stepxy 
                        (fn [t]
                          (let [r (radius t)]
                            [(+ xorigin (* r (Math/cos t))) 
                             (+ yorigin (* r (Math/sin t)))]))]
                   (map #(conj (stepxy %) %) samples)))                 
       pointstream  (->group (for [[x y t] (coords 0 0)]  
                               (plotf color x y (alpha t))))]
       (->translation halfwidth halfwidth
         (->scale scale-factor scale-factor pointstream))))

(defn spirograph
  "Creates a scene of n circular points plotted in what appears to be 
   a spiraling trajectory.  color determines the points' color, stepsize 
   determines how the distance between steps in the trajectory function, which 
   either contracts [for small sizes] or expands [for larger sizes] the spread
   of the points.  growthf determines how fast the radius of the spiral grows 
   as a function of time (if user supplies (fn [t] 1.0) then only a circle is 
   rendered).  direction describes whether the spiral appears to originate from 
   the edge of the screen, zooming toward the center, or vice versa.  plotf 
   is the renderer for each coordinate of the spiral, typically faded points."
  [n width & {:keys [color stepsize growthf direction plotf] 
                             :or {color :red
                                  stepsize (/ Math/PI 4)
                                  growthf (fn [t] (* 1.10 t))
                                  direction :positive
                                  plotf (fast-plot :red)}}]
  (let [halfwidth (halve width)
        get-step (fn [t] (if (= 0 t) 0
                           (/ t stepsize)))
        scale-factor (if (> (growthf n) halfwidth)
                       (/  halfwidth (growthf n))
                       1.0)
        samples (let [s (take n (iterate (fn [t] (+ stepsize t)) 0))] ;weak
                  (if (= direction :positive) 
                    s
                    (reverse s)))
        alpha  (fn [t] (- 1 (/ (get-step t) n)))
        radius (fn [t] (growthf (/ t stepsize)))
        coords (fn [xorigin yorigin]
                 (let [stepxy 
                        (fn [t]
                          (let [r (radius t)]
                            [(+ xorigin (* r (Math/cos t))) 
                             (+ yorigin (* r (Math/sin t)))]))]
                   (map #(conj (stepxy %) %) samples)))                 
       pointstream  (make-scene (for [[x y t] (coords 0 0)]  
                               (plotf color x y (alpha t))))]
       (->translation halfwidth halfwidth
         (->scale scale-factor scale-factor pointstream))))

;(defn demo
;  "Renders a spirograph with a user-defined background color, particle count, 
;   stepsize, growthrate, etc.  This lets us create different spirals and view 
;   them interactively."
;  [& {:keys [width background n stepsize growthrate direction] :or 
;               {width 500 
;                background :black
;                n 100
;                stepsize 0.2
;                growthrate 1.1
;                direction :negative}}] 
;  (paint-scene
;    width width
;      [(->rectangle background 0 0 width width)
;       (spirograph n width :stepsize stepsize 
;                           :direction direction 
;                           :growthf (fn [t] (* growthrate t)))])) 

(defn demo
  "Renders a spirograph with a user-defined background color, particle count, 
   stepsize, growthrate, etc.  This lets us create different spirals and view 
   them interactively."
  [& {:keys [width background n stepsize growthrate direction] :or 
               {width 500 
                background :black
                n 100
                stepsize 0.2
                growthrate 1.1
                direction :negative}}]
      (->group 
        (->rectangle background 0 0 width width)
       (make-sprite 
         (spirograph n width :stepsize stepsize 
                     :direction direction 
                     :growthf (fn [t] (* growthrate t))) :bitmask 0 0))) 

(comment	
	
	(def lineball 
	  (->color :white
	   (->translation (halve 500) (halve 500)
	         (->group 
            (->circle :red -20 -20 20 20)
            (->translation 20 20
	            (->line :black -20 -20 20 20))))))
	
	(def line 
	  (->color :white 
	    (->translation (halve 500) (halve 500)                   
	       (->line :black -20 -20 20 20))))                   
	
	(def vertical-line 
	  (->color :black 
	    (->translation (halve 500) (halve 500)
	       (->rotation (* 0.25 Math/PI)                    
	           (->line :black -20 -20 20 20)))))

 (def h-lines 
   (->color :black 
	    (->translation (halve 500) (halve 500)
          (->group (->line :black -20 -20 -20 20)
                   (->line :black -15 -20 -15 20)
                   (->line :black -10 -20 -10 20)))))
 
 (def vertical-lines 
   (->color :black 
	    (->translation (halve 500) (halve 500)
	       (->rotation (* 0.25 Math/PI)
            (->group (->line :black -20 -20 20 20)
                     (->line :black -15 -20 15 20)
                     (->line :black -10 -20 10 20))))))
 
  (def vertical-lines2
   (->color :black 
	    (->translation (halve 500) (halve 500)
	       (->rotation (* 0.25 Math/PI)
            (make-scene [(->line :black -20 -20 20 20)
                         (->line :black -15 -20 15 20)
                         (->line :black -10 -20 10 20)])))))
)

