;Extends the rendering model for drawing shapes into something a bit more 
;robust.  The idea is to allow declarative rendering.  Note, there is quite 
;a substantial library in cljgui.gui, which contains most of the primitive 
;functions used to interact with Java2D, wrapped with Clojure.

(ns spork.cljgui.scene.scene2
  (:use  [spork.cljgui.graphics2d.canvas :only 
          [IShape get-color set-color get-alpha set-alpha draw-shape 
           shape-bounds translate-2d scale-2d rotate-2d with-color 
           with-translation with-rotation with-transform with-alpha 
           with-scale bitmap-graphics]]         
         [spork.protocols.spatial :only [bbox scale-bounds translate-bounds 
                                         rotate-bounds group-bounds get-bounding-box
                                         bound-intersects? unbounded? assoc-bounds
                                         dissoc-bounds get-bounds conj-bounds 
                                    ]])
  (:require [spork.util [toplist :as top]]
            [clojure [zip :as zip]]           
            [spork.cljgui.components [swing :as swing]]
            [spork.cljgui.graphics2d [swing :as jgraphics] [image :as image]]))

;note -> I may be able to just ignore this guy.....wondering if the whole 
;scene protocol is unnecessary. 
(defprotocol IScene
  (draw [s g] "Draws the scene onto graphics context g.")
  (child-scenes [s] "Returns nested child scenes, if any exist.")
  (conj-scene [s child]   
   "Conjoins child to parent, so that when more-scenes is called on parent, 
    child will return as the last element.")
  (scene-bounds [s] "Returns a rectangular bounding box that contains the scene
                     and its children."))

;;The scene context is intended to provide information about every element of 
;;the scene.  Specifically, internal scene nodes, like shape nodes, transforms,
;;alphas, etc.  These nodes are stored in a topograph.  The logical hierarchy 
;;is encoded via arcs in the graph.  Unlike the old process, where we had 
;;elements stored locally within a nested scene structure (via a children field)
;;we now have such information stored in a digraph.  The effect on rendering is 
;;this:  When we render, using a scene context, we start at a node-index in the 
;;scene, the default is :root, and fold over its children.  The context, and 
;;the rendering target, are passed via an accumulator.  So we have a scene-lens
;;function that has access to the local information for the node.  The node's 
;;data and rendering information is then used to enqueue the appropriate 
;;transforms.  We technically don't have to do anything until we hit a primitive
;;renderable, at which point we apply the accumulated render state to the 
;;graphics context, and render the primitive shape.   

(defprotocol ISceneContext
  (draw [ctx nd g] 
    "Render an element of the scene context, nd, to graphics context g.")
  (conj-scene [s child] )
  (child-scenes [ctx index] "Return the children of a node.")  
  (scene-bounds [ctx]))
;  (query-scene [ctx q] "Execute a query against the scene context.")
;  (get-node [ctx nd] "Fetch a node from the context."))
(defprotocol IRenderOp
  (emit-instructions [elem ctx])) 
  
(defn compile-scene
  "Given a scene, return a set of rendering instructions that describe how 
   to draw the scene.  Instructions are phrased in terms of the nodes that 
   compose the scene.  The idea is to have a high level scene description, in 
   the form of a graph, act as an abstract syntax tree for describing how to 
   draw the elements of scene.  A smart interpreter can decide the order to 
   proceed, and can apply said instructions to a graphics context.  So we can
   encode a declarative description of the scene via a graph, and traverse it 
   to decode a set of imperative drawing instructions that a lower order machine
   implements."
  [ctx start-index]
  (for [nd (child-scenes ctx start-index)]
    (let [x (get-node ctx nd)]
      (if (satisfies? IRenderOp x)
        (emit-instructions ctx nd)
        x)))) 

;(defn scene?     [s] (satisfies? IScene s))
;(defn composite? [s] (and (scene? s) (> (count (child-scenes s)) 0)))



;;New...we store scenes in a topological graph. 
(extend-protocol ISceneContext 
  cljgui.toplist.topograph
  (draw [ctx nd g] 
    (reduce (fn [ctx nd] (draw ctx nd g)) g (child-scenes nd)))
  (child-scenes [s] (map top/get-node (top/child-nodes s :root)))
  (conj-scene [s child] (let [id (get-id child)]
                          (top/conj-arc :root id child)))
  (scene-bounds [s] (:bounds (root-node s))))

;do we really want bounding information in here? 
(defn get-scene-bounds [s] 
  (cond (scene? s)            (scene-bounds s)
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

;(defn render
;  "Render scene onto graphics context g.  If scene is a primitive IShape, 
;   draw-shape from the IShape protocol is called, else draw from the IScene 
;   interface is called.  We use render as an auxillary function to allow us 
;   to differentiate between primitive shapes and composite scenes.  You will
;   not see recursive calls to draw in any IScene's implementation of draw, but 
;   calls to render instead, which allows us to invoke draw-shape."
;  [scene g]  
;  (cond (satisfies? IShape scene) (draw-shape scene g)
;        (satisfies? IScene scene) (draw scene g)
;        :else g))

;(defn- sequential-draw
;  "Draw all scenes in s sequentially, onto g."
;  [s g]
;  (loop [gr g
;         coll s]
;    (if (empty? coll) gr
;      (recur (render (first coll) gr) (rest coll)))))

;(defn paint-scene
;  "Passes the scene's renderer as the drawing function for a paintpanel from 
;   cljgui.gui, and displays the paintpanel on an empty frame."
;  ([height width scene]
;    (swing/display (swing/empty-frame) 
;                   (swing/paintpanel height width #(render scene %))))
;  ([scene] (paint-scene 500 500 scene)))

;scene elements
(defprotocol ISceneNode 
  (-render-node [data ctx gr] 
    "Render node data to gr, using scene context ctx.")
;  (-conj-node [data children child] 
;    "Let node influence how children are created.")
  (-node-bounds [data ctx] 
    "Let node determine how scene-bounds are computed."))

(defrecord node [id data children]
  IScene 
  (draw [s g] (-render-node data children g)) 
  (child-scenes [s] children)
  (conj-scene [s child] (-conj-node data children child))
  (scene-bounds [s] (-node-bounds data children)))

;(defn ->leaf [data] (->node data []))
;(defn ->tree [data children] (->node data children))
;(defn leaf? [n] (= (:children n) [])) 
;(defn tree? [n] (not (leaf? n)))

;already have 
;(defn get-node-bounds [nd] 
;(defn conj-node-bounds
  

;(defn conj-node [n1 n2] (assoc n1 :children (conj (get n1 :children) n2))
 
(def default-render '(-render-node [data children gr]  
                         (sequential-draw children gr))) ;do nothing.
;(def default-conj-node '(-conj-node [data children child] 
;                          (->node data (conj children child))))

(def default-conj-node 
  '(-conj-node [data ctx child] 
               (let [bounds (get data :bounds)]
                 (->node (assoc data :bounds 
                            (if bounds 
                              (group-bounds [bounds (get-scene-bounds child)])
                              (get-scene-bounds child))) 
                       (conj children child)))))

(def default-node-bounds '(-node-bounds [data children] (get data :bounds)))

(def leaf-conj-node '(-conj-node [data children child] 
                       (throw 
                         (Exception. "Leaf node, cannot contain children."))))

(defn leaf-fields [fields] 
  (if (some (fn [fld] (= fld 'bounds)) fields)
    (conj fields 'bounds))) 

(def scene-node-defaults 
  {:-render-node   default-render 
   :-conj-node     default-conj-node      
   :-node-bounds   default-node-bounds})  

(extend nil ISceneNode 
  scene-node-defaults)

(def empty-node (->node nil []))

;(defnode translation [x y]
(defmacro defnode [name fields & {:keys [-render-node -conj-node -node-bounds]
                                  :or {-render-node  default-render
                                       -conj-node    default-conj-node 
                                       -node-bounds  default-node-bounds}}]
  (let [rector (symbol (str name "-node"))
        make  (symbol (str "->" name))]
    `(do (defrecord ~rector ~fields
           ~'ISceneNode            
             ~-render-node 
             ~-conj-node 
             ~-node-bounds)
         (~'defn ~make [~@fields ~'& ~'xs]
            (reduce conj-scene 
                    (~'->node 
                      (~(symbol (str "->" rector)) ~@fields) '[] )                    
                   (~'vec ~'xs))))))            

;Translation defines a relative 2D translation.  All children will be affected 
;by the x/y coordinate shift.  As with color, once child scenes are rendered, 
;the graphics context is reset to its pre-translated state by applying an 
;inverse translation to the graphics context. 
(defnode translation [x y]  
  :-conj-node 
  (-conj-node [data children child] 
      (let [bounds (get data :bounds)]
        (->node (assoc data :bounds 
                 (let [child-bounds (translate-bounds (:x data) (:y data) 
                                        (get-scene-bounds child))]
                   (if bounds 
                     (group-bounds [bounds child-bounds])
                     child-bounds))) 
                (conj children child))))
  :-render-node 
  (-render-node [data ctx gr] 
      (with-translation (int x) (int y) gr
        #(sequential-draw (children %)))))

(def ctx empty-scene)
(->translation 10 10)
(def tr {:id :shift10 
         :op :with-translation [10 10]
         [:a :b :c]})

  

;Color allows us to change the color of the scene, which modifies the color 
;of any shapes being drawn.  After the child scenes are drawn, the color context
;reverts to its previous state.
(defnode color [colorkey]
  :-render-node
  (-render-node [data children gr]
    (with-color colorkey gr
      #(sequential-draw children %))))

;Rotation defines a relative 2D rotation to the scene.  It has a single arg, 
;theta, which defines, in radians, the angle of rotation.  All child scenes are 
;affected by the rotated context of the scene, and the rotation is inverted 
;after children are rendered.
(defnode rotation [theta]
  :-conj-node 
  (-conj-node [data children child] 
      (let [bounds (get data :bounds)]
        (->node (assoc data :bounds 
                 (let [child-bounds (rotate-bounds (:theta data) 
                                       (get-scene-bounds child))]
                   (if bounds 
                     (group-bounds [bounds child-bounds])
                     child-bounds))) 
                (conj children child))))
  :-render-node
  (-render-node [data children gr]
    (with-rotation theta gr
      #(sequential-draw children %))))

;Fade changes the alpha blending of the scene, defined by the argument alpha.
;Alpha values range between 0.0 (transparent) and 1.0 (opaque).
(defnode fade [alpha]
  :-render-node
  (-render-node [data children gr]
    (with-alpha alpha gr
      #(sequential-draw children %))))

;Scales the scene in two dimensions.  xscale and yscale are typical 
;linear scaling factors (i.e. numbers that coordinates get multiplied by).
(defnode scale [xscale yscale]
  :-conj-node 
  (-conj-node [data children child] 
      (let [bounds (get data :bounds)]
        (->node (assoc data :bounds 
                 (let [child-bounds (scale-bounds 
                                      (:xscale data) (:yscale data) 
                                      (get-scene-bounds child))]
                   (if bounds 
                     (group-bounds [bounds child-bounds])
                     child-bounds))) 
                (conj children child))))
  :-render-node
  (-render-node [data children gr]
    (with-scale xscale yscale gr
      #(sequential-draw children %))))


;might need to redefine this guy.  probably cause a lot of allocation.
;(defnode transform [xt yt xscale yscale alpha theta color]
;  :-render-node
;  (-render-node [data children gr]
;      (render (->scale xscale yscale 
;                 (->fade alpha
;                   (->rotation theta
;                       (->translation xt yt children)))) gr)))              

;this covers basic shapes and images.
(defnode shape [shape-data bounds]
  :-conj-node 
  (-conj-node [data children child] 
      (throw (Exception. "Leaf node, cannot contain children.")))
  :-render-node
  (-render-node [data children gr] (draw-shape shape-data gr)))

;if we define bounds upon construction, then we can compute and extend the 
;bounds on-line...
(defnode grouping [bounds]  
  :-conj-node  ;group node will extend its bounds.
  (-conj-node [data children child]
              (let [bounds (:bounds data)]
                (->node (assoc data :bounds 
                               (if bounds 
                                 (group-bounds [bounds (get-scene-bounds child)])
                                 (get-scene-bounds child))) 
                 (conj children child))))
  :-render-node 
  (-render-node [data children gr]  (sequential-draw  children gr)))

(def empty-group (->grouping  nil)) 

(defn ->group [& xs]
  (reduce conj-scene empty-group xs))            

(defn get-children [s] 
  (if (satisfies? IScene s)
    (child-scenes s)
    nil))

(defn scene-zipper
  "Creates a zipper structure, from clojure.zip, which allows traversal, editing
   and updating of the scene in a purely functional manner."
  [s & {:keys [branchf] 
        :or   {branchf composite?}}]
  (zip/zipper scene?  
              get-children 
              (fn [nd xs] (->node (:data nd) xs)) 
              s))
  
(defn get-node [s]  (or (:data s) 
                      s))
(defn scene-dfs [s]
  (map (comp get-node zip/node) (take-while #(not (zip/end? %))  
              (iterate #(zip/next %) (scene-zipper s)))))

(defn filter-scene
  "Filter the scene, according to a f, which returns a - likely
   - smaller scene.  For each node, if f is false, the node (and any children)
   will be pruned from the scene."  
  [f scene]
  (loop [sz (scene-zipper scene)]
    (if (zip/end? sz)
      (zip/root sz)
      (if (f (zip/node sz)) 
        (recur (zip/next sz))
        (recur  (zip/next (zip/remove sz)))))))
      
(defn map-scene
  "Map function f to each node of the scene.  Preserves the structure of the 
   scene."  
  [f scene]
  (loop [sz (scene-zipper scene)]
    (if (zip/end? sz)
      (zip/root sz)
      (recur (zip/next (zip/edit sz f))))))

(defn scene-bvh [scene]
  (map-scene (fn [nd]
                (->node (get-scene-bounds nd)
                        (get-children nd))) scene))

(defn reduce-scene [f init scene] 
  (loop [sz (scene-zipper scene)]
    (if (zip/end? sz)
      (zip/root sz)
      (recur (zip/next (zip/edit sz f))))))

(defn cull-scene-bvh
  "Excutes a depth-first bounding-volume-hierarchy cull against the scene.
   Given bounds, possibly a bounding box indicating a user selection, 
   searches the scene, depth-first, to find any child scenes that are 
   intersected by the bounds.  If a parent node intersects the bounds, then 
   each of the children are tested as well.  Conversely, if the parent does not
   intersect the bounds, then none of the children are traversed.  
   Returns a scene that reflects the results of the cull."
  [bounds spatial scene]
  (filter-scene (partial bound-intersects? bounds) scene)) 

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

;(defn scene-dfs [scene find-func]

  
(comment
  ;micro testing.
  
	(use '[cljgui.geometry.shapes])
 
	(def lineball 
      (->translation (halve 500) (halve 500)
           (->circle :red -20 -20 20 20)
            (->translation 20 20
               (->line :black -20 -20 20 20))))
 ;;If we define scenes, we can define operations on scenes.
 ;;The semantics follow: 
 ;;Every scene has a root.  
 ;;Nesting a scene within another scene, i.e. adding a scene as a child,  
 ;;means that the root of the child is a child of the parent's root.
 ;;The children of the child are added as children to the parent.
 ;;  So scene definition via composition is a matter of establishing a parent 
 ;;  child relationship, and upon merging scenes, adding the content of the 
 ;;  children, I.E. the nodes of the child, to include the root node, to the 
 ;;  nodes of the parent.  

 ;;For now, there is only one root in a scene.
 ;;In the future, we may allow a scene to be a forest, which would imply 
 ;;independence in rendering order, which would further imply chances to exploit
 ;;parallelism.

 ;;a feasible scene expansion.  We need to build functions that fit this form.
 ;;Of note: at the top, we always have a singular root node.  This is more or 
 ;;less a constant between every scene.  It means we can always compose scenes 
 ;;by finding their roots, and then subordinating one to the other, or by 
 ;;grouping both as children of a new scene root.  Since we enforce a consistent
 ;;rooting by construction, any scene that can be composed will have a way to 
 ;;find a root.  For leaves, like shapes and meshes, we can probably have an 
 ;;implicit root or something.  
(comment  
(->scene :id :root
         :node {}
         :children 
         [(->scene :id :tr1 
                   :node {:op :translation
                          :data {:x 250 :y 250}}
                   :children
                   [(->scene :id :redcircle 
                             :node {:op :shape 
                                    :data {:color :red :x -20 :y -20 :h 20 :w 20}}
                             :children nil)
                    (->scene :id :tr2 
                             :node {:op :translation 
                                    :data {:x 20 :y 20}}
                             :children 
                             [(->scene :id :blackline 
                                       :node {:op :shape
                                              :data {:x -10 :y 20 :h 20 :w 20}}
                                       :children nil)])])]) 
) 
 
(defn genkey [root] (keyword (gensym root)))
(defn get-id [n] 
  (cond (keyword? n) n 
        (map? n) (get n :id (genkey "rand")) 
        :else (genkey "rand")))

(defn conj-scene-element 
  ([ctx parent-idx el]
    (let [child-idx (get-id el)]
      (-> ctx 
        (top/conj-node child-idx el)
        (top/conj-arc parent-idx child-idx))))
  ([ctx el] (conj-scene-element ctx :root el)))

(def unbounded (bbox 0 0 0 0))
(def empty-bounds-node {:bounds unbounded})
(def empty-scene (-> top/empty-topograph 
                   (top/conj-node :root empty-bounds-node)))

(defn ->empty-scene 
  ([id] (top/conj-node empty-scene :root {:id id}))
  ([] (->empty-scene (genkey "scene"))))

;;Scenes only have one root node.
(defn scene-nodes [s] (map top/get-node (top/ordered-walk s :root)))
(defn root-node   [s] (top/get-node s :root))
                
(defn scene-bounds [s] (get (root-node s) :bounds))
;(defn change-root [s k] 
;  (let [r (root-node s)
;        new-nodes (assoc (dissoc s :root))]

(defn walk-nodes [scene]
  (map #(top/get-node scene %) (top/ordered-walk scene :root)))

(defn ->translation [x y]
  {:id (genkey "translation") :op :translation :data {:x x :y y}})

(defn root? [x] (when (map? x) (= :root (get x :id))))
(defn scene-type [x] (if (root? x) :tree :leaf))

(defn chain
  "Given a scene context, registers node2 as a child of node1, and adds 
   the nodes to the scene if they don't exist."
  [scene node1 node2]
    (let [id1 (get-id node1)
          id2 (get-id node2)]
      (-> scene
        (top/conj-node id1 node1)
        (top/conj-node id2 node2)
        (top/conj-arc id1 id2))))

;;add a line to an empty scene.
;;ensure that that empty scene's bounds reflect the newly added line's 
;;bounds.

(add-child empty-scene :root [:black-line (->line :black 10 10 20 20)])
;;==
(->scene :children [(->scene :id :black-line (->line :black 10 10 20 20))]) 

(defn add-child 
  ([scene parent-id [child-id child]]
    (-> scene
      (top/conj-node child-id child)
      (top/conj-arc  parent-id child-id)))
  ([scene child-vec] (add-child scene :root child-vec)))

(defn has-children? [s] 
  (and (top/topograph? s) 
       (> (count (top/child-nodes s :root)) 0)))

(defn ->leaf 
  ([id node] {:id id :node node})
  ([node] (->leaf (genkey "leaf") node)))

(defn merge-scenes
  [base-scene other-scene]
  (if (has-children? other-scene)
    (let [base-root   (top/get-node  base-scene  :root)
          other-root  (top/get-node  other-scene :root)
          other-children  (top/nodes other-scene :root)
          new-root    (merge-roots base-root other-root)
          new-sinks   (merge-sinks (top/-get-sinks )) ]
      
  
  
                

(defn into-scene 
  ([s xs] (reduce chain s xs))
  ([xs] (into-scene empty-scene xs)))
    
(defn conj-scene
  "Merge nodes from child-scene into s.  Create a directed relationship between 
   the root node of s and the root node of child-scene.  Updates the bounds of 
   s with the bounds of the child scene."
  [s child-scene]
  (let [child-nodes (top/nodes child-scene)
        child-root  (:root xs)
        xs (dissoc child-nodes :root)]
    ;first, update the bounds of the root.
    (-> (set-bounds s (group-bounds (scene-bounds s) (scene-bounds child-scene)))
        (update-in [:nodes] merge xs)        
        )))
      
;;only root nodes can be scenes...
;;Therefore, every scene is an indexed tree.
;;Therefore, operations to compose scenes should be a matter of handling a 
;;couple of cases: 
;;  appending a root to a root - 
;;  appending a leaf to a root - simple append operation, updating the bounds.
;;  appending a leaf to a leaf - create a new tree, with leaves as children of root.

(defn scene? [x] (= (get :id x) :root))    

(defn ->scene [& {:keys [:id :node :children] :or 
                  {id (genkey "scene") node {} children nil}}]
  (reduce empty-scene 
          
          ))  

          
(comment 


  (defrecord scene [bounds scenes]
  INode 
  (-node-type [n] :scene)
  (-node-data[n] (dissoc n :scenes))
  (-node-children [n] scenes)                  
  IScene
  (draw [s g] (do (sequential-draw scenes g) g))
  (child-scenes [s] scenes)
  (conj-scene [parent child] (scene. (group-bounds  [bounds (get-scene-bounds child)]) 
                                     (conj scenes child)))
  (scene-bounds [s] bounds)
  IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] bounds))

(def empty-scene  (->scene unbounded []))

(defrecord scene [bounds scenes]
  INode 
  (-node-type [n] :scene)
  (-node-data[n] (dissoc n :scenes))
  (-node-children [n] scenes)                  
  IScene
  (draw [s g] (do (sequential-draw scenes g) g))
  (child-scenes [s] scenes)
  (conj-scene [parent child] (scene. (group-bounds  [bounds (get-scene-bounds child)]) 
                                     (conj scenes child)))
  (scene-bounds [s] bounds)
  IShape 
  (draw-shape [s g] (draw s g))
  (shape-bounds [s] bounds))

(def empty-scene  (->scene unbounded []))

(defn ->group
  "Creates a nested scene from one or more child scenes, with scene bounds 
   derived from child bounds.  Optionally, we can pass in the bounds,  
   if we already know ahead of time what they may be."
  [xs]
  (reduce conj-scene empty-scene xs))

;note, we're currently using bounding volume hierarchies for the scenegraph.
;we could just as easily create a quad-scene, which uses quadtrees to store 
;its scenes.  When adding a child scene, we simply modify the conj-scene 
;behavior to consult the quadtree as to where the child should go. 
;still thinking on this one.

(extend-type nil
  IScene
  (draw [s g] nil)
  (child-scenes [s] nil)
  (conj-scene [parent child] (->group [child]))
  (scene-bounds [s] unbounded)
  INode 
  (-node-type [n] nil)
  (-node-data[n] nil)
  (-node-children [n] nil))                

;; a scene renderer with plugins for pre and post rendering... 
(defn render-with [& {:keys [pre post] 
                      :or {pre nil post nil}}]
  (fn [s g] (let [g (if pre (pre s g) g)
                  g (render s g)]
              (if post (post s g) g))))                  

;(defn render-bounds [s g] 
;  (draw (->visual-bounds scene color) g))
;
;(defn bounds-renderer [bound-color]  
;  (render-with :pre render-bounds))


;rendering -> 
;queue visible objects - by depth - depth is implicit in scene graph, each 
;preceding object has 1 level of depth, so all we need to do is traverse the 
;scene breadth-first. 


;for obj queue
;  [pre obj]
;  [render obj]
;  [post obj]



;;replaced with scene record type.  

;  clojure.lang.PersistentArrayMap
;  (draw [s g] (sequential-draw (:scene s) g))
;  (child-scenes [s] (get s :scene))
;  (conj-scene [parent child] (make-scene (conj-scene (:scene parent) child) 
;                                         (group-bounds 
;                                           (map get-scene-bounds 
;                                                [parent child]))))
  ;(scene-bounds [s] (:bounds s)))
  
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
        trans (if t t :bitmask)
        b (image/make-imgbuffer (inc width) (inc height)  trans)]        
    (do (draw s (translate-2d 
                  (bitmap-graphics b) (* -1 x) (* -1 y)))
      b))) 

  
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
  INode 
  (-node-type [n] :hide)
  (-node-data[n] (dissoc n :scene))
  (-node-children [n] [scene])
	IScene
	  (draw [s g])
    (child-scenes [s] [scene])
    (conj-scene [s child] (hide. (conj-scene scene child)))
    (scene-bounds [s] (shape-bounds s))
  IShape 
	  (draw-shape [s g])
	  (shape-bounds [s] unbounded))

;(defrecord show-bounds [color scene]
;  	IScene
;	  (draw [s g] (render (->visual-bounds (scene-bounds scene) color g))
;    (child-scenes [s] [scene])
;    (conj-scene [s child] (->show-bounds (conj-scene scene child)))
;    (scene-bounds [s] (scene-bounds scene))


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


(comment	
	(use '[cljgui.geometry.shapes])
 
	(def lineball 
      (->translation (halve 500) (halve 500)
         (->group (->circle :red -20 -20 20 20)
                  (->translation 20 20
                     (->line :black -20 -20 20 20)))))
	
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
          (->group [(->line :black -20 -20 -20 20)
                    (->line :black -15 -20 -15 20)
                    (->line :black -10 -20 -10 20)]))))
 
 (def vertical-lines 
   (->color :black 
	    (->translation (halve 500) (halve 500)
	       (->rotation (* 0.25 Math/PI)
            (->group [(->line :black -20 -20 20 20)
                      (->line :black -15 -20 15 20)
                      (->line :black -10 -20 10 20)])))))
 
  (def vertical-lines2
   (->color :black 
	    (->translation (halve 500) (halve 500)
	       (->rotation (* 0.25 Math/PI)
            (->group [(->line :black -20 -20 20 20)
                      (->line :black -15 -20 15 20)
                      (->line :black -10 -20 10 20)])))))
  
  (defn simplescene
  "Draws a rudimentary scene across a canvas dimensioned by width and height.
   The scene has a white background, with a red circle drawn in the center, 
   and a black line drawn from the circle radiating out about 20 units length."
  [width height] 
  (let [lineball
        (->color :white
           (->group [(->rectangle :white 0 0 width height) 
                     (->translation (halve width) (halve height) 
                        (->group [(->circle :red -20 -20 20 20)
                                  (->translation 12 12
                                  (->rotation (* 3 (/ Math/PI 4))                                 
                                    (->line  :black 0 0 0 20)))]))]))]
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
       pointstream  (apply ->group (for [[x y t] (coords 0 0)]  
                               (plotf color x y (alpha t))))]
       (->translation halfwidth halfwidth
         (->scale scale-factor scale-factor pointstream))))

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
  (paint-scene
    width width
      (->group (->rectangle background 0 0 width width)
               (spirograph n width :stepsize stepsize 
                            :direction direction 
                            :growthf (fn [t] (* growthrate t)))))) 
  
(defn demo2
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
      (paint-scene 
        (->group 
          [(->rectangle background 0 0 width width)
           ;(make-sprite 
             (spirograph n width :stepsize stepsize 
                         :direction direction 
                         :growthf (fn [t] (* growthrate t))) ;:bitmask 0 0)])))
           ]
           ))))
)
