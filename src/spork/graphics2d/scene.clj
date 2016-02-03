;;This is our default implementation of scene-graph nodes.
;;Ideally, we can establish a protocol that's applicable to
;;different backends for scenes (ala piccolo2d and other
;;scene graphs).  Right now, piccolo2d / java2d are our
;;targets.
(ns spork.graphics2d.scene
  (:require [spork.graphics2d.canvas :refer :all]
            [spork.geometry.shapes :refer :all]
            [spork.protocols [spatial :as space]]
            [spork.graphics2d.stroke :as stroke]))

;;the fundamental nodes are
;;transform (translation, scale)
;;state     (alpha, color, stroke)
;;geometry  (rendering instructions / shape / bounds / intersections)
;;What about the phenomenon of parametric nodes?
;;  Where we have many flyweight nodes that are not part of the 
;;  explicit node set. Case in point (repeat-up)
;;  In this case, we basically imply the creation of one or more
;;  nodes that are instances of a parent.  In other words, they
;;  don't exist, but they contribute to the bounds and hit-testing.
;;  We "can" create lightweight instances of the node...
;;  For instance, an array of transforms that reference the
;;  shape.  We use the same node, and when rendering, treat
;;  them as a set of translations to be iterated over.
;;  Alternately, we could provide a reducer that does the
;;  iterating.  Hit detection then means checking the bounds
;;  For each repeated element.

(defmacro get-else [m k alt]
  `(if-let [res# (get ~m ~k) ] res#
          ~alt))

;;for now, stock nodes will be stored in maps.
(extend-protocol IShapeable
  clojure.lang.PersistentArrayMap
  (as-shape [obj] (get-else (.meta ^clojure.lang.PersistentArrayMap obj) :shape (:children obj)))
  clojure.lang.PersistentHashMap
  (as-shape [obj] (get-else (.meta ^clojure.lang.PersistentArrayMap obj) :shape (:children obj))))
                       
(extend-protocol IShape
  clojure.lang.PersistentArrayMap
  (draw-shape [s c] (draw-shape  (get-else (.meta ^clojure.lang.PersistentArrayMap s) :shape (:children s))
                                c))
  (shape-bounds [s] (shape-bounds   (get-else (.meta ^clojure.lang.PersistentArrayMap s) :shape (:children s))))                                  
  clojure.lang.PersistentHashMap
  (draw-shape [s c] (draw-shape  (get-else (.meta ^clojure.lang.PersistentArrayMap s) :shape (:children s))
                                c))
  (shape-bounds [s] (shape-bounds (get-else (.meta ^clojure.lang.PersistentArrayMap s) :shape (:children s)))))
  
;;every node has:
;;local transform
;;bounds => (f child-bounds)
;;children (zero or more nodes)

;;the scene graph communicates transforms and
;;bound information.  That's it.

;;Under this api, we should be able to wrap/extend
;;pnodes and other scenegraph implementations.

;;For instance:
;;if we have a pnode, it already implements the protocol
;;for bounds information, and for 

;;How do we derive custom nodes, i.e. via combinators?
;;currently we reify the shape protocol.
;;This could work for nodes.

;;One idea is to just define a macro that makes nodes.

;;We can simplify a lot of transforms too...
;;if we force every node to have a transform.
;;then we don't have to talk about translation, scale, rotation.
;;On the other hand, what about xy->uv transforms? where we
;;scale and translate independently?

;;We typically won't be doing millions of nodes, especially
;;using something like repeat-up...

;;We can also implement something with mutable bounds and
;;instancing, like a point cloud, if we have a spatial database
;;backing the node, and we can test for intersection in it.
;;For instance, if we want to know if a point intersects
;;the point cloud, we check to see if the point is
;;in the set of points and we're done.

;;This opens up the possibility of having local spatial
;;databases (of which the bvh can be seen as a global spatial
;;db).
(defn with-bounds [bnds v]
  (vary-meta v assoc :shape-bounds bnds))
(definline atom? [x]
  `(instance? ~'clojure.lang.Atom ~x))
;;for now, we store the shape information in meta.
;;this keeps us clean when we go to visually
;;inspect the tree.  Since the shape is actually
;;a compiled object, it keeps the symbolic
;;node separate from the objectified shape.
(defmacro defnode [nm args & {:keys [as-shape]}]
  `(defn ~nm [~@args]
     (vary-meta  (~(if (<= (count args) 16) 'array-map 'hash-map)
                  :type ~(keyword nm)
                  ~@(flatten (for [a args]
                               [(keyword a) a])))
                 assoc :shape ~as-shape)))

;;we might want to keep the shape cached once we
;;compute it...

;;this is just the structure of the scene ast.
;;we can actually ignore how to draw and just stick with
;;the data for now.  Then have the graph define an interpreter.
(defnode translate [x y children]
  :as-shape 
  (if (not (and (atom? x) (atom? y)))
    (if (and (zero? x) (zero? y) ) children
        (reify IShape 
          (shape-bounds [s] (space/translate-bounds x y (shape-bounds children)))
          (draw-shape   [s c] (with-translation x y 
                                c #(draw-shape children %)))))
    (reify IShape 
      (shape-bounds [s] (space/translate-bounds @x @y (shape-bounds children)))
      (draw-shape   [s c] (with-translation @x @y 
                            c #(draw-shape children %))))))
;;so the idea is that we maintain the
;;symbolic interpretation (the map syntax), while
;;compiling/caching the functional representation (the shape)
(defnode scale     [xscale yscale children]
  :as-shape
  (if (not (and (atom? xscale) (atom? yscale)))
    (let [xscale (double xscale)
          yscale (double yscale)]
      (if (and (== xscale 1.0) (== yscale 1.0)) children
          (reify IShape 
            (shape-bounds [s]   (space/scale-bounds xscale yscale (shape-bounds children)))    
            (draw-shape   [s c] (with-scale xscale yscale c #(draw-shape children %))))))
    (reify IShape 
      (shape-bounds [s]   (space/scale-bounds @xscale @yscale (shape-bounds children)))    
      (draw-shape   [s c] (with-scale @xscale @yscale c #(draw-shape children %))))))

(defnode rotate    [theta children]
  :as-shape
  (if (not (atom? theta))
    (reify IShape 
      (shape-bounds [s]   (space/rotate-bounds theta (shape-bounds children)))
      (draw-shape   [s c] (with-rotation theta  c #(draw-shape children %))))
    (reify IShape 
      (shape-bounds [s]   (space/rotate-bounds @theta (shape-bounds children)))
      (draw-shape   [s c] (with-rotation @theta  c #(draw-shape children %))))))

(defnode fade      [alpha children]
  :as-shape
  (if (not (atom? alpha))
    (reify IShape 
      (shape-bounds [s] (shape-bounds children))
      (draw-shape   [s c] (with-alpha  alpha 
                            c #(draw-shape children %))))
    (reify IShape 
      (shape-bounds [s] (shape-bounds children))
      (draw-shape   [s c] (with-alpha  @alpha 
                            c #(draw-shape children %))))))

;;we can have a little interpreter here...
;;thicken can be rewritten in terms of stroke.
;;{:stroke stroke
;; :width width}

(defnode stroke    [stroke children]
  :as-shape
  (let [stroke-by (if (fn? stroke) stroke                       
                      (stroke/stroke-by stroke))]
    (reify IShape
      (shape-bounds [s] (shape-bounds children))
      (draw-shape   [s c]
        (let [strk (get-stroke c)
              new-stroke (stroke-by strk)]
          (with-stroke new-stroke c
            #(draw-shape children %)))))))

;;thicken is really just using another stroke.
;;shows how we can alter the node's behavior by making
;;the node smarter about its args, i.e. taking
;;functions.
(defn thicken [amount children]
  (stroke   (fn [s]  (stroke/widen amount s)) children))             

(defnode color     [color children]
  :as-shape
  (reify IShape
    (shape-bounds [s] (shape-bounds children))
    (draw-shape   [s c]
        (with-color color c
          #(draw-shape children %)))))

;;this is a little hacky atm...
(defnode state     [states children]
  :as-shape
  (reify IShape
    (shape-bounds [s] (shape-bounds children))
    (draw-shape   [s c]        
      (draw-shape children (set-state c states)))))
;;what about custom operations l

;;now we define combinators on these nodes...
;;we should be able to build up more complex expressions...
(defn smooth [shp children] (state  {:antialias true} children))


