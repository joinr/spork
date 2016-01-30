;;An implementation of segmented paths and primitive verticee.  Used as a
;;primitive facility for defining shapes and constructive geometry.
;;Work in progress, should free us from J2d entirely...
(ns spork.graphics2d.primitive
  (:require [spork.util [vectors :as v]])
  (:import  [spork.util.vectors vec2 vec3]))

(defn ^vec2 ->point
  ([^double x ^double y] (v/->vec2 x y))
  ([p] (v/->vec2 (v/vec-nth p 0) (v/vec-nth p 1))))

(defn ^double point-x [p] (v/vec-nth p 0))
(defn ^double point-y [p] (v/vec-nth p 1))

(def origin (->point 0.0 0.0))

;;adapted from j2d, not sure i'll need it though.
(defn segment-type [^long id]
  (case id
    0 :close
    1 :cubic
    2 :line
    3 :quad
    4 :move
    5 :even
    6 :nonzero))

(defprotocol ISegment
  (seg-id [s])
  (seg-p1 [s])
  (seg-p2 [s])
  (seg-p3 [s]))

(defmacro seg-op [name args body]
  (let [op-name (symbol (str "seg-" name))]
   `(defn ~name [~@args]
      (if (satisfies? ISegment ~'s)
        (~op-name ~'s)
        ~body))))

(seg-op id [s] (v/vec-nth s 0))
(seg-op p1 [s] (->point (v/vec-nth s 1) (v/vec-nth s 2)))
(seg-op p2 [s] (->point (v/vec-nth s 2) (v/vec-nth s 3)))
(seg-op p3 [s] (->point (v/vec-nth s 4) (v/vec-nth s 5)))

(defrecord seg [^long id ^vec2 p1 ^vec2 p2  ^vec2 p3]
  ISegment
  (seg-id [s] id)
  (seg-p1 [s] p1)
  (seg-p2 [s] p2)
  (seg-p3 [s] p3))

(defn ^seg line-to  [xy]
  (->seg 2 (->point xy) origin origin))
(defn ^seg curve-to [xy control1-xy control2-xy]
  (->seg 1 (->point xy) (->point control1-xy) (->point control2-xy)))
(defn ^seg quad-to  [xy control-xy]
  (->seg 3 (->point xy) (->point control-xy) origin))
(defn ^seg move-to  [xy]
  (->seg 4 (->point xy) origin origin))
(def ^seg close
  (->seg 0 origin origin origin))

(defn quad-at      [s] (p1 s))
(defn quad-through [s] (p2 s))
(defn cube-at      [s] (p1 s))
(defn cube-left    [s] (p2 s))
(defn cube-right   [s] (p3 s))

(defn mid-point [p1 p2]
  (->point (/ (+ ^double (point-x p1) ^double (point-x p2)) 2.0)
           (/ (+ ^double (point-y p1) ^double (point-y p2)) 2.0)))

(defn divide-quad
  "Divides a quad segment into two quad segments."
  [l ctrl r]
  (let [x1 (point-x l)
        y1 (point-y l)
        ctrlx (point-x ctrl)
        ctrly (point-y ctrl)
        x2 (point-x r)
        y2 (point-y r)
        lc (mid-point l ctrl)
        ctrlx1 (point-x lc)
        ctrly1 (point-y lc)
        cr (mid-point ctrl r)
        ctrlx2 (point-x cr)
        ctrly2 (point-y cr)
        new-ctrl  (mid-point lc cr)
        new-ctrlx (point-x new-ctrl)
        new-ctrly (point-y new-ctrl)]
    [x1 y1 ctrlx1 ctrly1 new-ctrlx new-ctrly
     new-ctrlx new-ctrly ctrlx2 ctrly2 x2 y2]))

(defn ^doubles seg->doubles [^seg s]
  (double-array (into [(:id s) (point-x (:p1 s)) (point-y (:p1 s))
                               (point-x (:p2 s)) (point-y (:p2 s))
                               (point-x (:p3 s)) (point-y (:p3 s))])))

(defn ^seg doubles->seg [^doubles xs]
  (assert (= (alength xs) 7) "Expected a seven-element array.")
  (->seg (aget xs 0)
         (->point (aget xs 1) (aget xs 2))
         (->point (aget xs 3) (aget xs 4))
         (->point (aget xs 5) (aget xs 6))))

;;a path is defined as a sequence of segments.
;;so...anything can be a path if it can provide a sequence of
;;segments...

(defprotocol IPath
  (get-path [s] "Return a sequence of segments that define the shape."))

(defrecord path [segments]
  IPath
  (get-path [s] segments))

(defn as-points [xs]
  (cond (number? (first xs)) (map #(apply ->point %) (partition 2 xs))
        (satisfies? v/IDoubleVector (first xs)) xs
        :else (throw (Exception.
                       (str "Cannot coerce to 2d point:" (first xs))))))


;;In progress
(comment
(defn flatten-quadratic-segment
  "If s2 defines a segment that is a quad, then we generate a set of points.
   We recursively subdivide the quadratic curve between (p1 s1), until a
   desired flatness is achieved or a limit is reached."
  [limit s1 s2]
  (let [l    (p1 s1)
        r    (quad-at s2)
        ctrl (quad-through s2)
        divide-seg ]
    ))
)

(comment
;;if p2 is a cubic, or a quad, we need to flatten it.
(defn flatten-path [limit s1 s2]
  (case (segment-type s2)
    :cubic (flatten-cubic limit s1 s2)
    :quad  (flatten-quadratic limit s1 s2)
    nil))
)

(defn ->polygon [coords]
  (let [pts (as-points coords)]
    (-> (reduce (fn [acc p] (conj! acc (line-to p)))
              (transient [(move-to (first pts))])
              (rest pts))
        (conj! close)
        (persistent!)
        (->path))))


;;so a general path defines a shape like so:
;gp1 = new GeneralPath();
;gp1.moveTo(50, 10);
;gp1.lineTo(70, 80);
;gp1.lineTo(90, 40);
;gp1.lineTo(10, 40);
;gp1.lineTo(50, 80);
;gp1.closePath();

;;analogously

;(->polygon [50 10
;            70 80
;            90 40
;            10 40
;            50 80])

;;so, all this came about as a result of trying to reify the existing
;;sketch api into something that can be interpreted into a scene
;;(for rendering in piccolo2d and friends).
;;The original api focused on elements defined in canvas/graphics.
;;around the graphicscontext protocol.
;;in truth, MOST of the drawing calls are fixed around calls to
;;supplemental functions in the canvas ns,
;;namely the with-[] series of graphics swaps.
;;the existing api uses (with-blah arg1 arg2 .... ctx)
;;so that we can pipeline the operations through.
;;If we use a protocol-defined context, then we
;;end up with (with-blah ctx arg1 arg2.....) etc.
;;The initial idea was to just implement these
;;with macros and define them per implementation.
;;Another option is to define some way of tagging
;;the graphics operation, via an annotation, so
;;that we can get all the good stuff.
;;Perhaps a better mechanism is just to generate the
;;ast directly, i.e. build the nodes (rather than
;;operate on the graphics canvas itself, recording
;;the instructions as output).
;;This would be more straightforward, and allow us to
;;port to more infrastructures (just define interpreters
;;for the ast to whatever backend).
(defmacro swap-graphics
  [nm ctx-name f new-expr]
  (let [old (gensym (str "old-" nm))
        get (symbol (str "spork.graphics2d.canvas/get-" nm))
        set (symbol (str "spork.graphics2d.canvas/set-" nm))
        ]
    `(let [~old (~get ~ctx-name)]
       (-> (~f (~set ~ctx-name ~new-expr))
           (~set ~old)))))
               
;;original definitions (for now)...
(defmacro with-color*
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the color of the context if necessary, then reverts to
   the original color."
  [color ctx f]
  `(swap-graphics ~'color ~ctx ~f ~color))

(defmacro with-font*
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the color of the context if necessary, then reverts to
   the original color."
  [font ctx f]
  `(swap-graphics ~'font ~ctx ~f (~'spork.graphics2d.font/get-font ~font)))

(defmacro with-translation*
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the translation of the context, applies f, then undoes
   the translation."
  [x y ctx f] 
  `(-> (~f (spork.graphics2d.canvas/translate-2d ~ctx ~x ~y))
       (spork.graphics2d.canvas/translate-2d (* -1 ~x) (* -1 ~y))))

(defmacro with-rotation*
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the rotation of the context, applies f, then undoes
   the rotation."
  [theta ctx f] 
  `(-> (~f (spork.graphics2d.canvas/rotate-2d ~ctx ~theta))
    (spork.graphics2d.canvas/rotate-2d (* -1 ~theta))))

(defmacro with-scale*
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the translation of the context, applies f, then undoes
   the translation."
  [xscale yscale ctx f] 
  `(-> (~f (spork.graphics2d.canvas/scale-2d ~ctx (double ~xscale)  (double ~yscale)))
    (spork.graphics2d.canvas/scale-2d (double (/ 1  ~xscale)) (double (/ 1  ~yscale)))))

(defmacro with-transform*
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the rotation of the context, applies f, then undoes
   the rotation."
  [xform ctx f]
  `(swap-graphics ~'transform ~ctx ~f  ~xform))

(defmacro with-alpha*
  "Given a drawing function f, where f::IGraphicsContext->IGraphicsContext, 
   temporarily changes the alpha blending of the context, applies f, then undoes
   the blend."
  [alpha ctx f]
  `(swap-graphics ~'alpha ~ctx ~f  ~alpha))

(defmacro with-stroke*
  "Given a drawing function f, where f::IStroked->IStroked, 
   temporarily changes the stroke of the context, applies f, then undoes
   the blend."
  [stroke ctx f]
  `(swap-graphics ~'stroke ~ctx ~f  ~stroke))
(defn body [nm args]
  (let [ex (symbol (str nm "*"))]        
    `(~nm [~@args]
          ~(macroexpand-1 (macroexpand-1 `(~ex ~@args))))))

(def default-graphics-implementation
  {:alpha       (body 'with-font '[alpha ctx f]) 
   :stroke      (body 'with-stroke '[stroke ctx f])
   :transform   (body 'with-transform '[xform ctx f])
   :rotation    (body 'with-rotation '[theta ctx f])
   :scale       (body 'with-scale  '[xscale yscale ctx f])
   :translation (body 'with-translation  '[x y ctx f]) 
   :color       (body 'with-color  '[color ctx f]) 
   :font        (body 'with-font '[font ctx f])})

