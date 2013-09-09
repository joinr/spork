;;A wrapper for some strokes, for stylized drawing.
;;I've learned through this process that strokes are really just path transforms.
;;Where paths are simple instructions to a small FSM that knows how to draw 
;;primitive shapes.  The FSM understands commands like lineto, moveto, close. 
;;The FSM is extended with instructions for cubeto, quadto, and more.  As a 
;;consequence, ANY shape can be defined by a path.  Paths are the assembly of 
;;geometry, and are susceptible to transformations and other things.  That's 
;;where strokes come in, which are path-level transforms.  Strokes traverse a 
;;path, and either change coordinations, or add new instructions to the path.
;;It's almost like a shader in the 3d graphics pipeline...
;;Where triangles are primitives in 3d graphics, segments are primitives in 2d.
;;We can probably build a primitive abstraction to get at this....it would make
;;bridging the two much easier.
(ns spork.graphics2d.strokes
  (:import  [java.awt Shape Stroke BasicStroke ]
            [java.awt.geom  GeneralPath PathIterator FlatteningPathIterator
             ]))

(defn ^FlatteningPathIterator shape->path 
  ([^Shape s ^Double flatness]
    (FlatteningPathIterator. (.getPathIterator s nil) flatness))
  ([^Shape s] (shape->path s 1.0)))  


(defn ^Shape shape->stroked [^Shape s] (.createStrokedShape (BasicStroke. 10) s))   
(defn path-done? [^PathIterator p] (.isDone p))

(defn ^doubles ->coords [& xs]
  (double-array (take 6 (if (seq xs) xs (repeat 0.0)))))

(def empty-coords 
  (let [xs (double-array 6)]
    (^doubles fn [] (aclone xs))))

(def path-types {PathIterator/SEG_MOVETO :move-to
                 PathIterator/SEG_CLOSE  :close 
                 PathIterator/SEG_LINETO :line-to})                        

(defrecord segment [path-type ^long type ^doubles coords])    
(defn ^segment path->segment 
  ([^PathIterator it ^segment acc]  
    (if (path-done? it) nil
      (let [pts (:coords acc)
            res (.currentSegment it pts)
            path-type (get path-types res res)]
        (-> (assoc acc :type res)
          (assoc :path-type path-type)))))
  ([^PathIterator it] (path->segment it (->segment nil 0 (->coords)))))

(defn path-seq [^PathIterator it]
  (if-let [seg (path->segment it)]
    (lazy-seq (cons seg (path-seq (doto it (.next)))))))  

(defn path-vec [^PathIterator it]
  (loop [acc (transient [])
         it  it]
    (if (path-done? it) (persistent! acc)
      (recur (conj! acc (path->segment it))
             (doto it (.next))))))

(defn ^float randomize 
  ([^double amplitude ^double x]  (float (+ x (- (* (rand) amplitude) 1.0))))
  ([^double x] (float (randomize 2.0 x))))

;;Path basics: 
;;MoveTo initializes a subpath at a coordinate. 
;;LineTo, QuadTo, CubeTo, define an extension of the path's current coordinate
;;to an outbound coordinate, perhaps with a control point (or two). 
;;Winding rules determine how to perform hit testing on paths, to support 
;;geometric queries on the general path. 

GeneralPath path = new GeneralPath();
path.moveTo(100,100); ;;initialize the path. 
path.lineTo(300,205); ;;draw a line from previous (100,100) to 300,205
path.quadTo(205,250,340,300);; draw a quad line from previous (300,205), 
                            ;; to 205,250, with CP at 340,300 
path.lineTo(340,350); ;;draw a line from previous, 340,300
path.closePath();

(defn cover-distance [the-path detail distance next dy dx]  
  (if (< distance next)  
    (float (- next distance))                 
    (let [r (/ 1.0 distance)
          angle (Math/atan2  dy dx)]
      (loop [distance (float distance)
             next (float next)]
        (if (< distance next) (float (- next distance))
          (let [x (float (randomize (+ lastx (* next dx r))))
                y (float (randomize (+ lasty (* next dy r))))]
            (do (.lineto the-path  x y)
              (recur distance (float (+ next detail))))))))))

 ;;Note...this is basically just a little state machine.
 ;;We can abstract this much better...
;;Pattered off of Jerry Huxtable's excellent demo code in java.
(defn ^GeneralPath jittered-path [detail jitter ^Shape s]
  (let [^GeneralPath the-path (GeneralPath.)
        ^PathIterator it (shape->path (shape->stroked s))        
        points  (float-array 6)]
    (loop [movex   (float 0.0)
           movey   (float 0.0)
           lastx   (float 0.0) 
           lasty   (float 0.0)
           thisx   (float 0.0)
           thisy   (float 0.0)
           first   false
           next    (float 0.0)]
      (if (path-done? it) the-path
        (case (.currentSegment it points)
          PathIterator/SEG_MOVETO ;if we run into a move instruction
          (let [movex (float (randomize (aget points 0))) ;jitter the movement
                lastx (float  movex)
                movey (float (randomize (aget points 1)))
                lasty (float  movey)]
            (do (.moveto the-path (float movex) (float movey))
              (recur movex movey lastx lasty thisx thisy true 0.0)))
          PathIterator/SEG_CLOSE ;if we close 
          (do (aset points 0 (float movex))
              (aset points 1 (float movey))
              (recur movex movey lastx lasty thisx thisy first next))
          PathIterator/SEG_LINETO
          (let [thisx (float (randomize (aget points 0))) ;extract the x + jitter
                thisy (float (randomize (aget points 1))) ;extract the y + jitter
                dx    (- thisx lastx) ;how far we jittered horizontally
                dy    (- thisy lasty) ;how far we jittered vertically
                distance (Math/sqrt (+ (* dx dx) (* dy dy))) ;total distance
                next-nxt (cover-distance the-path detail distance next dx dy)]
            (recur movex movey thisx thisy thisx thisy false next-nxt))
          (do (.next it)
            (recur movex movey lastx lasty thisx thisy first next)))))))
 
(defn ->jitter-stroke [detail jitter]
  (reify java.awt.Stroke
    (createStrokedShape [this shape] (jittered-path detail jitter shape))))  
