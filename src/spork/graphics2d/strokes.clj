;;A wrapper for some strokes, for stylized drawing.
(ns spork.graphics2d.strokes
  (:import  [java.awt Shape Stroke BasicStroke ]
            [java.awt.geom  GeneralPath PathIterator FlatteningPathIterator]))

(defn ^FlatteningPathIterator shape->path 
  ([^Shape s ^Double flatness]
    (FlatteningPathIterator. (.getPathIterator s nil) flatness))
  ([^Shape s] (shape->path s 1.0)))  


(defn ^Shape shape->stroked [^Shape s] (.createStrokedShape (BasicStroke. 10) s))   
(defn path-done? [^PathIterator p] (.isDone p))

(defn ^doubles ->coords [& xs]
  (double-array (take 6 (if (seq xs) xs (repeat 0.0)))))
(def path-types {PathIterator/SEG_MOVETO :move-to
                 PathIterator/SEG_CLOSE  :close 
                 PathIterator/SEG_LINETO :line-to})                        

(defrecord segment [path-type ^long type ^doubles coords])    
(defn ^segment path->segment 
  ([^PathIterator it ^segment acc]  (if (path-done? it) nil
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
          PathIterator/SEG_MOVETO
          (let [movex (float (randomize (aget points 0)))
                lastx (float movex)
                movey (float (randomize (aget points 1)))
                lasty (float movey)]
            (do (.moveto the-path (float movex) ^float (float movey))
              (recur movex movey lastx lasty thisx thisy true 0.0)))
          PathIterator/SEG_CLOSE
          (do (aset points 0 (float movex))
            (aset points 1 (float movey))
            (recur movex movey lastx lasty thisx thisy first next))
          PathIterator/SEG_LINETO
          (let [thisx (float (randomize (aget points 0)))
                thisy (float (randomize (aget points 1)))
                dx    (- thisx lastx)
                dy    (- thisy lasty)
                distance (Math/sqrt (+ (* dx dx) (* dy dy)))
                next-nxt
                   (float (if (< distance next)  (- next distance)                 
                     (let [r (/ 1.0 distance)
                           angle (Math/atan2  dy dx)]
                       (loop [distance (float distance)
                              next (float next)]
                         (if (< distance next) (- next distance)
                           (let [x (float (randomize (+ lastx (* next dx r))))
                                 y (float (randomize (+ lasty (* next dy r))))]
                             (do (.lineto the-path  x y)
                                 (recur distance (float (+ next detail))))))))))]
            (recur movex movey thisx thisy thisx thisy false next-nxt))
          (do (.next it)
            (recur movex movey lastx lasty thisx thisy first next)))))))
 
(defn ->jitter-stroke [detail jitter]
  (reify java.awt.Stroke
    (createStrokedShape [this shape] (jittered-path detail jitter shape))))  
