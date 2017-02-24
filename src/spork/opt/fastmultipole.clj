;Implementation of Dr. John Harrop's F# fast multipole method for tree
;based force computation, in clojure.

;;Pedagogical port, possibly applications for speeding up
;;approximate neighbor queries and the like for collision
;;detection and more.  2012-ish
(ns spork.opt.fastmultipole)

(defrecord particle [mass radius])
(defn rand-particle 
  ([] (particle. (rand) (rand)))
  ([_] (rand-particle)))

(defn force2 [p1 p2] 
  (let [d (reduce - (map :radius [p2 p1]))]
    (/ (* (:mass p1) (:mass p2)) (* d (Math/abs d)))))  

;(force2 (particle. 1 0.1) (particle. 3 0.8))

;array-based force computation 
(defn array-force [p ps]
  (reduce (fn [f p2] (+ f (force2 p p2))) 0.0 ps))

(defn rand-system [n]
  (let [acc (transient [])]
    (persistent! (reduce conj! acc (take n (repeatedly rand-particle))))))

;this is quadratic....ugh!
;(def atest 
;  (let [origin (rand-particle)
;        sys (rand-system 100)
;        f (array-force origin sys)]
;    {:force f :origin origin :system sys}))  


;---let's look at tree-based force calcluation using the fast multipole method ! 

(defrecord leaf [plist])
(defrecord node [l p r])

(defrecord accu [mp mprp p])

(defn make-leaf [ps]
  (let [sum (partial reduce + 0.0)
        m (sum (map :mass ps))  
        mr (sum (map #(* (:mass %) (:radius %)) ps))]
    (accu. m mr (leaf. ps))))

(defn- key-reduce2 [f k m1 m2]
  (f (get m1 k) (get m2 k)))

(def keyplus (partial key-reduce2 +))
  
(defn make-node [a1 a2]
  (let [mp (keyplus :mp a1 a2)
        mprp (keyplus :mprp a1 a2)]
    (accu. mp mprp (node. (:p a1) (particle. mp (/ mprp mp)) (:p a2))))) 

(defrecord system [lower tree upper])

(def epsilon 0.0000001)

(defn make-partition
  "Recursively bisect the range x0 and x2, and a list of particles 
   known to be in the range, until no particles remain, one 
   particle remains, or the range has shrunk to nothing.  We construct 
   nodes with psuedo particles on the way 'back' from the leaves."
  [x0 ps x2]
  (cond
    (or (empty? ps) 
        (= 1 (count ps))
        (< (- x2 x0) epsilon))
      (make-leaf ps)
    :else 
      (let [x1 (/ (+ x0 x2) 2)
            [ps1 ps2] (split-with #(< (:radius %) x1) ps)]
        (make-node (make-partition x0 ps1 x1)
                   (make-partition x1 ps2 x2)))))

(defn make-system
  "Build an unbalanced particle-tree, with pseudo-particle nodes, 
   across the distance [lower upper] containing particles ps."
  [lower ps upper]
  (system. lower (:p (make-partition lower ps upper)) upper))

(defn- between [x lower upper]
  (and (<= lower x) 
       (> upper x)))

(defn- metric
  "Measure the potential error when using FMM to compute the 
   force on p using pp over range [x0 x2]"
  [p pseudo x0 x2]
  (if (between (:radius p) x0 x2) 
     Double/POSITIVE_INFINITY
   (let 
     [sqr #(* % %)
      pm (:mass p)
      ppm (:mass pseudo)
      pr- (:radius p)
      ppr (:radius pseudo) 
      fmin (/ (* pm ppm) 
              (sqr (- pr- ppr)))
      g (fn [x y] (/ (- ppr x)
                     (sqr (- pr- y))))
      fmax (* (/ (* pm ppm) (- x2 x0)) 
              (- (g x0 x2) (g x2 x0)))]
     (- fmax fmin))))
                        
(defn leaf? [nd] (= leaf (class nd)))
(defn node? [nd] (= node (class nd)))

;this function needs re-looking, and verification.  It appears to be working
;but you never know....
(defn- force-aux [p x0 x2 nd delta]
  (if (leaf? nd)
     (reduce (fn [f p2] (+ f (force2 p p2))) 0.0 (:plist nd))                               
     (if (< (metric p (:p nd) x0 x2) delta) 
	           (force2 p (:p nd))
	           (let [x1 (* 0.5 (+ x0 x2))]
	             (+ (force-aux p x0 x1 (:l nd) delta) 
	                (force-aux p x1 x2 (:r nd) delta)))))) 
          
(defn get-particles [psystem]
  (let [walk (fn walk [nd]
               (if (node? nd)
                 (mapcat walk [(:l nd) (:r nd)])
                 (:plist nd)))]
    (walk (:tree psystem))))            
                        
(defn calc-force
  "Calculate the force on particle p, using psuedo particles from a tree of 
   particles defined by system, across the range [lower upper] defined by 
   system, within a bounded error value delta."
  [p system delta]
    (force-aux p (:lower system) (:upper system) (:tree system) delta))

(def test-particles [(particle. 3.0 0.1) (particle. 1.0 0.8) 
                     (particle. 1.0 0.82)])
(def plist (rand-system 10))
(def s (make-system 0.0 plist 1.0))
(def origin (rand-particle))
(def stats (map (partial calc-force origin s) 
                (map #(Math/exp (* -1 %)) [9 6 3])))
