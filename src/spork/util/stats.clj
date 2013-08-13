;this is a quick port of the stats work from megan's file...
;Tom-> I moved this out of the main routines... .it's just a stats library....re
(ns spork.util.stats
  (:import [java.util.Random]))

(defn pow [n x] (Math/pow n x)) 
(defn square [n] (pow n 2)) 
(defn sqrt [n] (Math/sqrt n)) 
(defn ln [n] (Math/log n)) 
(defn exp [n] (Math/exp n)) 
(def  E Math/E)
(defn round [n] (Math/round n))
(defn distribute!
  "Provides a generating function of one argument that generates samples." 
  [f] (fn [_] (f)))


(defn ^java.util.Random make-random [^long seed]
  (java.util.Random. seed))

(defn ^double draw [^java.util.Random gen] 
  (.nextFloat gen))

(def ^:dynamic *rand* clojure.core/rand) 

(defmacro with-generator
  "Temporarily overrides clojure.rand to use a new random number 
   generator seeded by seed.  All calls in the body that use 
   rand will instead use the seeded generator."
  [gen-expr & body]
  `(let [g# ~gen-expr]
     (binding [*rand* (~'fn [] (draw g#))]
       ~@body)))

(defmacro with-seed [n & body]
  `(with-generator (make-random ~n) ~@body)) 

(defn sample-seq 
  ([gen f]
    (if gen 
	    (->> (iterate 
	           (fn [[x gen]]  [(binding [*rand* #(draw gen)] (f))  gen]) 
	           [0 gen])
	      (drop 1)
	      (map first)))
    (repeatedly f))
  ([f] (repeatedly f)))

(defn normal-dist
  "Computes a normal random variable. Ported from Malone's work, ported
   from Simulation, Modeling, & Analysis by Law 8.3.6 on pp. 453-454."
  [m s]
  (fn []
	  (loop [w 2]
	    (let [u1 (*rand*)
	          u2 (*rand*)
	          v1 (dec (* 2 u1)) v2 (dec (* 2 u2))
	          wnext (+ (square v1) (square v2))]
	      (if (<= wnext 1)
	        (+ m (* s (* v1 (pow (/ (* -2.0 (ln wnext)) wnext) 0.5))))
	        (recur wnext))))))

(defn gamma-dist
  [alpha beta]
  (fn []
	  (if (< alpha 1)
	    (let [b (/ (+ E alpha) E)]
	      (loop [x 100]
	        (if (not= x 100)
	          (* x beta)
	          (let [u1 (*rand*) 
	                u2 (*rand*)
	                p (* b u1)]
	            (if (< p 1)
	              (let [y (pow p (/ 1 alpha))]
	                (recur (if (<= u2 (exp (* -1 y))) y x)))
	              (let [y (* -1 (ln (/ ( - b p) alpha)))]
	                (recur (if (<= u2 (pow y (dec alpha))) y x))))))))
	    (let [a (/ 1 (sqrt (dec (* 2 alpha))))
	          b (- alpha (ln 4.0))
	          q (+ alpha (/ 1 a))
	          th 4.5
	          d (inc (ln th))]
	      (loop [x 100]
	        (if (not= x 100)
	          (* x beta)
	          (let [u1 (*rand*)
	                u2 (*rand*)
	                v (* a (ln (/ u1 (- 1 u1))))
	                y (* alpha (exp v))
	                z (* u2 (pow u1 2))
	                w (- (+ b (* q v)) y)]
	            (if (or (>= (- (+ w d) (* th z)) 0)
	                    (>= w (ln z)))
	              (recur y)
	              (recur x)))))))))
(defn beta-dist
  [a1 a2]
  (let [sample-g1 (gamma-dist a1 1)
        sample-g2 (gamma-dist a2 1)]
  (fn []
    (let [y1 (sample-g1)
          y2 (sample-g2)]
      (/ y1 (+ y1 y2))))))

(defn triangle-dist [low mode high]
  (fn []
	  (let [u (*rand*)]
	    (if (<= u (/ (- mode low) (- high low)))
	      (+ low (sqrt (* u (- high low) (- mode low))))
	      (- high (sqrt (* (- 1 u) (- high low) (- high mode))))))))

(defn uniform-dist 
  [low high]
  (fn []
    (+ low (* (*rand*) (- high low)))))

(defn log-normal-dist [m v]
  (fn []
    (let [mm (ln (/ (square m) (sqrt (+ (square m) v))))
          s (sqrt (ln (inc (/ v (square m)))))]
      (exp ((normal-dist mm s)) ))))

(defn exponential-dist 
  [lambda]
  (fn []
    (* (* -1 lambda) (ln (*rand*)))))

(defn log-logistic-dist
  [a b]
  (fn []
    (let [u (*rand*)]
      (* b (pow (/ u (- 1 u)) (/ 1 a))))))

(def lower-case clojure.string/lower-case)

(def distribution-map
  {:normal        normal-dist
   :gamma         gamma-dist
   :beta          beta-dist
   :triangle      triangle-dist
   :triangular    triangle-dist
   :uniform       uniform-dist
   :log-normal    log-normal-dist
   :exponential   exponential-dist
   :log-logistic  log-logistic-dist
   :fix           (fn [n] #(round n))
   "normal"       normal-dist
   "gamma"        gamma-dist
   "beta"         beta-dist
   "triangle"     triangle-dist
   "triangular"   triangle-dist
   "uniform"      uniform-dist
   "log normal"   log-normal-dist
   "log-normal"   log-normal-dist
   "exponential"  exponential-dist
   "log logistic" log-logistic-dist
   "loglogistic"  log-logistic-dist
   "log-logistic" log-logistic-dist
   "fix"          (fn [n] #(round n))})


(defn distribution-error [dname]
  (throw (Exception. (str "distribution " dname " does not exist!"))))

(defn get-distribution
  "Fetches a canonical distribution constructor from a common string name."
  [distribution-name]
  (let [k (if (keyword? distribution-name) 
            distribution-name
            (lower-case distribution-name))
        v (get distribution-map k)]
    (or v (distribution-error k))))

;testing
(comment 

(require '[cljgui.components.swing :as gui])
(require '[util.table :as tbl]) 
(require '[util.excel [core :as xl]])
        
         
(def test-parameters
  ['normal-dist [50 3]
   'exponential-dist [5]
   'log-normal-dist [20 20]
   'log-logistic-dist [2 10]
   'uniform-dist [10 20]
   'beta-dist [2 10]
   'triangle-dist [0 50 100]
   'gamma-dist [2 10]])

(defn sample-with [[k v]]
  (eval `(~'apply ~k ~v)))

(defn test-samples 
  [n & {:keys [gen params] 
        :or   {gen nil
               params test-parameters}}]
  (let [args (map vec (partition 2 test-parameters))
        ks   (map str args)
        m    (zipmap ks (map sample-with args)) 
        make-record (fn [] (into {} 
                                 (for [[k f] m] [k (f)])))]
    (->> (sample-seq gen make-record)
         (take n))))

(defn sample-table 
  [n & {:keys [params seed]}]
    (->> (test-samples n :params params :gen (when seed (make-random seed)))
      (tbl/records->table)))

(defn sample-all [n]
  (sample-table n)) 


(def log-normal-tests [[20 20] [20 40]])
(defn log-normal-samples [n xs]
  (reduce 
    (fn [acc args]
      (->> (sample-seq (fn [] (sample-with ['log-normal args])))
           (take n)
           (vec)
           (assoc acc ['log-normal args])))
    {} xs))

(defn spit-samples [table & {:keys [path] :or {path (gui/select-file)}}]
  (xl/table->xlsx path "Samples" table))
)
  


  
  


   
   




                