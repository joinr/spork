;;A collection of basic stastical distributions and means for sampling from them.
;;Implementations based on Law and Kelton.

;;Feb 2017 update: Some of this stuff is in Incanter, so we could unify it at some point.
;;However, the use-case at the time necessitated a boutique implementation.
(ns spork.util.stats
  (:require [spork.util [vectors :as v]]
            [spork.util.numerics :refer :all]
            [spork.util [ranges :as r]]
            [spork.util [general :as gen]])
  (:import [java.util.Random]))

(set! *warn-on-reflection* true)

(def ^:dynamic *rand* clojure.core/rand) 

(def even-samples [3, 6, 7, 8, 8, 10, 13, 15, 16, 20])
(def odd-samples (conj even-samples 22))

(defn quantiles
  "Lazily computes the quantiles, of width q, from xs."
  [q xs]
  (let [xs (vec (sort xs))
        n  (count xs)
        rank (fn [k] (long (Math/ceil (+ (* n (/ k q)) 0.5))))]
    (map #(get xs (dec %)) 
         (take-while #(<= % n) (map rank (iterate inc 1))))))    

(defn median
  "Computs the median of a set of values xs.  Does not assume xs are ordered."
  [xs] (first (quantiles 2 xs)))
(def quartiles   (partial quantiles 4))
(def deciles     (partial quantiles 10))
(def percentiles (partial quantiles 100))

;;a slow mean computation.
(defn mean
  "A simple, but potentially slow computation of the mean."
  [xs] 
  (loop [tot 0.0 
         n   0
         ks  xs]
    (if (empty? ks) (/ tot n)
        (recur (+ tot (first ks))
               (unchecked-inc n)
               (rest ks)))))
(defprotocol IDistribution 
  (^double sample [d rng])
  (^double pdf [d x] 
   "Samples the probability density function from d, using u.")
  (^double cdf [d  x]
   "Samples the cumulative distribution function from d, using u.")
  (^double invcdf [d p]
   "Samples the inverse cumulative distribution function, given p."))

(defn sample! 
  ([dist rng] (sample dist rng))
  ([dist]     (sample dist *rand*)))

(defn ranged-distribution
  "Given an IDistribution d, derives a new distribution that constrains values 
   according to the given range specification, where range-spec is a valid 
   range as defined by spork.util.ranges ."
  [dist range-spec]
  (let [clamp (r/as-range range-spec)]
    (reify IDistribution
      (sample [s rng] (clamp     (sample dist rng)))             
      (pdf    [s  x]  (pdf dist  (clamp  dist x)))        
      (cdf    [s  x]  (cdf dist  (sample dist x)))
      (invcdf [d  p]  (clamp     (invcdf dist p))))))   

(defn no-op [] (throw (Exception. "Not implemented")))

(defn distribute!
  "Provides a generating function of one argument that generates samples." 
  [f] (fn [_] (f)))

(defn ^java.util.Random make-random [^long seed]
  (java.util.Random. seed))

(defprotocol IRNG 
  (^double draw [prng]))

(extend-protocol IRNG 
  java.util.Random
  (^double draw [gen] (.nextDouble gen))
  clojure.core$rand 
  (^double draw [gen] (gen)))

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

(defrecord normald [^double m ^double s]
  IDistribution
  (sample [d rng]  (loop [w 2.0]
                     (let [u1 (draw rng)
                           u2 (draw rng)
                           v1 (dec (* 2.0 u1)) 
                           v2 (dec (* 2.0 u2))
                           wnext (double (+ (square v1) (square v2)))]
                       (if (<= wnext 1.0)
                         (+ m (* s (* v1 (pow (/ (* -2.0 (ln wnext)) wnext) 0.5))))
                         (recur wnext)))))
  (pdf [d x] (no-op))
  (cdf [d x] (no-op))
  (invcdf [d x] (no-op)))

(def unormal (->normald 0.0 1.0))

(defn normal-dist
  "Computes a normal random variable. Ported from Malone's work, ported
   from Simulation, Modeling, & Analysis by Law 8.3.6 on pp. 453-454."
  [m s]
  (fn []
	  (loop [w 2.0]
	    (let [u1 (*rand*)
	          u2 (*rand*)
	          v1 (dec (* 2.0 u1)) 
            v2 (dec (* 2.0 u2))
	          wnext (double (+ (square v1) (square v2)))]
	      (if (<= wnext 1.0)
	        (+ m (* s (* v1 (pow (/ (* -2.0 (ln wnext)) wnext) 0.5))))
	        (recur wnext))))))

;;cauchy distribution, ported from GSL
(defrecord cauchyd [^double scale ^double loc]
   IDistribution 
   (sample [s rng] (invcdf s (draw rng)))             
   (pdf [s  x]   
     (let [u (/ (- x loc) scale)]
       (/ (/ 1.0 (* Math/PI scale)) (+ 1.0 (square u)))))
   (cdf    [s  x]  (+ (/ (Math/atan (/ (- x loc) scale)) Math/PI) 0.5))
   (invcdf [d  p] 
     (+ (* scale (tan (* Math/PI (- p 0.5)))) loc)))   

(def ucauchy (->cauchyd 1.0 0.0))


;;another way of generating cauchy vars....
;;have two random variables that are normally distributed.  If their mean is 
;;0, then we get a cauchy distribution.
(comment ;;still working on this guy. 
	(defn ->normal-ratiod [x y]
	  (let [x (->normald loc 1.0)
	        y (->normald loc 1.0)]        
	   (reify IDistribution 
	     (sample [s rng] (invcdf s (draw rng)))             
	     (pdf    [s  x]   
	       (let  [u (/ (- x loc) scale)]
	         (/ (/ 1.0 (* Math/PI scale)) (+ 1.0 (square u)))))
	     (cdf    [s  x]  (+ (/ (Math/atan (/ (- x loc) scale)) Math/PI) 0.5))
	     (invcdf [d  p]))))
)


(defn rand-cauchy
  "Projects a random variable drawn from a normal cauchy distribution 
   onto the range [0,1]  Acts a drop-in replacement for the standard random 
   variable, which is based on a [0 1] uniform distribution."
  ([] (cdf ucauchy (sample! ucauchy)))
  ([rng] (cdf ucauchy (sample! ucauchy rng))))

;gsl_ran_gaussian_ratio_method (const gsl_rng * r, const double sigma)
;{double u, v, x, y, Q;
;  const double s = 0.449871  ;    /* Constants from Leva */
;  const double t = -0.386595 ;		
;  const double a = 0.19600   ;
;  const double b = 0.25472   ;
;  const double r1 = 0.27597  ;
;  const double r2 = 0.27846  ;
;  do                            
;     /* This loop is executed 1.369 times on average  */
;    {/* Generate a point P = (u, v) uniform in a rectangle enclosing
;        the K+M region v^2 <= - 4 u^2 log(u). */
;      /* u in (0, 1] to avoid singularity at u = 0 */
;      u = 1 - gsl_rng_uniform (r);
;      /* v is in the asymmetric interval [-0.5, 0.5).  However v = -0.5
;         is rejected in the last part of the while clause.  The
;         resulting normal deviate is strictly symmetric about 0
;         (provided that v is symmetric once v = -0.5 is excluded). */
;      v = gsl_rng_uniform (r) - 0.5;
;      /* Constant 1.7156 > sqrt(8/e) (for accuracy); but not by too
;         much (for efficiency). */
;      v *= 1.7156;
;      /* Compute Leva's quadratic form Q */
;      x = u - s;
;      y = fabs (v) - t;
;      Q = x * x + y * (a * y - b * x);
;      /* Accept P if Q < r1 (Leva) */
;      /* Reject P if Q > r2 (Leva) */
;      /* Accept if v^2 <= -4 u^2 log(u) (K+M) */
;      /* This final test is executed 0.012 times on average. */
;      while (Q >= r1 && (Q > r2 || v * v > -4 * u * u * log (u)));
;      return sigma * (v / u)};       /* Return slope */
;    }
(let [s  0.449871  ;    /* Constants from Leva */
      t  -0.386595 ;		
      a   0.19600  ;
      b   0.25472  ;
      r1  0.27597  ;
      r2  0.27846]
  (defn ^double gaussian-rand [r ^double sigma]
    (loop [u (- 1.0 (draw r))
           v (* (- (draw r) 0.5) 1.7156) ;magic constant (sqrt (/ 8 e))
           x (- u s)
           y (- (abs v) t)
           Q (+ (* x x) (* y (- (* a y) (* b x))))]
       (if (not (and (>= Q r1) 
                     (or (> Q r2) (> (* v v) (* -4.0 u u (ln u))))))
         (* sigma (/ v u))
         (recur (- 1.0 (draw r))
                (* (- (draw r) 0.5) 1.7156) ;magic constant (sqrt (/ 8 e))
                (- u s)
                (- (abs v) t)
                (+ (* x x) (* y (- (* a y) (* b x)))))))))
         
;;ported from the GSL, using the Ratio method for the Gaussian
(defrecord gaussiand [^double sigma]
  IDistribution
  (sample [s rng] (gaussian-rand rng sigma))  
  (pdf    [s  x]   
    (let [u (/ x (abs sigma))]
      (* (/ 1.0 (* (sqrt (* 2.0 Math/PI)) 
                   (abs sigma)))
         (exp (/ (* (- u)  u)  2.0)))))
  (cdf    [s  x] (no-op))
  (invcdf [d  p] (no-op)))

(def ugaussian (->gaussiand 1.0))

(defrecord normald [^double mean ^double sigma]
  IDistribution
  (sample [s rng] (gaussian-rand rng sigma))  
  (pdf    [s  x]   
    (let [u (/ x (abs sigma))]
      (* (/ 1.0 (* (sqrt (* 2.0 Math/PI)) 
                   (abs sigma)))
         (exp (/ (* (- u)  u)  2.0)))))
  (cdf    [s  x] (no-op))
  (invcdf [d  p] (no-op)))

(defn gamma-dist
  [^double alpha ^double beta]
  (fn []
	  (if (< alpha 1.0)
	    (let [b (/ (+ E alpha) E)]
	      (loop [x 100.0]
	        (if (not= x 100.0)
	          (* x beta)
	          (let [u1 (*rand*) 
	                u2 (*rand*)
	                p  (* b u1)]
	            (if (< p 1.0)
	              (let [y (double (pow p (/ 1.0 alpha)))]
	                (recur (if (<= u2 (exp (* -1.0 y))) y x)))
	              (let [y (* -1.0 (ln (/ ( - b p) alpha)))]
	                (recur (if (<= u2 (pow y (dec alpha))) y x))))))))
	    (let [a (/ 1.0 (sqrt (dec (* 2.0 alpha))))
	          b (- alpha (ln 4.0))
	          q (+ alpha (/ 1.0 a))
	          th 4.5
	          d (inc (ln th))]
	      (loop [x 100.0]
	        (if (not= x 100.0)
	          (* x beta)
	          (let [u1 (*rand*)
	                u2 (*rand*)
	                v (* a (ln (/ u1 (- 1.0 u1))))
	                y (* alpha (exp v))
	                z (* u2 (pow u1 2.0))
	                w (- (+ b (* q v)) y)]
	            (if (or (>= (- (+ w d) (* th z)) 0.0)
	                    (>= w (ln z)))
	              (recur y)
	              (recur x)))))))))


;;From numerical recipes.
(defn ^double cauchy [^double loc ^double scale]
  (* (/ 1.0 Math/PI) (/ scale (+ (square (- (- (rand) 0.5) loc)) (square scale))))) 


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

;;added to support empirical sampling
(defn non-replacing-samples 
  "Returns a 0 arg function that repeatedly draws from the sample set, and 
   returns a value from xs akin to rand-th.  Every time a sample is drawn, the 
   sample will not have the opportunity to be drawn again until all the "
  [xs]
  (let [sample      (vec xs)]
    (if (== (count sample) 1)  (fn [] (first sample))
      (let [current     (atom sample)
            bound       (count sample)
            upper-bound (atom  bound)
            push!       (fn [from to]
                          (let [v @current]
                            (do (reset! current
                                        (gen/swapv from to v))
                                (swap! upper-bound dec))))]
        (assert (pos? bound) (str "cannot create an empirical sample from nothing : " xs))
        (fn [] 
          (let [ub @upper-bound]
            (if (== ub 1) 
              (do (reset! upper-bound bound)
                  (first @current))
              (let [drawn (rand-int ub)
                    val   (nth @current drawn)]
                (do (push! drawn (dec ub))
                    val)))))))))           

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
  


  
  


   
   




                
