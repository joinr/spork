;;A port of some useful libraries from Numerical Recipes 
;;and other convenience functions for dealing with numerics
;;in a portable manner.
(ns spork.util.numerics)
(set! *warn-on-reflection* true)

;;Another option is to make these macros instead of functions, for inlining..
(defn ^double pow [^double n ^double x] (Math/pow n x)) 
(defn ^double square [^double n] (* n n)) 
(defn ^double sqrt [^double n] (Math/sqrt n)) 
(defn ^double ln [^double n] (Math/log n)) 
(defn ^double exp [^double n] (Math/exp n)) 
(def  E Math/E)
(defn ^long round [^double n] (Math/round n))
(defn ^double floor [^double n] (Math/floor n))
(defn ^double ceil [^double n] (Math/ceil n))
(defn ^double abs [^double n] (Math/abs n))
(defn ^double tan [^double n] (Math/tan n))

(defmacro SQR [n] `(* ~n ~n))

(def cofs 
  [-1.26551223 1.00002368 0.37409196 0.09678418
                 -0.18628806 0.27886807 -1.13520398 1.48851587
                 -0.82215223 0.17087277])
  
(defmacro plus-mult [op xs]
  (if (empty? (rest  xs))
    (let [x (first xs)] x)
    `(~'+ ~(first xs) 
         (~'* ~op (plus-mult ~op ~(rest xs))))))  


(defn tst [t] 
  (+ -1.26551223 
     (* t 
       (+ 1.00002368 
          (* t 
             (+ 0.37409196 
                (* t 
                   (+ 0.09678418
                      (* t
                        (+ -0.18628806 
                           (* t 
                              (+ 0.27886807 
                                 (* t 
                                    (+ -1.13520398
                                       (* t 
                                          (+ 1.48851587
                                             (* t 
                                                (+ -0.82215223 
                                                   (* t 0.17087277)))))))))))))))))))
  

;;Gamma Function, Beta Function, Factorials, Binomial Coefficients
;;================================================================

;;Inspired by Numerical Recipes v3.  Double Check the licensing information.
;;I believe this is a-okay, since I have copied no source code, but have ported 
;;implementations into another language entirely.

(let [cof (double-array [57.15623566586292 -59.59796035547549 14.136097974741746 
                         -0.4919138160976202 3.399464998481189E-5 4.652362892704858E-5 
                         -9.837447530487956E-5 1.580887032249125E-4 -2.1026444172410488E-4 
                         2.1743961811521265E-4 -1.643181065367639E-4 8.441822398385275E-5 
                         -2.6190838401581408E-5 3.6899182659531625E-6])      
      rational (double (/ 671 128))]
  (defn ^double gammln [^double xx]  
    (if (<= xx 0.0) (throw (Exception. "bad arg in gammln"))
      (let [x xx              
            tmp (+ x rational)
            tmp (- (* (+ x 0.5) (ln tmp)) tmp)
            ser (loop [idx 0
                       y   xx
                       acc 0.999999999999997092]
                  (if (= idx (alength cof)) acc
                    (let [ynext (unchecked-inc y)]
                      (recur (unchecked-inc idx) 
                             ynext 
                             (+ acc (/ ^double (aget cof idx) ynext))))))]
        (+ tmp (ln (/ (* 2.5066282746310005 ser) x)))))))

(let [^doubles fac-table 
      (let [arr (double-array 170)]
        (do (aset arr 0 1.0) 
          (loop [idx 1]
            (if (= idx (alength arr)) arr
              (do (aset arr idx 
                        (double (* idx (aget arr (unchecked-dec idx)))))
                (recur (unchecked-inc idx)))))))]
  (defn ^double factrl
    "Returns the factorial of n, or n!.  Uses an internal lookup table.  Covers 
    factorials up to 170, which is the upper bound of a double-precision 
    float."
    [n]
    (if (or (< n 0) (> n 170)) (throw (Exception. "factrl out of range"))
      (aget fac-table n))))

(let [ntop     2000
      ^doubles cache (double-array (map #(gammln (inc %)) (range ntop)))] 
  (defn ^double factln
    "Returns the natural log of the n factorial, or ln(n!)."
    [^double n]
    (if (< n 0) (throw (Exception. "negative arge in factln"))
        (if (< n ntop) (aget cache n) (gammln n)))))      
      
(defn ^double bico
  "Returns the binomial coefficient for n and k, or (n choose k).  Uses a 
   tabular lookup for small values of n, otherwise uses a log transform to 
   compute the binomial coefficient."
  [^double n ^double k]  
  (if (or (< n 0.0) (< k 0.0) (> k n)) (throw (Exception. "bad args bico"))
      (if (< n 171.0) 
         (floor (+ 0.5 (/ ^double  (factrl n) 
                        (* ^double  (factrl k) ^double  (factrl (- n k))))))
         (floor  (+ 0.5 (double 
                          (exp (- (double (factln n)) 
                                  (double (factln k)) 
                                  (double (factln (- n k)))))))))))
(defn ^double beta
  "Computes the beta function."
  [^double z ^double w]
  (exp (- (+ (gammln z) (gammln w)) (gammln (+ z w)))))  








(comment 
(let [ncof 28
      ;;Stored Chebyshev coefficients.  We can probably calc these on our own.
      ^doubles cof (double-array
                 [-1.3026537197817094   0.6419697923564902  0.019476473204185836 
                  -0.00956151478680863 -9.46595344482036E-4 3.66839497852761E-4 
                  4.2523324806907E-5  -2.0278578112534E-5 -1.624290004647E-6   
                  1.30365583558E-6     1.5626441722E-8    -8.5238095915E-8
                  6.529054439E-9       5.059343495E-9     -9.91364156E-10  
                  -2.27365122E-10       9.6467911E-11       2.394038E-12       
                  -6.886027E-12         8.94487E-13         3.13092E-13        
                  -1.12708E-13          3.81E-16            7.106E-15
                  -1.523E-15           -9.4E-17             1.21E-16        
                  -2.8E-17])
      erfc-vals [-1.26551223 1.00002368 0.37409196 0.09678418
                 -0.18628806 0.27886807 -1.13520398 1.48851587
                 -0.82215223 0.17087277]]
  (defn ^double erf [^double x]
    (if (>= x 0.0) 
      (- 1.0 (erfccheb x))
      (- 1.0 (erfccheb (- x)))))
  (defn ^double erfc [^double x]
    (if (>= x 0.0) 
      (erfccheb x)
      (- 2.0 (erfccheb (- x)))))
  
  (defn ^double erfccheb
    "Evaluates eq. t*exp[-z^2 + P(t)] using stored Chebyshev coefficients."
    [^double z]
    (assert (< z 0.0) "erfccheb requires non-negative arg.")
    (let [t (/ 2.0 (+ 2.0 z))
          ty (- (* 4.0 t) 2.0)]
      (loop [idx (dec nconf)
             d   0.0
             dd  0.0
             tmp 0.0]
        (if (= idx 0) (* t (exp (+ (* z (- z)) 
                                   (* 0.5 (+ (aget cof 0) (* ty d)))
                                   (- dd))))
          (recur (unchecked-dec idx) 
                 (+ (* t y) (aget cof idx) (- dd))  
                 tmp 
                 d)))))
  
  (defn ^double inverfc
    "Inverse complementary error function.  Returns x s.t. erfc(x) = p for 
     0 >= p >= 2"
    [^double p]
    (cond (>= p 2.0) -100.0 
          (<= p 0.0)  100.0
          (let [pp (if (< p 1.0) p (- 2.0 p))
                t  (sqrt (* -2.0 (ln (/ pp 2.0))))
                x0 (* -0.70711 (- (/ (+ 2.30753 (* t 0.27061))
                                     (+ 1.0 (* t (+ 0.99229 (* t 0.04481)))))
                                  t))]
            (loop [idx 0
                   x   x0
                   err 0.0]
              (if (= idx 2) (if (< p 1.0) x (- x))
                (let [errnxt (- (erfc x) pp)]
                  (recur (unchecked-inc idx)
                         errnext 
                         (+ x (/ err
                                 (- (* 1.12837916709551257 (exp (- (SQR x))))
                                    (* x err)))))))))))

  
  (defn ^double erfcc 
    "Returns complementary error function erfc, with fractional error 
     everywhere less than 1.2 x 10^-7"
    [^double x]
    (let [z (abs z)
          t (/ 2.0 (+ 2.0 z))
          expansion  
          (+ -1.26551223 
             (* t 
                (+ 1.00002368 
                   (* t 
                      (+ 0.37409196 
                         (* t 
                            (+ 0.09678418
                               (* t
                                  (+ -0.18628806 
                                     (* t 
                                        (+ 0.27886807 
                                           (* t 
                                              (+ -1.13520398
                                                 (* t 
                                                    (+ 1.48851587
                                                       (* t 
                                                          (+ -0.82215223 
                                                             (* t 0.17087277))))))))))))))))))
          ans (* t (exp (+ (* (- z) z)
                           expansion)))]
      (if (>= x 0.0) ans (- 2.0 ans)))))
)                           
                           
                           
                           
                          

(comment 

;;Transcribed data for gammaln, the reader is truncating the last 2 digits.  
;  [57.1562356658629235
;   -59.597960355475912
;   14.1360979747417471
;   -0.491913816097620199
;   0.339946499848118887e-4
;   0.465236289270485756e-4
;   -0.983744753048795646e-4
;   0.158088703224912494e-3
;   -0.210264441724104883e-3
;   0.217439618115212643e-3
;   -0.164318106536763890e-3
;   0.844182239838527433e-4
;   -0.261908384015814087e-4
;   0.368991826595316234e-5]
)
