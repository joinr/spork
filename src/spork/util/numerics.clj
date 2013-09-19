;;A port of some useful libraries from Numerical Recipes 
;;and other convenience functions for dealing with numerics
;;in a portable manner.
(ns spork.util.numerics)

(defn ^double pow [^double n ^double x] (Math/pow n x)) 
(defn ^double square [^double n] (* n n)) 
(defn ^double sqrt [^double n] (Math/sqrt n)) 
(defn ^double ln [^double n] (Math/log n)) 
(defn ^double exp [^double n] (Math/exp n)) 
(def  E Math/E)
(defn ^long round [^double n] (Math/round n))

;;From numerical recipes v3
(let [cof (double-array [ 57.1562356658629235
                         -59.597960355475912
                          14.1360979747417471
                         -0.491913816097620199
                          0.339946499848118887e-4
                          0.465236289270485756e-4
                         -0.983744753048795646e-4
                          0.158088703224912494e-3
                         -0.210264441724104883e-3
                          0.217439618115212643e-3
                         -0.164318106536763890e-3
                          0.844182239838527433e-4
                         -0.261908384015814087e-4
                          0.368991826595316234e-5])
      rational (double (/ 671 128))]
  (defn ^double gammln [^double xx]  
    (if (<= xx 0.0) (throw (Exception. "negative arg in gammln"))
      (let [x xx              
            tmp (+ x rational)
            tmp (- (* (+ x 0.5) (ln tmp)) tmp)
            ser (loop [idx 0
                       y   xx
                       acc 0.999999999999997092]
                  (if (= idx (alength cof)) acc
                    (let [ynext (unchecked-inc y)]
                      (recur (+ acc (/ ^double (aget cof idx) ynext)) ynext))))]
        (+ tmp (log (/ (* 2.5066282746310005 ser) x)))))))

;(let [fac-table (let [arr (double-array 170)]
;                  (do (aset arr 0 1.0) arr))
;      (loop [acc fac-table
;             idx 0]
;        (if (= idx 171) acc 
;          (do (aset
;      
