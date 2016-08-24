;A collection of utilities useful when operating on vectors
;effeciently.
(ns spork.util.vector)
(defn vec-filter
  "Vector-specific filter operation.  Since we're using vectors for our
   tables, this should be optimized a bit.  Not sure if it'll pay off though."
  [f v]
  (persistent! (reduce conj! (transient []) (filter f v))))  

(defn vec-vals
  "Returns an in-order vector of the values of persistent map m.
   Coercing vals into a vector using "
  [m]
  (vec (reverse (vals m))))

(defn transient-vector? [v]
  (= type v clojure.lang.PersistentVector$TransientVector))

(defn transient-columns
  "Returns a transient set of columns"
  [cols]
  (reduce (fn [acc v] (conj! acc (transient v)))
                  (transient []) cols))

(defn persistent-columns!
  "Persists the columns from a transient state."
  [tcols]
  (reduce (fn [acc tc]  (conj acc (persistent! tc))) [] (persistent! tcols)))

(defn transpose
  "Given a vector of vectors, changes row-vectors to column-vectors.  
   Fast using transients."
  [rowvectors]
  (let [colcount (count (first rowvectors))
        res 
        (reduce (fn [tcols row] 
                  (reduce (fn [cols [j fld]]  
                            (assoc! cols j (conj! (get cols j) fld)))
                          tcols 
                          (map-indexed vector row)))
                (reduce conj! (transient []) 
                        (take colcount (repeatedly (fn []  (transient [])))))
                rowvectors)]
    (persistent-columns! res)))
