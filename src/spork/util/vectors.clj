;;Vector math stuff.  There are several extremely good vector and linear algebra
;;libraries for clojure, many of which support the highly useful core.matrix 
;;API.  For my use case - and partly as a learning experience - I implement a 
;;small library for working with vectors of primitive doubles.  These are as 
;;fast as working with arrays of doubles, but provide a map-like interface, 
;;thanks to the use of records.   
(ns spork.util.vectors)

;;Optimization.  This lets us go faster doing operations on primitives, with 
;;the chance that boned calculations will overflow the data type.
(set! *unchecked-math* true)

;stubs for the funk array classes.  we just store them in vars.
(defmacro double-arr []    `(class (double-array [0])))
(defmacro double-matrix [] `(class (make-array Double/TYPE 0 0)))

(defprotocol IDoubleVector 
  (dimension [v])
  (^double vec-nth  [v idx])
  (set-vec [v idx x])
  (map-vec  [v1 f] [v1 v2 f]))

(defn vec-mult [v1 v2] (map-vec v1 v2 *))
(defn vec-add  [v1 v2]  (map-vec v1 v2 +))
(defn vec-update [v1 kvps]
  (if (and (map? v1) (map? kvps))
      (loop [acc (transient v1)
             ks (keys kvps)]
        (if (empty? ks) 
          acc
          (let [k (first ks)
                v (double (get kvps k))]
            (recur (assoc! acc k v) (rest ks)))))))

;(defn elements [v]
;  (let [l (int (dimension v))]
;    (loop [idx (int 0)
;           acc ]                     

(extend-protocol IDoubleVector
  (double-arr)
  (dimension [v]    (alength ^doubles v))
  (vec-nth  [v idx] (aget ^doubles v idx))
  (set-vec  [^doubles v idx x] (aset ^doubles v idx  (double x)))
  (map-vec
    ([v1 f] 
      (amap ^doubles v1 idx ^doubles ret  
             (double (f (aget ^doubles ret idx)))))               
    ([v1 v2 f] 
      (amap ^doubles v1 idx ^doubles ret  
            (double (f (aget ^doubles ret idx)
                       (vec-nth v2 idx))))))  
  clojure.lang.PersistentVector
  (dimension [v] (count v ))
  (vec-nth   [v idx] (double (nth v idx)))
  (set-vec [v idx x] (assoc v idx x))
  (map-vec ([v1 f] (mapv f v1))               
           ([v1 v2 f] (mapv f v1 v2)))
  clojure.core.Vec 
  (dimension [v] (count v ))
  (vec-nth   [v idx] (double (nth v idx)))
  (set-vec [v idx ^double x] (assoc v idx x))
  (map-vec ([v1 f] (mapv f v1))               
           ([v1 v2 f] (mapv f v1 v1))))

(def  vecs (atom {}))
(defn vec-exists? [n] (contains? @vecs n))

;;this is terrible.
(defn map-vec-indexed [v f]
  (let [idx (atom -1)]
    (map-vec v (fn [x] (do (swap! idx unchecked-inc)
                           (f @idx x))))))

(defmacro defvec
  "Facilitates easy, ad-hoc creation of clojure records that map to 
   n-dimensional primitive vectors.  They can be used interchange-ably, and 
   efficiently, with other numerical vectors."
  [name primitive-type dimension & imps]
  (let [xify    #(symbol (str "x" %))
        xs (map xify (range dimension))
        hint  #(with-meta % {:tag primitive-type})
        hinted-xs (map hint xs)
        idx->x (flatten (map vector (range dimension) xs))
        ctor (symbol (str name "."))
        arrow-ctr (symbol (str "->" name))]
    `(do 
       (defrecord ~name [~@hinted-xs]
         IDoubleVector
         (~'dimension [~'v] ~dimension)     
         (vec-nth [~'v ~'idx] 
           (case  (long ~'idx)
             ~@idx->x         
             (throw (Exception. (str "Index out of bounds: " ~'idx)))))
         (set-vec [~'v ~'idx ~'x]            
           (let [k# (if (integer? ~'idx)
                      (case (long ~'idx) 
                        ~@(flatten 
                            (map-indexed (fn [i x] [i (keyword x)]) xs)))
                      ~'idx)]  
             (assoc ~'v k# (double ~'x))))
         (map-vec [~'v1 ~'f] 
           (~ctor ~@(map-indexed (fn [idx x] `(~'f ~x)) xs)))
         (map-vec [~'v1 ~'v2 ~'f] 
           (~ctor ~@(map-indexed 
                      (fn [idx x] `(~'f (vec-nth ~'v2 ~idx) ~x)) xs)))
         ~@imps)
       (~'swap! vecs ~'assoc ~dimension ~arrow-ctr))))


(defn get-vec-ctor
  "Fetches the constructor for record-backed double vectors.  If no constructor 
   exists, and instance is built at runtime and cached."
  [dimension] 
  (if-let [f (get @vecs dimension)]
    f
    (do (eval `(defvec ~(symbol (str "vec" dimension)) "double" ~dimension))
        (get-vec-ctor dimension))))

(def get-empty-vec 
  "Simple function for cached access to n-dimensional vector constructors.
   Given a dimension, will return an empty vector of said dimension."
  (memoize (fn [n] 
             (apply (get-vec-ctor n) (into [] (take n (repeat 0.0)))))))

(defn make-vec 
  "Quickly initialize vectors of arbitrary dimension.  This is a primary 
   mechanism for creating double vectors."
  ([n] (get-empty-vec n))
  ([n inits] (-> (get-empty-vec n)               
                 (vec-update inits))))  

;;Basic vectors, from 1 to 4 dimensions.
;;This should cover a wide range of problems. 
(doseq [i (map inc (range 4))]
  (eval `(~'defvec ~(symbol (str "vec" i)) "double" ~i)))
