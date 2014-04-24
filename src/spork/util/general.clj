;A collection for general utilities.  This is basically a dumping ground for
;small utilities that are undeserving of a seperate library.
(ns spork.util.general
  (:require [clj-tuple :as tup]))

(defn swapv 
  "Swaps the indices in vector v at from and to."
  [from to v] (let [tov (nth v to)] (-> v (assoc to (nth v from)) (assoc from tov))))

(defn swapv! 
  "Swaps the indices in vector v at from and to.  Uses transients"
  [from to v] (let [tov (nth v to)] (-> v (assoc! to (nth v from)) (assoc! from tov))))

;;replacement for memoize, much much faster for small cache lookups.
;;clojure.core/memoize actually uses varargs, creates a RestFn, and 
;;require using equiv to lookup the hashed value.  This guy uses 
;;a much more efficient key mechansim, Zach Tellman's tuple, which
;;is - at LEAST - 4 times faster than the default varargs stuff.
(defmacro memo-fn 
  "Creates a memoized function, like defn, using a FINITE number of 
   arguments.  Intended for use with 1 to 6 arguments. Allows 
   arg masking, as with _ in defn."
  [raw-args body]
  (let [args (filterv (fn [a] (not= a '_)) raw-args)
        n (count args)
        tup  (case n
                   1 (first args)
                   2 `(clj_tuple.Tuple2. ~@args nil)
                   3 `(clj_tuple.Tuple3. ~@args nil)
                   4 `(clj_tuple.Tuple4. ~@args nil)
                   5 `(clj_tuple.Tuple5. ~@args nil)
                   6 `(clj_tuple.Tuple6. ~@args nil)
                   (throw (Exception. "blah")))]
    `(let [hash# (java.util.HashMap.)]
       (fn memoized# [~@raw-args] 
         (let [k# ~tup]
           (if-let [v# (.get hash# k#)]
             v#
             (let [newv# ~body]
               (do (.put hash# k# newv#)
                   newv#))))))))   

;;testing some stuff...
;; (defn the-func [x y & {:keys [positive? blah] :or {positive? true blah 2}}]
;;   (if positive? (+ x y) (- (+ x y))))

;; (defn the-func 
;;   ([x y from to] (+ (+ x y) (+ from to)))
;;   ([x y] (+ (+ x y) (+ 0 0)))
;;   ([x y opts] (+ (+ x y) (+ (:from opts) (:to opts)))))

;; [[x y from to] :or [x y 0 0]] -> 

;; (defn the-func [x y from to]
;;   (let [from (or from 0)
;;         to   (or to 0)]

     

(defmacro with-ns 
  "Evaluate a form in another namespace.  Useful for repl jobs, to keep from 
   swapping between namespaces."
  [nsname & expr]
  (let [current-ns (.name *ns*)]
    `(do 
       (ns ~nsname)
       ~@expr
       (ns ~current-ns))))

;helper functions....I need these somewhere else, since they're universal.


(defn align-by
  "Given a vector, v, generates a sorting function that compares elements using
   the following rules: 
   elements that exist in v have order in v as their key.
   elements that do not exist in v are conjed onto v after they're found.
   align-by makes no guarantee that ks even have to exist in coll, only that 
   if they do, the resulting vector will have values in the same left-
   to-right order."
  [ks coll]
  (let [ordering (atom (reduce (fn [acc kv]
                           (if (contains? acc (first kv)) 
                                acc 
                                (conj acc kv))) {}
                         (map-indexed (fn [i v] [v i]) ks)))
        keyfn (fn [x] (if (contains? @ordering x) 
                        (get @ordering x)
                        (let [y (inc (count @ordering))]
                          (do (swap! ordering assoc x y)
                            y))))]                                                    
    (vec (sort-by keyfn coll))))

(defn align-fields-by
  "Similar to align-by, except it operates on maps of key-val pairs, returning 
   a sorted map."
  [ks m]
  (let [ordering (atom (reduce (fn [acc kv]
                           (if (contains? acc (first kv)) 
                             acc 
                             (conj acc kv))) {}
                               (map-indexed (fn [i v] [v i]) ks)))
        keyfn (fn [x] (if (contains? @ordering x) 
                        (get @ordering x)
                        (let [y (inc (count @ordering))]
                          (do (swap! ordering assoc x y)
                            y))))]
    (into (sorted-map-by (fn [lkey rkey] (compare (keyfn lkey)
                                                  (keyfn rkey))))
          (seq m))))
    
    

(defn clump
  "Returns a vector of a: results for which keyf returns an identical value.
   b: the rest of the unconsumed sequence."
  ([keyf coll]
    (when (seq coll) 
      (let [k0 (keyf (first coll))]
        (loop [acc (transient [(first coll)])
               xs (rest coll)]
          (if (and (seq xs) (= k0 (keyf (first xs))))
            (recur (conj! acc (first xs)) (rest xs))
            [[k0 (persistent! acc)] xs])))))
  ([coll] (clump identity coll)))
                                
(defn clumps
  "Aux function. Like clojure.core/partition-by, except it lazily produces
   contiguous chunks from sequence coll, where each member of the coll, when
   keyf is applied, returns a key. When the keys between different regions
   are different, a chunk is emitted, with the key prepended as in group-by."
  ([keyf coll]
    (lazy-seq
      (if-let [res (clump keyf coll)]
        (cons  (first res) (clumps keyf (second res))))))
  ([coll] (clumps identity coll)))

(defn unfold
  "unfold takes a generating function, f :: state -> state | nil,
   a halting function, halt?:: state -> bool, and an intial state s.  Returns
   a sequence of the application of (f (f (f s))) while not halt?"
  [halt? f s]
  (take-while #(not (halt? %)) (iterate f s)))     

(defn generate
  "generate is akin to unfold, except it uses recursion instead of sequences to
   avoid overhead associated with sequences - if needed.  Bear in mind that
   unfold may be about 5x slower due to uses of seqs (from naive testing), which
   makes generate more useful when performance matters.  Takes a generating
   function, f :: state -> state | nil, a halting function,
   halt?:: state -> bool, and an intial state s."
  [halt? f s]
  (loop [state s
         nextstate s]
    (if (halt? nextstate)
      state
     (recur  nextstate (f nextstate)))))


(defn serial-comparer
  "Given a sequence of functions [f & rest], where f is a function of 
   two arguments that maps to comparison values: 
      |0   -> x = y 
      |< 0 -> x < y
      |> 0 -> x > y     
   composes a function of two arguments that applies each comparison in turn, 
   terminating early if a non-equal comparison is found."
  [comparers]
  (fn [x y] 
    (loop [cs comparers]
      (if (empty? cs) 0
          (let [res ((first cs) x y)]
            (if (zero? res)
              (recur (rest cs))
              res))))))              

(defn orient-comparer
  "If direction is descending, wraps f in a function that negates its comparison 
   result."
  [f direction]
  (if (identical? direction :descending)
      (fn [x y] (* -1  (f x y)))
      f))


;;These functions are really friggin useful for nested tables.
  
;;These should be exported to a lib.
;;They are both faster for nested lookups and associations.
(definline get2 [m from to default]
  `(get (get ~m ~from) ~to ~default))

;;Looking at optimizing this to 
;;allow us to avoid calculating default unless we
;;have to.
(defmacro get2* [m from to default-expr]
  `(let [outer# (get ~m ~from)]
     (if-let [inner#  (get outer# ~to)]
       inner#
       ~default-expr)))

(defmacro hinted-get2 [hint m from to default]
  (let [outer-map (with-meta (gensym "outer") {:tag hint})
        inner-map (with-meta (gensym "inner") {:tag hint})]
    `(let [~outer-map ~m
           ~inner-map (.valAt ~outer-map ~from)]       
       (.valAt ~inner-map ~to ~default))))

(defmacro get-else [m k else-expr]
  `(if-let [res# (get ~m ~k)]
     res#
     ~else-expr))

(definline assoc2 [m from to v]
  `(assoc ~m ~from (assoc (get ~m ~from {}) ~to ~v)))

(definline assoc2! [m from to v]
  `(assoc! ~m ~from (assoc! (get-else ~m ~from (transient {})) ~to ~v)))

(definline transient2 [coll] 
  `(reduce-kv (fn [m# k# v#] 
                (assoc! m#  k# (transient v#)))
           (transient ~coll)  ~coll))

(definline persistent2! [coll]  
  `(let [realized# (persistent! ~coll)]
     (reduce-kv (fn [m# k# v#] 
               (assoc m# k# (persistent! v#)))
             realized# realized#)))

;;It might be nice to pull this out into a protocol at some point...
;;There are other things, which are functors, that can be folded or 
;;reduced.


;;maps over a 2-deep nested collection, ala reduce, using a 
;;function taking 3 arguments : k1 k2 v, the first key, the second
;;key,  and the value associated with [k1 k2] in the structure.
;;Uses internal reduce, so it should be much, much faster than
;;clojure's default map function.
(definline kv-map2 [f coll]  
  `(persistent! 
    (reduce-kv 
     (fn [outer# k1# m2#] 
       (assoc! outer# k1# 
               (persistent! (reduce-kv (fn [inner# k2# v#] 
                                         (assoc! inner# k2# (~f k1# k2# v#)))
                                       (transient {})
                                       m2#))))
     (transient {})
     ~coll)))

;;reduces over a 2-deep nested collection, ala reduce, using a 
;;function taking 4 arguments : acc k1 k2 v, the accumulator, 
;;the first key, the second key, and the value associated with [k1 k2] 
;;in the structure.  Uses internal reduce, so it should be much much 
;;faster than clojure's default reduce function.
(definline kv-reduce2 [f init coll]  
  `(reduce-kv 
    (fn [acc# k1# m2#]       
      (reduce-kv (fn [inner# k2# v#] 
                   (~f inner# k1# k2# v#))
                 acc#
                 m2#))
    ~init
    ~coll))


;;Filters over a 2-deep nested collection, ala reduce, using a
;;function taking 3 arguments: l r v.
(definline kv-filter2 [f coll]
  `(persistent! 
    (kv-reduce2 (fn [acc# l# r# v#] (if (~f l# r# v#) (assoc2! acc# l# r# v#) acc#))
                (transient {}) ~coll)))

;;Flattens a nested 2d collection into a vector of [[k1 k2] v] map entries.
(definline kv-flatten2 [coll]
  `(persistent! 
    (kv-reduce2 (fn [acc# l# r# v#]  (conj! acc# (clojure.lang.MapEntry. (clojure.lang.MapEntry. l# r#) v#)))
                (transient []) ~coll)))

;;Convertes a collection of [[k1 k2] v] entries into a nested map.
(definline kv2 [coll]
  `(loop [xs# ~coll
          acc# (transient {})]
     (if (empty? xs#) (persistent2! acc#)                                                    
         (let [lrv#  (first xs#)
               lr#   (first lrv#)]
           (recur (rest xs#) 
                  (assoc2! acc# (first lr#) (second lr#) (second lrv#)))))))

;;list-spectific optimizations.
(definline cons-head [the-list] `(.first ~(vary-meta the-list assoc        :tag 'clojure.lang.Cons)))
(definline cons-next [the-list] `(.first (.next ~(vary-meta the-list assoc :tag 'clojure.lang.Cons))))



;;More effecient memoization functions.  Clojure's built in memo 
;;memoizes args using a variadic code path, which forces the creation
;;of tons of arrayseqs....this is horrible for small, fast lookups.
(defn memo-1 [f]
  (let [tbl (java.util.HashMap.)]
    (fn [k] (if-let [res (.get tbl k)]
              res 
              (let [res (f k)]
                (do (.put tbl k res)
                    res))))))

(defn mutable-memo 
  "DEPRECATED. Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret))))) 

;;Tuples are indeed faster than other things btw.  This allows us to 
;;have decent access to sparse tables.  Still, nested tables are 
;;faster for most lookups. The functions in spork.util.general 
;;with a 2 postfix highlight this fact.

(defmacro assoc-n [m & idxsv]
  (let [arity (dec (count idxsv))                
        _     (assert (> arity 1) "need at least one key and one value")
        idxs  (butlast  idxsv) ;awesome idiom..thanks
        v     (last idxsv)]
    `(assoc ~m (tup/tuple ~@idxs) ~v)))

(defmacro assoc-n! [m & idxsv]
  (let [arity (dec (count idxsv))                
        _     (assert (> arity 1) "need at least one key and one value")
        idxs  (butlast  idxsv) ;awesome idiom..thanks
        v     (last idxsv)]
    `(assoc! ~m (tup/tuple ~@idxs) ~v))) 

(defmacro get-n! [m & idxs]
  (let [arity (count idxs)
        _     (assert (> arity 1) "need at least one key and one value")]
    `(get ~m (tup/tuple ~@idxs))
    ))
  

(definterface IIntegerPair
  (^long fst [])
  (^long snd []))
  
(deftype intPair [^long x ^long y ^:unsynchronized-mutable ^int hashcode]
  IIntegerPair 
  (^long fst [n] x)
  (^long snd [n] y)
  Object 
  (hashCode [this]
    (when (== -1 hashcode)
      (set! hashcode (int (+ (* 31 (+ 31 x)) y))))
    hashcode)
  (equals  [this that]
    (or (identical? this that)
        (and (instance? spork.util.general.IIntegerPair that)             
             (== x (.fst ^spork.util.general.IIntegerPair that))
             (== y (.snd ^spork.util.general.IIntegerPair that))))))
