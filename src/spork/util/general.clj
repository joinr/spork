;A collection for general utilities.  This is basically a dumping ground for
;small utilities that are undeserving of a seperate library.
(ns spork.util.general)


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
  (if (= direction :descending)
      (fn [x y] (* -1  (f x y)))
      f))


;;These functions are really friggin useful for nested tables.
  
;;These should be exported to a lib.
;;They are both faster for nested lookups and associations.
(definline get2 [m from to default]
  `(get (get ~m ~from) ~to ~default))

(defmacro hinted-get2 [hint m from to default]
  `(let [m# (with-meta ~m {:tag hint})]
     (.valAt (.valAt ~m ~from) ~to ~default)))

(definline assoc2 [m from to v]
  `(assoc ~m ~from (assoc (get ~m ~from {}) ~to ~v)))

(definline assoc2! [m from to v]
  `(assoc! ~m ~from (assoc! (get ~m ~from {}) ~to ~v)))

(definline transient2 [coll] 
  `(reduce (fn [m# kv#] 
             (assoc! m# (first kv#) (transient (second kv#))))
           (transient ~coll) (seq ~coll)))

(definline persistent2! [coll]  
  `(let [realized# (persistent! ~coll)]
     (reduce (fn [m# kv#] 
               (assoc m# (first kv#) (persistent! (second kv#))))
             realized# (seq realized#))))

