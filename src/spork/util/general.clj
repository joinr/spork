;;A collection for general utilities.  This is basically a dumping ground for
;;small utilities that are undeserving of a seperate library.
(ns spork.util.general
  (:require [clj-tuple :as tup]
            [spork.util [zipfile :as z]
                        [io :as io]]
            [clojure.pprint :as pprint]))


(defmacro iter [[x xs] & body]
  (let [it (with-meta (gensym "iterable")  {:tag 'java.lang.Iterable})]
    `(let [~it ~xs]
       (loop [r# (.iterator ~it)]
         (when (.hasNext r#)
           (let [~x (.next r#)]
             (do ~@body
                 (recur r#))))))))

(defn ref?
  "Predicate yields true if the obj supports (deref ...)"
  [obj] (instance? clojure.lang.IDeref obj))

(defn collect
  "Given a seq of functions | a function, fs, and a seq xs, 
   maps fs over xs, returning either a sequence of vectors 
  [(fs1 x1) (fs2 x1) ...] or a sequence of (fs x)"
  [fs xs]  
  (let [f (if (coll? fs) (apply juxt fs) fs)]
    (map f xs)))

(defn atom?
  "Quick check to see if x is a clojure.lang.Atom."
  [x] (instance? clojure.lang.Atom x))

(definline empty-string?
  "Determines if input is empty string....slightly optimized version."
  [x]
  `(= ~x ""))

(defn debug-print
  "Prints message and returns obj, like a side-effecting identity."
  [msg obj]
  (do (println msg) obj))

(defn float-trunc
  "Computes a truncated floating-point number from n, 
   where the number is trunctated to places in decimal."
  [n places]
  (let [scale (Math/pow 10 places)]
    (/ (long (* n scale)) scale)))

(defn print-float
  "Convenience function to pretty print floats.."
  [n]
  (pprint/cl-format nil
    "~f" n))


(defn approx-order
  "provide a unified ordering of a known ordering
  and a set of candidate fields.  We want
  to return a vector of field names that
  are sorted according to ordered, derived
  from candidates."
  [ordered candidates]
  (vec (if (seq ordered)
         (let [order (atom (into {} (map-indexed (fn [idx fld]
                                        [fld idx]) ordered)))
          keyf (fn [fld]
                 (if-let [n (get @order fld)]
                   n
                   (let [n (count @order)
                         _ (swap! order assoc fld n)]
                     n)))
          ]
       (sort-by keyf candidates))
       candidates)))


(defmacro case-identical?
  "Like clojure.core/case, except instead of a lookup map, we
   use `condp` and `identical` in an unfolding macroexpansion
   to allow fast case lookups for smaller cases where we may
   beat the o(1) cost of hashing the clojure.core/case incurs
   via its lookup map.  Some workloads are substantially (2-3x)
   faster using linear lookup and `identical?` checks.`"
  [k & kvs]
  (let [n (count kvs)
        _ (assert (if (odd? n) (> n 1) true)
                  "either use even cases or odd with final as a default")
        s (gensym "k")]
    `(condp identical? ~k ~@kvs)))

(defmacro case-identical?
  "Like clojure.core/case, except instead of a lookup map, we
   use `condp` and `identical?` in an unfolding macroexpansion
   to allow fast case lookups for smaller cases where we may
   beat the o(1) cost of hashing the clojure.core/case incurs
   via its lookup map.  Some workloads are substantially (~3x)
   faster using linear lookup and `identical?` checks.

   Caller should be aware of the differences between `identical?`
   and `=` or other structural hashing comparisons.  `identical?`
   is appropriate for object (e.g. pointer) equality between
   instances, and is more restrictive than structural equality
   per `clojure.core/=`; objects may be = but not `identical?`,
   where `indentical?` objects are almost certainly `=`."
  [e & clauses]
  (let [ge      (with-meta (gensym) {:tag Object})
        default (if (odd? (count clauses))
                  (or (last clauses) ::nil)
                  `(throw (IllegalArgumentException. (str "No matching clause: " ~ge))))
        conj-flat   (fn [acc [k v]]
                      (conj acc k v))]
    (if (> 2 (count clauses))
      `(let [~ge ~e] ~default)
      (let [pairs     (->> (partition 2 clauses)
                           (reduce (fn [acc [l r]]
                                     (if (seq? l)
                                       (reduce conj acc (for [x l] [x r]))
                                       (conj acc [l r])))  []))
            dupes    (->> pairs
                          (map first)
                          frequencies
                          (filter (fn [[k v]]
                                    (> v 1)))
                          (map first))
            args     (reduce conj-flat [] pairs)]
        (when (seq dupes)
          (throw (ex-info (str "Duplicate case-identical? test constants: " (vec dupes)) {:dupes dupes})))
        `(let [~ge ~e]
           (condp identical? ~ge
             ~@(if default (conj args (case default ::nil nil default)) args)))))))

;;Derived from clojure.core/case
(defmacro fast-case
   "Drop-in replacement for clojure.core/case that attempts to optimize
    identical? case comparison (e.g. keywords).
    Takes an expression, and a set of clauses.

    Each clause can take the form of either:

    test-constant result-expr

    (test-constant1 ... test-constantN)  result-expr

    The test-constants are not evaluated. They must be compile-time
    literals, and need not be quoted.  If the expression is equal to a
    test-constant, the corresponding result-expr is returned. A single
    default expression can follow the clauses, and its value will be
    returned if no clause matches. If no default expression is provided
    and no clause matches, an IllegalArgumentException is thrown.

    Unlike cond and condp, fast-case does a constant-time dispatch for
    ints and non-keyword constants; the clauses are not considered
    sequentially.

    If all test cases are keywords, then fast-case will leverage an
    optimized path for `identical?` checks, where we balance the
    performance of a linear comparison of entries by object
    identity with the cost of an associative lookup and hashing
    of the case objects.  This can yield signficant savings
    for cases that are all keywords, and when there may be
    benefit for short-circuiting operations (e.g. the most
    likely case is first).

    All manner of constant expressions are acceptable in case,
    including numbers, strings,  symbols, keywords, and (Clojure)
    composites thereof. Note that since lists are used to group
    multiple constants that map to the same expression, a vector
    can be used to match a list if needed. The  test-constants
    need not be all of the same type."
  [e & clauses]
  (let [ge (with-meta (gensym) {:tag Object})
        default (if (odd? (count clauses))
                  (last clauses)
                  `(throw (IllegalArgumentException. (str "No matching clause: " ~ge))))
        conj-flat   (fn [acc [k v]]
                      (conj acc k v))]
    (if (> 2 (count clauses))
      `(let [~ge ~e] ~default)
      (let [pairs (->> (partition 2 clauses)
                       (reduce (fn [acc [l r]]
                                 (if (seq? l)
                                   (reduce conj acc (for [x l] [x r]))
                                   (conj acc [l r])))  []))]
        (if (and (every? keyword? (map first pairs))
                 (<= (count pairs) 20))
          `(case-identical? ~e ~@clauses)
          `(clojure.core/case ~e ~@clauses))))))

(defn ^java.io.BufferedReader string-reader [^String s]
  (-> (java.io.StringReader. s) (java.io.BufferedReader.)))

;;This is actually a pretty naive way to parse paths...but I think it'll work for
;;our use cases.
(defn path? [s]
  (or (io/fexists? s)
      ;;not sure wtf this is...revisit?
      (with-open [rdr (string-reader s)]
        (let [[l1 l2] (take 2 (line-seq rdr))]
          (when  (and (nil? l2)
                      (seq (re-seq #"\\|/" s)))
            true)))))

;;replacement for line-seq, allows a more useful idiom
;;for reading files, and is slightly more efficient (no intermediate
;;calls to seq, less garbage).
(defn ->line-reducer
  "Given a string literal that encodes a path, or a newline-delimited 
   sequence of lines, returns a reducible obj that iterates over each line (string) 
   delimited by \newline."
  [path-or-string & {:keys [reader-fn]}]
  (let [reader-fn (or reader-fn
                      (if (path? path-or-string)
                        clojure.java.io/reader
                        string-reader))]
    (reify clojure.core.protocols/CollReduce
      (coll-reduce [o f init]
        (with-open [^java.io.BufferedReader rdr (reader-fn path-or-string)]
          (loop [acc init]
            (if (reduced? acc) @acc 
                (if-let [ln (.readLine rdr)]
                  (recur (f acc ln))
                  acc)))))
      (coll-reduce [o f]
        (with-open [^java.io.BufferedReader rdr (reader-fn path-or-string)]
          (if-let [l1 (.readLine rdr)]
            (loop [acc l1]
              (if (reduced? acc) @acc 
                  (if-let [ln (.readLine rdr)]
                    (recur (f acc ln))
                    acc)))
            nil)))
      )))

(defn line-reducer
  "Outer API for line-reducers, uses ->line-reducer internally.    
   Now we can automatically grab lines from compressed files too.
   Given a string literal that encodes a path, or a newline-delimited 
   sequence of lines, returns a reducible obj that iterates over each line (string) 
   delimited by \newline.  If the path is a gz or lz4 file, will automatically 
   decompress and stream the file-lines."
  [path-or-string & {:keys [reader-fn]}]
  (let [reader-fn (or reader-fn
                      (if (path? path-or-string)
                        (case  (re-find  #".gz|.lz4" path-or-string)
                          ".gz"   z/zip-reader
                          ".lz4"  z/lz4-reader
                          clojure.java.io/reader)
                        string-reader))]                      
    (->line-reducer path-or-string :reader-fn reader-fn)))

(defn compress-file!
  "Given a path to an existing file, compresses is using either 
   :gzip or :lz4 compression.  Writes a corresponding filename with the 
   .gz or .lz4 extension."
  [from & {:keys [type] :or {type :gzip}}]
  (let [writer-fn (case type
                    :gzip z/zip-writer
                    :lz4 z/lz4-writer
                    (throw (Exception. (str "unknown compressor! " type))))]
    (with-open [w (writer-fn
                   (str from
                        (case type
                          :gzip ".gz"
                          :lz4 ".lz4"
                          (throw (Exception. (str "unknown compressor! " type))))))]
      (reduce (fn [_ ^String l] (io/writeln! w l)) nil (line-reducer from)))))


(defn reducer? [x]
  (extends? clojure.core.protocols/CollReduce (class x)))

(defn first-any [x]
  (first (transduce (take 1) (completing (fn [acc x] (conj acc x))) '() x)))

(defmacro clone-meta [obj expr]
  `(with-meta ~expr (meta ~obj)))

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

;;note: programming targetting his (old) internal implementation
;;caused compile time errors.  Plus there's not a huge benny.
;;Switching back to his API funcall.
(defmacro memo-fn 
  "Creates a memoized function, like defn, using a FINITE number of 
   arguments.  Intended for use with 1 to 6 arguments. Allows 
   arg masking, as with _ in defn."
  [raw-args body]
  (let [args (filterv (fn [a] (not= a '_)) raw-args)
        ;n (count args)
        ;; tup  (case n
        ;;            1 (first args)
        ;;            2 `(clj_tuple.Tuple2. ~@args nil)
        ;;            3 `(clj_tuple.Tuple3. ~@args nil)
        ;;            4 `(clj_tuple.Tuple4. ~@args nil)
        ;;            5 `(clj_tuple.Tuple5. ~@args nil)
        ;;            6 `(clj_tuple.Tuple6. ~@args nil)
        ;;            (throw (Exception. "blah")))
        ]
    `(let [hash# (java.util.HashMap.)]
       (fn memoized# [~@raw-args] 
         (let [k# (~'tup/tuple ~@args)]
           (if-let [v# (.get hash# k#)]
             v#
             (let [newv# ~body]
               (do (.put hash# k# newv#)
                   newv#))))))))   

;;TODO: Check to see if we still need this due to performance
;;reasons...possibly deprecate.
(defn rvals
  "Returns a countable, reducible  view over the vals of rv, taking
   advantage of reduce-kv."
  [kvs]
  (reify
    clojure.lang.Counted 
    (count [this] (count kvs))
    clojure.lang.Seqable 
    (seq [this] (seq kvs))
    clojure.core.protocols/CollReduce
    (coll-reduce [this f1]
      (reduce-kv (fn [acc k v] (f1 acc v)) (f1) kvs))
    (coll-reduce [_ f1 init]
      (reduce-kv (fn [acc k v] (f1 acc v)) init kvs))))

;;TODO: Check to see if we still need this due to performance
;;reasons...possibly deprecate.
(defn rkeys
  "Returns a countable, reducible  view over the keys of rv,
   taking advantage of reduce-kv."
  [kvs]
  (reify
    clojure.lang.Counted 
    (count [this] (count kvs))
    clojure.lang.Seqable 
    (seq [this] (seq kvs))
    clojure.core.protocols/CollReduce
    (coll-reduce [this f1]
      (reduce-kv (fn [acc k v] (f1 acc k)) (f1) kvs))
    (coll-reduce [_ f1 init]
      (reduce-kv (fn [acc k v] (f1 acc k)) init kvs))))

;;TODO Verify, then deprecate!
#_(defn collectr
  "Similar to collect, but returns a reducer."
  [fs xs]  
  (let [f (if (coll? fs) (apply juxt fs) fs)]
    (reify     
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]   (reduce f1 (f1) (r/map f xs)))        
      (coll-reduce [_ f1 init] (reduce f1 init (r/map f xs)))
      clojure.lang.Seqable 
      (seq [this]  (seq (map f xs))))))

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
(defn distinct-zipped
  "Finds distinct elements of multiple collections, where collections
   are represnted by n-tuples, which are elements of n-colls.
   Returns a sequence of [#{s1} #{s2} #{s3}] for each element of the
   n-tuples across the collection sequence."
  [n-colls]
   (let [knowns   (atom (mapv (fn [i] (transient #{})) (range (count (first n-colls)))))
         add-row  (fn [xs] (reduce (fn [idx x]
                                     (let [known (nth @knowns idx)]
                                       (do (when (not (known x))
                                             (swap! knowns assoc idx (conj! known x)))
                                           (unchecked-inc idx))))
                                   0
                                   xs))]
     (do (doseq [xs n-colls]  (add-row xs))
         (mapv persistent! @knowns))))

(defn drop-nth
   "Drops the nth item in coll"
   [n coll]
   (concat
     (take n coll)
     (drop (inc n) coll)))

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
                        (@ordering x)
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

;;this is faster, and works on transient collections.
(defmacro zero-items? 
  "A more general replacement for empty?, specifically use for accessing 
   transient, indexed collections."
  [coll] `(zero? (count ~coll)))

(definline dissoc2 [m from to]
  `(let [res# (dissoc (get ~m ~from) ~to)]
     (if (zero-items? res#) 
       (dissoc ~m ~from)
       (assoc ~m ~from res#))))

(definline dissoc2! [m from to]
  `(let [res# (dissoc! (get ~m ~from) ~to)]
     (if (zero-items? res#)
       (dissoc! ~m ~from)
       ~m)))

(definline transient2 [coll]
  `(reduce-kv (fn [m# k# v#]
                (assoc! m#  k# (transient v#)))
           (transient ~coll)  ~coll))

(definline persistent2! [coll]
  `(let [realized# (persistent! ~coll)]
     (reduce-kv (fn [m# k# v#]
               (assoc m# k# (persistent! v#)))
             realized# realized#)))

;;faster merge
(defn rmerge! [l  r]
  (if (instance? clojure.lang.IKVReduce l)
    (.kvreduce ^clojure.lang.IKVReduce l (fn [^clojure.lang.ITransientAssociative acc k v]
                                           (if-not (acc k)
                                             (.assoc acc k v)
                                             acc)) r)
    (clojure.core.protocols/kv-reduce l (fn [^clojure.lang.ITransientAssociative acc k v]
                                          (if-not (acc k)
                                            (.assoc acc k v)
                                            acc)) r)))

;;~50% faster.
(defn fast-merge
  "Optimized implementaiton of clojure.core/merge, which
   - prefer discrete args vs. the default var-args for everything version,
   - uses transients instead of persistent map-based conj,
   - use direct method invocation everywhere,
   - accumulates keys from r->l instead of l->r, since we can prune
     more (in some cases, e.g. 2-arg versions like
     (merge {:a 2 :b 3 :c 4} {:a 1 :b 1 :c 1}) we can exclude
     all keys from l since they already appear in r,
     leading to 0 actual assoc's)

   Assumes all inputs support assoc.  Drop-in replacement for
   clojure.core/merge."
  ([] {})
  ([m] m)
  ([m1 m2]          (->> (transient m2) (rmerge! m1) persistent!))
  ([m1 m2 m3]       (->> (transient m3) (rmerge! m2) (rmerge! m1) persistent!))
  ([m1 m2 m3 m4]    (->> (transient m4) (rmerge! m3) (rmerge! m2) (rmerge! m1) persistent!))
  ([m1 m2 m3 m4 m5] (->> (transient m5) (rmerge! m4) (rmerge! m3) (rmerge! m2) (rmerge! m1) persistent!))
  ([m1 m2 m3 m4 m5 & ms]
   (let [rs (reverse ms)]
     (->> (reduce rmerge! (transient (first rs)) (rest rs))
          (rmerge! m5)
          (rmerge! m4)
          (rmerge! m3)
          (rmerge! m3)
          (rmerge! m1)
          persistent!))))


#_(defmacro static-merge
  [& ms]
  (assert (every? map? (rest ms)))
  (assert (every? #(= 1 %) (->> (rest ms) (mapcat keys) frequencies vals)))
  (let [kvs (mapcat seq (rest ms))]
    (reduce (fn [acc [k v]]
              `(assoc ~acc ~k ~v)) (first ms) kvs)))

;;type hinted method invocations avoid clojure.lang.RT
(defmacro static-merge
  "If we enforce the invariant that all inputs to merge (barring the
   first) must be maps, we can statically compile an efficient
   merge where we don't have to traverse all the keys.
   This then becomes a simple bunch of assoc calls to
   specific maps in the input, removing any need to iterate
   or assoc redundant values."
  [& ms]
  (assert (every? map? (rest ms)))
  (assert (every? #(= 1 %) (->> (rest ms) (mapcat keys) frequencies vals)))
  (let [kvs (mapcat seq (rest ms))
        assoc! (fn [m k v]
                 `(.assoc  ~(with-meta m {:tag 'clojure.lang.Associative})
                              ~k ~v))]
    (reduce (fn [acc [k v]]
              (assoc! acc k v)) (first ms) kvs)))

;;Operations optimized for speed.  the -in and friends
;;are not sufficient...
(defmacro deep-assoc
  "Replacement for assoc-in, but without the function call overhead.
   If the key-path is composed of literals, this is about
   3 times faster then assoc-in."
  [m [k & ks] v]
  (if ks
    `(assoc ~m ~k (deep-assoc (get ~m ~k) ~ks ~v))
    `(assoc ~m ~k ~v)))

;;This is a clone of clojure.core/get-in, directly from source,
;;modified to support short circuiting.
(defn deep-get
    "Returns the value in a nested associative structure,
     where ks is a sequence of keys. Returns nil if the key
     is not present, or the not-found value if supplied.
     Short circuits if get returns nil along the path."
    ([m ks]
     (reduce (fn [acc k]
               (if-let [res (get acc k)]
                 res
                 (reduced nil))) m  ks))
    ([m ks not-found]
     (loop [sentinel (Object.)
            m m
            ks (seq ks)]
       (if ks
         (let [m (get m (first ks) sentinel)]
           (if (identical? sentinel m)
             not-found
             (recur sentinel m (next ks))))
         m))))

;;This is on average 3x faster than get-in,
;;and up to 8x faster if short circuiting applies.
(defmacro deep-get-map
  "Like get-in, except it's optimized to work with a sequence of
   map lookups where we have java.util.Map values
   and short circuits on nil, as opposed to clojure.core/get-in
   which naively reduces over the keys regardless of nil.
   More general than invoke-in since it works with
   java map implementations, less general than invoke-in
   since it doesn't work with arbitrary function applications."
  ([m ks]
   (if (seq ks)
     `(let [^java.util.Map m# ~m]
        (if-let [res# (.get ^java.util.Map m#  ~(first ks))]
          (deep-get-map res# ~(rest ks))))
     `~m))
  ([m ks not-found]
   `(if-let [res# (deep-get-map ~m ~ks)]
      res#
      ~not-found)))

(defmacro deep-invoke-aux
  ([m _ ks]
   (if (seq ks)
     `(if-let [res# (~m  ~(first ks))]
        (deep-invoke-aux res# nil ~(rest ks)))
     `~m))
  ([m ks]
   `(let [m# ~m]
      (deep-invoke-aux m# ~m ~ks))))

;;This is on average 3x faster than get-in,
;;and up to 8x faster if short circuiting applies.
(defmacro deep-invoke
  "Like get-in, except it's optimized to work with a sequence of
   function invocations - which for maps/sets is application -
   and short circuits on nil, as opposed to clojure.core/get-in
   which naively reduces over the keys regardless of nil."
  ([m ks]   `(deep-invoke-aux m ks))
  ([m ks not-found]
   `(if-let [res# (deep-invoke m ks)]
      res#
      ~not-found)))

(defmacro deep-update
  "Replacement for update-in, but without the function call overhead.
   If the key-path is composed of literals, this is about
   3 times faster then assoc-in."
  [m [k & ks] f & args]
   (if ks
     `(assoc ~m ~k (deep-update (get ~m ~k) ~ks ~f ~@args))
     `(assoc ~m ~k (~f (get ~m ~k) ~@args))))

(defmacro deep-dissoc [m ks]
  (let [preds (vec (butlast ks))
        k     (last ks)]
    `(deep-update ~m ~preds ~dissoc ~k)))

(defn prune-in
  "If function results in an empty map, contained within another map, 
   removes the entry associated with the empty map."
  [m ks f & args]
  (let [updated (apply update-in ks f args)]
    (if (empty? (get-in updated ks))
      (let [path   (butlast ks)
            parent (get-in m path)]
        (assoc-in m path (dissoc parent (last ks))))
      updated)))

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
(definline cons-head [the-list]
  `(.first ~(vary-meta the-list assoc :tag 'clojure.lang.Cons)))
(definline cons-next [the-list]
  `(.first (.next ~(vary-meta the-list assoc :tag 'clojure.lang.Cons))))



;;#Faster String Building
;;clojure.core/string has a lot of inefficiencies, mostly due to 
;;a lot of vararg usage.  If you're building strings a lot, i.e. 
;;passing messages in a simulation using strings, or logging, etc.
;;then make-string is roughly 5x faster.  It's a drop-in replacement 
;;for clojure.core/str.
(defn ^String simple-str [^Object x]
  (if (nil? x) "" (.toString x)))

(defmacro build-string [& args]
  `(let [x#  ~(first args)
         sb# (StringBuilder. (simple-str x#))]
     (.toString
      (doto sb#
        ~@(for [a (rest args)]
            `(.append (simple-str  ~a)))))))

(def ^:constant +max-params+ 15)
(defn string-func-body [n]
  (let [obj (with-meta (gensym "obj") {:tag 'Object})]
    (assert (>= n 0))
    (if (<= n +max-params+)
      (case n 
        0 `(~(with-meta [] {:tag 'String}) "")
        1 `(~(with-meta [obj] {:tag 'String})
            (if (nil? ~obj) "" (. ~obj (toString))))    
        (let [args (-> (vec (for [x (range n)] (gensym "str")))
                       (with-meta {:tag String}))]    
          `(~args
            (build-string ~@args))))
      (let [baseargs (vec (for [x (range (+ 2 +max-params+))] (gensym "str")))
            args (-> baseargs
                     (conj '&)
                     (conj 'rest)
                     (with-meta {:tag String}))] 
      `(~args
        (build-string (build-string ~@baseargs) 
                      (apply clojure.core/str ~'rest)))))))

;;Positional definitions of str, to eliminate arrayseq overhead due 
;;to varargs version of str.  Since we're making lots of strings,
;;it's stupid to incur the varargs cost here...
(let [bodies (for [n (range (+ +max-params+ 2))]
               (string-func-body n))]
  (eval
   `(defn ~'make-string
           "Drop-in replacement for clojure.core/str, designed for faster 
            string concatenation when creating strings is on a critical 
            performance path.

            With no args, returns the empty string. With one arg x, returns
            x.toString().  (str nil) returns the empty string. With more than
            one arg, returns the concatenation of the str values of the args.
            When creating strings - many times over - using arity > 2, avoids
            the overhead of calls to first/next that clojure.core/str invokes.
            Roughly 33% faster for concatenating 3 strings, approaching  
            60% faster for larger arities, up to 15."
      ~@bodies)))

;;eventually deprecate these in favor of the ones from clj-fast

;;More effecient memoization functions.  Clojure's built in memo 
;;memoizes args using a variadic code path, which forces the creation
;;of tons of arrayseqs....this is horrible for small, fast lookups.
(defn memo-1 [f]
  (let [^java.util.HashMap tbl (java.util.HashMap.)]
    (fn [k] (if-let [res (.get tbl k)]
              res
              (let [res (f k)]
                (do (.put tbl k res)
                    res))))))

;;4x faster than clojure.core/memoize...
;;we can do better with a macro, but I haven't sussed it out.
;;This is a much as we probably need for now though, meh.
(defn memo-2 [f]
  (let [xs (java.util.HashMap.)]
    (fn [x y]
      (if-let [^java.util.HashMap ys (.get xs x)]
        (if-let [res (.get ys y)]
          res
          (let [res (f x y)]
            (do (.put ys y res)
                res)))
        (let [res     (f x y)
              ys    (doto (java.util.HashMap.)
                      (.put y res))
              _     (.put xs x ys)]
          res)))))

(defmacro deref!! [v]
  (let [v (with-meta v {:tag 'clojure.lang.IDeref})]
    `(.deref ~v)))

(defmacro val-at
  "Synonimous with clojure.core/get, except it uses interop to
   directly inject the method call and avoid function invocation.
   Intended to optimize hotspots where clojure.core/get adds
   unwanted overhead."
  [m & args]
   (let [m (with-meta m  {:tag 'clojure.lang.ILookup})]
    `(.valAt ~m ~@args)))


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

;;optimized short-circuiting set intersection.
;;about 3x faster than (first (clojure.set/intersection s1 s2))
;;about 2x as fast as
;; (defn some-member [s1 s2]
;;   (let [[l r]  (if (< (count s1) (count s2)) [s1 s2]
;;                    [s2 s1])]
;;     (reduce (fn [acc x]
;;               (if (r x)
;;                 (reduced x)
;;                 acc)) nil l)))

(defn some-member
  "Detects if s1 contains any element of s2, returns first
   found intersecting member as fast as possible.
   Akin to clojure.core/some optimized for sets."
  [^clojure.lang.APersistentSet s1
   ^clojure.lang.APersistentSet s2]
  (let [l  (if (< (.count s1) (.count s2)) s1
               s2)
        r   (if (identical? l s1) s2 s1)]
    (reduce (fn [acc x]
              (if (r x)
                (reduced x)
                acc)) nil l)))

;;faster reduce-kv implementation...
(defn kv-type [t]
  (cond (isa? t clojure.lang.IKVReduce) :.kvreduce
        (isa? t clojure.core.protocols.IKVReduce :.kv-reduce)
        (extends? clojure.core.protocols/IKVReduce t :kv-reduce)
        :else :reduce-kv))

(alter-var-root #'kv-type memo-1)

(definline kvreduce [f init m]
  (let [s (with-meta (gensym "ikv") {:tag 'clojure.lang.IKVReduce})]
    `(let [~s ~m]
       (.kvreduce  ~s ~f ~init))))

(definterface IIntegerPair
  (^long fst [])
  (^long snd []))

(binding [*unchecked-math* true]
  
  (defmacro pair-hash [x y] 
    `(+  (* 31 (+ 31 ~x)) ~y))

  (deftype intPair [^long x ^long y  ^int hashcode]
    IIntegerPair 
    (^long fst [n] x)
    (^long snd [n] y)
    Object 
    (hashCode [this] hashcode)
    (equals  [this that] (== (.hashCode this) (.hashCode that))))
  (defmacro coord [x y]
    `(intPair. ~x ~y (pair-hash ~x ~y)))
    
)


;;Note -> nth-binding actually works okay...it's not preferable, 
;;but it will not kill performance.  This may work out okay for nd 
;;keys.

(defmacro apply-n [n f coll]
  `(~f ~@(for [i (range n)]
               `(nth ~coll ~i))))

(defmacro apply-2 [f args] `(apply-n 2 ~f ~args))
(defmacro apply-3 [f args] `(apply-n 3 ~f ~args))
(defmacro apply-4 [f args] `(apply-n 4 ~f ~args))
(defmacro apply-5 [f args] `(apply-n 5 ~f ~args))
(defmacro apply-6 [f args] `(apply-n 6 ~f ~args))


(defmacro interval-loop 
  "Debugging tool.  Allows caller to define an expression identical to loop, 
   where bindings and body are identical to loop semantics.  expr will be 
   evaluated every intervalms (in milliseconds), and two lexical vars *start*
   and *now* will be bound and available in expr and the loop body."
  [intervalms expr bindings & body]
  `(let [~'*start* (System/currentTimeMillis)
         prev# (atom ~'*start*)]
       (loop [~@bindings]
         (let [~'*now* (System/currentTimeMillis)] 
           (if (>= (- ~'*now* @prev#) ~intervalms)
             (do (reset! prev# ~'*now*)
                 ~expr))
           ~@body))))

;;operations for working with camelcase and lispy forms,
;;primarily for munging java interop.
(defn camelize [x]
  (str (clojure.string/upper-case (subs x 0 1))
       (clojure.string/lower-case (subs x 1))))

(defn camel-join [xs]
  (reduce (fn [acc x]
            (str acc (camelize x)))
          (clojure.string/lower-case (first xs))
          (rest xs)))
;(def camel-regex #"[a-z]+|[A-Z][a-z]*")
(defn camel-split [x]  (re-seq   #"[a-z]+|[A-Z][a-z]*" x))
(defn camel->lisp [x]
  (->> (camel-split x)
       (map clojure.string/lower-case)
       (clojure.string/join "-")))

(defn constant->key [x]
  (when-let [ys  (clojure.string/split (name x) #"_")]
    (keyword (camel-join ys))))

(defn constant->lisp-key [x]
  (when-let [ys  (clojure.string/split (name x) #"_")]
    (keyword (camel->lisp (camel-join ys)))))


;;Collection interpolation utilities.
;;===================================
(defn time-weighted-samples
  "Given a function keyf, which maps elements of
   the collection coll onto a numeric value (typically
   some notion of time), returns a lazy sequence of
   [entry t dt], where t is the value of (keyf entry), and dt is the result of
  comparing the difference in keyf values for adjacent entries in the
  collection."
  [keyf coll]
  (let [final (atom nil)]
    (concat
     (for [[l r] (partition 2 1 coll)]
       (let [lt (keyf l)
             rt (keyf r)
             _  (reset! final [r rt 1])]
           [l  lt (- rt lt)]))
     (lazy-seq (when-let [f @final]
                 (vector f))))))

(defn interpolate
  "Given a function keyf, which maps elements of
   the collection coll onto a numeric value (typically
   some notion of time), and a
   function lerpf, which projects an initial value x, an initial time t0, and a dt
   onto a new value x'.  lerpf will be mapped over the range defined by
   the initial time, t0, (+ t0 dt), such that multiple interpolated
   entries will be generated.  The resulting sequence is a concatenation
   of the interpolated values.

  (interpolate :t (fn [r t dt] (assoc r :t (+ t dt)))
     [{:t 0 :id :a} {:t 10 :id :b} {:t 20 :id :c} {:t 25 :id :d} {:t 26 :id :d}])
  =>
  ({:t 0, :id :a} {:t 1, :id :a} {:t 2, :id :a} {:t 3, :id :a} {:t 4, :id :a}
  {:t 5, :id :a} {:t 6, :id :a} {:t 7, :id :a} {:t 8, :id :a} {:t 9, :id :a} {:t
  10, :id :b} {:t 11, :id :b} {:t 12, :id :b} {:t 13, :id :b} {:t 14, :id :b}
  {:t 15, :id :b} {:t 16, :id :b} {:t 17, :id :b} {:t 18, :id :b} {:t
  19, :id :b} {:t 20, :id :c} {:t 21, :id :c} {:t 22, :id :c} {:t 23, :id :c}
  {:t 24, :id :c} {:t 25, :id :d} {:t 26, :id :d})
  "

  [keyf lerpf coll]
  (apply concat
         (for [[x t0 dt] (time-weighted-samples keyf coll)]
           (if (= dt 1)  (list x)
               (for [n (range dt)]
                 (lerpf x t0 n))))))

(defn lerp-1d
  "Performs a 1-dimensional linear interpolation over a sequence of
   records xs.  Given key-functions (typically keywords) for x and y
   values, these values are extracted pair-wise from the records and
   interpolated per a simple 1d linear function.
   Caller may supply a custom interpolation function::

   record -> dt -> slope

   which will be provided with positional information for two adjacent
   samples [l r], where dt is drawn from the range [1 .. (width l r)], where
   width is the difference in x values from r to l, e.g. r.x - l.x.
   Slope is r.y - l.y.  From this information, caller may provide additional
   interpolated values via lerpf.

   ex.

   (lerp-1d :level :fill [{:level 0,   :fill 0.0}
                          {:level 2,  :fill 0.2}])
   ({:level 0, :fill 0.0} {:level 1, :fill 0.1} {:level 2, :fill 0.2})

   ;;with optional user-defined lerp function:

   (lerp-1d :level :fill
      (fn [{:keys [level fill] :as l} dt slope]
         (assoc l :level (+ level dt) :fill (+ fill (* slope dt))
                  :original-level level))
    [{:level 0,   :fill 0.0}
     {:level 2,  :fill 0.2}])

   ({:level 0, :fill 0.0}
    {:level 1, :fill 0.1, :original-level 0}
    {:level 2, :fill 0.2})"

  ([xkey ykey lerpf xs]
   (let [tail (atom nil)
         head (first xs)]
     (-> (->> xs
              (partition 2 1)
              (mapcat (fn [[l r]]
                        (let [_ (reset! tail r)
                              dt (- (xkey r) (xkey l))
                              lv (ykey l)
                              rv (ykey r)
                              distance (- rv lv)
                              slope    (/ distance dt)]
                          (if (> dt 1)
                            (concat [l]
                                    (for [x (range 1 dt)]
                                      (lerpf l x slope)))
                            [l])))))
         (concat (lazy-seq  [(or @tail head)])))))
  ([xkey ykey xs]
   (lerp-1d xkey ykey (fn [l dt slope]
                        (assoc l xkey (+ dt (xkey l))  ykey (+ (l ykey) (* dt slope) ))) xs )))


;;cached implementation of extends?
(let [cached (memo-2 extends?)]
  (defn implements? [protocol atype]
     (cached protocol atype)))
