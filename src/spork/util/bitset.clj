;;A wrapper around the java.util.BitSet class.  Will replace this with
;;a more portable protocol-backed implementation in the near future. 
;;Currently used for bitstring operations, supporting genetic algorithms.
(ns spork.util.bitset 
  (:import java.util.BitSet))

;;The persistent bitset implementation here is a copy-on-write wrapper around
;;the java.util.BitSet .  As such, its performance characteristics for large
;;sets are dubious, but for small bitstrings, it may be nice.

(defn ^BitSet make-bitset [n]
  (BitSet. (long n)))

(def empty-bits (BitSet. 0))

(def ^:dynamic *mutate!* nil)

(defn ^BitSet clone [^BitSet bs]
  (if *mutate!* bs
      (.clone bs)))

(defn ^BitSet and-bits 
  "Performs a logical AND of this target bit set with the argument bit set."
  [^BitSet bs ^BitSet other]
  (doto (clone bs) (.and other)))
           
(defn ^BitSet and-not-bits 
  "Clears all of the bits in this BitSet whose corresponding bit is set in the specified BitSet."
  [^BitSet bs ^BitSet other]
  (doto (clone bs) (.andNot other)))

(defn cardinality 
  "Returns the number of bits set to true in this BitSet."
  [^BitSet bs]
  (.cardinality bs))
 
(defn clear-bits 
  "Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to false."
  ([^BitSet bs ^long from ^long to]
     (doto (clone bs) .clear from to))
  ([^BitSet bs ^long idx]
     (doto (clone bs) .clear idx))
  ([^BitSet bs]
     (doto (clone bs) .clear)))

(defn flip-bits 
  "Sets each bit from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the complement of its current value."
  ([^BitSet bs ^long idx] (doto (clone bs) (.flip idx)))
  ([^BitSet bs ^long from ^long to] (doto (clone bs) (.flip from to))))

(defn ^boolean get-bit 
  "Returns the value of the bit with the specified index."
  [^BitSet bs ^long idx]
  (.get bs idx))
 

(defn ^BitSet get-bits 
  "Returns a new BitSet composed of bits from this BitSet from fromIndex (inclusive) to toIndex (exclusive)."
  [^BitSet bs from to]
  (.get bs from to))
  
 
(defn ^long hash-code [^BitSet bs]
  (.hashCode bs))

(defn ^boolean intersects? 
  "Returns true if the specified BitSet has any bits set to true that are also set to true in this BitSet."
  [^BitSet bs ^BitSet other]
  (.intersects bs other))

(defn ^boolean is-empty? [^BitSet bs] (.isEmpty bs))

(defn ^long length
  "Returns the \"logical size\" of this BitSet: the index of the highest set bit in the BitSet plus one."
  [^BitSet bs] (long (.length bs)))

(defn ^long next-set-bit 
  "Returns the index of the first bit that is set to true that occurs on or after the specified starting index."
  [^BitSet bs ^long idx]
  (.nextSetBit bs idx))

(defn ^long next-clear-bit 
  "Returns the index of the first bit that is set to false that occurs on or after the specified starting index."
  [^BitSet bs ^long idx]
  (.nextClearBit bs idx))

(defn ^BitSet set-bits
  "Sets the bits from the specified fromIndex (inclusive) to the specified 
   toIndex (exclusive) to true."
  ([^BitSet bs ^long from ^long to v]
     (doto (clone bs) (.set  from  to (boolean v))))
  ([^BitSet bs ^long idx  v] (doto (clone bs) (.set  idx  (boolean v))))
  ([^BitSet bs ^long idx] (doto (clone bs) (.set idx))))

(defn size 
  "Returns the number of bits of space actually in use by this BitSet to represent bit values."
  [^BitSet bs]
  (.size bs))

(defn toString 
  "Returns a string representation of this bit set."
  [^BitSet bs] (.toString bs))
 
(defn ^BitSet xor-bits
  "Performs a logical XOR of this bit set with the bit set argument."
  [^BitSet bs ^BitSet other]
  (doto (clone bs) (.xor other)))

(defn ^BitSet or-bits 
  "Performs a logical OR of this bit set with the bit set argument."
  [^BitSet bs ^BitSet other]
  (doto (clone bs) (.or other)))

(defn ^boolean bit-equals [^BitSet bs other]
  (.equals bs other))

(defn stepper [bs f idx bound]  
  (lazy-seq
   (let [res (f bs idx)]
     (when (and (>= res 0) (< res bound))
       (concat (list res) (stepper bs f (+ res 1) bound))))))

(defn bit-step [^BitSet bs f idx]
  (let [bound  (length bs)]
    (stepper bs f idx bound)))

(defn get-clear-bits [^BitSet bs]  (bit-step bs next-clear-bit 0))
(defn get-set-bits   [^BitSet bs]  (bit-step bs next-set-bit 0))

(defn conj-bits [bs xs]
  (let [copy (clone bs)]
    (binding [*mutate!* true]
      (reduce (fn [bs idx] (set-bits bs idx)) copy xs))))

(defn disj-bits [bs xs]
  (let [copy (clone bs)]
    (binding [*mutate!* true]
      (reduce (fn [bs idx] (set-bits bs idx false)) copy xs))))

(defn random-bitset
  "Creates a random bitset of length n."
  [n]
  (binding [*mutate!* true]
    (loop [^BitSet acc (make-bitset n)
           idx 0]
      (if (= idx n) acc
          (recur (if (> (rand) 0.5) (set-bits acc idx true) acc)
                 (unchecked-inc idx))))))

;;Testing 
(comment 

(def the-bits (->bitset 10))
(def two-three (-> the-bits (set-bits 1 3 true)))
(def four-five (-> the-bits (set-bits 3 5 true)))

;(conj-bits empty-bits [1 2 3 4])

;(-> (conj-bits empty-bits [1 2 3 4]) (disj-bits [2 4]))

)

;;a bitset is our clojure-themed wrapper for the BitSet.  It makes the BitSet look like
;;a persistent clojure set.
(deftype bitset [^BitSet bits _meta]
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.IPersistentSet
  (count [this] (length bits))
  (empty [this] (bitset. empty-bits _meta))
  ;cons defines conj behavior
  (cons  [this n]  (bitset. (set-bits bits n true) _meta))
  (equiv [this o]  (.equals bits o)) 
  (hashCode [this] (.hashCode bits))
  (equals [this o] (or (identical? this o) (.equals bits o)))
  (contains [this n] (get-bit bits n))
  (get    [this n]   (get-bit bits n))
  (seq    [this]     (get-set-bits bits))
  clojure.lang.Indexed
  (nth [this  i]
    (if (and (>= i 0) (<  i ^long (length bits)))
      (get-bit bits i)
      (throw (Exception. (str "Index out of range " i)))))
  (nth [this i not-found] 
    (if (and  (>= i 0) (< i ^long (length bits)))
      (get-bit bits i)
      not-found))
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
  clojure.lang.IFn
  ;makes bitset usable as a function
  (invoke [this k] (.contains this k))
  (invoke [this k not-found] (.contains this k not-found))
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (bitset. bits m))      
  java.io.Serializable ;Serialization comes for free with the other things implemented
  )

(defn bit-vector [& xs]
  (->bitset (conj-bits empty-bits xs) {}))

;;bit-backed operations from clojure.set
(defn bit-union
  ([l] l)
  ([l r]
     (->bitset (or-bits (.bits l) (.bits r)) {}))
  ([l r & xs]
     (reduce bit-union l (conj xs r))))
     
(defn bit-intersection
  ([l] l)
  ([l r] (->bitset (and-bits (.bits l) (.bits r)) {}))
  ([l r & xs] (reduce bit-intersection (conj xs r))))

(defn bit-difference
  ([l] l)
  ([l r]
     (if  (< (count l) (count r))  ;lesser
       (let [copy (clone (.bits l))
             bits (binding [*mutate!* true]
                    (reduce (fn [result item]                 
                              (if (contains? r item)
                                (set-bits result item false)
                                result))
                            copy l))]
         (->bitset copy {}))
       (->bitset (disj-bits  (.bits l) r) {})))
  ([l r & xs] (reduce bit-difference l (conj xs r))))

(defn random-bit-vector [n] (->bitset (random-bitset n) {}))

    
  
;;testing deftype
(comment
  (def the-set (bit-vector 1 2 3 4 5))
; (difference the-set the-set)
  
  )