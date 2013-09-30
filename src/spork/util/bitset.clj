;;A wrapper around the java.util.BitSet class.  Will replace this with
;;a more portable protocol-backed implementation in the near future. 
;;Currently used for bitstring operations, supporting genetic algorithms.
(ns spork.util.bitset 
  (:import java.util.BitSet))

(defn ^BitSet ->bitset [n]
  (BitSet. (long n)))

(def empty-bits (BitSet. 0))

(defn ^BitSet clone [^BitSet bs] (.clone bs))

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
 [^BitSet bs] (.length bs)) 
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

(defn get-clear-bits [^BitSet bs]
  (take-while #(>= % 0) (iterate (fn [idx] (next-clear-bit bs idx))  0)))
(defn get-set-bits [^BitSet bs]
  (take-while #(>= % 0) (iterate (fn [idx] (next-set-bit bs idx)) 0)))
         
    
    
    
         

;;Testing 
(comment 

(def the-bits (->bitset 10))
(def two-three (-> the-bits (set-bits 1 2 true)))
(def four-five (-> the-bits (set-bits 3 4 true)))

)

(deftype bitset [^BitSet bits _meta]
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.IPersistentSet
  (count [this] (count basemap))
  (empty [this] (ordered-map. empty-bits _meta))
  ;cons defines conj behavior
  (cons  [this n]  (bitset. (set-bits bits n true) _meta))
  (equiv [this o]  (.equiv bits o)) 
  (hashCode [this] (.hashCode bits))
  (equals [this o] (or (identical? this o) (.equals bits o)))
  (contains [this n] (get-bit bits k))
  (get    [this n]   (get-bit bits n))
  (seq [this] (if (is-empty? bits) (seq #{})
                  (map 
  ;without implements (dissoc pm k) behavior
  (without [this k] 
    (if (not (contains? basemap k)) this
        (ordered-map. n
                      (dissoc basemap k) 
                      (dissoc idx->key (get key->idx k))
                      (dissoc key->idx k)
                      _meta)))
    
  clojure.lang.Indexed
  (nth [this i] (if (and (>= i 0) 
                         (< i (count basemap))) 
                  (let [k (get idx->key i)]
                    (generic/entry k (get basemap k)))
                  (throw (Exception. (str "Index out of range " i )))))
  (nth [this i not-found] 
    (if (and (< i (count basemap)) (>= i 0))
        (get basemap (get idx->key i))        
         not-found))  
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (ordered-map. n basemap idx->key key->idx m))
      
  clojure.lang.Reversible
  (rseq [this]
    (seq (map (fn [k] (clojure.lang.MapEntry. k (get basemap k))) 
              (reverse (vals idx->key)))))

  java.io.Serializable ;Serialization comes for free with the other things implemented
  clojure.lang.MapEquivalence
  
  java.util.Map ;Makes this compatible with java's map
  (size [this] (count basemap))
  (isEmpty [this] (zero? (count basemap)))
  (containsValue [this v] (some #{v} (vals (basemap this)) v))
  (get [this k] (.valAt this k))
  (put [this k v] (throw (UnsupportedOperationException.)))
  (remove [this k] (throw (UnsupportedOperationException.)))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (set (keys basemap))) ;;modify
  (values [this] (map val (.seq this)))
  (entrySet [this] (set (.seq this))))
