;;A wrapper around the java.util.BitSet class.  Will replace this with
;;a more portable protocol-backed implementation in the near future. 
;;Currently used for bitstring operations, supporting genetic algorithms.
(ns spork.util.bitset 
  (:import java.util.BitSet))

(defn ^BitSet ->bitset [n]
  (BitSet. (long n)))

(defn ^BitSet clone [^BitSet bs] (.clone bs))

(defn ^BitSet and-bits 
  "Performs a logical AND of this target bit set with the argument bit set."
  [^BitSet bs ^BitSet other]
  (.and (clone bs) other)) 
           
(defn ^BitSet and-not-bits 
  "Clears all of the bits in this BitSet whose corresponding bit is set in the specified BitSet."
  [^BitSet bs ^BitSet other]
  (.andNot (clone bs) other))

(defn cardinality 
  "Returns the number of bits set to true in this BitSet."
  [^BitSet bs]
  (.cardinality bs))
 
(defn clear-bits 
  "Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to false."
  ([^BitSet bs ^long from ^long to]
     (.clear (clone bs) from to))
  ([^BitSet bs ^long idx]
     (.clear (clone bs) idx))
  ([^BitSet bs]
     (.clear (clone bs))))

(defn flip-bits 
  "Sets each bit from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the complement of its current value."
  ([^BitSet bs ^long idx] (.flip (clone bs) idx))
  ([^BitSet bs ^long from ^long to]
     (.flip (clone bs) from to)))

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
  [^BitSet bs ^long from ^long to ^boolean v]
  (. clone bs) set from to v)
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
  (.xor (clone bs) other))

(defn ^BitSet or-bits 
  "Performs a logical OR of this bit set with the bit set argument."
  [^BitSet bs ^BitSet other]
  (.or (clone bs) other))

(defn ^boolean bit-equals [^BitSet bs other]
  (.equals bs other))
