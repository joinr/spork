;;Work in progress for a generic bitset data structure.
(ns spork.data.bitset)

;;I realized I'd also like something to act as a set of binary variables.
;;I call it a choice-map.
;;The idea is that we have a vector of binary choices.  The choices, like 
;;combinations, can be lexicographically ordered.  So we can project an integer
;;onto the "choice range" and get a choice back. 
;;If we have a set of elements, we have 2^k choices.
;;The 0th choice is the empty set.
;;The first K choices are singleton sets, or are vectors with only one element
;;chosen. 
;;We can represent pretty large choice maps with a long integer, and use the 
;;"active" bits to index into the items that are set.
;;So our mth choice, in binary, will have a set number of bits.  We only need 
;;to map these bits to an underlying vector of values to retrieve the active 
;;set of choices.

(def ^:const mask (byte 31))

(defn count-choices [n depth]
  (bit-and mask (bit-shift-right n depth)))

;(defn active-choices [n depth]
;  (if (== (chount-choices n depth) 0) nil
;    (let [offset (bit-shift-left 1 depth)]
;      (loop [i (Math/pow 2 depth)