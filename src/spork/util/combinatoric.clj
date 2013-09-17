;;Simple combinatoric utilities used for solution representations.
(ns spork.util.combinatoric)

;;An implementation of the algorithm described in:  
;;*Generating the mth Lexicographical Element of a Mathematical Combination*  
;;__http://msdn.microsoft.com/en-us/library/aa289166%28VS.71%29.aspx__  

;;The novelty of the algorithm hinges on these facts: 

;;We can impose a lexicographical ordering on the elements of  
;;(choose n k).  In the lexicographical ordering, the magnitudes of the indices 
;;of our elements are ordered [smallest -> biggest], and range between 
;;[0 (- n 1)] .  An element contains k entries, ordered by the following 
;;comparison criteria:  

;;>(greater? elem1 elem2) = (greater-entry? k)  
;; (greater-entry? 0) = false  
;; (greater-entry? i) =  
;;>>  (if (<> (get elem1 i) (get elem2 i))  
;;        (> (get elem1 i) (get elem2 i))  
;;        (greater-entry? (- i 1)))

;;The lexicographical ordering provides an implicit relationship between 
;;pairs of entries in the set of indices, via lexicographic duality.  Any 
;;pair of integers, drawn from [0 (- n 1)], whose sum is (- n 1), are 
;;considered dual.  

;;>(dual i w)   = (- (- w 1) i) =  (- w 1 i)  = 0  
;; (dual 0 10)  = (- (- 10 1) 0) = (- 10 1 0) = 9

;;We can efficiently project an integer into a combinatoric numerical basis,  
;;which has k digits, each of which also range between [0 (- n 1)] where the 
;;k(i+1) digit is greater than the k(ith) digit.  This allows us to encode a 
;;combination for an the mth lexicographically-ordered element,  
;;Ex. if n = 5, k =3,  
;;>(element 0) = [0 1 2], (combinadic 0) = [2 1 0]  
;; (element 1) = [0 1 3], (combinadic 1) = [3 1 0]  
;; (element 2) = [0 1 4], (combinadic 2) = [3 2 0]

;;Due to the lexicographical ordering, and the notion of duality:  
;;>the digits of  
;; the lexicographic dual of the  
;; the combinadic of  
;; the mth element in  
;; the lexicographic ordering of  
;; the mathematical combination n choose k  

;;differ from the actual indices of the elements of the mth element by (n - 1).  

;;For n = 5, k = 3, max-digit = (- n 1) = 4  
;;> (element 0)                         =  
;;  (inverse (combinadic (dual 5 3 0))) =  
;;  (inverse (combinadic 9))            =  
;;  (inverse [4 3 2])                   =  
;;  [(subtract max-digit  4)            =  
;;  (subtract max-digit 3)              =  
;;  (subtract max-digit 2)]             =  
;;  [(- 4 4) (- 4 3) (- 4 2)]           =  
;;  [0 1 2]  

;;The mth lexicgraphical element of a mathematical combination (n k) can be  
;;computed by finding the combinadic encoding of the lexicographic dual of m,  
;;relative to n and k, and recovering the entries in the element by  
;;subtracting the dual's combinadic digits from the maximum combinadic digit,  
;;which is n - 1  for a zero-based counting scheme.  The resulting ordered  
;;set of integers directly correspond to the entries of the mth element of  
;;(n k) when the combinations of (n k) are sorted in lexicographical order.


(set! *warn-on-reflection* true)
(def ^:const upper-bound 
  "An upper bound on long integers" java.lang.Long/MAX_VALUE) 

;;My representation of a combination is a primitive record.  n is the number 
;;of items, k is the partition or choice number, and digits are a valid 
;;encoding, of k digits, in a combinatorial basis. 
(defrecord combination [^Boolean big ^long n ^long k ^longs digits]) 

;;Returns the number of combinations possible when choosing k items at a time 
;;from n elements.

;;Better implementations of choose.
(defn ^long choose [^long n ^long k]
    (if (= k 0) 1
        (quot (* n (choose (unchecked-dec n) (unchecked-dec k))) k)))

(defn big-choose [^long n ^long k]
    (if (= k 0) 1
        (/ (*' n (big-choose (dec n) (dec k))) k)))

(defn digits->idx 
  ([^long k digitvec]
   (loop [idx (dec k)
          k   k
          acc 0]
     (if (< idx 0) acc
         (recur (unchecked-dec idx) (unchecked-dec  k) 
                (long (+ (choose (nth digitvec idx)  k) acc))))))
  ([digitvec] (digits->idx (count digitvec) digitvec)))

(defn long-able?
  "Yields true if x is within the maximum integers representable by a Long."
  [x] (<= x upper-bound))

(defn big-combination?
  "Returns false if the combinations of n choose k exceed the upper bounds of 
   a Long."
  [n k] (not (long-able? (big-choose n k))))   

(defn scan
  "Auxillary function for incrementing digits in a combinatorially encoded base.
   Finds the digit to be incremented."
  [^long k ^long base ^longs digits]
  (loop [idx (dec k)
         current (aget digits idx)
         bound (+ base idx)]
    (if (< idx 0) -1 
      (if (< current bound)
        idx
        (recur (dec idx) 
               (aget digits (dec idx)) 
               (+ base (dec idx)))))))

(defn add-digit
  "Auxillary function to represent the addition of a digit in a combinatorial 
   base.  Due to some tricks in the algorithm, we simply increment the kth 
   digit, at idx, and change every digit to the right - of lesser significanc
   - to be left neighbor + 1."
  [^combination c idx]
  (let [^longs ds (:digits c) 
        xs     (aclone ds)
        len    (:k c)]        
    (loop [previous (inc (aset xs idx 1))
           i  (inc idx)]
      (when (< i len)
        (aset xs i  previous)
        (recur (inc previous) (inc i))))
    (assoc c :digits xs)))      
      
(defn increment
  "Auxillary function to increment a number represented in a combinatorial  
   basis. Increments the digit at k, or adds a digit via add-digit."
  [^combination {:keys [k n digits] :as c}]
  (let [base    (- n k)
        idx     (scan k base digits)]    
    (if (< idx 0) c
      (let [current (aget ^longs digits idx)]
        (if (= current (+ base idx))        
          (if (> idx 0)
            (add-digit c (dec idx))
            c)
          (let [xs (aclone ^longs digits)]
            (aset xs idx (inc current))
            (assoc c :digits xs))))))) 


(defn ^long middle
  "Aux Used for binary search."
  [^long l ^long u] (+ l (quot (- u l) 2)))

(defn big-middle
  "Aux Used for binary search."
  [^long l ^long u] (+ l (quot (- u l) 2)))

(defn binary-nearest [l u f x]
  (let [nearest-x (fn [left right]
                    (let [fl (f left)
                          fu (f right)]
                      (cond (and (<= fl x) (> fu x)) left
                            (<= fu x) right)))]                               
    (loop [lower l
           upper u]
      (cond  (= (- upper lower) 1) (nearest-x lower upper)
             (= upper lower) upper  
             :else 
             (let [n      (middle lower upper)
                   guess  (f n)]
               (cond (= guess x) n
                     (< guess x) (recur n upper)
                     (> guess x) (recur lower  n))))))) 

(defn big-binary-nearest [l u f x]
  (let [nearest-x (fn [left right]
                    (let [fl (f left)
                          fu (f right)]
                      (cond (and (<= fl x) (> fu x)) left
                            (<= fu x) right)))]                               
    (loop [lower l
           upper u]
      (cond  (= (-' upper lower) 1) (nearest-x lower upper)
             (= upper lower) upper  
             :else 
             (let [n      (big-middle lower upper)
                   guess  (f n)]
               (cond (= guess x) n
                     (< guess x) (recur n upper)
                     (> guess x) (recur lower  n)))))))
  
(defn ^long choice-search
  "Search function that, given n elements chosen k at a 
   time, and a target quantity, finds the lowest value for n, such that 
   (choose n k) is less than or equal to the target."
  [^long n ^long k ^long target]
  (assert (>= target 0) "Target cannot be negative")
  (binary-nearest 0 n #(choose % k) target))

(defn big-choice-search
  "Searches across big (arbitrary precision) integers."
  [^long n ^long k target]
  (assert (>= target 0) "Target cannot be negative")
  (big-binary-nearest 0 n #(big-choose % k) target))

;;Projecting Integers on a Combinatoric Basis
;;===========================================
;;A combinadic, in its usage here, is a mapping of a combinatorial basis, n k,  
;;onto a set of digits, dk dk-1 dk-2...d1,  to provide a basis function for  
;;numbers.  In the case, our digits [dk...d1] correspond to coeffecients in  
;;(combi-sum digits) = (sum (choose dk k) (choose dk-1 k-1) ... (choose d1 1))  
;;(combinadic->int digits) =  (combi-sum digits)

;;In other words, we convert between integers and a combinatorial numerical 
;;basis by "counting" in base (choose _ k).

;;Replace the naive-choice-search with (choice-search n-bound current-k acc)

(defn ^combination int->combinadic
  "Given a combinatorial basis, n choose k, represents a target integer as a 
   combinadic, or a combination of k digits, which composed under the 
   combinatorial basis, equal target.  We compute the combinadic from
   most-significant to least-significant digit."
  [^long n ^long k ^long target]
  (let [^longs ds (long-array k (long 0))]
    (loop [n-bound   n
           kdx       0
           current-k k
           acc target]
      (if (= kdx k) (->combination false n k ds)
        (let [n-chosen  (long (choice-search n-bound current-k acc))                          
              delta     (long (choose n-chosen current-k))]
          (aset ds kdx n-chosen)                                  
          (recur n-chosen 
                 (unchecked-inc kdx) 
                 (unchecked-dec current-k) 
                 (unchecked-subtract acc delta)))))))

;;Replace the naive-choice-search with (big-choice-search n-bound current-k acc)

(defn ^combination bigint->combinadic
  "Identical to int->combinadic, but applies to big (arbitrary precision) 
   integers"
  [^long n ^long k target]
  (let [ds (long-array k (long 0))]
    (loop [n-bound   n
           kdx       0
           current-k k
           acc target]
      (if (= kdx k) (->combination true n k ds)
        (let [n-chosen  (long (big-choice-search n-bound current-k acc))                          
              delta     (big-choose n-chosen current-k)]
          (aset ^longs ds kdx n-chosen)                                  
          (recur n-chosen 
                 (unchecked-inc kdx) 
                 (unchecked-dec current-k) 
                 (-' acc delta)))))))

;;Computing Indices
;;=================

;;In the algorithm, if we have 5 choose 3 - or 10 - combinations, we can apply 
;;a lexicographic ordering to all possible combinations, establishing a mapping 
;;between the integers [0 1 2 3 4 5 6 7 8 9] and a unique combination.  
;;There is a dual mapping of [9 8 7 6 5 4 3 2 1 0].
;;Each combinatioral "number" in this ordering is - in a sense - an equal 
;;"distance" from the "number" at its dual.  There is a way to compute said 
;;distance, and use it to transform between the duals.

;;We can exploit an interesting property: 
;;Given a desired index m, say 0, x - 9 - is the dual of m, and x maps to a 
;;combinadic whose digits, subtracted from n - 1, yield the combinadic for m.
;;For example in the lexicographical ordering of the combinations of 5 choose 3,
;;assuming a function 'element', which takes n, k, and the desired entry m, 
;;the 0th element m is:
;;>(element 5 3 m) = (for [d (int->combinadic 5 3 (dual (choose 5 3) m))]
;;                     (subtract (dec n) d)
;;                 = (for [d (int->combinadic 5 3 9)]
;;                     (subtract 4 d))
;;                 = (for [d [4 3 2]] (- 4 d))
;;                 = (0 1 2)
;;the 9th element is:
;;>(element 5 3 9) = (for [d (int->combinadic 5 3 0)] (subtract 4 d))
;;                 = (for [d [2 1 0]] (subtract 4 d))
;;                 = (2 3 4)  

;;We have a function that maps from indices to dual combinadics to combinations.

(defn get-dual-index
  "Computes the lexicgraphic 'dual' of the index m, where m is assumed to be the 
   mth lexicgraphical element of a combination defined by n k."
  [^long n ^long k ^long m] (- (dec (choose n k)) m))

(defn big-get-dual-index 
  "Identical to get-dual-index, but applies to big (arbitrary precision) 
   integers."
  [^long n ^long k m] (-' (dec' (big-choose n k)) m))


(defn ^combination small-mth-lexicographic-element [^long n ^long k ^long m]  
  (let [x (long (get-dual-index n k m))
        ^combination c (int->combinadic n k x)
        ^longs digits  (:digits c) 
        mapped  (amap ^longs digits idx ^longs ret 
                      (- (dec n) (aget ^longs digits idx)))]
    (->combination false n k mapped)))

(defn big-mth-lexicographic-element [^long n ^long k m]
  (let [x (big-get-dual-index n k m)
        ^combination c (bigint->combinadic n k x)
        ^longs digits (:digits c)
        mapped (amap digits idx ^longs ret (- (dec n) (aget digits idx)))]
    (->combination true n k mapped)))

;;API Functions
;;=============

(defn get-lexographer
  "Convenience function to derive an appropriate mapping function for us.
   Returns a function the maps integers combinations."
  [n k]
  (let [big-limit (big-choose n k)]
    (if (long-able? big-limit)
      (fn [m]       
        (assert (and (>= m 0) (<= m (dec (choose n k)))) 
                (str "index " m "out of bounds: "  [0 (dec (choose n k))]))
        (small-mth-lexicographic-element n k m))
      (fn [m] 
        (assert (and (>= m 0) (<= m (dec' big-limit))) 
                (str "index " m "out of bounds: "  [0 (dec' big-limit)]))
        (big-mth-lexicographic-element n k m)))))

(defn mth-lexicographic-element
  "Centralized api for computing the mth lexicographic element of a mathematical
   combination.  Dispatches depending on whether the possible combinations of 
   m can be computed using longs, or whether arbitrary precision integers are 
   required."
  [n k m]
  ((get-lexographer n k) m))

(defn comb-stream [n k]
  (let [f (if (big-combination? n k) big-choose choose)]
    (map (partial mth-lexicographic-element n k) (range (f n k)))))

(defn combination->domain
  "Given an indexed domain of elements, vector v, and a combination, projects 
   the combination into a vector of choices from the domain."
  [v ^combination c]
  (->> (:digits c)
       (reduce (fn [acc ^long idx] (conj! acc (nth v idx))) (transient []))
       (persistent!)))

;;Auxillary function.
(defn memoize-if [v f] (if v (memoize f) f))

;;NOTE I AM MEMOIZING, and mixing possibly huge inputs.  We may want to rethink
;;this.
(defn combination-map 
  "Given a sequence of inputs s, and a choice value, returns a function that 
   maps x, an index in the lexigraphic ordering of the unique elements of 
   s when chosen k at a time, to a k-length vector with entries drawn from the 
   unique elements of s."
  [s k & {:keys [cached?] :or {cached true}}]
  (let [v (vec (distinct s))
        n (count v)
        combination-map (memoize-if cached? (partial combination->domain v))
        m->combination  (get-lexographer n k)]
    (comp combination-map m->combination)))

(defn span
  "Returns 10 samples spanning the combinatorial domain s, k items at a time."
  [s k]
  (let [n (count (distinct s))
        f (combination-map s k)
        width (quot (choose n k) 10)]
    (->> (iterate #(+ % width) 0)
      (take 10)
      (map f))))

;;Primary Data Structure
;;======================

(import clojure.lang.MapEntry)

;;Here we implement a light facade over the combination-map abstraction, 
;;providing something that can be viewed and queried much like a native clojure 
;;data structure.

;;I adopted the code from Mark Engelberg's excellet priority map example.
;;

(def no-op #(throw (Exception. "operation not supported")))

(deftype lexmap [elements bin-size size mapping]    
  Object
  (toString [this] (str "#collective.combinatoric.lexmap" 
                        [(count elements) bin-size]))      
  
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this item] (mapping item))
  (valAt [this item not-found] (mapping item))
  
  clojure.lang.IPersistentMap
  (count [this] size)
  (assoc [this item priority] (no-op))        
  (empty [this] this)
  
  ; cons defines conj behavior
  (cons [this e] (no-op))
  (equiv [this o] (.equiv elements o))
  (hashCode [this] (.hashCode elements))
  (equals [this o] (identical? this o))
  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this item] (< item (dec size)))
  (entryAt [this k]
    (let [v (.valAt this k this)]
      (when-not (identical? v this) ;might need to yank this guy.
        (MapEntry. k v))))
  (seq [this] 
    (map-indexed (fn [i k] (MapEntry. i (mapping i))) (range size))) 
  ;      ;without implements (dissoc pm k) behavior
  (without [this item] (no-op))
    
  clojure.lang.Indexed
  (nth [this i] (mapping i))
  (nth [this i not-found] (if (and (>= i 0) (< i size)) (mapping i) not-found))
  
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
;      clojure.lang.IObj
;      ;adds metadata support
;      (meta [this] {})
;      (withMeta [this m] (no-op))
      
  clojure.lang.Reversible
  (rseq [this]
    (seq (map (fn [i] (MapEntry. i (mapping i))) (reverse (range size))))))

(defn combinatoric-map
  "Creates a handle for an abstract mapping of integers to a lexicographically 
   ordered combination of the distinct elements of s, chosen k at a time.  
   Provides typical useful clojure library functions.
   Caller may supply a number as the value for s, akin to describing the 
   combinations of a number of undefined elements, in which the ordering will 
   return combinations of integers as expected."
  [s k & {:keys [cached?] :or {cached true}}]
  (let [s  (if (number? s) (range s) s) 
        mapping (combination-map s k :cached cached?)
        v (vec (distinct s))
        n (count v)        
        size (big-choose n k)]
    (->lexmap v k size mapping)))

(defn combination->key
  "Returns the key, or a value for m where m is inverse mapping from combation
   to lexicographic indices.  Essentially the inverse of 
   mth-lexicographic-element"
  [cmap combination]
  (let [binfunc (if (long-able? (count cmap))
                  binary-nearest 
                  big-binary-nearest)]
    (binfunc 0 (count cmap) #(get cmap %) combination)))

(defn profile-combinatoric-map [sample-count m]
  (let [n       (count m)
        samples (vec (take sample-count (repeatedly #(rand-int n))))]
    (time 
      (loop [xs samples]
        (when (not (empty? xs))         
          (do (nth m (first xs))
              (recur (rest xs))))))))

;;testing 
(comment 

  (def people [:john :rick :tom])
  (def alphabet ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P"
                 "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"])  
  (def nums  (range 1000))
  
  (def people-combos (combination-map people 2))
  (def ys (map people-combos [0 1 2]))
  ;;([:john :rick] [:john :tom] [:rick :tom])
  
  (def xs (map (combination-map nums 5) 
               [0 55 233 567 899 905 100000 230000]))
;  ([0 1 2 3 4]
;   [0 1 2 3 59]
;   [0 1 2 3 237]
;   [0 1 2 3 571]
;   [0 1 2 3 903]
;   [0 1 2 3 909]
;   [0 1 2 108 989]
;   [0 1 2 269 579])

  (def bigger-xs (span nums 5))  
;  ([0 1 2 3 4]
;   [20 353 429 549 640]
;   [43 221 339 667 952]
;   [68 317 817 932 957]
;   [96 527 550 599 799]
;   [129 174 463 514 534]
;   [167 192 265 789 920]
;   [213 362 685 740 902]
;   [274 450 465 555 936]
;   [368 423 570 663 795])

;;This is possibly wrong.  Original test data might've been wrong though..
  (def alphabet-quadruples (span alphabet 4)) 
;(("A" "B" "C" "D")
; ("A" "I" "J" "V")
; ("B" "E" "W" "Z")
; ("C" "D" "N" "W")
; ("C" "Q" "R" "X")
; ("D" "P" "Q" "W")
; ("F" "G" "H" "N")
; ("G" "J" "P" "W")
; ("I" "J" "P" "V")
; ("K" "P" "V" "W"))

  (def num-lits (span (vec (map (comp keyword str) (range 26))) 4)) 
;a map-like structure
(def the-map (combinatoric-map alphabet 2))
(def random-pairs 
  (map the-map (take 10 (repeatedly #(rand-int (count the-map))))))
;(["A" "Z"]
; ["M" "T"]
; ["C" "R"]
; ["Q" "V"]
; ["H" "R"]
; ["A" "D"]
; ["F" "J"]
; ["B" "M"]
; ["A" "C"]
; ["C" "T"])      

(= (count the-map) (choose (count alphabet) 2))
;true

;;Large combinatorial domains...

(def bigmap (combinatoric-map 1000 8))

(first bigmap)
;;[0 [0 1 2 3 4 5 6 7]]

(second bigmap)
;;[1 [0 1 2 3 4 5 6 8]]

(take 10 bigmap)
;;([0 [0 1 2 3 4 5 6 7]]
;; [1 [0 1 2 3 4 5 6 8]]
;; [2 [0 1 2 3 4 5 6 9]]
;; [3 [0 1 2 3 4 5 6 10]]
;; [4 [0 1 2 3 4 5 6 11]]
;; [5 [0 1 2 3 4 5 6 12]]
;; [6 [0 1 2 3 4 5 6 13]]
;; [7 [0 1 2 3 4 5 6 14]]
;; [8 [0 1 2 3 4 5 6 15]]
;; [9 [0 1 2 3 4 5 6 16]])

(get bigmap 1000)
;;[0 1 2 3 4 5 7 15]

(big-choose 1000 8)
;;24115080524699431125N

(count bigmap)
;;1420194005 ;this seems off, should be 24115080524699431125N

(get bigmap (/ (big-choose 1000 8) 2))
;;[82 229 332 342 467 699 704 980]

(profile-combinatoric-map 1000 the-map)    

)