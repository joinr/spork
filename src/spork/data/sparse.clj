;;Namespace for sparse persistent data structures, and
;;flyweight data structures, particularly for use with
;;spork.util.table.
(ns spork.data.sparse)

;;lookup the item associated with the nested vectors in xs.
;; (definline row-col [row col xs]
;;   (let [xs (with-meta xs {:tag 'clojure.lang.PersistentVector})
;;         c  (with-meta (gensym "c") {:tag 'clojure.lang.PersistentVector})]
;;     `(let [~c (.nth ~xs ~col)]
;;        (.nth ~c ~row))))

(defn row-col [row col ^clojure.lang.IPersistentVector cols]
  (.nth ^clojure.lang.Indexed (.nth cols col) row))
(defn assoc-row-col [^clojure.lang.IPersistentVector cols row col v]
  (let [^clojure.lang.IPersistentVector c (.nth cols col)]
    (.assocN cols col (.assocN c row v))))

(defn index-of [itm xs]
  (reduce-kv (fn [acc idx x]
               (if (= x itm)
                 (reduced idx)
                 acc)) nil xs))
(defn drop-indices [idxs v]
  (reduce-kv (fn [acc idx x]
               (if (contains? idxs idx)
                 acc
                 (conj acc x)))
             [] v))

(defn sparse-seq
  ([idx rows length]
   (when (< idx length)
     (if-let [l (first rows)]
       (if (< idx (first l))
         (concat (repeat (- (first l) idx) nil)
                 (sparse-seq
                  (first l) rows length))
         (let [r (or (second rows) [length nil])
               [lidx lval] l
               [ridx rval] r
               len  (dec (- ridx lidx))]
           (cons lval (concat (repeat len nil)
                              (sparse-seq
                               ridx (rest rows) length)))))
       (if (== idx (dec length)) nil
           (repeat (- length idx) nil)))))
  ([rows length] (sparse-seq 0 rows length)))

;;__Sparse Hashing__
;;So...we'd like our sparse columns to be considered
;;structurally equal to normal vectors.  They just
;;take far fewer entries.
;;The hashing scheme clojure uses, with some tweaks, is this:
(defn simple-hash [xs]
  (reduce (fn [acc x]
            (unchecked-add-int
             (unchecked-multiply-int acc 31)
             (hash x)))
          1
          xs))
;;After which we pass it to another function for better hashing,
;;called mix-collection-hash.

;;Some ground rules: nil, like 0, hashes to 0.
;;So...if we have a vector of 10 nils, we get
;;(simple-hash (vec (repeat 10 nil))) => 31^10, although
;;with intentional integer overflow, we end up with
;;something negative....
;;-1796951359
;;(= (simple-hash (vec (repeat 10 nil))) (int-pow 31 10))
(defn int-pow [base exp]
  (cond (zero? exp) 1
        (neg? exp) (throw (Exception. "no negative powers at the moment"))
        :else
        (loop [i 1
               acc base]
          (if (== i exp) acc
              (recur (unchecked-inc i)
                     (unchecked-multiply-int acc base))))))
;;Since we're working with multiplication and addition, we
;;can use int-pow to compute the factor for our zeroes
;;in one step.

;;__Data Structures__
;;Simulates a vector with one or more rows.  Backed by a hashmap.
;;No equality checks at the moment, so we can't use it for hashkeys.
;;A sparse vector is no different than a vector, so we should be
;;able to simulate hash-codes easily enough.
;;We know the formula for hashing... multiply the hash of the value
;;by 31 and sum.
;;If every other value is nil, then the contribution is....
(deftype sparserows [^clojure.lang.IPersistentMap rows length]
  clojure.lang.ILookup
  ;;gives us (Get pm key) ... behavior
  (valAt [this k]
    (if-let [v (.valAt rows k)]
      v
      (cond (and (>= k 0) (< k length)) nil
            :else (throw (Exception.
                          (str "Index " k " out of bounds for sparserows"))))))
  (valAt [this k not-found]
    (if-let [v (.valAt rows k)]
      v
      (cond (and (>= k 0) (< k length)) nil
            :else not-found)))
  clojure.lang.IPersistentVector
  (count [this] length)
  (assocN [this k v]
    (if (>= k 0)
      (sparserows. (assoc rows k v) (max (unchecked-inc k) length))
      (throw (Exception. (str "cannot assoc a negative index in sparserows! " k)))))
  (empty [this] (sparserows. {} 0))
  ;;cons defines conj behavior
  (cons [this e] (.assocN this e length))
  (equiv [this o] (throw (Exception.
                          (str "Equiv not implemented for sparserows!"))))
  (hashCode [this] (* (hash rows) length))
  (equals [this o] (identical? this o))
  ;;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (and (pos? k) (< k length)))
  (entryAt [this k]
    (if-let [e (.entryAt rows k)] e
            (if (< k length) nil
                (throw (Exception. (str "Entry " k " out of bounds in sparserows!"))))))
  (seq [this] (sparse-seq 0 rows length))
  ;;without implements (dissoc pm k) behavior
  ;; (without [this k]
  ;;   (if (< k length)
  ;;     (sparserows. (dissoc rows k) (max (unchecked-inc k)
  ;;                                       (unchecked-dec length)))))
  clojure.lang.Indexed
  (nth [this i] (.valAt this i))
  (nth [this i not-found] (.valAt this i not-found))
  )


;;Simulates a persistent vector.  Only stores one value,
;;however will act like a persistent vector in that
;;calls will return nil for values within the range
;;defined by length.
;;Since we know the count, and the number of nils, we
;;can hash this as if it were a bunch of entries.
(deftype sparsecolumn [row val length]
  clojure.lang.ILookup
  (valAt [this k]
    (cond (== k row) val
          (and (pos? k) (< k length)) nil
          :else (throw (Exception.
                        (str "Index " k " out of bounds for sparserows")))))
  (valAt [this k not-found]
    (cond (== k row) val
          (and (pos? k) (< k length)) nil
          :else not-found))
  clojure.lang.IPersistentVector
  (count [this] length)
  (assocN [this k v]
    (cond (== k row) (sparsecolumn. row v length)
          (not (neg? k)) (.assocN (sparserows. {row k} length) k v) ;convert to sparserows
               :else (throw (Exception. (str "Cannot assoc a negative index in sparserows! " k)))))
  (empty [this] (sparsecolumn. 0 nil 0))
  ;cons defines conj behavior
  (cons [this e] (.assocN this length e))
  (equiv [this o] (throw (Exception.
                          (str "Equiv not implemented for sparsecolumn!"))))
  (hashCode [this] (* (hash val) row length))
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (and (pos? k) (< k length)))
  (entryAt [this k]
    (cond (== k row) (clojure.lang.MapEntry. row val)
          (< k length) nil
          :else (throw (Exception. (str "Entry " k " out of bounds in sparsecolumn")))))
  (seq [this] (concat (take row (repeat nil))
                      (cons val (take (dec (- length row))
                                      (repeat nil)))))
  clojure.lang.Indexed
  (nth [this i] (if (== i row) val
                    (.valAt this i)))
  (nth [this i not-found] (if (== i row) val
                              (.valAt this i not-found)))
  )
  

(defn ->sparsecolumn
  "This simulates a vector with a single entry, surrounded by nil values.
   Length is known, so it's indexed.  Really, we only have a value at index row.
   This is specifically meant to support flyrecords, and 
   to allow assoc and dissoc behaviors on them rather than building maps."
  ([row val length]
   (sparsecolumn. row val length))
  ([rows length] (sparserows. rows length)))

;;Easiest thing to do for efficient assoc/dissoc behavior...
;;is to use the exdisting columns and fields (as we would a table).
;;Assoc => add a sparse column.
(defprotocol ICursor
  (set-cursor [obj n]))
(defn mapeq [l r]
  (reduce-kv (fn [acc k v]
               (if-let [other (get r k)]
                 (if (= other v)
                   acc
                   (reduced nil))
                 (reduced nil)))
             true l))

;;__Flyrecord__
;;a flyrecord can have hashequivalence, based in its fields.


;;We can have a read-only recordset....
;;A subtable, if you will, that shares structure with the parent
;;table.
;;There might be an argument to move out tables to this format in the
;;future.  For now, it's an optimization on the underlying naive
;;vector-based table implementation.

(deftype flyrecord [^:unsynchronized-mutable ^long n
                    ^clojure.lang.PersistentVector fields
                    ^clojure.lang.PersistentVector columns
                    ^:unsynchronized-mutable ^int _hasheq
                    ^:unsynchronized-mutable ^int _hash]
  ICursor
  (set-cursor [obj idx] (do (set! n (long idx))
                            (set! _hasheq (int -1))
                            (set! _hash   (int -1))
                            obj))
  clojure.core.protocols.IKVReduce
  (kv-reduce [amap f init]
    (reduce-kv   (fn [acc col fld]
                    (f acc fld (row-col n col columns)))
               init fields))
  clojure.core.protocols.CollReduce
  (coll-reduce [coll f]
    (reduce-kv (fn [acc idx fld]
                 (f acc [fld (row-col n idx columns)]))
               [0 (.nth fields 0)]
               (subvec fields 1)))
  (coll-reduce [coll f val]
    (reduce-kv (fn [acc idx fld]
                 (f acc [fld (row-col n idx columns)]))
               val
               fields))
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (row-col n (index-of k fields)  columns))
  (valAt [this k not-found]
    (if-let [col (index-of k fields)]
      (row-col n col columns)
      not-found))
  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq (int -1))
      (let [h (hash-unordered-coll (seq this))]
        (do (set! _hasheq (int h))
            h))
      _hasheq))
  clojure.lang.IPersistentMap
  (count [this] (.count fields))
  (assoc [this k v]
    (if-let [idx (index-of k fields)]
      (flyrecord. n fields (assoc-row-col columns n idx v) -1 -1)
      (let [new-column (->sparsecolumn n v (count (first columns)))]
        (flyrecord. n (conj fields k) (conj columns new-column) -1 -1))))
  (empty [this] (flyrecord. 0 [] [] -1 -1))
  ;cons defines conj behavior
  (cons [this e]   (.assoc this (first e) (second e)))
  (equiv [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (instance? clojure.lang.IPersistentMap o) (and (== (count this) (count o))
                                                         (mapeq this o))
          (or (instance? clojure.lang.Sequential 0)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
              :else nil))  
  (hashCode [this]
    (if (== _hash (int -1))
      (let [h (hash-unordered-coll (seq this))]
        (do (set! _hash (int h))
            h))
      _hash))
  (equals [this o] (identical? this o))  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k]  (reduce (fn [acc k]
                                   (if (= k k) (reduced true)
                                       acc)) nil fields))
  (entryAt [this k]
    (reduce-kv
     (fn [acc idx fld]
       (if (= k fld)
         (let [^clojure.lang.PersistentVector col (.nth columns idx)]
           (reduced (clojure.lang.MapEntry. fld (.nth col n)))
           acc))) nil fields))
  (seq [this]
    (map-indexed (fn [idx fld]
                   (clojure.lang.MapEntry. fld (row-col n idx columns)))
                 fields))
  ;without implements (dissoc pm k) behavior
  (without [this k]
    (if-let [idx (index-of k fields)]
      (flyrecord. n (drop-indices #{idx} fields )
                  (drop-indices   #{idx} columns) -1 -1)))
  clojure.lang.Indexed
  (nth [this i] (row-col n i columns))
  (nth [this i not-found]  (if (<= i (.count fields))
                             (row-col n i columns)
                             not-found))
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (.seq this)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  ;; clojure.lang.IObj
  ;; ;adds metadata support
  ;; (meta [this] (meta entries))
  ;; (withMeta [this m] (avec. (.withMeta entries m)))
      
  clojure.lang.Reversible
  (rseq [this] (reverse (.seq this )))

  java.io.Serializable ;Serialization comes for free with the other things implemented
  ;clojure.lang.MapEquivalence
  )

(defn ->flyrecord
  "Creates a lightweight record abstraction on top of  a columnar table with a 
   matching vector of fields.  Each field corresponds to a column in the table.
   Fly records, for flyweight records, will provide what looks like a 
   hashmap, but is actually a view onto the underlying fields and column
   vectors. For computational tasks, this may not be preferable to other methods, 
   but for memoery intensive traversal and queries, we should see definite 
   benefits over copying record sequences as we currently do.  These are 
   particularly useful when we retain large tables in memoery and wish 
   to compute views on them, or view them as records instead of columns."
  [row flds cols]
  (flyrecord. row flds cols -1 -1))
