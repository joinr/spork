;;Possibly useful operations on records.
(ns spork.util.record
  (:use [spork.util.general :only [serial-comparer orient-comparer
                                   align-fields-by]])
  (:require [clojure.set]
            [clojure [walk :as walk]]))

(defn sub-record
  "Returns a selection of fields from r.  Automatically aligns the record
   by the order of supplied fields, if fields is a seqeuential collection."
  [r fields]
  (let [fset (set fields)
        m (into {} (for [[k v] r :when (contains? fset k)] [k v]))]
    (if (sequential? fields)
      (align-fields-by fields m)
      m)))

(defn get-fields
  "Converts fields xs into a vector of values ys, where each value y is 
   the value associated with x in map r."
  [r xs]
  (vec (map (fn [x] (get r x)) xs)))

;stolen from stack overflow
(defn static? [field]
  (java.lang.reflect.Modifier/isStatic
   (.getModifiers field)))

(defmacro record-headers
  "Returns a vector of the field names from a record type."
  [recname]
  (let [rname (symbol (str "->" recname))]
  `(vec (map str (first (:arglists (meta #'~rname)))))))

(defn get-record-field-names [record]
  (->> record
       .getDeclaredFields
       (remove static?)
       (map #(.getName %))
       (remove #{"__meta" "__extmap"})))

(defmacro empty-record [record]
  (let [klass (Class/forName (name record))
        field-count (count (get-record-field-names klass))]
    `(new ~klass ~@(repeat field-count nil))))

;(defn replace [record fld v] (merge record {fld v}))

(defn inc-field 
  ([m k amt] (assoc m k (+ (get m k) amt)))
  ([m k] (inc-field m k 1)))

(defn dec-field 
  ([m k amt] (inc-field m k (* amt -1)))
  ([m k] (inc-field m k -1)))

(defn get-vals [m ks]
  (map (partial get m) ks))

(defn get-path
  ([m k] (get m k))
  ([m k & ks] (let [res (get m k)]
                (if ks
                  (recur res (first ks) (next ks))
                  res))))

(defn assoc-many
  "Initialize a record with default value v.  If fields are specified, only 
   the fields received the default"
  ([rec v flds] (merge rec (zipmap flds (repeat v))))
  ([rec v] (assoc-many rec v (keys rec))))


(defn transfer 
  "Transfer key/value pair associated with k from record r1 to 
   records in rs.  The result of the donor record is held in metadata."
  ([r1 k r2] (with-meta [(assoc r2 k (get r1 k))] {:from (dissoc r1 k)}))
  ([r1 k r2 & rs] (let [v (get r1 k)
                        init (transfer r1 k r2)] 
                    (reduce #(conj %1 (assoc %2 k v)) init  rs))))

(defn trans 
  "Transfer key/value pair associated with k from record r1 to 
   records in rs.  The result of the donor record is held in metadata."
  ([r1 k r2] [(dissoc r1 k) (assoc r2 k (get r1 k))])
  ([r1 k r2 & rs] (let [v (get r1 k)
                        init (trans r1 k r2)] 
                    (reduce #(conj %1 (assoc %2 k v)) init  rs))))

(defn- parse-fields
  "Parses a vector of field definitions for defrecord+ .  Where values are
   nested vectors of symbols, ala [fieldname expr], an entry is added to 
   the defaults map under {:fieldname expr}.  Where values are symbols
   expressions, an entry is added under {:fieldname nil}."
  [xs]
  (loop [defaults {}
         fields xs]
  (if (empty? fields) 
    defaults
    (let [f (first fields)
          nextdef 
          (cond (symbol? f) (assoc defaults (keyword f) nil)
                (vector? f) (assoc defaults (keyword (first f))
                                   (second f))
                :else 
                (throw 
                  (Exception. 
                    (str "bad specification in parse-fields:" (str f)))))]
      (recur nextdef (rest fields))))))

(defn- make-constructor
  "Aux for defrecord+. Makes a parameterized constructor for record defined by 
   name and the fieldmap.  If default values specified by the fieldmap, they 
   get used, otherwise fields are nil.  Allows fine-grained control over the 
   creation of records."
  [name fieldmap fields]
  (let [keymap  (into {} (map (fn [k] 
                                [(symbol (subs (str k) 1)) k]) 
                              (keys fieldmap)))
        defaults (reduce (fn [acc k] 
                           (assoc acc k (get fieldmap (get keymap k)))) {}
                         fields)
        args {:keys fields :or defaults}]
    `(defn ~(symbol (str "make-" name)) [& ~args] 
       (~(symbol (str "->" name)) ~@fields))))


(defn with-record [r  & update-keys]
  (merge r 
       (apply hash-map update-keys)))

(defn merge-from
  "Similar semantics to clojure.core/merge, except that it only 
   merges keys that already exist in m.  Thus, the first map m effectively
   constrains the fields that may be merged.  Used to update records 
   in a safe manner that does not violate their fields, thus causing a 
   conversion from the record type to a generic hashmap."
  [m & ms]
  (let [fields (set (keys m))]
    (reduce (fn [acc m2]
              (reduce (fn [m1 k] (assoc m1 k (get m2 k))) 
                      acc
                      (clojure.set/intersection fields (set (keys m2)))))
            m ms)))
                

(defn- parse-record-opts
  "Parses a list of opts+specs to extract doc strings, fields, and specs.
   Not currently used....intended to allow documentation support in defrecord+"
  [opts+specs]
  (loop [acc {}
         args opts+specs]
    (if (empty? args) 
      acc
      (let [arg (first args)
            nextacc (cond (string? arg) (assoc acc :doc arg)
                          (vector? arg) (if (contains? acc :fields) 
                                          (assoc acc :specs arg)
                                          (assoc acc :fields arg))
                          :else 
                          (throw (Exception. 
                                   (str "Unknown option in defrecord+ spec: " 
                                        arg))))]
        (recur nextacc (rest args))))))  

(defmacro defrecord+ 
  "Wrapper for defrecord, which follows the Common Lisp tradition of allowing
   inline definition of default fields ala defstruct.  When parsing the record
   definition, if a vector is encountered as a field definition, the vector is 
   parsed as [name defaultvalue].  A make-[recordname] function is created, 
   which takes keys as optional arguements, one for each field.  It might be 
   nice to include type hinting and all that...but i'm not there yet..."
  [name [& fields] & opts+specs]
  (let [hinted-fields (second &form) 
        rawfields     (into [] (map (fn [x] 
                                      (let [m (meta x)]
                                        (with-meta (if (coll? x) (first x)  x) m))) fields))
        default-constructor (make-constructor name 
                                              (parse-fields fields) 
                                              rawfields)]
    `(do
       (defrecord ~name ~rawfields ~@opts+specs)
       ~default-constructor)))

(defn field-comparer
  "Builds a comparison function that extracts a fld from two records
   and, if keyfunc is provided, applies keyfunc to the values, comparing the 
   resulting keys with clojure.core/compare.   If no key function is provided, 
   the raw field values are compared.
   
   Note: fields are pretty general: this fn only relies on 
   clojure.core/get, so anything that can be treated by get can be used as a 
   source of fields.  That includes vectors, which are treated as associations 
   between numeric fields (indices) and values by clojure.core/get." 
  [fld & {:keys [keyfunc]}]
  (if keyfunc
    (fn [rec1 rec2] 
      (compare (keyfunc (get rec1 fld))
               (keyfunc (get rec2 fld))))
    (fn [rec1 rec2] 
      (compare (get rec1 fld)
               (get rec2 fld)))))

(defn- ordering->fieldcomp
  "Parses a field ordering into field comparer f, where field orderings are
   of the form:
     :field  
   | [comparison-function :ascending|:descending]
   | [fieldname comparison-function :ascending|:descending]     
   | [fieldname :ascending|:descending]

   and f::a->a->comparison.  Returns a function f, which is either a properly 
   oriented field comparison function, or an oriented record-comparison 
   function - if caller passed in a custom comparison function in the spec."  
  ([field keyfunc ordering] (orient-comparer 
                              (field-comparer field :keyfunc keyfunc) ordering))
  ([field ordering]  (orient-comparer (field-comparer field) ordering))
  ([spec-or-field] (if (coll? spec-or-field)
                     (if (fn? (first spec-or-field))
                       (orient-comparer 
                         (first spec-or-field) (or (second spec-or-field)
                                                   :ascending))
                       (case (count spec-or-field)
                         1 (ordering->fieldcomp (first spec-or-field)
                                                :ascending)
                         2 (ordering->fieldcomp (first spec-or-field) 
                                                (second spec-or-field))
                         3 (ordering->fieldcomp (first spec-or-field) 
                                                (second spec-or-field)
                                                (nth spec-or-field 2))
                         (throw (Exception. 
                                  (str "Too many elements in spec, 3 max"
                                       spec-or-field)))))
                     (ordering->fieldcomp spec-or-field :ascending))))
                             
(defn serial-field-comparer
  "Given a sequence of field orderings, where values are  either atoms 
   of the form:
   :field  
   | [comparison-function :ascending|:descending],
   | [fieldname comparison-function :ascending|:descending]     
   | [fieldname :ascending|:descending]
 
   creates a function that compares two records sequentially using the 
   orderings, as with util.general/compare-many.  Short circuits as soon as a 
   valid comparison is found."
  [orderings]
  (serial-comparer (map ordering->fieldcomp orderings)))                                                                                                                                                                                     


;A function for generating keys from records or associative structures.
(defmulti key-function class)
(defmethod key-function :default [f] (fn [rec] (f rec)))
(defmethod key-function clojure.lang.Keyword [x] (fn [rec] (get rec x)))
(defmethod key-function java.lang.String [x] (fn [rec] (get rec x)))
(defmethod key-function clojure.lang.PersistentVector [x]
  (let [keyfs (apply juxt (map key-function x))]
    (fn [rec]  (keyfs rec))))

;(defmacro query-with [m querydef]
;  `(let [m# ~m
;         ?# (fn [k v] (get m k))]
;     (apply (fn [& {:keys [~'add-fields ~'by ~'as]}]
;              (when ~'add-fields 
;                (
;
;
;(query m
;	'{:select [:ACStr :RCStr :NGStr :GhostStr :OtherStr]
;    :by (let [strength (? :STR)
;              as-Filled (fn [k] (keyword 
;                                  (str (subs (str k) 0 1) "Filled")))] 
;          (fn [k v] (* strength (? (as-Filled k)))))})
;
;(query m
;	'{:select [:ACStr :RCStr :NGStr :GhostStr :OtherStr]
;    :as }
;    
;
;
;
;(fn [{:keys [STR TotalRequired] :as m}]
;  (let [? (fn [f] (get m f))]  
;    {:ACFilled (* STR (? :ACFilled)) 
;     :RCFilled (* STR (? :NGFilled))
;     :GhostFilled (* STR (? :GhostFilled)) 
;     :OtherFilled (* STR (? :OtherFilled))}))
    

;;quick example of how to do field lookup thunks in clojure.
(comment 
(deftype fieldlookup [^long x y z ^clojure.lang.IPersistentMap extmap]
  clojure.lang.ILookup
  (valAt [this k] (.valAt this k nil))
  (valAt [this k else] 
    (case k
      :x x 
      :y y 
      :z z
      (get extmap k else)))
  clojure.lang.IKeywordLookup
  (getLookupThunk [this k] 
    (case k
      :x (reify clojure.lang.ILookupThunk
           (get [thunk gtarget]
             (if (identical? (class gtarget) fieldlookup)
               (. ^fieldlookup gtarget -x)
               thunk)))
      :y  (reify clojure.lang.ILookupThunk
            (get [thunk gtarget]
              (if (identical? (class gtarget) fieldlookup)
                (. ^fieldlookup gtarget -y)
                thunk)))
      :z (reify clojure.lang.ILookupThunk
           (get [thunk gtarget]
             (if (identical? (class gtarget) fieldlookup)
               (. ^fieldlookup gtarget -z)
               thunk)))
      nil)))
  
)


;;somehow condp is faster than mine; they look identical, although the
;predicate is bound earlier...
(defmacro case-if [k & kvs]
  (let [n (count kvs)
        _ (assert (if (odd? n) (> n 1) true)
                  "either use even cases or odd with final as a default")
        s (gensym "k")]
    `(condp identical? ~k ~@kvs)))

(defmacro fastrecord
  "Like defrecord, but adds default map-like function application
   semantics to the record.  Fields are checked first in O(1) time,
   then general map lookup is performed.  Users may supply and optional
   ^:static hint for the arg vector, which will enforce the invariant
   that the record always and only has the pre-defined fields, and
   will throw an exception on any operation that tries to access
   fields outside of the predefined static fields.  This moves
   the record into more of a struct-like object.

   Note: this is not a full re-immplementation of defrecord,
   and still leverages the original's code emission.  The main
   difference is the implementation of key-lookup semantics
   ala maps-as-functions, and drop-in performance that should
   be equal-to or superior to the clojure.core/defrecord
   implementation.  Another refinement that makes arraymaps
   superior for fields <= 8, is the avoidance of a case dispatch
   which is slower in practice than a linear scan or a
   sequential evaluation of if identical? expressions.
   Small records defined in this way should be competitive
   in general purpose map operations."
  [name keys & impls]
  (let [fields (map keyword keys)
        binds  (reduce (fn [acc [l r]]
                     (conj acc l r))
                   []
                   (map vector fields (map #(with-meta % {})  keys)))
        [_ name keys & impls] &form
        this (gensym "this")
        k    (gensym "k")
        extmap (with-meta '__extmap {:tag 'clojure.lang.ILookup})
        default (gensym "default")
        n       (count keys)
        caser   (if (<= n 8) 'case-if 'case)
        lookup (fn [method]
                 `[(~method [~this ~k]
                    (~caser ~k
                     ~@binds
                     ~(if (-> keys meta :strict)
                        `(throw (ex-info "key not in strict record" {:key ~k}))
                        `(~'.valAt ~extmap ~k))))
                   (~method [~this ~k ~default]
                    (~caser ~k
                     ~@binds
                     ~(if (-> keys meta :strict)
                        `(throw (ex-info "key not in strict record" {:key ~k}))
                        `(~'.valAt ~extmap ~k ~default))))])
        replace-val-at (fn [impls]
                         (->> impls
                              (remove (fn [impl]
                                        (and (seq impl)
                                             (#{'valAt  'clojure.core/valAt}
                                              (first impl)))))
                              (concat (lookup 'valAt))))
        replace-deftype (fn [emitted]
                          (->> emitted
                               (reduce (fn [acc x]
                                         (if (and (seq? x)
                                                  (= (first x) 'deftype*))
                                           (let [init (take 6 x)
                                                 impls (drop 6 x)]
                                             (conj acc (concat init
                                                               (replace-val-at impls))))
                                           (conj acc x))) [])
                               seq))
        rform (->> `(~'defrecord ~name ~keys ~@impls
                     ~'clojure.lang.IFn
                     ~@(lookup 'invoke))
                   macroexpand-1
                   replace-deftype
                   (walk/postwalk-replace {'clojure.core/case caser
                                           'case caser}))]
    `(~@rform)))


spork.util.record> (let [fr (assoc (fr. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get fr :x)))
Evaluation count : 53604480 in 6 samples of 8934080 calls.
             Execution time mean : 9.381978 ns
    Execution time std-deviation : 0.057688 ns
   Execution time lower quantile : 9.321729 ns ( 2.5%)
   Execution time upper quantile : 9.436063 ns (97.5%)
                   Overhead used : 1.860554 ns
nil
spork.util.record> (let [fr (assoc (fr. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get fr :c)))
Evaluation count : 34729932 in 6 samples of 5788322 calls.
             Execution time mean : 15.436899 ns
    Execution time std-deviation : 0.237012 ns
   Execution time lower quantile : 15.263644 ns ( 2.5%)
   Execution time upper quantile : 15.810472 ns (97.5%)
                   Overhead used : 1.860554 ns

Found 1 outliers in 6 samples (16.6667 %)
	low-severe	 1 (16.6667 %)
 Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
nil
spork.util.record> (let [fr (assoc (r. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get fr :x)))
Evaluation count : 31582332 in 6 samples of 5263722 calls.
             Execution time mean : 17.620178 ns
    Execution time std-deviation : 0.734314 ns
   Execution time lower quantile : 17.137678 ns ( 2.5%)
   Execution time upper quantile : 18.777557 ns (97.5%)
                   Overhead used : 1.860554 ns

Found 1 outliers in 6 samples (16.6667 %)
	low-severe	 1 (16.6667 %)
 Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
nil
spork.util.record> (let [fr (assoc (r. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get fr :c)))
Evaluation count : 22709778 in 6 samples of 3784963 calls.
             Execution time mean : 24.598666 ns
    Execution time std-deviation : 0.030144 ns
   Execution time lower quantile : 24.567735 ns ( 2.5%)
   Execution time upper quantile : 24.637346 ns (97.5%)
                   Overhead used : 1.860554 ns
nil
spork.util.record> (let [fr (assoc (r. 10 10) :a 2 :b :3 :c 4)]  (c/quick-bench (get fr :c)))
