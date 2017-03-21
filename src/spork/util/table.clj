;;TOM SPOON 9 July 2012 
;;A simple library for reading and writing tabular representations of data.
;;Uses map as a container.
;;Vector-of-vector columnar data store.  Backing vectors may be primitive,
;;or rrb-trees or anything that implements persistentvector.
;;Pending implementation of clojure.core.matrix.dataset protocols to
;;enable compatibility with other apps.

;;Update Feb 2017 - util.table has grown significantly in scope, utility,
;;and effeciency.  It's pervasively used to define tables from schema,
;;lazily read streams of records, coerce to and from tables/files/record
;;seqs, query tables, perform arbitrary transformations, etc.  It fits
;;fairly well into the clojure idioms, although some of the operations
;;and API could use a review.  Still, this is one of the better libraries.
;;Based loosely off Peter Seibel's work in "Practical Common Lisp."
(ns spork.util.table
  (:require [clojure [string :as strlib]]
            [clojure [set :as setlib]]
            [clojure.core.reducers :as r]
            [spork.util.reducers]
            [spork.util [clipboard :as board] [parsing :as parse] [io :as io]
                        [string :as s] [stream :as stream]]
            ;;this costs us.
            [spork.cljgui.components [swing :as gui]]
            [spork.util.general  :as general :refer [align-by approx-order]]
            [spork.data.sparse :as sparse]
            [clojure.core.rrb-vector :as rrb])
  (:use [spork.util.vector]
        [spork.util.record  :only [serial-field-comparer key-function]]
        [clojure.pprint :only [pprint]]))


;;__Aux Functions__

;;RRB-Vectors are the primary backing for our typed-tables, since they
;;have transient primitive-backed collections (clojure.core/vector doesn't
;;at the time of writing) and are drop-in replacements for persistent vectors.
(extend-protocol clojure.core.protocols/CollReduce
 ;;If we don't have this, our vectors go through .nth instead of the internal
 ;;reduce implementation in VecSeq.  As such
 clojure.core.rrb_vector.rrbt.Vector
 (coll-reduce
   ([o f] (reduce f (seq o)))
   ([o f init] (reduce f  init (seq o)))))

;;primitive-backed table IO
;;Added for parsing and storing large tables.
(def primitives  #{:long   
                   :double
                   :int    
                   :float        
                   :boolean
                   :char
                   :short})
;;generate a type-column based on t, if it's primitive we use
;;primitive-backed vectors.  optionally initialize column with seq xs.
;;type information is stored in meta for type queries and more efficient
;;serialization/deserialization.
(defn typed-col
  ([t]
   (let [prim (or (primitives t)                 
                 :object)]
     (with-meta (rrb/vector-of prim) {:column-type t})))
  ([t xs] (if (primitives t)
            (into (typed-col t) xs)
            (with-meta (rrb/vec xs) {:column-type t}))))

(defn col-type [xs]  (get (meta xs) :column-type))

;;Simple map of field->transient column, stored in
;;volatiles for quick insertion.  Intended for creating
;;tables.
(defn volatile-hashmap! [m]
  (reduce-kv (fn [acc k v]
               (assoc (vary-meta acc  assoc k (meta v))
                      k
                      (volatile!  (transient v))))
             m m))

;;Persisists each associated column, providing the basis for
;;a column-based persistent table.
(defn unvolatile-hashmap! [m]
  (reduce-kv (fn [acc fld vcol]
               (assoc  acc  fld
                       (vary-meta (persistent! @vcol)
                                  merge (get (meta m) fld)) )) {} m))

;;__Generic Table Defnitions__

;Moved generic protocols to util.protocols
;note-> a field is just a table with one field/column....
;thus a table is a set of joined fields....

(defprotocol ITabular 
  (table-fields [x] "Get a vector of fields for table x")
  (table-columns [x] "Get a nested vector of the columns for table x"))
 
(defn tabular? [x] (satisfies? ITabular x))
  
(defprotocol IUnOrdered
  (-unordered? [x] 
   "Helper protocol to indicate that table fields are not ordered."))

(defprotocol ITabularMaker 
  (-make-table [x fields columns] "Allows types to define constructors."))

(defprotocol IFieldDropper
  (-drop-field [x fieldname] "Allows types to implement fast drop operations."))

(defprotocol IField
  (field-name [x] "Returns the name of a field")
  (field-vals [x] "Returns a vector of values for a field"))

(extend-protocol IField 
  nil 
    (field-name [x] nil) 
    (field-vals [x] nil) 
  clojure.lang.PersistentArrayMap 
    (field-name [x] ((comp first keys) x)) 
    (field-vals [x] ((comp first vals) x)) 
  clojure.lang.PersistentHashMap 
    (field-name [x] ((comp first keys) x)) 
    (field-vals [x] ((comp first vals) x)) 
  clojure.lang.PersistentVector 
    (field-name [x] (first  x)) 
    (field-vals [x] (or (second x) []))
    clojure.lang.MapEntry
    (field-name [x] (key  x)) 
    (field-vals [x] (or (val x) [])))

(declare empty-table 
         -conj-field
         -disj-field
         make-table
         table->map
         conj-field)

(defn find-where
  "Similar to clojure.core/some...except it returns the indices where any member 
   of items can be found.  Used to quickly address entries in a vector."
  [items v] 
  (let [valid? (set items)] 
    (->> (map-indexed (fn [i x] (when (valid? x) [i x])) v) 
         (filter #(not (nil? %)))))) 
 
(defn- empty-columns [n] (vec (map (fn [_] []) (range n)))) 
(defn- normalized? 
  "Returns true if every column in the table has the same number of rows." 
  [tbl] 
  (every?  
    (fn [col] (= (count col)  
                 (count (first (table-columns tbl)))))  
    (rest (table-columns tbl)))) 
 
(defn- nil-column [n]  
  (persistent!   
    (reduce conj! (transient []) (take n (repeat nil)))))  
 
(defn- normalize-column [col n] 
  (cond (empty? col) (nil-column n) 
        (vector? col) (if (zero? n) 
                        col 
                        (let [colcount (count col)] 
                          (cond  
                            (= colcount n)  col 
                            (> colcount n) (subvec col 0 n) 
                            :else (persistent! 
                                    (reduce conj! (transient col)  
                                            (nil-column (- n (count col)))))))) 
        :else (normalize-column (vec col) n))) 
 
(defn- normalize-columns [cols] 
  (loop [maxcount (count (first cols)) 
         remaining (rest cols) 
         dirty? false] 
    (if (empty? remaining) 
      (if dirty? 
        (vec (map (fn [c] (normalize-column c maxcount)) cols)) 
        cols)         
      (let [nextcount (count (first remaining))] 
        (if (= nextcount maxcount) 
          (recur maxcount (rest remaining) dirty?) 
          (recur (max maxcount nextcount) (rest remaining) true)))))) 

;;We're offering primitive columns now, using rrb-vectors (until clojure
;;core actually accepts the patch for transients in vector-of for gvec).
;;This should allow us to have a more efficient storage of our tables,
;;particularly if we provide types for them (like int and friends).
;;The biggest reason for this change is to help with memory restricted
;;environs, specifically when we're working with larger datasets. Ideally,
;;we can keep 99% of our interface and get some serious space savings without
;;sacrifing performance.  It may be that space savings end up going toward
;;our strings though, which is a beneifit/limitation of the JVM.
;;RRBVectors may also be desireable for our CES...

;;Consider making this seqable...turning it into a deftype.
(defrecord column-table [fields columns] 
  ITabular  
    (table-fields [x]  fields) 
    (table-columns [x] columns) 
  ITabularMaker 
    (-make-table [x fields columns]  
      (column-table. fields (normalize-columns columns)))    
  clojure.core.protocols/CollReduce
  (coll-reduce [this f]  
    (let [bound   (count fields)
          rbound  (dec (count (first columns)))
          cursor  {} ;(transient {})
          idx (atom 0)
          fetch-record (fn [n m] 
                         (loop [m m
                                i 0]
                           (if (== i bound) m
                               (recur 
                                (assoc ;assoc!
                                 m (nth fields i)
                                          (nth (nth columns i) n))
                                (unchecked-inc i)))))
          next-record! (fn [] (if (== @idx rbound) nil
                                  (fetch-record (swap! idx inc)
                                                cursor)))]
      (when-let [r (zipmap fields (mapv #(nth % 0) columns))]
        (cond (zero? rbound) r
              (== rbound 1) (f r (next-record!))
              :else         (let [init (next-record!)]
                              (loop [ret (f r init )]
                                (if (reduced? ret) @ret
                                    (if-let [nxt (next-record!)]                
                                      (recur (f ret nxt))
                                      ret))))))))
  (coll-reduce [this f init]
    (let [bound   (count fields)
          rbound  (dec (count (first columns)))
          cursor  {};(transient {})
          idx (atom -1) ;ugh...negative really?
          fetch-record (fn [n m] 
                         (loop [m m
                                i 0]
                           (if (== i bound) m
                               (recur 
                                (assoc ;assoc!
                                 m (nth fields i)
                                          (nth (nth columns i) n))
                                (unchecked-inc i)))))
          next-record! (fn [] (if (== @idx rbound) nil
                                  (fetch-record (swap! idx inc)
                                               cursor)))]
      (when-let [r (next-record!)]
        (loop [ret (f init r)]
          (if (reduced? ret) @ret
              (if-let [nxt (next-record!)]                
                  (recur (f ret nxt))
                  ret))))))
  )

(defn make-table  
  "Constructs a new table either directly from a vector of fields and  
   vector of columns, or from a collection of IFields, of the form 
   [[field1 & [column1-values...]] 
    [field2 & [column2-values...]]]  or  
   {:field1 [column1-values]  
    :field2 [column2-values]} " 
  ([fields columns] 
    (->column-table fields (normalize-columns columns))) 
  ([Ifields] (reduce (fn [acc fld]
                      (conj-field  [(field-name fld) (field-vals fld)] acc)) (->column-table [] []) (seq Ifields))))
              
(def empty-table (make-table [] [] ))
(defn ordered-table?
  "Indicates if tbl implements table fields in an unordered fashion."
  [tbl]
  (not (satisfies? IUnOrdered tbl)))

(defn enumerate-fields 
  "Returns a sequence of [fld [column-values]]" 
  [flds cols]  
  (for [[id f] (map-indexed vector flds)] [f (get cols id)]))

(extend-protocol  ITabular
  nil
    (table-fields [x] nil) 
    (table-columns [x] nil)
  clojure.lang.PersistentArrayMap
    (table-fields [x] (vec (keys x)))
    (table-columns [x] (vec (vals x)))
  clojure.lang.PersistentHashMap 
    (table-fields [x] (vec (keys x))) 
    (table-columns [x] (vec (vals x))) 
  clojure.lang.PersistentVector
    (table-fields [x] (vec (map first x))) 
    (table-columns [x] (vec (map #(get % 1) x))))

(extend-protocol ITabularMaker
  nil
    (-make-table [x fields columns] (make-table fields columns))  
  clojure.lang.PersistentArrayMap
    (-make-table [x fields columns]  
       (#(into {} (reverse (enumerate-fields %1 %2))) fields columns)) 
  clojure.lang.PersistentHashMap
    (-make-table [x fields columns] 
       (#(into {} (reverse (enumerate-fields %1 %2))) fields columns)) 
  clojure.lang.PersistentVector 
    (-make-table [x fields columns]  
       (comp vec enumerate-fields) fields columns)) 

(extend-protocol IFieldDropper
  clojure.lang.PersistentArrayMap 
    (-drop-field [x fieldname] (dissoc x fieldname)) 
  clojure.lang.PersistentHashMap 
    (-drop-field [x fieldname] (dissoc x fieldname)))                  

(extend-protocol IUnOrdered 
  clojure.lang.PersistentHashMap
    (-unordered? [x] true)
  clojure.lang.PersistentArrayMap
    (-unordered? [x] true))

(defn count-rows [tbl] 
  (count (first (table-columns tbl))))

(defn has-fields?
  "Determines if every field in fnames exists in tbl as well."
  [fnames tbl]
  (every? (set (table-fields tbl)) fnames))

(defn has-field?
  "Determines if tbl has a field entry for fname."
  [fname tbl] (has-fields? [fname] tbl))

(defn get-field
  "Returns the fieldspec for the field associated with fname, if any.
   Field entry is in the form of {:fieldname [& column-values]}"
  [fname tbl]
  (when (has-field? fname tbl)
    (assoc {} fname (->> (find-where #{fname} (table-fields tbl))
                      (ffirst) 
                      (get (table-columns tbl))))))
 
(defn conj-field 
  "Conjoins a field, named fname with values col, onto table tbl. 
   If no column values are provided, conjoins a normalized column of  
   nils.  If values are provided, they are normalized to fit the table. 
   If the field already exists, it will be shadowed by the new field." 
  ([[fname & [col]] tbl]  
    (if-not (has-field? fname tbl)  
       (-make-table tbl 
         (conj (table-fields tbl) fname)  
         (conj (table-columns tbl)  
               (normalize-column col (count-rows tbl)))) 
       (let [flds  (table-fields tbl) 
             idx (ffirst (find-where #{fname} flds))] 
         (-make-table tbl 
           flds  
           (assoc (table-columns tbl) idx  
                  (normalize-column col (count-rows tbl))))))))  

(defn rename-fields
  "Rename existing fields according to a mapping of old to new fields."
  [lookup tbl]
  (reduce (fn [acc fldname] 
            (if-let [new-name (get lookup fldname)]
              (let [[_ xs] (vec (first (get-field fldname tbl)))] 
                (conj-field [new-name xs] acc))
              (conj-field (vec (first (get-field fldname tbl))) acc)))
          empty-table
          (table-fields tbl)))

(defn conj-fields
  "Conjoins multiple fieldspecs into tbl, where each field is defined by a  
   vector of [fieldname & [columnvalues]]"
  [fieldspecs tbl]
  (let [fieldspecs   (if (tabular? fieldspecs)
                         (enumerate-fields 
                           (table-fields fieldspecs) 
                           (table-columns fieldspecs))
                         fieldspecs)]
    (reduce #(conj-field %2 %1) tbl fieldspecs)))

(defn drop-fields
  "Returns a tbl where the column associated with fld is no longer present."
  [flds tbl]
  (let [keep-set (clojure.set/difference (set (table-fields tbl)) (set flds))
        cols     (table-columns tbl)]
    (reduce (fn [newtbl [j fld]]
              (if (keep-set fld)
                (conj-field [fld (get cols j)] newtbl)
                newtbl))
            (-make-table tbl [] [])
            (map-indexed vector (table-fields tbl)))))
 
(defn drop-field
  "Returns a tbl where fld is removed.  Structures that implement IFieldDropper
   for effecient removal are preferred, otherwise we build a table without 
   the dropped fields."
  [fld tbl]
  (if (satisfies? IFieldDropper tbl)
    (-drop-field tbl fld )
    (drop-fields [fld] tbl)))


;;Ran across a bug in clojure 1.6, where the type information was not
;;equating an instance of column-table  as a column-table, and the 
;;optimization in here passed through.  Produced invalid results...
(defn table->map 
  "Extracts a map representation of a table, where keys are  
   fields, and values are column values. This is an unordered table." 
  [tbl]  
;  (if (and (map? tbl) (not= (type tbl) spork.util.table.column-table)) tbl 
    (let [cols (table-columns tbl)] 
      (reduce (fn [fldmap [j fld]]  (assoc fldmap fld (get cols j))) {}  
              (reverse (map-indexed vector (table-fields tbl))))))
;)  
 
(defn map->table
  "Converts a map representation of a table into an ordered table."
  [m] 
  (assert (map? m)) 
  (conj-fields (seq m) empty-table)) 
 
(defn order-fields-by 
  "Returns a tbl where the fields are re-arranged to conform with the ordering  
   specified by applying orderfunc to a sequence of  [fieldname column-values], 
   where f returns a sequence of field names.  Resorts to a default ordered
   table representation.  If orderfunc is a vector of fields, like [:a :b :c],
   rather than applying the function, the fields will be extracted in order." 
  [orderfunc tbl]   
  (let [fieldmap (table->map tbl)        
        ordered-fields 
        (cond (vector? orderfunc)  (do (assert (clojure.set/subset? 
                                                  (set orderfunc) 
                                                  (set (table-fields tbl)))  
                                         (str "Table is missing fields!"  
                                              orderfunc (table-fields tbl)))
                                     (align-by orderfunc (table-fields tbl))) 
              (fn? orderfunc) (orderfunc (seq fieldmap))
              :else 
                (throw (Exception. "Ordering function must be vector or fn")))] 
    (reduce (fn [newtbl fld] (conj-field [fld (get fieldmap fld)] newtbl))  
            empty-table ordered-fields)))

(defn all-fields? [fieldnames] 
  (or (= fieldnames :*) (= fieldnames :all)))

(defn select-fields 
  "Returns a table with only fieldnames selected.  The table returned by the  
   select statement will have field names in the order specified by fieldnames." 
  [fieldnames tbl]
  (if (all-fields? fieldnames) 
    tbl 
    (let [res (drop-fields (clojure.set/difference (set (table-fields tbl))
                                                   (set fieldnames))  
                           tbl)]     
    (order-fields-by  
      (if (vector? fieldnames) fieldnames (vec fieldnames)) res)))) 

(defn field->string [f] (cond (string? f) f 
                              (keyword? f) (str (subs (str f) 1)) 
                              :else (str f)))
(defn keywordize-field-names
  "Flips the fields to be keywords instead of strings."
  [t] 
  (make-table  (reduce #(conj %1 (keyword %2)) [] (table-fields t))
               (table-columns t)))

(defn stringify-field-names 
  "Flips the fields to be string instead of keywords." 
  [t]  
  (make-table (reduce #(conj %1 (field->string %2)) [] (table-fields t)) 
              (table-columns t)))

(defn valid-row?
  "Ensures n is within the bounds of tbl."
  [tbl n]  (and (>= n 0) (< n (count-rows tbl))))
  
(defn nth-row 
  [tbl n]
  "Extracts the nth row of data from tbl, returning a vector."
  (assert (valid-row? tbl n) (str "Index " n " Out of Bounds"))
  (mapv #(nth % n) (table-columns tbl)))

(defn table-rows 
  "Returns a vector of the rows of the table." 
  [tbl]  (vec (map #(nth-row tbl %) (range (count-rows tbl))))) 

(definline zip-record!! [n xs cs]
  (let [ks    (with-meta (gensym "ks") {:tag 'objects})
        cols  (with-meta (gensym "cols") {:tag 'objects})
        col   (with-meta (gensym "col") {:tag 'clojure.lang.Indexed})
        arr   (with-meta (gensym "arr") {:tag 'objects})]
    `(let [~ks ~xs
           ~cols ~cs
           ~arr (object-array (unchecked-multiply 2 (alength ~ks)))
           bound#  (alength ~ks)
           n# (long ~n)]
       (loop [idx# 0
              inner# 0]
         (if (== idx# bound#) (clojure.lang.PersistentArrayMap/createAsIfByAssoc ~arr)
             (let [~'_ (aset ~arr inner# (aget ~ks idx#))
                   ~col (aget ~cols idx#)
                   ~'_  (aset ~arr (unchecked-inc inner#) (.nth ~col n#))]
               (recur (unchecked-inc idx#)
                      (unchecked-add inner# 2))))))))
           
;;optimizedish..
(defn table-records
  [tbl]  
  (let [flds   (object-array (table-fields tbl))
        cols   (object-array (table-columns tbl))]
    (map (fn [n] (zip-record!! n flds cols)) (range (count-rows tbl)))))

;;#Optimize!
;;Dogshit slow....should clean this up...both zipmap and 
;;the varargs are bad for performance.  This will get called a lot.
(defn nth-record
  "Coerces a column-oriented vector representation of tables into 
   a map where keys are field names, and vals are column values at 
   the nth record."
  [tbl n & [flds]]
  (assert (valid-row? tbl n) (str "Index " n " Out of Bounds"))  
  (zip-record!! n (object-array (table-fields tbl))
                  (object-array (table-columns tbl))))
                  
(defn last-record 
  "Fetches the last record from table.  Returns nil for empty tables."
  [tbl]
  (when (> (count-rows tbl) 0) 
    (nth-record tbl (dec (count-rows tbl)))))

(defn first-record  
  "Fetches the first record from table.  Returns nil for empty tables." 
  [tbl]
  (when (>  (count-rows tbl) 0) 
    (nth-record tbl 0)))

;;This is an optimized side-effecting aux function.
;;Can we use arrayfor...
(definline conj-row!! [transientcolumns
                       rowvector]
  (let [cols (with-meta (gensym "cols") {:tag 'objects})
        row  (with-meta (gensym "row")  {:tag 'clojure.lang.PersistentVector})
        col  (with-meta (gensym "col")  {:tag 'clojure.lang.ITransientCollection})]
    `(let [~cols  ~transientcolumns
           ~row   ~rowvector
           bound# (.count ~row)]
    (loop [j# 0]
      (if (== j# bound#) ~cols
          (let [~col (aget ~cols j#)
                ~'_  (aset ~cols j# (.conj ~col (.nth ~row j#)))]                
            (recur (unchecked-inc j#))))))))

(definline conj-row-obj!! [transientcolumns
                           rowvector]
  (let [cols (with-meta (gensym "cols") {:tag 'objects})
        row  (with-meta (gensym "row")  {:tag 'objects})
        col  (with-meta (gensym "col")  {:tag 'clojure.lang.ITransientCollection})]
    `(let [~cols  ~transientcolumns
           ~row   ~rowvector
           bound# (alength ~row)]
    (loop [j# 0]
      (if (== j# bound#) ~cols
          (let [~col (aget ~cols j#)
                ~'_  (aset ~cols j# (.conj ~col (aget ~row j#)))]                
            (recur (unchecked-inc j#))))))))
;;this is now optimized to contain the columns in an object array,
;;which eliminates extra calls to .assoc that we had.
(defn conj-rows
  "Conjoins multiple rowvectors.  Should be fast, using transients.
   Returns a persistent collection."
  [columns rowvectors]
  (let [fv (general/first-any rowvectors)]
    (assert (= (count fv) (count columns)))
    (if (vector? fv)
      (mapv persistent! 
            (reduce conj-row!!
                    (object-array (map transient columns))
                    rowvectors))
      (mapv persistent! 
            (reduce conj-row-obj!!
                    (object-array (map transient columns))
                    rowvectors)))))

(defn conj-row
  "Conjoins a rowvector on a vector of columns."
  [columns rowvector]
  (assert (= (count rowvector) (count columns)))
  (reduce (fn [acc [j x]] (assoc acc j (conj (get acc j) x)))
          columns (map-indexed vector rowvector)))


;;Should be a more efficient way to add maps/records when growing a
;;table.
(definline assoc-row!! [fields
                        transientcolumns
                        rowmap]
  (let [flds (with-meta (gensym "flds") {:tag 'objects})
        row  (with-meta (gensym "row")  {:tag 'clojure.lang.ILookup})
        cols (with-meta (gensym "cols") {:tag 'objects})
        col  (with-meta (gensym "col")  {:tag 'clojure.lang.ITransientCollection})]
    `(let [~cols  ~transientcolumns
           ~row   ~rowmap
           ~flds  ~fields
           bound# (alength ~flds)]
    (loop [j# 0]
      (if (== j# bound#) ~cols
          (let [fld# (aget ~flds  j#)
                ~col (aget ~cols j#)                
                ~'_  (aset ~cols j# (.conj ~col (.valAt ~row fld#)))]                
            (recur (unchecked-inc j#))))))))

(defn conj-records!!
  "Conjoins multiple rowmaps.  Should be fast, using transients.
   Returns a persistent collection."
  [fields columns rowmaps]
  (assert (= (count (general/first-any rowmaps)) (count columns)))
  (let [fields (object-array fields)]
    (mapv persistent! 
          (reduce (fn [acc m]
                    (assoc-row!! fields acc m))
                  (object-array (map transient columns))
                  rowmaps))))
;;Optimized#
(defn records->table
  "Interprets a sequence of records as a table, where fields are the keys of 
   the records, and rows are the values."
  [recs]
  (let [flds (vec (let [r (general/first-any recs)]
                    (if (record? r) (keys r)                        
                        (sort (keys r)))))
        cols (mapv (fn [_] []) flds)]
    (make-table flds (conj-records!! flds
                                     cols
                                     recs
                                     ))))

(defn filter-rows
  "Returns a subset of rows where f is true.  f is a function of 
   type f::vector->boolean, since rows are vectors...."
  [f tbl ]
  (vec (filter f (table-rows tbl)))) 

(defn filter-records
  [f tbl]
  "Returns a subtable, where the rows of tbl have been filtered according to 
   function f, where f is of type ::record->boolean, where record is a map 
   where the keys correspond to tbl fields, and the values correspond to row  
   values."
  (records->table (filter f (table-records tbl))))

(defn map-field-indexed 
  "Maps function f to field values drawn from tbl.  f takes arguments akin to  
   map-indexed, [i v], treating the mapping as an indexed traversal over the  
   field entries.  Returns a table with the result of the mapping." 
  [field f tbl] 
  (let [newvals (map-indexed f (field-vals (get-field field tbl)))] 
    (conj-field [field (vec newvals)] tbl)))

(defn map-field 
  "Maps function f to field values drawn from tbl, akin to clojure.core/map. 
  Returns a table with the results of the mapping." 
  [field f tbl]
  (map-field-indexed field (fn [_ x] (f x)) tbl))


(defn negate [n] (- n))

(defn order-with 
    "Returns a new table, where the rows of tbl have been ordered according to  
     function f, where f is of type ::record->key, where record is a map  
     where the keys correspond to tbl fields, and the values correspond to row   
     values."
    [f tbl]
  (let [t (->> (table-records tbl) 
            (sort-by f) 
            (records->table))] 
    (make-table (vec (reverse (table-fields t))) 
                (vec (reverse (table-columns t))))))
                            
(defn order-by
  "Similar to the SQL clause.  Given a sequence of orderings, sorts the 
   table accordingly.  orderings are of the form :
   :field, 
   [comparison-function :ascending|:descending],
   [fieldname comparison-function :ascending|:descending]     
   [fieldname :ascending|:descending]"
  [orderings tbl]
  (let [t (->> (table-records tbl)
            (sort (serial-field-comparer orderings))
            (records->table))]
    (make-table (vec (reverse (table-fields t)))
                (vec (reverse (table-columns t))))))

(defn concat-tables
  "Concatenates two or more tables.  Concatenation operates like union-select
   in SQL, in that fields between all tables must be identical [including 
   positionally].  Returns a single table that is the result of merging all 
   rows together."
  [tbls]
  (let [flds (table-fields (first tbls))]
    (assert (every? #(= (table-fields %) flds) tbls))
     (->> (mapcat (fn [tbl] (table-rows tbl)) tbls)
       ((comp vec distinct))
       (conj-rows (empty-columns (count flds)))
       (make-table flds))))

(defn view-table [tbl] (pprint (table-records tbl)))
(defn select-distinct
  "Select only distinct records from the table.  This treats each record as a 
   tuple, and performs a set union on all the records.  The resulting table is 
   returned (likely a subset of the original table).  Used to build lookup 
   tables for intermediate queries."
  [tbl]
  (make-table (table-fields tbl) 
              (transpose (vec (distinct (table-rows tbl))))))

(defn group-records  
  "Convenience function, like group-by, but applies directly to ITabular 
   structures.  Allows an optional aggregator function that can be run on  
   each group, allowing domain aggregation queries and the like.  If no  
   aggregator is supplied via the :aggregator key argument, acts identical to  
   clojure.core/group-by" 
  [keygen table & {:keys [aggregator] :or {aggregator identity}}] 
  (let [get-key (key-function keygen)] 
    (let [groups  
          (->> (select-distinct table) 
            (table-records) 
            (group-by get-key))] 
      (into {} (map (fn [[k v]] [k (aggregator v)]) (seq groups))))))

(defn subtables-by
  "groups records in table t by field f, creating a map of distinct values 
   of f, associated with correspond tables derived from the grouped records.
   Acts as a higher-order form of group-by, that maps one table to a 
   map of [key table] entries."
  [f t]   
  (let [fields (table-fields t)]
    (into {}
          (for [[fval xs]  (group-by f (table-records t))]
            [fval (->> xs
                       (records->table)
                       (order-fields-by fields))]))))
   
(defn make-lookup-table 
  "Creates a lookup table from a table and a list of fields.  If more than one 
   field is specified, composes the field values into a vector, creating a  
   compound key.  If more than one result is returned, returns the first  
   value in the grouped vectors." 
  [fields table] (group-records fields table :aggregator first))


(defn join-on 
  "Given a field or a list of fields, joins each table that shares the field." 
  [fields tbl1 tbl2]  
  (let [lookup (partial group-records (key-function fields))
        l (lookup tbl1)
        r (lookup tbl2)
        joins (clojure.set/intersection (set (keys l)) (set (keys r)))]
    (persistent! 
      (reduce (fn [acc k] (->> (for [x  (get l k)
                                     y  (get r k)]
                                 (merge x y))
                            (reduce conj! acc))) (transient #{}) joins))))     

(defn join-tables 
  "Given a field or a list of fields, joins each table that shares the field." 
  [fields tbls]
  (throw (Exception. "Currently not performing correctly, use join-on"))
  (assert (coll? fields)) 
  (let [field-set    (set fields) 
        valid-tables (->> tbls
                          (filter #(clojure.set/subset?  field-set 
                                     (set (table-fields %)))) 
                          (sort-by count-rows))
        joiner (partial join-on fields)]
    (when valid-tables
       (reduce (fn [l r] (records->table (joiner l r))) valid-tables)))) 
             

;protocol-derived functions 

(defn- process-if [pred f x] (if pred (f x) x))

(defn database? [xs]  
  (and (seq xs) (every? tabular? xs)))



(defmulti  computed-field  (fn [x] (if (fn? x) :function (type x))))
(defmethod computed-field :function [f] (fn [i rec] (f i rec)))
(defmethod computed-field clojure.lang.Keyword [k]  (fn [i rec] (get rec k)))

;implementation for 'as statements pending....
(defn- select- 
  "A small adaptation of Peter Seibel's excellent mini SQL language from 
   Practical Common Lisp.  This should make life a little easier when dealing 
   with abstract tables...."   
  [columns from where unique orderings]
  (if (and (not (database? from)) (tabular? from)) ;simple selection....
    (->> (select-fields columns from) ;extract the fields.
      (process-if where (partial filter-records where))
      (process-if unique select-distinct)
      (process-if order-by (partial order-by orderings)))))

;;changed default for uniqueness to false.
(defn select 
  "Allows caller to compose simple SQL-like queries on abstract tables.  
   Joins are implemented but not supported just yet.  The :from key for the 
   select query is intended to be a table, specifically something supporting 
   ITabular."
  [& {:keys [fields from where unique orderings]
      :or   {fields :* from nil where nil
             unique false orderings nil }}]
  (select- fields from where unique orderings)) 


;;Parsing tables from external files...

;;We'd like to include the notion of a schema, so that if we have
;;strongly typed fields, we can parse them relatively quickly.
;;If a field doesn't exist in the schema, we can still fall back to 
;;parse string.  Parse-string is the biggest bottleneck at the 
;;moment.

(defn pair [a b] [a b])
(def re-tab (re-pattern (str \tab)))
;;split-by-tab is hurting is.
;;specifically, because of the overhead of having
;;to create all the intermediate vectors we're
;;parsing.
;; (def split-by-tab
;;   #(strlib/split % re-tab))
 
;;Roughly 2X as fast as clojure.string/split
;;Note, it doesn't cost us much to wrap it as a
;;persistent vector.
(defn split-by-tab [^String s]
  (clojure.lang.LazilyPersistentVector/createOwning
   (.split s "\t")))

;;note: we use spork.util.string/split-by 
(defn check-header
  "Checks to see if a stringified header begins with a :.  If it does, removes the:"
  [h]
  (if (= (str (first h)) ":") (subs h 1) h))

(defn unify-schema [s fields]
  (let [fk    (keyword? (first fields))
        xform (if fk (fn [x] (if (keyword? x) x (keyword x)))
                  name)]
    (reduce-kv (fn [acc k v]
                 (assoc acc (xform k) v))
               {} s)))

;;if we're not provided a schema, we can ascertain what kind of
;;data it is based on the first column.
(defn derive-schema [row & {:keys [parsemode line->vec]
                            :or {line->vec split-by-tab}}]
  (let [xs     (line->vec row)
        parser (if (= parsemode :scientific)  parse/parse-string
                    parse/parse-string-nonscientific);clojure.edn/read-string
        types  (mapv (comp type parser) xs)]    
    (mapv (fn [t]
            (cond
               (identical? t java.lang.String) :text
               (identical? t java.lang.Integer) :long
              (identical? t java.lang.Long)    :long
              (identical? t java.lang.Double) :double
              :else (throw (Exception. "unsupported parsing type " t)))) types)
    ))

;older table abstraction, based on maps and records...
(defn lines->table 
  "Return a map-based table abstraction from reading lines of tabe delimited text.  
   The default string parser tries to parse an item as a number.  In 
   cases where there is an E in the string, parsing may return a number or 
   infinity.  Set the :parsemode key to any value to anything other than 
   :scientific to avoid parsing scientific numbers."
   [lines & {:keys [parsemode keywordize-fields? schema default-parser delimiter] 
             :or   {parsemode :scientific
                    keywordize-fields? true
                    schema {}
                    delimiter #"\t"}}] 
  (let [line->vec (s/->vector-splitter delimiter)
        tbl   (->column-table 
                 (vec (map (if keywordize-fields?  
                             (comp keyword check-header clojure.string/trim)
                             identity)
                           (line->vec (general/first-any lines)))) 
                 [])
        parsef (parse/parsing-scheme schema :default-parser  
                   (or default-parser
                       (if (= parsemode :scientific) parse/parse-string
                           parse/parse-string-nonscientific)))
        fields (table-fields tbl)      
        parse-rec (comp (parse/vec-parser! fields parsef) line->vec) ;this makes garbage.
        ]
      (->> (conj-rows (empty-columns (count (table-fields tbl))) 
                      (r/map parse-rec (r/drop 1 lines)))
           (assoc tbl :columns))))

(defn typed-lines->table
  "A variant of lines->table that a) uses primitives to build
   up our table columns, if applicable, using rrb-trees.
   b) enforces the schema, ignoring fields that aren't specified.
   c) throws an exception on missing fields."
  [ls schema & {:keys [parsemode keywordize-fields? delimiter] 
                :or   {parsemode :scientific
                       keywordize-fields? true
                       delimiter #"\t"}}]
  (let [line->arr    (s/->array-splitter  delimiter) ;maybe unecessary
        line->vec    (s/->vector-splitter line->arr)
        raw-headers  (mapv clojure.string/trim (line->vec (general/first-any ls)))
        fields       (mapv (fn [h]
                              (let [root  (if (= (first h) \:) (subs h  1) h)]
                                (if keywordize-fields?
                                  (keyword root)
                                  root)))
                            raw-headers)
        s         (unify-schema schema fields)
        parser    (spork.util.parsing/parsing-scheme s)
        idx       (atom 0)
        idx->fld  (reduce (fn [acc h]
                            (if (get s h)
                              (let [nxt (assoc acc @idx h)
                                    _   (swap! idx unchecked-inc)]
                                nxt)
                              (do (swap! idx unchecked-inc) acc))) {} fields)
        ;;throw an error if the fld is not in the schema.
        _ (let [known   (set (map name (vals idx->fld)))
                missing (filter (complement known) (map name (keys s)))]
            (assert (empty? missing) (str [:missing-fields missing])))
        cols    (volatile-hashmap! (into {} (for [[k v] s]
                                              [k (typed-col v)])))]                                          
    (->> ls
         (r/drop 1)
         (r/map  line->arr)
         (reduce (fn [acc  ^objects xs]
                   (reduce-kv (fn [acc idx fld]
                                (let [c (get cols fld)]
                                  (do (vswap! c conj! (parser fld (aget xs idx)))
                                      acc))) acc idx->fld)) cols)
         (unvolatile-hashmap!)
         (make-table))))

(defn lines->records
  "Produces a reducible stream of 
   records that conforms to the specifications of the 
   schema.  Unlike typed-lines->table, it does not store
   data as primitives.  Records are potentially ephemeral 
   and will be garbage collected unless retained.  If no 
   schema is provided, one will be derived.  Caller may supply 
   a regex pattern, via delimiter, for custom record delimiting. Defaults
   to tab-delimited records."
  [ls schema & {:keys [parsemode keywordize-fields? delimiter] 
                :or   {parsemode :scientific
                       keywordize-fields? true
                       delimiter #"\t"}}]
  (let [line->arr    (s/->array-splitter delimiter)
        line->vec    (s/->vector-splitter line->arr)        
        raw-headers   (mapv clojure.string/trim (line->vec (general/first-any ls)))
        fields        (mapv (fn [h]
                              (let [root  (if (= (first h) \:) (subs h  1) h)]
                                (if keywordize-fields?
                                  (keyword root)
                                  root)))
                            raw-headers)
        schema    (if (empty? schema)
                    (let [types (derive-schema (general/first-any (r/drop 1 ls))
                                               :parsemode parsemode
                                               :line->vec line->vec)]
                      (into {} (map vector fields types)))
                    schema)
        s         (unify-schema schema fields)
        parser    (spork.util.parsing/parsing-scheme s)
        idx       (atom 0)
        idx->fld  (reduce (fn [acc h]
                            (if (get s h)
                              (let [nxt (assoc acc @idx h)
                                    _   (swap! idx unchecked-inc)]
                                nxt)
                              (do (swap! idx unchecked-inc) acc))) {} fields)
        last-fld-idx (apply max  (keys idx->fld))  
        ;;throw an error if the fld is not in the schema.
        _ (let [known   (set (map name (vals idx->fld)))
                missing (filter (complement known) (map name (keys s)))]
            (assert (empty? missing) (str [:missing-fields missing])))]                                          
    (->> ls
         (r/drop 1)
         (r/map  line->arr)
         (r/map  (fn [^objects xs]
                  ;;if the final field is empty, we won't get an extra empty string when splitting by tab.
                  ;;If other fields are empty, we'll get an extra string.
                  (let [last-fld-empty? (= (alength xs) last-fld-idx)]
                   (reduce-kv (fn [acc idx fld]
                                (let [new-val (if (and (= idx last-fld-idx) last-fld-empty?)
                                                ""
                                                (aget xs idx))]
                                  (assoc acc fld (parser fld new-val))))
                              {} idx->fld))))
         )))

(defn tabdelimited->table 
  "Primary table-creation API. Returns a map-based table abstraction from 
   reading a string (or file) of tabdelimited text.  The default string parser 
   tries to parse an item as a number.  In  cases where there is an E in the 
   string, parsing may return a number or infinity.  Set the :parsemode key to
    any value to anything other than :scientific to avoid parsing scientific numbers."
   [s & {:keys [parsemode keywordize-fields? schema relaxed? default-parser delimiter] 
         :or   {parsemode :scientific
                keywordize-fields? true
                schema {}
                relaxed? false
                delimiter #"\t"}}]
   (if (or (empty? schema) (and relaxed? schema))
     (lines->table (general/line-reducer s)
                   :parsemode parsemode
                   :keywordize-fields? keywordize-fields?
                   :schema schema                               
                   :default-parser default-parser
                   :delimiter delimiter)
     (typed-lines->table  (general/line-reducer s)
                         schema
                         :parsemode parsemode
                         :keywordize-fields? keywordize-fields?
                         :schema schema
                         :delimiter delimiter)))

(defn tabdelimited->records 
  "Secondary table-creation API. Returns a map-based record abstraction from 
   reading a string (or file) of tabdelimited text.  The default string parser 
   tries to parse an item as a number.  In  cases where there is an E in the 
   string, parsing may return a number or infinity.  Set the :parsemode key to
   any value to anything other than :scientific to avoid parsing scientific numbers.
   the return value is reducible."
   [s & {:keys [parsemode keywordize-fields? schema relaxed? default-parser delimiter] 
         :or   {parsemode :scientific
                keywordize-fields? true
                schema {}
                relaxed? false
                delimiter #"\t"}}]
   (lines->records (general/line-reducer s)
                   schema
                   :parsemode parsemode
                   :keywordize-fields? keywordize-fields?                    
                   :default-parser default-parser
                   :delimiter delimiter))

;;deprecated
(defn record-seq  
	"Returns a sequence of records from the underlying table representation. 
	 Like a database, all records have identical fieldnames.
   Re-routed to use the new table-records function built on the ITabular lib." 
	[tbl]
 (table-records tbl))

(defn get-record  
	"Fetches the nth record from a tabular map.  
   Rerouted to use the new API.  nth-record." 
	[tbl n]
 (nth-record tbl n))

(defn record-count [t] (count-rows t))
(defn get-fields   [t] (table-fields t))
(defn last-record  [t] (get-record t (dec (record-count t))))

(defn row->string 
  ([separator r] (strlib/join separator r))
  ([r]           (strlib/join \tab r)))

(defn record->string [rec separator]
  (row->string (vals rec) separator))

(defn table->lines
  "Returns a Seqable and Reducible representation of strings delimited by row-separator 
   for rows, and col-separator between columns."
  [tbl &  {:keys [stringify-fields? row-writer row-separator col-separator] 
           :or   {stringify-fields? true 
                  row-writer  row->string
                  col-separator \tab
                  row-separator \newline}}]
  (let [row-writer (fn [xs] (str (row->string col-separator xs) row-separator))]
    (reify
      clojure.core.protocols/CollReduce
      (coll-reduce [o f]      
        (->> (table-rows tbl)
             (r/map row-writer)
             (reduce f (row-writer (if stringify-fields?
                                     (mapv field->string (table-fields tbl))
                                     (table-fields tbl))))))
      (coll-reduce [o f init]      
        (->> (r/cat [(if stringify-fields?
                       (mapv field->string (table-fields tbl))
                       (table-fields tbl))]
                    (table-rows tbl))                     
             (r/map row-writer)
             (reduce f init)))
      clojure.lang.ISeq
      (seq [o]
        (->> (concat [(if stringify-fields?
                       (mapv field->string (table-fields tbl))
                       (table-fields tbl))]
                     (table-rows tbl))                     
             (map row-writer))))))

(defn table->string  
  "Render a table into a string representation.  Uses clojure.string/join for
   speed, which uses a string builder internally...if you pass it a separator.
   By default, converts keyword-valued field names into strings.  Caller 
   may supply a different function for writing each row via row-writer, as 
   well as a different row-separator.  row-writer::vector->string, 
   row-separator::char||string" 
  [tbl & {:keys [stringify-fields? row-writer row-separator col-separator include-fields?] 
          :or   {stringify-fields? true 
                 row-writer  row->string
                 row-separator \newline
                 col-separator \tab
                 include-fields? true}}]
  (let [ls (table->lines tbl :stringify-fields? stringify-fields?
                             :row-writer row-writer
                             :row-separator row-separator
                             :col-separator col-separator)]
   (->> (if include-fields? ls (r/drop 1 ls))
        (strlib/join))))

(defn table->tabdelimited
  "Render a table into a tab delimited representation."  
  [tbl & {:keys [stringify-fields?] :or {stringify-fields? true}}] 
  (table->string tbl :stringify-fields? stringify-fields?))

(defn infer-format [path]
  (case (strlib/lower-case (last (strlib/split path #"\.")))
    "txt" :tab
    "clj" :clj 
    nil))

;;Ported from proc.util
;;Note: originally had a typehint on here for ln, of String, and
;;it was tossing a wierd classcast unhandled exception when building
;;uberjar, worked fine from repl....
(defn spit-table [path t]
  (with-open [^java.io.BufferedWriter out (clojure.java.io/writer path)]
    (doseq [ln (seq (table->lines t))]
      (io/write! out ln))))

(defmulti table->file (fn [tbl path & {:keys [stringify-fields? data-format]}]
                        data-format))

(defmethod table->file :tab 
  [tbl path & {:keys [stringify-fields? data-format]}] 
  (spit (clojure.java.io/file path)  
        (table->tabdelimited tbl :stringify-fields? stringify-fields?))) 
 
(defmethod table->file :clj 
  [tbl path & {:keys [stringify-fields? data-format]}] 
  (with-open [dest (clojure.java.io/writer (clojure.java.io/file path))] 
    (binding [*out* dest] 
      (print tbl)))) 
 
(defmethod table->file :clj-pretty 
  [tbl path & {:keys [stringify-fields? data-format]}] 
  (with-open [dest (clojure.java.io/writer (clojure.java.io/file path))] 
    (binding [*out* dest] 
      (pprint tbl)))) 

(defmethod table->file :default 
  [tbl path & {:keys [stringify-fields? data-format]}]
  (spit (clojure.java.io/file path) 
        (table->tabdelimited tbl :stringify-fields? stringify-fields?)))  
 
(defmulti as-table
  "Generic function to create abstract tables."
  (fn [t] (class t)) :default :empty)
 
(defmethod as-table java.lang.String [t] (tabdelimited->table t))
(defmethod as-table clojure.lang.PersistentArrayMap [t] t)

(defmulti read-table (fn [t & opts] (class t))) 
(defmethod read-table  java.io.File [t & opts] (as-table (slurp t))) 

(defn copy-table!
  "Copies a table from the system clipboard, assuming that the clipboard
   contains a string of tab-delimited text."
  [& [parsemode keywordize-fields?]]
  (tabdelimited->table (board/copy!) :parsemode (or parsemode :no-science)
                                     :keywordize-fields? keywordize-fields?))
(defn copy-table-literal! []
  "Copes a table from the system clipboard.  Does not keywordize anything..."
  (copy-table! :no-science false))

(defn slurp-records 
  "Parses string s into a record sequence."
  [s] (table-records (as-table s)))

(defn copy-records!
  "Copies a string from the system clipboard, assumed to be in tab delimited 
   format, and returns a record sequence."
  [] (slurp-records (board/copy!)))

(defn spit-records
  "Spits a sequence of records, xs, assumed to be identical in keys, as if it 
   were a tab delimited table."
  [xs] 
  (table->tabdelimited (records->table xs)))

(defn paste-records!
  "Pastes a sequence of records, xs, to the clipboard as a string that assumes 
   xs are records in a tabdelimited table."
  [xs]
  (board/paste! (spit-records xs)))

(defn records->file
  "Given xs - a sequence of records, i.e. k-v pairs the support (get r k v) -
   spits the records to the file at dest.  Optional arguments include
   a record separator, a field order, and a writer.  Defaults to a fresh 
   writer writing tab-delimited field values.  Writers headers."
  [xs dest & {:keys [field-order sep writer]
              :or {sep "\t"}}]
  (with-open [out (stream/->record-writer dest :sep sep :writer writer)]
    (reduce (fn [o r] (stream/write-record o r)) out  xs)))

;establishes a simple table-viewer.
;we can probably create more complicated views later....
(defmethod gui/view spork.util.table.column-table [obj & {:keys [title sorted]  
                                :or {title "Table" sorted false}}] 
    (gui/->scrollable-view 
      (gui/->swing-table (get-fields obj)   
                         (table-rows obj) :sorted sorted)
      :title title))

(defmethod gui/as-JTable spork.util.table.column-table [obj & {:keys [sorted]}]
  (gui/->swing-table (get-fields obj)   
                     (table-rows obj) :sorted sorted)) 

(defn visualize   [obj & {:keys [title sorted] :or {title "some data" sorted true}}]
  (gui/->scrollable-view 
   (gui/->swing-table (get-fields obj)   
                      (table-rows obj) :sorted sorted)))

;;Additional patches for extended table functionality.
 (defn paste-table! [t]  (spork.util.clipboard/paste! (table->tabdelimited t)))
 (defn add-index [t] (conj-field [:index (take (record-count t) (iterate inc 0))] t))
 (defn no-colon [s]   (if (or (keyword? s)
                              (and (string? s) (= (first s) \:)))
                        (subs (str s) 1)))

;;These need documentation.  Using them in proc, but should be better.
 (defn collapse [t root-fields munge-fields key-field val-field]
   (let [root-table (select :fields root-fields   :from t)]
     (->>  (for [munge-field munge-fields]
             (let [munge-col  (select-fields [munge-field] t)
                   munge-name (first (get-fields munge-col))
                   
                   key-col    [key-field (into [] (take (record-count root-table) 
                                                        (repeat munge-name)))]
                   val-col    [val-field  (first (vals (get-field munge-name munge-col)))]]
               (conj-fields [key-col val-col] root-table)))
           (concat-tables)          
           (select-fields  (into root-fields [key-field val-field])))))

 (defn rank-by  
   ([trendf rankf trendfield rankfield t]
      (->> (for [[tr xs] (group-by trendf  (table-records t))] 
             (map-indexed (fn [idx r] (assoc r trendfield tr  rankfield idx)) (sort-by rankf xs)))
           (reduce      concat)
           (records->table)
           (select-fields (into (table-fields t) [trendfield rankfield]))))
   ([trendf rankf t] (rank-by trendf rankf :trend :rank t)))

 (defn ranks-by [names-trends-ranks t]    
   (let [indexed (add-index t)
         new-fields (atom [])
         idx->rankings 
         (doall (for [[name [trendf rankf]] names-trends-ranks]
                  (let [rankfield (keyword (str (no-colon name) "-rank"))
                        _ (swap! new-fields into [name rankfield])]
                    (->> indexed
                         (rank-by trendf rankf name rankfield)
                         (select-fields [:index name rankfield])
                         (reduce (fn [acc r]
                                   (assoc acc (:index r) [(get r name) (get r rankfield)])) {})))))]
     (conj-fields      
      (->> idx->rankings
           (reduce (fn [l r] 
                     (reduce-kv (fn [acc idx xs]
                                  (update-in acc [idx]
                                             into xs))
                                l r)))
           (sort-by first)
           (mapv second)
           (spork.util.vector/transpose)
           (mapv vector @new-fields)
           )
      indexed)))

;;Added support for flyweight records.
(defn ->flyrecord
  ([n t] (sparse/->flyrecord n (table-fields t) (table-columns t)))
  ([n fields columns] (sparse/->flyrecord n fields columns)))
  

(defn table->flyrecords [t]
  (let [flds (table-fields t)
        cols (table-columns t)]
    (reify
      clojure.lang.ISeq
      (seq [x]
        (map (fn [n] (->flyrecord n flds cols)) (range (count-rows t))))
      clojure.core.protocols/CollReduce
      (coll-reduce [obj f init]
        (let [bound (count-rows t)
              c (->flyrecord 0 t)]
          (loop [idx 0
                 acc init]
            (cond (reduced? acc) @acc
                  (== idx bound) acc
                  :else
                  (do (sparse/set-cursor c idx)
                      (recur (unchecked-inc idx)
                             (f acc c)))))))
      (coll-reduce [obj f]
        (let [bound (count-rows t)
              c (->flyrecord 1 t)]
          (loop [idx 1
                 acc (->flyrecord 0 t)]
            (cond (reduced? acc) @acc
                  (== idx bound) acc
                  :else
                  (let [_ (sparse/set-cursor c idx)]
                    (recur (unchecked-inc idx)
                           (f acc c))))))))))

(comment ;testing
  (def the-table
    (->> empty-table
         (conj-fields {:a [1 2 3 4 5 6]
                           :b [1 2 3 4 5 6]
                           :c [1 2 3 4 5 6]})))
  (def recs (table->flyrecords the-table))
  (def newrecs (->> (seq recs)
                    (map (fn [r]
                           (assoc r :c 01 :d "New")))))
  (assert (= newrecs '({:a 1, :b 1, :c 1, :d "New"}
                       {:a 2, :b 2, :c 1, :d "New"}
                       {:a 3, :b 3, :c 1, :d "New"}
                       {:a 4, :b 4, :c 1, :d "New"}
                       {:a 5, :b 5, :c 1, :d "New"}
                       {:a 6, :b 6, :c 1, :d "New"}))
          "Expected map equivalence, each record should be hash-equivalent to the 
           test data.")
)

(comment ;testing big records.

  (def big-table (let [xs (vec (range 500000))]
                   (->> empty-table
                        (conj-fields {:a xs
                                      :b xs
                                      :c xs}))))
  (def bigrecs (table->flyrecords big-table))
  (def newrecs (->> (seq bigrecs)
                    (map (fn [r]
                           (assoc r :c 01 :d "New")))))
                     

)

(comment   ;testing.... 
  (def mytable  (conj-fields [[:first ["tom" "bill"]] 
                              [:last  ["spoon" "shatner"]]] empty-table)) 
  (def mymaptable {:first ["tom" "bill"] 
                   :last  ["spoon" "shatner"]}) 
   
  (def othertable (->> empty-table  
                    (conj-fields [[:first ["bilbo"]] 
                                  [:last  ["baggins"]]]))) 
  (def conctable (concat-tables [mytable othertable])) 
  (def query (->> [mytable othertable] 
               (concat-tables) 
               (conj-field  
                 [:age [31 65 400]]))) 
   
  (def sortingtable (->> query  
                      (conj-field [:xcoord [2 2 55]]) 
                      (conj-field [:home   ["USA" "Canada" "Shire"]]))) 
  (def closest-geezer (->> sortingtable 
                        (order-by [:xcoord 
                                   [:age :descending]]))) 
  (defn compound-query []  
    (->> closest-geezer  
      (select-fields [:home :first :last]) 
      (vector {:home ["PA"] 
               :first ["Barry"] 
               :last ["Groves"]}) 
      (map #(order-fields-by [:home :first :last] %)) 
      (concat-tables)                                               
      view-table))  

  (defn join-test []
    (let [names ["Able" "Baker" "Charlie"]
          professions {:name names
                       :profession [:soldier :farmer :baker]}
          instruments {:name (conj names "Dan") 
                       :instrument [:rifle :plow :oven :guitar]}]
      (join-on :name professions instruments)))
                  
  
  (defn join-test2 [] 
    (let [names ["Able" "Baker" "Charlie"] 
          professions {:name names 
                       :profession [:soldier :farmer :baker]} 
          instruments {:name (conj names "Dan")  
                       :instrument [:rifle :plow :oven :guitar]}
          ages {:name names 
                :age [20 30 40]}] 
      (join-tables [:name] [professions instruments ages])))
  
  (defn select-example [] 
    (->> closest-geezer  
      (select :fields [:home :first :last] 
              :ordering [[:home :descending]] 
              :from) 
      (conj-field [:instruments [:guitar :fiddle]])))
  
  (defn select-from-join []
    (let [names ["Able" "Baker" "Charlie"] 
          professions {:name names 
                       :profession [:soldier :farmer :baker]} 
          instruments {:name (conj names "Dan")  
                       :instrument [:rifle :plow :oven :guitar]} 
          ages {:name names  
                :age [20 30 40]}] 
      (select :fields [:instrument :profession]
              :from (join-tables [:name] [professions instruments ages]))))
  
  (defn map-field-test [] 
    (let [names ["Able" "Baker" "Charlie"] 
          professions {:name names 
                       :profession [:soldier :farmer :baker]} 
          instruments {:name (conj names "Dan")  
                       :instrument [:rifle :plow :oven :guitar]} 
          ages {:name names  
                :age [20 30 40]}
          joined (join-tables [:name] [professions instruments ages])
          names  (field-vals (get-field :name joined))]
           
      (->> joined
           (map-field :name (fn [x] (.toLowerCase x)))
           (conj-field [:upper-name 
                        (field-vals 
                          (get-field :name 
                            (map-field :name (fn [x] (.toUpperCase x)) 
                               joined)))])
           (order-fields-by [:age 
                             :name 
                             :upper-name 
                             :profession 
                             :instrument])))) 
  ;performance testing...
  (def small-table-path "C:\\Users\\thomas.spoon.NAE\\Documents\\sampledata\\djdata.txt")
  ;;Read in some pre-baked data
  (def the-string (clojure.string/replace (slurp small-table-path) \; \tab))
  ;Some data is encoded as "32,35" etc.
  (defn drop-quotes [x] (subs  x 1 (dec (count x))))
  (defn comma-numbers [^String x] x )
  ;; (defn comma-numbers [^String x] 
  ;;   ((parse/vec-parser :int) 
  ;;    (clojure.string/split (drop-quotes x) #",")))

  (def parseinfo (partition 2 [:id      :int
                               :date    :string ;:date
                               :open    comma-numbers
                               :am      comma-numbers
                               :pm      comma-numbers
                               :close   comma-numbers
                               :total   :number
                               :average comma-numbers
                               :stock   :string]))
  
  (def fields (mapv first  parseinfo))
  (def types  (mapv second parseinfo))
  
  (def dj-schema (zipmap fields types))

  ;;not working as expected.  Binding isn't updating like I expected.
  (parse/with-parsers {:comma-numbers comma-numbers} 
    (def rec-parser  (parse/record-parser  dj-schema))
    (def line-parser (parse/vec-parser fields (parse/parsing-scheme dj-schema))))
  
  ;  (defn select-as []
;    (let [names ["Able" "Baker" "Charlie"] 
;          professions {:name names 
;                       :profession [:soldier :farmer :baker]} 
;          instruments {:name (conj names "Dan")  
;                       :instrument [:rifle :plow :oven :guitar]} 
;          ages {:name names  
;                :age [20 30 40]}] 
;      (select :fields [:instrument 
;                       :profession 
;                       [:age   (fn [i rec] (* 10 i))]
;                       [:index (fn [i rec] i)]]               
;              :from (join-tables [:name] [professions instruments ages]))))
  )
  

;;Older, slower version.                    
(comment 
;;Optimize#
;;we're using lazy seq here..
;;map-indexed vector...
;;another optimization is to use a reducer..
;;since it's a reduction, we could just go low-level here.
;;could also use transducer...
(defn- conj-row! [transientcolumns rowvector]
  (let [bound (count rowvector)]
    (loop [j 0
           acc transientcolumns]
      (if (== j bound) acc
          (recur (unchecked-inc j)
                 (assoc! acc j (conj! (get acc j)
                                      (nth rowvector j))))))))
(defn conj-rows
  "Conjoins multiple rowvectors.  Should be fast, using transients.
   Returns a persistent collection."
  [columns rowvectors]
  (assert (= (count (general/first-any rowvectors)) (count columns)))
  (persistent-columns! 
   (reduce conj-row! (transient-columns columns) rowvectors)))

(definline hm! [xs]
  `(clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array ~xs)))

(definline zip-record! [xs ys]
  (let [ks  (with-meta (gensym "ks") {:tag 'objects})
        vs  (with-meta (gensym "vs") {:tag 'objects})
        arr (with-meta (gensym "arr") {:tag 'objects})]
    `(let [~ks ~xs
           ~vs ~ys
           ~arr (object-array (unchecked-multiply 2 (alength ~ks)))
           bound#  (alength ~ks)]
       (loop [idx# 0
              inner# 0]
         (if (== idx# bound#) (clojure.lang.PersistentArrayMap/createAsIfByAssoc ~arr)
             (do (aset ~arr inner# (aget ~ks idx#))
                 (aset ~arr (unchecked-inc inner#) (aget ~vs idx#))
                 (recur (unchecked-inc idx#)
                        (unchecked-add inner# 2))))))))

;;we can access any table record in constant time.
;;#Optimize!
;;Dogshit slow.  Optimize....
(defn table-records
  "Fetches a sequence of n records, where records are maps where keys correspond
   to fields in the table, and values correspond to the values at row (n - 1)."
  [tbl]
  (let [flds (reverse (table-fields tbl))]
    (map (fn [n] (nth-record tbl n flds))
         (range (count-rows tbl)))))
)
