;TOM SPOON 9 July 2012 
;A simple library for reading and writing tabular representations of data.
;Uses map as a container.
(ns spork.util.table
  (:require [clojure [string :as strlib]]
            [clojure [set :as setlib]]
            [util [clipboard :as board]]
            [util [gui :as gui]])
  (:use [util.vector]
        [util.record :only [serial-field-comparer key-function]]
        [util.general :only [align-by]]
        [clojure.pprint :only [pprint]])) 


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
    (field-vals [x] (or (second x) [])))

(declare empty-table 
         -conj-field
         -disj-field
         make-table
         table->map)

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

(defrecord column-table [fields columns]
  ITabular 
    (table-fields [x]  fields)
    (table-columns [x] columns)
  ITabularMaker
    (-make-table [x fields columns] 
       (column-table. fields (normalize-columns columns))))

(defn make-table 
  "Constructs a new table either directly from a vector of fields and 
   vector of columns, or from a collection of IFields, of the form
   [[field1 & [column1-values...]]
    [field2 & [column2-values...]]]  or 
   {:field1 [column1-values] 
    :field2 [column2-values]} "
  ([fields columns]
    (->column-table fields (normalize-columns columns)))
  ([Ifields] (->> (if (map? Ifields) 
                       (reverse Ifields)
                       Ifields)
                  (fn [specs] (make-table (vec (map field-name specs))
                                          (vec (map field-vals specs)))))))
    
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

(defn table->map
  "Extracts a map representation of a table, where keys are 
   fields, and values are column values. This is an unordered table."
  [tbl] 
  (if (and (map? tbl) (not= (type tbl) util.table.column-table)) tbl
    (let [cols (table-columns tbl)]
      (reduce (fn [fldmap [j fld]]  (assoc fldmap fld (get cols j))) {} 
              (reverse (map-indexed vector (table-fields tbl))))))) 

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
  (vec (map #(get % n) (table-columns tbl))))

(defn table-rows
  "Returns a vector of the rows of the table."
  [tbl]  (vec (map (partial nth-row tbl) (range (count-rows tbl)))))

(defn nth-record
  "Coerces a column-oriented vector representation of tables into 
   a map where keys are field names, and vals are column values at 
   the nth record."
  [tbl n & [flds]]
  (assert (valid-row? tbl n) (str "Index " n " Out of Bounds"))  
  (zipmap (if flds flds (reverse (table-fields tbl))) 
          (reverse (nth-row tbl n))))

(defn table-records
  "Fetches a sequence of n records, where records are maps where keys correspond
   to fields in the table, and values correspond to the values at row (n - 1)."
  [tbl]
  (let [flds (reverse (table-fields tbl))]
    (map (fn [n] (nth-record tbl n flds)) (range (count-rows tbl)))))

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

(defn conj-row
  "Conjoins a rowvector on a vector of columns."
  [columns rowvector]
  (assert (= (count rowvector) (count columns)))
  (reduce (fn [acc [j x]] (assoc acc j (conj (get acc j) x)))
          columns (map-indexed vector rowvector)))


(defn- conj-row! [transientcolumns rowvector]
  (reduce (fn [acc [j x]] (assoc! acc j (conj! (get acc j) x)))
          transientcolumns (map-indexed vector rowvector)))

(defn conj-rows
  "Conjoins multiple rowvectors.  Should be fast, using transients.
   Returns a persistent collection."
  [columns rowvectors]
  (assert (= (count (first rowvectors)) (count columns)))
  (persistent-columns! 
    (reduce conj-row! (transient-columns columns) rowvectors)))  

(defn records->table
  "Interprets a sequence of records as a table, where fields are the keys of 
   the records, and rows are the values."
  [recs]
  (let [flds (vec (reverse (keys (first recs))))]
    (make-table flds (conj-rows (vec (map (fn [_] []) flds))
                                (map (comp vals reverse) recs)))))

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


(defn negate [n] (* n -1))

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

; design to support the as syntax.
; [:age   (fn [i rec] (* 10 i))]

(defmulti computed-field  (fn [x] (if (fn? x) :function (type x))))
(defmethod computed-field :function [f] (fn [i rec] (f i rec)))
(defmethod computed-field clojure.lang.Keyword [k]  (fn [i rec] (get rec k)))

;implementation for 'as statements pending....

;(defn parse-as-map [x]
;  (cond (or (keyword? x) (string? x)) 
;          {:x (computed-field x)}
;        (vector? x) (let [[fld v &rest] x]
;                      {fld (computed-field v)})
;        :else (throw (Exception. (str "Unsupported type in 'as query" x)))))
;(defmacro with-computed-fields [fieldmap & body]
  

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

(defn select 
  "Allows caller to compose simple SQL-like queries on abstract tables.  
   Joins are implemented but not supported just yet.  The :from key for the 
   select query is intended to be a table, specifically something supporting 
   ITabular."
  [& {:keys [fields from where unique orderings]
      :or   {fields :* from nil where nil
             unique true orderings nil }}]
  (select- fields from where unique orderings)) 

(defn parse-string 
	"Parses a string, trying various number formats.  Note, scientific numbers,
	 or strings with digits sandwiching an E or an e will be parsed as numbers,
	 possibly as Double/POSITIVE_INFINITY, i.e. Infinity."
	[value]	
  (try (Integer/parseInt value)
    (catch NumberFormatException _
      (try (Double/parseDouble value)
        (catch NumberFormatException _ value)))))

(def scientific-reg 
	"A regular expression gleefully borrowed from Stack Overflow.  Matches 
	 strings that correspond to scientific numbers."
	#"-?\d*\.?\d+[Ee][+-]?\d+")
	
(defn parse-string-nonscientific 
	"Parses a string, trying various number formats.  Scientific numbers,
	 or strings with digits sandwiching an E or an e will be kept as strings.  
	 Helpful in contexts where there are alphanumeric string values."
	[value]
	(if-let [res (re-find scientific-reg value)]
		value
		(parse-string value)))
	
(defn pair [a b] [a b])
(def re-tab (re-pattern (str \tab)))
(def split-by-tab #(strlib/split % re-tab))

;older table abstraction, based on maps and records...

(defn tabdelimited->table 
  "Return a map-based table abstraction from reading a string of tabdelimited 
   text.  The default string parser tries to parse an item as a number.  In 
   cases where there is an E in the string, parsing may return a number or 
   infinity.  Set the :parsemode key to any value to anything other than 
   :scientific to avoid parsing scientific numbers."
   [s & {:keys [parsemode keywordize-fields?] 
         :or   {parsemode :scientific
                keywordize-fields? true}}]
  (let [lines (strlib/split-lines s )
        parsef (if (= parsemode :scientific) 
                 parse-string 
                 parse-string-nonscientific)
        parse-rec (comp vec #(map parsef %) split-by-tab)
        tbl (->column-table 
              (vec (map (if keywordize-fields? 
                          keyword
                          identity) (split-by-tab (first lines ))))
              [])]
      (->> (conj-rows (empty-columns (count (table-fields tbl))) 
                      (map parse-rec (rest lines)))
        (assoc tbl :columns))))

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
(defn get-fields [t] (table-fields t))
(defn last-record [t] (get-record t (dec (record-count t))))

;(defn table->tabdelimited 
;  "Render a table into a tab delimited representation.
;   Rerouted to use the new API."
;  [tbl & {:keys [stringify-fields?]
;          :or {stringify-fields? true}}]
;  (reduce 
;    (fn [acc rec] (str (apply str acc (interleave rec (repeat \tab))) \newline))
;    "" (concat (if stringify-fields? 
;                 [(vec (map field->string (table-fields tbl)))] 
;                 [(table-fields tbl)]) (table-rows tbl))))

(defn row->string 
  ([separator r] (strlib/join separator r))
  ([r] (row->string \tab)))

(defn record->string [rec separator] (row->string (vals rec) separator)) 
(defn table->string 
  "Render a table into a string representation.  Uses clojure.string/join for
   speed, which uses a string builder internally...if you pass it a separator.
   By default, converts keyword-valued field names into strings.  Caller 
   may supply a different function for writing each row via row-writer, as 
   well as a different row-separator.  row-writer::vector->string, 
   row-separator::char||string"
  [tbl & {:keys [stringify-fields? row-writer row-separator]
          :or   {stringify-fields? true 
                 row-writer (partial row->string \tab)
                 row-separator \newline}}]
  (let [xs (concat (if stringify-fields? 
                     [(vec (map field->string (table-fields tbl)))] 
                     [(table-fields tbl)]) 
                   (table-rows tbl))]
    (strlib/join row-separator (map row-writer xs))))

(defn table->tabdelimited
  "Render a table into a tab delimited representation."  
  [tbl & {:keys [stringify-fields?] :or {stringify-fields? true}}] 
  (table->string tbl :stringify-fields? stringify-fields?))

(defn infer-format [path]
  (case (strlib/lower-case (last (strlib/split path #"\.")))
    "txt" :tab
    "clj" :clj 
    nil))

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
  [s] (record-seq (as-table s)))

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

;establishes a simple table-viewer.
;we can probably create more complicated views later....
(defmethod gui/view util.table.column-table [obj & {:keys [title sorted] 
                                :or {title "Table" sorted false}}]
    (gui/->scrollable-view
      (gui/->swing-table (get-fields obj)  
                         (table-rows obj) :sorted sorted)
      :title title))

(defmethod gui/as-JTable util.table.column-table [obj & {:keys [sorted]}]
  (gui/->swing-table (get-fields obj)  
                     (table-rows obj) :sorted sorted))

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
  

;it'd be nice to have simple sql-like operators....
;we've already got select 
;in SQL, we use
