;;Defines useful operations for ripping tables from excel workbooks, and 
;;piping data do excel workbooks programatically.  Uses Docjure, which in 
;;turn uses the Apache POI libraries to interact with Excel docs.

;;Intended to be used seamlessly with spork.util.table abstract tables,
;;or record sequences.
(ns spork.util.excel.core
  (:require [spork.util [table :as tbl]
                        [vector :as v]
                        [io :as io]
                        [string :as s]]
            [spork.util.excel.docjure]
            [dk.ative.docjure.spreadsheet :as doc]
            [clojure.string :refer [lower-case]])
  (:import [org.apache.poi.ss.usermodel Sheet Cell Row DataFormatter]))

;;we want to enable the ability for tables that have
;;a completely empty rowvector, i.e. all nil or blank,
;;to be seen as a table terminator.

(def +blank+ "")
(defn blank? [v]
  (or (nil? v)
      (and (string? v)
           (identical? v +blank+))))

(defn empty-row? [xs]
  (every? blank? xs))

(set! *warn-on-reflection* true)

;;TODO: revisit this, we can probably do mo betta.
;;causing problems here...
;;Now we allow a custom function for cell->val to be passed in.
;;Note: bound may not be necessary...
;;Since rows are sparse, we're trying to fill in empties that
;;we find.
(defn row->vec
  ([^Row r bound cell->val]
   (let [bounded? (if (not (nil? bound))
                    (fn [n] (> n bound))
                    (fn [_] false))
         vs (seq r)]
     (loop [acc []
            idx (int 0)
            xs   vs]
       (cond (empty? xs) acc
             (bounded? idx) (subvec acc 0 bound)
             :else (let [^Cell x (first xs)
                         y       (cell->val x) ;;This is where we'd hook in if we only wanted text.
                         i       (.getColumnIndex x)
                         ;; if i <> idx, we have skipped (i.e. sparse) values
                         missed  (reduce conj acc
                                         (take (- i idx)
                                               (repeat nil)))]
                     (recur (conj missed y) (inc i) (rest xs)))))))
  ([r bound] (row->vec r bound doc/read-cell))
  ([r] (row->vec r nil doc/read-cell)))

;;Todo: revisit this.  we're eagerly building the rows, we could
;;also stream them.
(defn rows->table
  "Converts an excel worksheet into a columnar table.  Assumes first row defines 
   field names."
  [xs] 
  (when (seq xs)
    (let [rows    (->> xs 
                       (reduce (fn [acc r]
                                 (conj acc (row->vec r))))
                       [])
          fields  (first (subvec rows 0 1))
          records (v/transpose (subvec rows 1))]
      (tbl/make-table fields records))))

;;maybe not used?
(defn nth-row [idx ^Sheet sheet] (.getRow sheet idx))
(defn first-row [sheet] (nth-row 0 sheet))

(defn truncate-row [v] 
  (let [n (dec (count v))]
    (loop [idx 0]
      (cond (> idx n) v
            (nil? (get v idx)) (subvec v 0 idx)
            :else (recur (inc idx))))))

(defn contiguous-rows
  "Fetch a seq of contiguous rows, starting at startrow.  Rows may have
   noncontiguous cells, however...."
  [sheet]
  (let [rows (doc/row-seq sheet)
        parts (->> rows
                (map    (fn [^Row row] [(.getRowNum row) row]))
                (partition 2 1)
                (filter (fn [[[i1 _] [i2 _]]] (= i1 (dec i2)))))]
    (if (empty? parts)
        rows
        (flatten 
          (concat (map second (first parts)) 
                  (map (comp second second) (rest parts)))))))


(def +default-options+
  {:skip            0
   :read-cell       doc/read-cell
   ;:sheet->rows     tabular-region ;;maybe later..
   :ignore-dates?   false})

(defn replace-newlines
  "Given any input x, applies a replacement ala clojure.string/replace
   to x iff x is a string, to swap out the corner case of newlines in
   the input."
  ([x replacement]
    (if (string? x)
      (clojure.string/replace x "\n" replacement)
      x))
  ([x] (replace-newlines x "")))

;;collection of shorthands for our cell readers.
;;could grow, who knows.
(def cell-readers
  {:default         doc/read-cell
   :strip-newlines  (fn newline-reader [cl]
                      (-> (doc/read-cell  cl)
                          (replace-newlines "")))
   :dash-newlines   (fn newline-reader [cl]
                      (-> (doc/read-cell  cl)
                          (replace-newlines "-")))
   :under-newlines  (fn newline-reader [cl]
                      (-> (doc/read-cell  cl)
                          (replace-newlines "_")))
   :verbose-newlines (fn newline-reader [cl]
                      (-> (doc/read-cell  cl)
                          (replace-newlines "[NEWLINE]")))})

(defn multi? [x] (instance? clojure.lang.MultiFn x))

(defn as-cell-reader
  "aux function, coerces x into a known cell reader, or allows
   functions to pass through."
  [x]
  (cond (keyword? x)
        (or (get cell-readers x)
            (throw (ex-info "unknown reader keyword"
                            {:input x :expected (keys cell-readers)})))
        (or (fn? x) (multi? x))   x
        :else (throw (ex-info "expected keyword or function"
                              {:input x :cause :invalid-cell-reader}))))

;;this buys us the opportunity to load any relevant options.
#_{:sheetnames    ["Blah" "Blee"]
   :options       {:default +default-options+
                   "Blah" {:skip 1
                           :read-cell :strip-newlines}
                   "Blee" {:read-cell (fn [cl]
                                        (let [res (doc/read-cell cl)]
                                          (if (string? res)
                                            (str res "[Blee!]")
                                            res)))}}}

(defn tabular-region
  "Assumes that sheet represents a table, in which case, the upper-left 
   corner, cell A1, is the beginning of a set of adjacent cells, which form
   a rectangle.  Nil values are allowed in cells in each row, except for the 
   first row, which is assumed to denote field names.  The rectangular region 
   will be truncated after the first nil is found in the field names.
   Callers may supply a map of options to control the behavior of reading
   the region.  Options take the form:
  
   :skip - [0] integer number of lines to skip before reading the table fields

   [CURRENTLY INACTIVE]
   :ignore-dates? - [false] a boolean indicator of whether additional
   parsing (potentially more time) should be spent trying to parse
   dates. Performance sensitive cases may opt for true.
  
   :read-cell - [:default] a keyword defining a pre-defined cell
   reader, one of #{:default :strip-newlines :dash-newlines
   :under-newlines :verbose-newlines} or a function that takes a cell
   and returns a value.

   Typically, the easiest way for callers to override behavior of read-cell
   is to wrap the default spork.util.excel.docjure/read-cell, applying it
   to the input value, then computing the result."
  ([sheet] (tabular-region sheet +default-options+))
  ([sheet options]
   (let [{:keys [skip read-cell ignore-dates?]
          :or   {skip          0
                 read-cell     doc/read-cell
                 ignore-dates? true}} options
         read-cell  (as-cell-reader read-cell)
         fields     (truncate-row (row->vec (nth (contiguous-rows sheet) skip)))
         fieldcount (count fields)
         pooled     (s/->string-pool 100 1000)
         read-cell-pooled (fn [cl]
                            (let [res (doc/read-cell cl)]
                              (if (string? res) (pooled res) res)))]
     (do #_#_binding [doc/*date-checking* (not ignore-dates?)]
       (when ignore-dates?
         (println [:spork.util.excel/core "date checking avoidance is bypassed currently due to docjure."]))
       (->> (contiguous-rows sheet)
            (drop skip)
            (map (fn [r]
                   (let [r (row->vec r nil read-cell-pooled)
                         rcount (count r)]
                     (cond (= rcount fieldcount) r
                           (> rcount fieldcount) (subvec r 0 fieldcount)
                           (< rcount fieldcount) (into r (take (- fieldcount rcount)
                                                               (repeat nil)))))))
            (take-while (complement empty-row?)) ;;we infer a blank row as the end of the table.
            )))))

;;Maybe revisit this....
;;lots of garbage here.  We should be able to directly map the tabular
;;region into corresponding field/rows without creating intermediate
;;vectors...
(defn sheet->table
  "Converts an excel worksheet into a columnar table.  Assumes first row defines
   field names.  Truncates remaining dataset to the contiguous, non-nil fields
   in the first row."
  ([sheet] (sheet->table sheet +default-options+))
  ([sheet options]
   (let [rows    (tabular-region sheet options)]
     (when-let [fields  (first rows)]
       (if-let [records (vec (rest rows))]
         (tbl/make-table fields (v/transpose  records))
         (tbl/make-table fields))))))

(defn wb->tables
  "Extract sheets from the workbook located at wbpath, coercing them to tables 
   as per util.table."
  [wb & {:keys [sheetnames options] :or {sheetnames :all}}]
  (let [sheets  (doc/sheet-seq wb)]
    (->> (if (= sheetnames :all) sheets
           (let [names (set (map lower-case sheetnames))]
             (filter #(contains? names ((comp lower-case doc/sheet-name) %))
                     sheets)))
         (map (fn [s] (let [nm (doc/sheet-name s)
                            _  (println nm)
                            options (or (get options nm)
                                        (get options :default)
                                        +default-options+)]
                       [(doc/sheet-name s) (sheet->table s options)])))
      (into {}))))

(defn tables->workbook
  "Given a map of {tablename0 table0...tablenameN tableN}, renders them 
   to a workbook object, which can be persisted as an xlsx."
  [tablemap]  
  (assert (map? tablemap))
  (let [specs (map (fn [[nm t]]
                     [nm (reduce conj [(tbl/table-fields t)]
                                 (tbl/table-rows t))]) (seq tablemap))
        wb (let [[n data] (first specs)]
             (doc/create-workbook (tbl/field->string n) data))]
    (do (doseq [[n data] (rest specs)]
                (let [sheet (doc/add-sheet! wb (tbl/field->string n))]
                  (doc/add-rows! sheet data)))
      wb)))

(defn tables->xlsx
  "Given a map of {tablename0 table0...tablenameN tableN}, renders the
   tables as worksheets in a workbook, saving the workbook at path."
  [wbpath tablemap]
  (assert (map? tablemap))
  (doc/save-workbook! wbpath (tables->workbook tablemap)))

(defn table->xlsx
  "Renders table t as a singleton sheet, named sheetname, in an xlsx workbook 
   at wbpath."
  [wbpath sheetname t]
  (tables->xlsx wbpath {sheetname t}))

;;using portable file-path from spork.util.io
(defn workbook-dir [wbpath] 
  (-> wbpath 
     (clojure.string/replace  ".xlsx" "\\")
     (clojure.string/replace  ".xlsm" "\\")
     (io/file-path)))

(defn tables->tabdelimited
  "Auxillary function.  Provided a path to a root directory, and
   a map (or seq) of [nm tbl], emits the table as a tabdelimited
   text file in /root/nm.txt .  This assumes valid names for the
   potential files-to-be.  Responsibility is on caller for now!
   May be composed with output from xlsx->tables for
   custom parsing/xforms."
  [rootdir tmap]
  (doseq [[nm t] (seq tmap)]
    (let [textpath (io/relative-path rootdir [(str nm ".txt")])]
      (io/hock textpath (tbl/table->tabdelimited t)))))

(defn xlsx->tabdelimited 
  "Dumps all the tabular worksheets in an xlsx file into a set of tabdelimited 
   text files.  By default, the text files are dumped in a folder sharing the 
   same name as the original workbook.  Caller can supply a seq of sheetnames 
   and an alternate directory to dump the text files in using :sheetname and 
   :rootdir key arguments.

   Caller may supply a map of sheet-related options, as per
   spork.excel.core/tabular-regions.  When processing worksheets
   into tables, this map of options will be consulted to see
   if a specific options map exists for the current sheetname.
   If not, options associated with :default will be used,
   and spork.excel.core/+default-options+ serving in case
   no :default is specified.

   ex. options: sheets named \"Blah\" will have the first row skipped,
   and cells will have newlines removed.

   sheets names \"Blee\" will be read normally, but every string
   cell value will have \"[Blee!]\" appended to it in the resulting
   table:
  
   {:default +default-options+
     \"Blah\" {:skip 1
               :read-cell :strip-newlines}
     \"Blee\" {:read-cell (fn [cl]
                             (let [res (doc/read-cell cl)]
                               (if (string? res)
                                 (str res \"[Blee!]\")
                                  res)))}}"
  [wbpath & {:keys [rootdir sheetnames options] 
             :or {sheetnames :all rootdir (workbook-dir wbpath)}}]
 (let [options (merge {:default +default-options+} options)] 
   (->> (wb->tables (doc/load-workbook wbpath) :sheetnames sheetnames :options options)
        (tables->tabdelimited rootdir))))
  
(defn xlsx->tables
  "Extract one or more worksheets from an xls or xlsx workbook as a map of 
   tables, where each sheet is rendered as a contiguous table, with first row 
   equal to field names.

   Caller may supply a map of sheet-related options, as per
   spork.excel.core/tabular-regions.  When processing worksheets
   into tables, this map of options will be consulted to see
   if a specific options map exists for the current sheetname.
   If not, options associated with :default will be used,
   and spork.excel.core/+default-options+ serving in case
   no :default is specified.
  
   ex. options: sheets named \"Blah\" will have the first row skipped,
   and cells will have newlines removed.

   sheets names \"Blee\" will be read normally, but every string
   cell value will have \"[Blee!]\" appended to it in the resulting
   table:
  
   {:default +default-options+
     \"Blah\" {:skip 1
               :read-cell :strip-newlines}
     \"Blee\" {:read-cell (fn [cl]
                             (let [res (doc/read-cell cl)]
                               (if (string? res)
                                 (str res \"[Blee!]\")
                                  res)))}}"
  [wbpath & {:keys [sheetnames ignore-dates? options] 
             :or {sheetnames :all ignore-dates? false}}]
  (let [options (-> (merge {:default +default-options+} options)
                    (assoc-in [:default :ignore-dates?] ignore-dates?))] 
    (wb->tables (doc/load-workbook wbpath) :sheetnames sheetnames :options options)))
   
(defn xlsx->wb
  "API wrapper for docjure/load-workbook.  Loads an excel workbook from 
   a given workbook path."
  [wbpath] 
  (doc/load-workbook wbpath))

(defmulti as-workbook class)
(defmethod as-workbook java.lang.String [wb] (doc/load-workbook wb))
(defmethod as-workbook org.apache.poi.xssf.usermodel.XSSFWorkbook [wb]
  wb)

;;Probably a resource
(defmethod as-workbook java.net.URL [^java.net.URL wb]
  (with-open [stream (.openStream wb)]
    (doc/load-workbook-from-stream stream)))

(defmethod as-workbook :default [wb] 
  (throw (Exception. (str "Method not implemented for type " (type wb)))))

(defmulti as-sheet (fn [sheet wb] (class sheet)))
(defmethod as-sheet java.lang.String [sheet wb] 
  (doc/select-sheet sheet (as-workbook wb)))

(defmethod as-sheet :default [sheet wb] 
  (throw (Exception. (str "Method not implemented for type " (type sheet)))))

(comment 
(def wbpath
  "~Documents/sampling-utils/record-rules-large.xlsx")

;testing  
(def wbpath
  "~Documents/Marathon_NIPR/OngoingDevelopment/MPI_3.76029832.xlsm")
(def outpath "~Documents/newWB.xlsx")


(def wb (as-workbook wbpath))
(def tables ["Deployments"
             "DemandTrends" 
             "InScope"
             "OutOfScope"
             "DemandRecords"
             "Parameters"
             "PeriodRecords"
             "RelationRecords"
             "SRCTagRecords"
             "SupplyRecords" 
             "Titles"])
(def supply (select-sheet "SupplyRecords" wb))
(def demand (select-sheet "DemandRecords" wb))
(def tmap (wb->tables wb :sheetnames tables))


(def bigpath 
  "~/Documents/sampling-utils/record-rules-large.xlsx")
)

(comment 
; Load a spreadsheet and read the first two columns from the 
; price list sheet:
(->> (load-workbook "spreadsheet.xlsx")
     (select-sheet "Price List")
     (select-columns {:A :name, :B :price}))

;; Create a spreadsheet and save it
(let [wb (create-workbook "Price List"
                          [["Name" "Price"]
                           ["Foo Widget" 100]
                           ["Bar Widget" 200]])
      sheet (select-sheet "Price List" wb)
      header-row (first (row-seq sheet))]
  (do
    (set-row-style! header-row (create-cell-style! wb {:background :yellow,
                                                       :font {:bold true}}))
    (save-workbook! "spreadsheet.xlsx" wb)))
)

;;not used
#_(def ^:dynamic *end-of-table*)


;;not used...already exists via cell-seq 
#_(defn row->seq
  [^Row r]
  (vec (for [^Cell item (iterator-seq (.iterator r))] item)))

;;not used...
#_(defn row->indexed-cells [^Row r] 
  (map (fn [^Cell c] 
         (vector (.getColumnIndex c) (read-cell c))) 
       (iterator-seq (.iterator r))))

;;not used...

