;Defines useful operations for ripping tables from excel workbooks, and 
;piping data do excel workbooks programatically.  Uses Docjure, which in 
;turn uses the Apache POI libraries to interact with Excel docs.
(ns spork.util.excel.core
;  (:use [dk.ative.docjure.spreadsheet])
  (:use [spork.util.excel.docjure])
  (:require [spork.util [table :as tbl] [vector :as v] [io :as io]])
  (:import (org.apache.poi.ss.usermodel  Sheet Cell Row DataFormatter)))

(set! *warn-on-reflection* true)

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

;(defn row->vec [r]  
;  (vec (map read-cell (into-seq r))))


(defn row->seq
  [^Row r]
  (vec (for [^Cell item (iterator-seq (.iterator r))] item)))

(defn row->indexed-cells [^Row r] 
  (map (fn [^Cell c] 
         (vector (.getColumnIndex c) (read-cell c))) 
       (iterator-seq (.iterator r))))

;;TODO: revisit this, we can probably do mo betta.
;;causing problems here...
(defn row->vec
  ([^Row r bound]
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
                         y       (read-cell x) ;;This is where we'd hook in if we only wanted text.
                         i       (.getColumnIndex x)                        
                         missed  (reduce conj acc
                                         (take (- i idx)
                                               (repeat nil)))]
                     (recur (conj missed y) (inc i) (rest xs)))))))
  ([r] (row->vec nil)))

(comment
;;We use the dataformatter here, just getting strings out.
(defn row->strings
  ([^Row r ^DataFormatter df]
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
                         y       (.formatCellValue df  x) ;;This is where we'd hook in if we only wanted text.
                         i       (.getColumnIndex x)                        
                         missed  (reduce conj acc
                                         (take (- i idx)
                                               (repeat nil)))]
                     (recur (conj missed y) (inc i) (rest xs))))))))
)

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

;; (defn rows->raw-table
;;   "Converts an excel worksheet into a columnar table.  Assumes first row defines 
;;    field names."
;;   [xs] 
;;   (when (seq xs)
;;     (let [rows    (->> xs 
;;                        (reduce (fn [acc r]
;;                                  (conj acc (row->vec r))))
;;                        [])
;;           fields  (first (subvec rows 0 1))
;;           records (v/transpose (subvec rows 1))]
;;       (tbl/make-table fields records))))   

(defn ucase [^String s] (.toUpperCase s))
(defn lcase [^String s] (.toLowerCase s))
(defn nth-row [idx ^Sheet sheet] (.getRow sheet idx))
(defn first-row [sheet] (nth-row 0 sheet))
(defn truncate-row [v] 
  (let [n (dec (count v))]
    (loop [idx 0]
      (cond (> idx n) v
            (nil? (get v idx)) (subvec v 0 idx)
            :else (recur (inc idx))))))

;(defn contiguous-rows
;  "Fetch a seq of contiguous rows, starting at startrow.  Rows may have
;   noncontiguous cells, however...."
;  [sheet & {:keys [startrow] :or {startrow 0}}]
;  (->> (iterate inc startrow)
;       (map (fn [idx] (nth-row idx sheet)))
;       (take-while #(not (nil? %)))))


(defn contiguous-rows
  "Fetch a seq of contiguous rows, starting at startrow.  Rows may have
   noncontiguous cells, however...."
  [sheet]
  (let [rows (row-seq sheet)
        parts (->> rows       
                (map    (fn [^Row row] [(.getRowNum row) row]))    
                (partition 2 1)
                (filter (fn [[[i1 _] [i2 _]]] (= i1 (dec i2)))))]
    (if (empty? parts)
        rows
        (flatten 
          (concat (map second (first parts)) 
                  (map (comp second second) (rest parts)))))))

;(defn tabular-region
;  "Assumes that sheet represents a table, in which case, the upper-left 
;   corner, cell A1, is the beginning of a set of adjacent cells, which form
;   a rectangle.  Nil values are allowed in cells in each row, except for the 
;   first row, which is assumed to denote field names.  The rectangular region 
;   will be truncated after the first nil is found in the field names."
;  [sheet]
;  (let [rows   (contiguous-rows sheet)
;        fields (truncate-row (row->vec (first rows)))
;        fieldcount (count fields)]    
;      (reduce (fn [acc r]
;                (let [r (row->vec r)]
;                  (conj acc (if (= (count r) fieldcount) r  
;                              (subvec r 0 (count fields))))))
;              [fields] (rest rows))))

(defn tabular-region
  "Assumes that sheet represents a table, in which case, the upper-left 
   corner, cell A1, is the beginning of a set of adjacent cells, which form
   a rectangle.  Nil values are allowed in cells in each row, except for the 
   first row, which is assumed to denote field names.  The rectangular region 
   will be truncated after the first nil is found in the field names."
  [sheet]
  (let [fields (truncate-row (row->vec (first (contiguous-rows sheet))))
        fieldcount (count fields)]    
    (map (fn [r]
           (let [r (row->vec r)
                 rcount (count r)]
             (cond (= rcount fieldcount) r
                   (> rcount fieldcount) (subvec r 0 (count fields))
                   (< rcount fieldcount) (into r (take (- fieldcount rcount) 
                                                       (repeat nil))))))
           (contiguous-rows sheet))))

;(defn sheet->table
;  "Converts an excel worksheet into a columnar table.  Assumes first row defines 
;   field names.  Truncates remaining dataset to the contiguous, non-nil fields 
;   in the first row."
;  [sheet] 
;  (let [rows   (tabular-region sheet)
;        fields (first (subvec rows 0 1))
;        records (v/transpose (subvec rows 1))]      
;      (tbl/make-table fields records)))


;;Maybe revisit this....
(defn sheet->table
  "Converts an excel worksheet into a columnar table.  Assumes first row defines 
   field names.  Truncates remaining dataset to the contiguous, non-nil fields 
   in the first row."
  [sheet] 
  (let [rows    (tabular-region sheet)]
    (when-let [fields  (first rows)]
      (if-let [records (vec (rest rows))]
        (tbl/make-table fields (v/transpose  records))
        (tbl/make-table fields)))))

(defn wb->tables
  "Extract sheets from the workbook located at wbpath, coercing them to tables 
   as per util.table."
  [wb & {:keys [sheetnames] :or {sheetnames :all}}]
  (let [sheets  (sheet-seq wb)]
    (->> (if (= sheetnames :all) sheets
           (let [names (set (map lcase sheetnames))]
             (filter #(contains? names ((comp lcase sheet-name) %))
                     sheets)))
      (map (fn [s] (do (println (sheet-name s))
                       [(sheet-name s) (sheet->table s)])))
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
             (create-workbook (tbl/field->string n) data))]
    (do (doseq [[n data] (rest specs)]
                (let [sheet (add-sheet! wb (tbl/field->string n))]
                  (add-rows! sheet data)))
      wb)))        

(defn tables->xlsx
  "Given a map of {tablename0 table0...tablenameN tableN}, renders the
   tables as worksheets in a workbook, saving the workbook at path."
  [wbpath tablemap]
  (assert (map? tablemap))
  (save-workbook! wbpath (tables->workbook tablemap)))

(defn table->xlsx
  "Renders table t as a singleton sheet, named sheetname, in an xlsx workbook 
   at wbpath."
  [wbpath sheetname t]
  (tables->xlsx wbpath {sheetname t}))

(defn workbook-dir [wbpath] 
  (-> wbpath 
    (clojure.string/replace  ".xlsx" "\\")
    (clojure.string/replace  ".xlsm" "\\")))

(defn xlsx->tabdelimited 
  "Dumps all the tabular worksheets in an xlsx file into a set of tabdelimited 
   text files.  By default, the text files are dumped in a folder sharing the 
   same name as the original workbook.  Caller can supply a seq of sheetnames 
   and an alternate directory to dump the text files in using :sheetname and 
   :rootdir key arguments."
  [wbpath & {:keys [rootdir sheetnames] 
             :or {sheetnames :all rootdir (workbook-dir wbpath)}}]
  (let [tmap (wb->tables (load-workbook wbpath) :sheetnames sheetnames)]
    (doseq [[nm t] (seq tmap)]
      (let [textpath (io/relative-path rootdir [(str nm ".txt")])]
        (io/hock textpath (tbl/table->tabdelimited t))))))  

(defn xlsx->tables
  "Extract one or more worksheets from an xls or xlsx workbook as a map of 
   tables, where each sheet is rendered as a contiguous table, with first row 
   equal to field names."
  [wbpath & {:keys [sheetnames ignore-dates?] 
             :or {sheetnames :all ignore-dates? false}}]
  (if ignore-dates?
    (ignoring-dates
      (wb->tables (load-workbook wbpath) :sheetnames sheetnames))
    (wb->tables (load-workbook wbpath) :sheetnames sheetnames)))
   

(defn xlsx->wb
  "API wrapper for docjure/load-workbook.  Loads an excel workbook from 
   a given workbook path."
  [wbpath] 
  (load-workbook wbpath))

(defmulti as-workbook class)
(defmethod as-workbook java.lang.String [wb] (load-workbook wb))
(defmethod as-workbook org.apache.poi.xssf.usermodel.XSSFWorkbook [wb]
  wb)

(defmethod as-workbook :default [wb] 
  (throw (Exception. (str "Method not implemented for type " (type wb)))))

(defmulti as-sheet (fn [sheet wb] (class sheet)))
(defmethod as-sheet java.lang.String [sheet wb] 
  (select-sheet sheet (as-workbook wb)))

(defmethod as-sheet :default [sheet wb] 
  (throw (Exception. (str "Method not implemented for type " (type sheet)))))

(comment 
(def wbpath
  "C:\\Users\\thomas.spoon\\Documents\\sampling-utils\\record-rules-large.xlsx")

;testing  
(def wbpath   
  "C:\\Users\\thomas.spoon\\Documents\\Marathon_NIPR\\OngoingDevelopment\\MPI_3.76029832.xlsm")
(def outpath "C:\\Users\\thomas.spoon\\Documents\\newWB.xlsx")

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
  "C:\\Users\\thomas.spoon\\Documents\\sampling-utils\\record-rules-large.xlsx")
)

