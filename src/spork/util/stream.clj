;;utilities for working with streams of things, namely
;;file streams.  In particular, we provide a useful
;;capability for handling multiple streams
;;through a single interface or handle.
(ns spork.util.stream
  (:require [spork.util [io :as io]
                        [general :as gen]]))

;;what we want is a multimap...
;;a map of streams we can push to.
;;that are keyed..

(declare close-all!)
(defrecord multistream [^String root filename  childname ^String headers ^clojure.lang.Atom writers]
  io/ICloseable 
  (close [x] (close-all! x)))

(defn mstream
  "Creates a multi-file-stream abstraction.  Serves as a handle for multiple writers, 
   supporting functions write-in! and writeln-in!"
  [root name headers & {:keys [childname] :or {childname (fn [x] (str x ".txt"))}}]
  (->multistream root name childname headers (atom {})))

(defn ^java.io.BufferedWriter get-writer! [^multistream ms nm]
  (if-let [w (get (deref (.writers ms)) nm)]
    w
    (let [newfile (str (.root ms) "/" ((.childname ms) nm))
          _       (io/hock newfile "")
          ^java.io.BufferedWriter w  (clojure.java.io/writer newfile)
          _  (println [:writing-to newfile])
          _  (swap! (.writers ms) assoc nm w)
          _  (io/writeln! w
               (if (gen/ref? (.headers ms))
                 (deref (.headers ms)) (.headers ms)))]
      w)))

;; on close, we want to record a manifest, in the root folder, of the 
;;files in the multistream, so that we can read the manifest and get a 
;;corresponding multireader of it.
(defn close-all! [^multistream ms] 
  (let [ws (deref (.writers ms))
        root (.root ms)
        manifest {root (vec (keys ws))}]
    (do (doseq [[nm ^java.io.BufferedWriter w] ws]
          (.close w))
        (reset! (.writers ms) {}) ;closed writers.
        (io/hock (str (.root ms) "/" (.filename ms)) (str manifest)))))

(defn write-in! [^multistream ms k v]
  (io/write! (get-writer! ms k) (str k)))
(defn writeln-in! [^multistream ms k v]
  (io/writeln! (get-writer! ms k) (str k)))


;;Creates an instance of a writer that implements
;;closeable.  Acts like a filestream.  Acts like
;;a reducible function so you can consume
;;streams of records.  Records are written according
;;to the field-order, to destination file at dest.
;;Else, field-order is derived from the first record.
(defprotocol IRecordWriter
  (write-record [w r]))

;;Uses transduce at the moment.  Not sure about
;;performance implications at the moment.  Probably
;;won't matter...
(defn spit-record!
  "Helper function.  Spits records to a writer, according 
   to a specified field order, using the specified 
   field separator sep."
  [^java.io.BufferedWriter w r fields ^String sep]
  (doto ^java.io.BufferedWriter
      (transduce (comp (map #(get r %))
                       (interpose sep))                       
            (completing
                  (fn [^java.io.BufferedWriter w x]
                    (doto w
                      (.write ^String (str x))
                      )))
            w fields)
          (.newLine)))

(deftype record-writer [dest
                        ^java.io.BufferedWriter writer
                        field-order
                        ^String sep
                        ^:unsynchronized-mutable fields
                        ^:unsynchronized-mutable hd]
  clojure.lang.IFn
  (invoke [this r] (write-record this r))
  IRecordWriter
  (write-record [this r]
    (do (when-not hd ;emit the header on first record.
          (let [flds (vec (keys r))
                flds (gen/approx-order field-order flds)                
                header-record (reduce-kv (fn [acc k v]
                                   (assoc acc k
                                          (name k)))
                                 r
                                 r)]            
            (set! hd header-record)
            (set! fields flds)
            (spit-record! writer hd fields sep)))
        (spit-record! writer r fields sep)
        this))
  java.io.Closeable
  (close [this] (.close writer)))

(defn ->record-writer
  "Creates a record writer on destination, which opens an 
   internally managed writer on the file specified by dest.
   Caller may specify optional field-order for the emitted 
   records, separator, and supply a writer 
   (for re-use/appending)."
  [dest & {:keys [field-order sep writer]
           :or {sep "\t"}}]
  (let [w (or writer (clojure.java.io/writer dest))]
    (record-writer. dest w field-order sep nil nil)))

