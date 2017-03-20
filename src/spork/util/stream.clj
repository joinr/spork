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
