;;A representation of mutable and persistent tables. Basically, 
;;2D/3D arrays or 2D/3D maps
(ns spork.data.tables
  (:require [spork.util [array :as arr] [general :as gen]]
            [clj-tuple :as tup]))

;;implementations for lookup tables, caches, mutable tables, 
;;sparse tables, dense tables, etc. 

;;Provides the options for tuple-based keys in tables as the 
;;default, rather than vectors.

;;Primarily for use as algorithmic building blocks.

;(defprotocol ISparseTable)
;(defprotocol IDenseTable) 



(defprotocol IFastTable
  (get-entry [tbl k1 k2]
             [tbl k])
  (push-entry [tbl k1 k2 v]
              [tbl k v])
  (drop-entry [tbl k1 k2]
              [tbl k]))

(defprotocol INDKey 
  (key-n [e n]))

(defprotocol IRowCol 
  (row [e])
  (col [e]))

;;how are two coordinates hashed? 
;;If they're integer coordinates, we can cache the hash...  

(extend-type   clojure.lang.MapEntry 
  IRowCol 
  (row [e] (.key e))
  (col [e] (.val e))
  INDKey
  (key-n [e n]
    (case (long n)
      0 (.key e) 
      1 (.val e)
      (loop [acc (unchecked-subtract n 1)
             ^clojure.lang.MapEntry e   (.val e)]
        (cond (== n 0) (.key e)
              (== n 1) (.val e)
              :else    (recur (unchecked-subtract n 2) (.val e)))))))
              

          
(definline get-k1 [m]
  `(if (instance? clojure.lang.MapEntry ~m) (.key m) (nth m 0)))

(definline get-k2 [m]
  `(if (instance? clojure.lang.MapEntry ~m) (.val m) (nth m 0)))

(defrecord nested-table2d [entries]
  IFastTable
  (get-entry [tbl k1 k2] (gen/get2 entries  k1 k2 nil))
  (get-entry [tbl k]     (gen/get2 entries (key k) (val k) nil))
  (push-entry [tbl k1 k2 v])
  (push-entry [tbl k v])
  (drop-entry [tbl k1 k2])
  (drop-entry  [tbl k]))

(defrecord dense-table2d [^objects entries h w]
  IFastTable
  (get-entry [tbl k1 k2] (arr/deep-aget objects entries (int k1) (int k2)))
  (get-entry [tbl k]     nil)
  (push-entry [tbl k1 k2 v] (arr/deep-aset objects entries (int k1) (int k2) v))
  (push-entry [tbl k v] nil)
  (drop-entry [tbl k1 k2])
  (drop-entry  [tbl k]))

(defrecord flat-table2d [entries]
  IFastTable
  (get-entry [tbl k1 k2] (get entries (clojure.lang.MapEntry. k1 k2)))
  (get-entry [tbl k]     (get entries k))
  (push-entry [tbl k1 k2 v])
  (push-entry [tbl k v])
  (drop-entry [tbl k1 k2])
  (drop-entry  [tbl k]))

(defrecord tuple-table2d [entries]
  IFastTable
  (get-entry ([tbl k1 k2])
             ([tbl k]))
  (push-entry ([tbl k1 k2 v])
              ([tbl k v]))
  (drop-entry ([tbl k1 k2])
              ([tbl k])))


(defrecord array-table2d [entries]
  IFastTable
  (get-entry ([tbl k1 k2])
             ([tbl k]))
  (push-entry ([tbl k1 k2 v])
              ([tbl k v]))
  (drop-entry ([tbl k1 k2])
              ([tbl k])))





    


