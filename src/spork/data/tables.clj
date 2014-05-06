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

;(defrecord table2d [])




    


