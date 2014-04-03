;;A representation of mutable tables.
(ns spork.data.tables
  (:require [spork.util [array :as arr] [general :as gen]]
            [clj-tuple :as tup]))


;;implementations for lookup tables, caches, mutable tables, 
;;sparse tables, dense tables, etc. 

;;Provides the options for tuple-based keys in tables as the 
;;default, rather than vectors.

;;Primarily for use as algorithmic building blocks.

;;Tuples are indeed faster than other things btw.  This allows us to 
;;have decent access to sparse tables.  Still, nested tables are 
;;faster for most lookups. The functions in spork.util.general 
;;with a 2 postfix highlight this fact.

(defmacro assoc-n [m & idxsv]
  (let [arity (dec (count idxsv))                
        _     (assert (> arity 1) "need at least one key and one value")
        idxs  (butlast  idxsv) ;awesome idiom..thanks
        v     (last idxsv)]
    `(assoc ~m (tup/tuple ~@idxs) ~v)))

(defmacro assoc-n! [m & idxsv]
  (let [arity (dec (count idxsv))                
        _     (assert (> arity 1) "need at least one key and one value")
        idxs  (butlast  idxsv) ;awesome idiom..thanks
        v     (last idxsv)]
    `(assoc! ~m (tup/tuple ~@idxs) ~v))) 

(defmacro get-n! [m & idxs]
  (let [arity (count idxs)
        _     (assert (> arity 1) "need at least one key and one value")]
    `(get ~m (tup/tuple ~@idxs))
    ))
  

    


