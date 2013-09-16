;;General comparison operations, and a macro to compose 
;;complex comparators.

(ns spork.util.comparison
   (:require [spork.util   [general :as gen]]))

;;Flip the comparison direction, to go from ascending to descending.
(defn flip    [f]   (fn [l r] (f r l)))


;;utility to tag values as key-generators to be used when comparing.
(defn ->key   [f]  (fn [l r] (compare (f l) (f r))))
(def  ->val (->key identity))

;;if function f, when compared to the left input, yields v, implies left is greater.
(defn ->where-key 
  [f v] (fn [l r] (compare (= (f l) v) (= (f r) v))))

;;applies pred to the left value in comparison.
;;gross definition of "less"
;;(defn ->lesser [pred] (fn [l r] (pred l)))

;;bases preference on an exact match of value v.
(defn ->where  [v]
  (if (fn? v) 
    (fn [l r] (v l r))
    (->where-key identity v)))



;;if any predicate returns a true, yields true.
(defn ->any   [preds] (fn [l r] (if (some (fn [p] (p l)) preds)
                                  true
                                  false)))  

;;Invert the default ordering 
(defn ->descending [f]  (flip f))

;;For nested rules, we traverse the rule set and compile them.  Just an 
;;optimization step.

(defn comparison-type [x] 
  (cond (sequential? x)  :serial 
        (fn?  x)         :fn
        (keyword? x)     :keyword
        :else (throw (Exception. (str "Invalid dispatch input " x)))))

;;allow composite comparer rules.  Given a (possibly nested) sequence of 
;;comparisons, it'll compile the comparers into a single rule.
(defn ->comparer [x]
  (case (comparison-type x)
    :fn  x
    :keyword (->key x)
    :serial  (gen/serial-comparer (vec (map ->comparer x)))))

(defn ->not [c]   
  (flip (if (fn? c) c (->comparer c))))

;;need a defcomparer....we have something like this in util.table, and 
;;util.record.
;;You could do something really cool here, and actually provide a special 
;;scripting language.  Maybe later.
(defmacro defcomparer
  "Defines unit comparison functions.  Creates a sequential comparer out of the 
   key functions provided.  For now, only sequential comparison is supported.
   Optionally, user may supply an argument for a context."
  [name rule]
  `(let [sc# (->comparer ~rule)]
     (defn ~name  [~'l ~'r] (sc# ~'l ~'r))))