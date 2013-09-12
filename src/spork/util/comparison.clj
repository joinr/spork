;;General comparison operations, and a macro to compose 
;;complex comparators.

(ns spork.util.comparison
   (:require [spork.util   [general :as gen]]))

;;Flip the comparison direction, to go from ascending to descending.
(defn flip    [f]   (fn [l r] (f r l)))

;;For nested rules, we traverse the rule set and compile them.  Just an 
;;optimization step.

(defn comparison-type [x] 
  (cond (sequential? x)  :serial 
        (fn?  x)         :fn
        (keyword? x)     :keyword
        :else (throw (Exception. (str "Invalid dispatch input " x)))))

;;allow composite comparer rules.  Given a (possibly nested) sequence of 
;;comparisons, it'll compile the comparers into a single rule.
(defn as-comparer [x]
  (case (comparison-type x)
    (:fn :keyword) x
    :serial   (gen/serial-comparer (vec (map as-comparer x)))))
 
;;utility to tag values as key-generators to be used when comparing.
(defn ->key   [f]   (fn [l r] (compare (f l) (f r))))

;;if function f, when compared to the left input, yields v, implies left is greater.
(defn ->where 
  ([f] (fn [l r] (f l r)))
  ([f v] (fn [l r] (= (f l r) v))))

;;if any predicate returns a true, yields true.
(defn ->any   [preds] (fn [l r] (some (fn [p] (p l)) preds)))  

;;Invert the default ordering 
(defn ->descending [f]  (flip f))

;;need a defcomparer....we have something like this in util.table, and util.record.
;;You could do something really cool here, and actually provide a special 
;;scripting language.  Maybe later.
(defmacro defcomparer
  "Defines unit comparison functions.  Creates a sequential comparer out of the 
   key functions provided.  For now, only sequential comparison is supported.
   Optionally, user may supply an argument for a context."
  [name rule]
  `(let [sc# (as-comparer ~rule)]
     (defn ~name  [~'l ~'r] (sc# ~'l ~'r))))