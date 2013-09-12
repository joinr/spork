;;General comparison operations, and a macro to compose 
;;complex comparators.

(ns spork.util.comparison
   (:require [spork.util   [general :as gen]]))

;;parsing comparison rules...
;;at the end of the day, we want a simple sequence of comparisons to combine.
;;not unlike parser combinators, or other little grammars.

;;a comparison rule can be:
;;|f:: a -> a -> comparison
;;|{:key-fn f} :: map (a, (a->b))
;;|{:compare-fn f}     :: map (a, (a->a->comparison))
;;So we use comparison-type as a dispatch function for a multimethod.
;;We'll read in "specs" to parse a comparer, and use this build our comparers 
;;up from specs (functions, or maps of {:key-fn f}|{:compare-fn f}

(defn comparison-type [x] 
  (cond (map? x) (cond (contains? x :key-fn)      :key-fn 
                       (contains? x :compare-fn)  :compare-fn
                       :else (throw (Exception. (str "Invalid dispatch map " x))))
        (sequential? x)  :serial 
        (fn?  x)         :fn
        (keyword? x)     :keyword
        :else (throw (Exception. (str "Invalid dispatch input " x)))))

;;Might be too much cruft here? 

;;utility function to convert maps of {:function|:key v} into functions that 
;;can be used to compare two values.
(defmulti  as-comparer (fn [x] (comparison-type x))) 
;;return the comparison function directly.
(defmethod as-comparer :fn  [f] f)
(defmethod as-comparer :keyword [k] 
  (fn [l r] (compare (k l) (k r))))
;;unpack the comparer 
(defmethod as-comparer :compare-fn [m] (get m :compare-fn))

;;For nested rules, we traverse the rule set and compile them.  Just an 
;;optimization step.

(defn compile-rules 
  "Compiles a possibly nested comparer by walking the rules and evaluating 
   as-comparer in a depth-first fashion." 
  [rule]
  (case (comparison-type rule)
    :serial (vec (map compile-rules rule))
    (as-comparer rule)))

;;allow composite comparer rules.  Given a (possibly nested) sequence of 
;;comparisons, it'll compile the comparers into a single rule.
(defmethod as-comparer :serial  [xs]
  (gen/serial-comparer (compile-rules xs)))
  
;;utility to tag values as direct comparison functions that can compare items.
(defn ->fn    [x]    {:compare-fn x})

;;utility to tag values as key-generators to be used when comparing.
(defn ->key   [f]    {:compare-fn (fn [l r] (f l) (f r))})
(defn ->eq    [f v]  {:compare-fn (fn [l r] (= (f l r) v))})

;;Flip the comparison direction, to go from ascending to descending.
(defn flip       [f]      {:compare-fn (fn [l r] (f r l))})

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