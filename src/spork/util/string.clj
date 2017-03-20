;;Utilities for string manipulation outside
;;of clojure.string.  Includes functions for
;;string canonicalization and the like.
(ns spork.util.string
  (:import [java.util.concurrent ConcurrentHashMap]
           [java.util.regex Pattern]
           [java.lang CharSequence]))

(defn ->string-pool
  "Creates a canonicalized string pool, starting with n, bounded up to
   size bound.  Can be used like a function to canonicalize strings. When 
   applied to a string, if the string exists in the known set of strings, 
   the sole reference to the alread-existing string is returned, rather
   than the 'new' string.  For datasets with a low cardinality, we get 
   a lot of referential sharing using this strategy.  Typically only
   used during parsing."
  [n bound]
  (let [^ConcurrentHashMap cm (ConcurrentHashMap. (int n))]
    (reify clojure.lang.IFn
      (invoke [this x]
        (do (when (> (.size cm) (int bound))
              (.clear cm))
            (if-let [canon (.putIfAbsent cm x x)]
              canon
              x)))
      clojure.lang.IDeref
      (deref [this] (map key (seq cm))))))


(defn pattern? [x] (instance? java.util.regex.Pattern x))
(defn pattern [^Pattern p] (.pattern p))

(defn ->array-splitter
  "Helper function to create efficient splitters that will
   let us split strings much faster than clojure.string/split.
   Input x may be a regex, or a string.  If a regex, we do 
   a quick check of the pattern to see if we can do a faster 
   split via the delimiter.  Returns an array of objects, as 
   produced by the .split method of either the regex or the string."
  [x]
  (cond (pattern? x) 
        (case (pattern x)
          "\\t" (->array-splitter "\t")
          ","   (->array-splitter ",")
            (fn [^CharSequence s] (.split ^Pattern x s)))
        (string? x) (fn [^String s]
                       (.split s ^String x)) ;much faster   
        :else (throw (Exception. (str [:cannot-delimit-by x])))))

(defn ->vector-splitter
  "Helper function to create efficient splitters that will
   let us split strings much faster than clojure.string/split.
   Input x may be a regex, or a string.  If a regex, we do 
   a quick check of the pattern to see if we can do a faster 
   split via the delimiter.  Returns a vector of strings, as 
   produced by clojure.string/split."
  [x]
  (let [f (if (fn? x) x ;;reuse array splitter if passed.
              (->array-splitter x))]
    (fn string->vector [^String s]
      (clojure.lang.LazilyPersistentVector/createOwning
       (f s)))))
