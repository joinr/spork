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

;;Per javadocs for java.util.string, string.split will need a
;;non-positive arg to continue matching and include whitespace
;;matches, else, the default (0) truncates white space.
(defn ->array-splitter
  "Helper function to create efficient splitters that will
   let us split strings much faster than clojure.string/split.
   Input x may be a regex, or a string.  If a regex, we do 
   a quick check of the pattern to see if we can do a faster 
   split via the delimiter.  Returns an array of objects, as 
   produced by the .split method of either the regex or the string.
   Calls to String.split use non-positive input for the number
   of matches, allowing for returning whitespace."
  [x]
  (cond (pattern? x) 
        (case (pattern x)
          "\\t" (->array-splitter "\t")
          ","   (->array-splitter ",")
            (fn [^CharSequence s] (.split ^Pattern x s)))
        (string? x) (fn [^String s]
                       (.split s ^String x -1)) ;much faster   
        :else (throw (Exception. (str [:cannot-delimit-by x])))))

(defn ->vector-splitter
  "Helper function to create efficient splitters that will
   let us split strings much faster than clojure.string/split.
   Input x may be a regex, or a string.  If a regex, we do 
   a quick check of the pattern to see if we can do a faster 
   split via the delimiter.  Returns a vector of strings, as 
   produced by clojure.string/split.  Does not truncate
   whitespace matches at the end of the line."
  [x]
  (let [f (if (fn? x) x ;;reuse array splitter if passed.
              (->array-splitter x))]
    (fn string->vector [^String s]
      (clojure.lang.LazilyPersistentVector/createOwning
       (f s)))))

;;custom spltter, rendered unnecessary by appropriate usage
;;of String.split (called with -1).

;;As a reducer, this is significantly faster than clojure.string/split
#_(defn split-by
  [^String input ^Character delim]
  (let [d (int delim)]
    (reify
      clojure.lang.ISeq
      (seq  [o]
        (loop [start 0
               end (.indexOf input d 0)
               acc (java.util.ArrayList.)]
          (if (== end -1)
            (do (seq acc))
              (recur (unchecked-inc end)
                     (.indexOf input d (unchecked-inc end))
                     (doto acc (.add (String. (.substring input start end))))))))
      clojure.core.protocols/CollReduce
      (coll-reduce [o f init]
        (loop [start 0
               end (.indexOf input d 0)
               acc init]     
          (cond (reduced? acc) @acc
                (== end -1) acc
                :else
                (recur (unchecked-inc end)
                       (.indexOf input d (unchecked-inc end))
                       (f acc  (String. (.substring input start end)))))))
      (coll-reduce [o f]
        (let [end1 (.indexOf input d 0)]
          (if (== end1 -1) nil
              (let [end2 (.indexOf input d  ^long (unchecked-inc end1))]
                (if (== end2 -1) (String. (.substring input 0 end1))                   
                    (loop [start (unchecked-inc end2)
                           end   (.indexOf input d  (unchecked-inc end2))
                           acc   (f (String. (.substring input 0 end1))
                                    (String. (.substring input (unchecked-inc end1) end2)))]
                      (cond (reduced? acc) @acc
                            (== end -1) acc
                            :else
                            (recur (unchecked-inc end)
                                   (.indexOf input d  (unchecked-inc end))
                                   (f acc (.substring input start end)))))))))))))
