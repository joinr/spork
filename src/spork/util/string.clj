;;Utilities for string manipulation outside
;;of clojure.string.  Includes functions for
;;string canonicalization and the like.
(ns spork.util.string
  (:import [java.util.concurrent ConcurrentHashMap]))

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
