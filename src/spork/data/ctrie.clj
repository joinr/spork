;;This wraps a nice interface around Michael Marczyk's
;;port of ctries from Scala.  We base a lot of
;;our operations on the ctrie implementation.
(ns spork.data.concurrent
  (:refer-clojure :exclude
     [into update update-in assoc-in assoc 
      dissoc dissoc-in conj disj contains?])
  (:require [ctries.clj :as ct]
            [spork.util.collections :refer :all])
  )

;;ctries are concurrent HAMTs.
;;They implement the transient map interface,
;;and some other goodies (as well as java concurrent maps).

;;compared to persistent maps, they're about 3x as slow on lookup..
;;and 2x as slow on insert...

;;my bet was that the ctrie would give us the ability to a)
;;perform updates without the need to use nested-associng,
;;and b) allow for concurrent updates into the structure to
;;take advantage of multiple cores.
;;We certainly don't get concurrency in the current strategy (using
;;atomic CAS operations).
;; (defn mtest [n]
;;   (let [the-world (atom (into {} (for [[idx (range n)]]
;;                                    [idx {:name idx :data (rand)}])))]
;;     (dotimes [i 100000]
;;       (
  
              
