(ns case
  (:require  [criterium.core :as c]))

(defmacro case-identical?
  "Like clojure.core/case, except instead of a lookup map, we
   use `condp` and `identical?` in an unfolding macroexpansion
   to allow fast case lookups for smaller cases where we may
   beat the o(1) cost of hashing the clojure.core/case incurs
   via its lookup map.  Some workloads are substantially (~3x)
   faster using linear lookup and `identical?` checks.

   Caller should be aware of the differences between `identical?`
   and `=` or other structural hashing comparisons.  `identical?`
   is appropriate for object (e.g. pointer) equality between
   instances, and is more restrictive than structural equality
   per `clojure.core/=`; objects may be = but not `identical?`,
   where `indentical?` objects are almost certainly `=`."
  [e & clauses]
  (let [ge      (with-meta (gensym) {:tag Object})
        default (if (odd? (count clauses))
                  (last clauses)
                  `(throw (IllegalArgumentException. (str "No matching clause: " ~ge))))
        conj-flat   (fn [acc [k v]]
                      (conj acc k v))]
    (if (> 2 (count clauses))
      `(let [~ge ~e] ~default)
      (let [pairs     (->> (partition 2 clauses)
                           (reduce (fn [acc [l r]]
                                     (if (seq? l)
                                       (reduce conj acc (for [x l] [x r]))
                                       (conj acc [l r])))  []))
            dupes    (->> pairs
                          (map first)
                          frequencies
                          (filter (fn [[k v]]
                                    (> v 1)))
                          (map first))
            args     (reduce conj-flat [] pairs)]
        (when (seq dupes)
          (throw (ex-info (str "Duplicate case-identical? test constants: " (vec dupes)) {:dupes dupes})))
        `(let [~ge ~e]
           (condp identical? ~ge ~@(if default (conj args default) args)))))))

(defmacro lookup-test [case-form k]
  `(~case-form ~k
    :a 0 :b 1 :c 2 :d 3 :e 4 :f 5 :g 6
    :h 7 :i 8 :j 9 :k 10 :l 11 :m 12 :n 13
    :o 14 :p 15 :q 16 :r 17 :s 18 :t 19
    :u 20 :v 21 :w 22 :x 23 :y 24 :z 25))

;;case is using a lookup map behind the scenes, so its bottleneck
;;should be constant time due to hashing (and the size of the map):
;; user> (let [k :a] (c/quick-bench (lookup-test case k))) 
;; Evaluation count : 49048872 in 6 samples of 8174812 calls.
;; Execution time mean : 10.604857 ns


;; user> (let [k :z] (c/quick-bench (lookup-test case k)))
;; Evaluation count : 52268994 in 6 samples of 8711499 calls.
;; Execution time mean : 9.967222 ns

;;case-identical?, using condp, is slower if we have to scan the entirety
;;of the cases, as in :z

;; user> (let [k :z] (c/quick-bench (lookup-test case-identical? k)))
;; Evaluation count : 44478186 in 6 samples of 7413031 calls.
;; Execution time mean : 12.023996 ns

;;if we can short circuit, or scan over a small input (e.g. 8 elements),
;;case-identical? should dominate.


;;case-identical? dominates since it's not hashing, and can do a very fast short circuiting
;;lookup
;; user> (let [k :a] (c/quick-bench (lookup-test case-identical? k)))
;; Evaluation count : 102362154 in 6 samples of 17060359 calls.
;; Execution time mean : 4.177209 ns

;;case-identical? should continue to dominate until the trade off
;;for hashing is lower than sequential scan and identical?
;;compares..

;;9 cases:

;; user> (let [k :i] (c/quick-bench (lookup-test case-identical? k)))
;; Evaluation count : 70461078 in 6 samples of 11743513 calls.
;; Execution time mean : 6.757059 ns

;;11 cases:
;; user> (let [k :k] (c/quick-bench (lookup-test case-identical? k)))
;; Evaluation count : 65722644 in 6 samples of 10953774 calls.
;; Execution time mean : 7.345973 ns

;;15 cases
;; user> (let [k :p] (c/quick-bench (lookup-test case-identical? k)))
;; Evaluation count : 57303390 in 6 samples of 9550565 calls.
;; Execution time mean : 8.794489 ns

;;20 cases
;; user> (let [k :t] (c/quick-bench (lookup-test case-identical? k)))
;; Evaluation count : 51325122 in 6 samples of 8554187 calls.
;; Execution time mean : 10.254777 ns

;;this yields the invariant that we can have a "fast case" dispatch
;;that should dominate clojure.core/case for small test cases (a majority)
;;and approach parity with the hash-based clojure.core/case when the
;;case count approaches 20.  Beyond 20, we use clojure.core/case.


;;Derived from clojure.core/case
(defmacro fast-case
   "Drop-in replacement for clojure.core/case that attempts to optimize
    identical? case comparison (e.g. keywords).
    Takes an expression, and a set of clauses.

    Each clause can take the form of either:

    test-constant result-expr

    (test-constant1 ... test-constantN)  result-expr

    The test-constants are not evaluated. They must be compile-time
    literals, and need not be quoted.  If the expression is equal to a
    test-constant, the corresponding result-expr is returned. A single
    default expression can follow the clauses, and its value will be
    returned if no clause matches. If no default expression is provided
    and no clause matches, an IllegalArgumentException is thrown.

    Unlike cond and condp, fast-case does a constant-time dispatch for
    ints and non-keyword constants; the clauses are not considered
    sequentially.

    If all test cases are keywords, then fast-case will leverage an
    optimized path for `identical?` checks, where we balance the
    performance of a linear comparison of entries by object
    identity with the cost of an associative lookup and hashing
    of the case objects.  This can yield signficant savings
    for cases that are all keywords, and when there may be
    benefit for short-circuiting operations (e.g. the most
    likely case is first).

    All manner of constant expressions are acceptable in case,
    including numbers, strings,  symbols, keywords, and (Clojure)
    composites thereof. Note that since lists are used to group
    multiple constants that map to the same expression, a vector
    can be used to match a list if needed. The  test-constants
    need not be all of the same type."
  [e & clauses]
  (let [ge (with-meta (gensym) {:tag Object})
        default (if (odd? (count clauses))
                  (last clauses)
                  `(throw (IllegalArgumentException. (str "No matching clause: " ~ge))))
        conj-flat   (fn [acc [k v]]
                      (conj acc k v))]
    (if (> 2 (count clauses))
      `(let [~ge ~e] ~default)
      (let [pairs (->> (partition 2 clauses)
                       (reduce (fn [acc [l r]]
                                 (if (seq? l)
                                   (reduce conj acc (for [x l] [x r]))
                                   (conj acc [l r])))  []))]
        (if (and (every? keyword? (map first pairs))
                 (<= (count pairs) 20))
          `(case-identical? ~e ~@clauses)
          `(clojure.core/case ~e ~@clauses))))))

(let [k :a]
  (c/quick-bench
   (fast-case k :a 0 :b 1 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 9 :j 10 :k 11 :l
     12 :m 13 :n 14 :o 15 :p 16 :q 17 :r 18 :s 19 :none)))

;; Evaluation count : 108369882 in 6 samples of 18061647 calls.
;; Execution time mean : 3.791586 ns

(let [k :a]
  (c/quick-bench
   (case k :a 0 :b 1 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 9 :j 10 :k 11 :l 12 :m
         13 :n 14 :o 15 :p 16 :q 17 :r 18 :s 19 :none)))

;; Evaluation count : 55105488 in 6 samples of 9184248 calls.
;; Execution time mean : 8.826299 ns

(let [k :s]
  (c/quick-bench
   (fast-case k :a 0 :b 1 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 9 :j 10 :k 11 :l
              12 :m 13 :n 14 :o 15 :p 16 :q 17 :r 18 :s 19 :none)))

;; Execution time mean : 9.388600 ns

(let [k :s]
  (c/quick-bench
   (fast-case k :a 0 :b 1 :c 2 :d 3 :e 4 :f 5 :g 6 :h 7 :i 9 :j 10 :k 11 :l
              12 :m 13 :n 14 :o 15 :p 16 :q 17 :r 18 :s 19 :none)))

;; Evaluation count : 55049616 in 6 samples of 9174936 calls.
;; Execution time mean : 9.402726 ns


(let [k :a] (c/quick-bench (fast-case k :a 0 :b 1 :none)))

;; Evaluation count : 105935916 in 6 samples of 17655986 calls.
;; Execution time mean : 3.748773 ns


(let [k :missing] (c/quick-bench (fast-case k :a 0 :b 1 :none)))

;; Evaluation count : 101388984 in 6 samples of 16898164 calls.
;; Execution time mean : 4.107746 ns

(let [k :a] (c/quick-bench (case k :a 0 :b 1 :none)))

;; Evaluation count : 58064934 in 6 samples of 9677489 calls.
;; Execution time mean : 8.382319 ns

(let [k :missing] (c/quick-bench (case k :a 0 :b 1 :none)))

;; Evaluation count : 56006400 in 6 samples of 9334400 calls.
;; Execution time mean : 8.979109 ns
