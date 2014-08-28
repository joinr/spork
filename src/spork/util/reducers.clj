;;A quick patch site to add some missing reducer functionality.
;;Likely to vanish with later clojure updates.
(ns spork.util.reducers
  (:require [clojure.core.reducers]))

(defn get-ns [ns symb]
  (when-let [n (ns-resolve ns symb)]
    (ns-name (:ns (meta n)))))

(defn foreign? [ns symb]
  (do (println [ns symb])
      (not= (str (get-ns ns symb)) (str (ns-name ns)))))
(defmacro eval-when [pred & expr]
  (if (eval pred)
     `(~@expr)))

;;#Additional Reducers 
;;These haven't made it into clojure.core yet, they probably will in
;;1.7  .  I hacked together a couple of useful ones, like range.
(in-ns 'clojure.core.reducers)

(doseq [x '[iterate repeatedly repeat range map-indexed]]
  (ns-unmap *ns* x))

;;we're going to add in iterate, range, and friends
;;Reducers patch for Clojure courtesy of Alan Malloy, CLJ-992, Eclipse Public License
(defcurried iterate
  "A reducible collection of [seed, (f seed), (f (f seed)), ...]"
  {:added "1.5"}
  [f seed]
  (reify
    clojure.core.protocols/CollReduce
    (coll-reduce [this f1] (clojure.core.protocols/coll-reduce this f1 (f1)))
    (coll-reduce [this f1 init]
      (loop [ret (f1 init seed), seed seed]
        (if (reduced? ret)
          @ret
          (let [next (f seed)]
            (recur (f1 ret next) next)))))
    
    clojure.lang.Seqable
    (seq [this]
      (seq (clojure.core/iterate f seed)))))
(defn repeatedly 
  "Creates a reducible sequence of values, produced by evaluating no-args function 
     repf, ala core/repeatedly ,except there is no intermediate collection to muck with."
  ([repf]
     (reify
       clojure.core.protocols/CollReduce
       (coll-reduce [this f] (clojure.core.protocols/coll-reduce this f (repf)))
       (coll-reduce [this f init]
         (loop [ret init]
           (if (reduced? ret)
             @ret
             (recur (f ret (repf))))))      
       clojure.lang.Seqable
       (seq [this]
         (seq (clojure.core/repeatedly repf)))))
  ([n repf] (clojure.core.reducers/take n (repeatedly repf))))
(defn repeat 
  "Creates a reducer that acts akin to core/repeat, returning an 
   infinite sequences of values v.  No intermediate sequences are 
   computed."
  [v]
  (reify
    clojure.core.protocols/CollReduce
        (coll-reduce [this f] (clojure.core.protocols/coll-reduce this f v))
        (coll-reduce [this f init]
          (loop [ret init]
            (if (reduced? ret)
              @ret
              (recur (f ret v)))))      
        clojure.lang.Seqable
        (seq [this]
          (seq (clojure.core/repeat v)))))

(defn range
  "Creates a reducible sequence of numbers, ala core/range, except 
   there is no intermediate collection to muck with."
  ([lower n]
     (reify clojure.core.protocols/CollReduce 
       (coll-reduce [coll f] 
         (loop [idx (+ 2 lower)
                res (f lower (inc lower))]
           (if (or (== idx n) (reduced? res))
             res
             (recur (unchecked-inc idx)
                    (f res idx)))))
           (coll-reduce [coll f val]
             (loop [idx lower
                    res val]
               (if (or (== idx n) (reduced? res))
                 res
                 (recur (unchecked-inc idx)
                        (f res idx)))))
           clojure.lang.Seqable ;;good idea...saw this from patch CLJ992
           (seq [this]  (clojure.core/range lower n))))
  ([n] (range 0 n)))

(defn map-indexed
  "Creates a reducer analogue to core/map-indexed"
  [f r] 
  (let [idx (atom 0)]
    (map (fn [x] 
           (f (swap! idx inc) x))  r)))
(in-ns 'spork.util.reducers)
