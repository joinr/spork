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

(doseq [x '[iterate repeatedly repeat range map-indexed first last nth]]
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

;; (defn range
;;   "Creates a reducible sequence of numbers, ala core/range, except 
;;    there is no intermediate collection to muck with."
;;   ([lower n]
;;      (reify clojure.core.protocols/CollReduce 
;;        (coll-reduce [coll f] 
;;          (loop [idx (+ 2 lower)
;;                 res (f lower (inc lower))]
;;            (if (or (== idx n) (reduced? res))
;;              res
;;              (recur (unchecked-inc idx)
;;                     (f res idx)))))
;;            (coll-reduce [coll f val]
;;              (loop [idx lower
;;                     res val]
;;                (if (or (== idx n) (reduced? res))
;;                  res
;;                  (recur (unchecked-inc idx)
;;                         (f res idx)))))
;;            clojure.lang.Seqable ;;good idea...saw this from patch CLJ992
;;            (seq [this]  (clojure.core/range lower n))))
;;   ([n] (range 0 n)))

;;Tom implementations:
;;I found out, after reading the postings on clojure dev on Jira, that
;;in fact, I have implemented a strategy very similar to some of the
;;pending strategies offered up by the core devs.  So that's not a bad
;;thing :)  
;;On the other hand, these guys will probably become obsolete in the 
;;near future, probably around the 1.7 release of clojure.  I think 
;;the sliceable (or splittable as I've now found it in other efforts)
;;protocol is actually really useful, since it abstracts the notion 
;;of producing countable, referentially transparent chunks from 
;;smaller bits.  Anyway, all this resides behind the wall, so to
;;speak.  Performance looks good, and it's foldable to boot. 
;;And I learned about fold and its implementations too ;)

(defprotocol ISliceable
  (slice [this from to])
  (empty-slice? [this]))

(defn left-slice  [s] (slice s (clojure.core/nth s 0) (clojure.core/nth s (quot (count s) 2))))
(defn right-slice [s] (slice s (clojure.core/nth s (quot (count s) 2)) (count s)))
;;Allows us to fold things that can be sliced, in parallel!
(defn- foldslice  [r n combinef reducef] 
  (cond  (empty-slice? r) (combinef) 
         (<= (count r) n) (reduce reducef (combinef) r) 
         :else 
         (let [split (quot (count r) 2) 
               r1    (slice r 0 split) 
               r2    (slice r split (count r)) 
               fc    (fn [child] #(foldslice child n combinef reducef))] 
           (fjinvoke 
            #(let [f1 (fc r1) 
                   t2 (fjtask (fc r2))] 
               (fjfork t2) 
               (combinef (f1) (fjjoin t2)))))))

(deftype Range [^long from ^long to ^long cnt]
  ISliceable
  (slice [this l r] 
    (assert (and (>= l from) (<= l to) (<= l r)) "range index out of bounds!")
    (Range. (long l) (long r) (long (- l r))))
  (empty-slice? [this] (== from to))
  clojure.lang.Seqable
  (seq [this] (clojure.core/range from to))
  clojure.lang.Indexed
  (nth [this  n] (if (> n cnt) (throw (Exception. "Index out of range!"))
                          (unchecked-add from n)))
  (nth [this  n not-found]   (.nth this n))
  clojure.lang.Counted
  (count [this] cnt)
  clojure.core.protocols/CollReduce
  (coll-reduce [this f] (loop [acc from
                               idx (unchecked-inc from)]
                     (if (or (== idx to) (reduced? acc)) acc
                         (recur (f acc idx) (unchecked-inc idx))))) 
  (coll-reduce [this f init] (loop [acc init
                                    idx from]
                          (if (or (== idx to) (reduced? acc)) acc
                              (recur (f acc idx) (unchecked-inc idx))))) 
  clojure.core.reducers/CollFold 
  (coll-fold [this n combinef reducef] (foldslice this n combinef reducef)))

(defn ^Range range 
   "Creates a reducible, foldable sequence of numbers, ala core/range, except 
    there is no intermediate collection to muck with."
  ([from to] (Range. from to (unchecked-inc (-  to from))))
  ([to]  (Range. 0 to to)))

(defn map-indexed
  "Creates a reducer analogue to core/map-indexed"
  [f r] 
  (let [idx (atom 0)]
    (map (fn [x] 
           (f (swap! idx inc) x))  r)))

(defn right [l r] r)
(defn last  [r] 
  (if (and (instance? clojure.lang.Indexed r) 
           (instance? clojure.lang.Counted r))
    (clojure.core/nth  r (dec (count r)))
    (reduce right nil r)))
(defn first [r] (reduce right nil (take 1 r)))
(defn nth   [n r] 
  (if (instance? clojure.lang.Indexed r)
    (nth r n)
    (cond (zero? n) (first r)
          (pos? n)  (last (take n r))                         
          :else (throw (Exception. "Index for reducers/nth must be non-negative!")))))                     

(in-ns 'spork.util.reducers)
