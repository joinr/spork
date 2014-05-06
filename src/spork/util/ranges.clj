;;Generic functions for defining ranges of values.  These are primitives used 
;;to constrain input values to a possible range of output values.  Various 
;;range constraints exist to map input domains to constrained (possibly integral
;;output domains).
;;__TODO__ Add type hints for clamped values.  I think we're going to hit 
;;reflection warnings.
(ns spork.util.ranges
  (:require [spork.util.combinatoric]))

;;Defining Ranges for Solution Representations
;;============================================

;;For normalized domains, [0.0 1.0], we define a little language, based on maps,
;;for encoding the range we'd like to map the normal domain to.  This will make
;;it easy to define potentially complicated n-dimensional solution 
;;representations that have an integer, float, or categorical range associated 
;;with each dimension.  The mapping will be 1:1, so that every member member 
;;of the normal domain maps to a unique member of the range.  That allows us 
;;to convert to and from a normalized representation - for portability and 
;;effecient computation -- for any solution representation we define.  
;;Additionally, the implementation is flexible enough that we can easily derive 
;;sub-representations by varying (i.e. tightening the range) of select 
;;dimensions from a parent specification.  Since maps and vectors serve as the
;;specification protocol, no witchery is needed to overload or derive new 
;;representations.  

(defprotocol IRangeSpec
  (as-range [x] "Parses x as a range specification compatible with normalize"))

(defn vector-range? [x] 
  (and (vector? x)  
       (not (vector? (first x)))
       (not (keyword? (first x)))))

(declare get-range)

(extend-protocol IRangeSpec
  clojure.lang.Keyword
  (as-range [x] (get-range x))
  clojure.lang.PersistentVector
  (as-range [x]
    (if (vector-range? x)
      (let [[lower upper & [step]] x]
        [lower upper step])
      (mapv as-range x)))
  clojure.lang.PersistentHashMap
  (as-range [x] (into {} (for [[k v] x] [k (as-range v)])))
  clojure.lang.PersistentArrayMap
  (as-range [x] (into {} (for [[k v] x] [k (as-range v)])))
  clojure.lang.PersistentHashSet 
  (as-range [x] x)
  spork.util.combinatoric.lexmap ;combinatoric maps are valid ranges now
  (as-range [x] x))


;;Bounds on numeric ranges using machine-precision values.
(def positive-inf-int Long/MAX_VALUE)
(def negative-inf-int Long/MIN_VALUE)
(def positive-inf Float/MAX_VALUE)
(def negative-inf Float/MIN_VALUE)

;;Some standard numeric ranges.  Note: I originally thought to allow unbounded
;;or infinite ranges for solutions, using the machine-precision to guide the
;;actual width of the range.  This is not as useful as I'd like.  Going forward,
;;I think we'll actually force the caller to supply a bound. They can make it 
;;arbitrarily large to fit the problem domain.
(def canonical-ranges 
  {:real          [negative-inf positive-inf]
   :integer       [negative-inf-int positive-inf-int]
   :positive-real [0.0 positive-inf]
   :negative-real [negative-inf 0.0]
   :positive-int  [0 positive-inf-int]
   :negative-int  [negative-inf-int 0]})

(def range-map 
  "A collection of defined ranges, to allow folks to use short-hand"
  (atom canonical-ranges))
(def ^:dynamic *ranges* "Dynamic binding for ad-hoc range references." {})
(defn add-range!
  "Define a new canonical range in the range-map."
  [name r] (swap! range-map assoc name r))

(defn get-range
  "Looks for a canonical range associated with k, first in *ranges*, then 
   in the range-map."
  [k]
  (get *ranges* k
       (get @range-map k)))

(defmacro defrange
  "Adds a named range of values to the working set of range definitions."
  [name r]
  (let [range-key (keyword name)]
    `(do (add-range! ~range-key ~r)
         (def ~name ~r))))

(defmacro with-ranges
  "Given a map of range specifications, allows us to temporarily overload the 
   canonical ranges for any expressions evaluated in body."
  [m & body]
  `(let [rs# (merge *ranges* ~m)]
     (binding [*ranges* rs#]
       ~@body)))                         

(comment 
  {:float :real
   :int   :integer 
   :real+ :positive-real
   :real- :negative-real 
   :int+  :positive-int
   :int-  :negative-int})

;;Range Constraints
;;=================

(defn step-clamp
  "Returns a function that maps inputs to float values, across intervals are
   that are step-width wide, and the least - leftmost - value of the interval is 
   returned."
  [lower step-width]
  #(+ lower (* (quot (- % lower)  step-width) step-width)))

(defn float-clamp
  "Returns a function that maps inputs to float values between lower and 
   upper."
  [lower upper & {:keys [step-width]}]  
  (assert (< lower upper) "Lower bound must be less than upper bound")     
  (let [f (if step-width (step-clamp lower step-width)
            identity)           
        upper (if step-width (f upper) upper)]           
    #(cond (< % lower) lower
           (> % upper) upper
           :else (f %))))

(defn int-clamp
  "Returns a function that maps inputs to integer values between lower 
   and upper."
  [lower upper & {:keys [step-width]}]
  (let [f (float-clamp lower upper :step-width step-width)]  
    #(f (double (long %)))))
