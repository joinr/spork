;;A set of functions for specifying high-level solution descriptions in as 
;;projections onto various normalized representations.  Default represntations
;;include arrays of normalized floating point values [0.0, 1.0], and bitstrings.
;;Primitive operations are wrapped for maximum effeciency.
(ns spork.opt.representation
  (:require [spork.util.vectors :refer :all]
            [spork.util.combinatoric :refer [combinatoric-map elements->key 
                                             combinatoric-map?]])) 

;;Solution Representations
;;========================
;;Solutions can take on a ton of different representation forms, each one 
;;custom to a specific problem domain.  However, we can build a language for 
;;defining solutions that consist of n-dimensional components, where each 
;;component can be an integer, a real number, or a function that maps 
;;integers to discrete values (i.e. a vector, a map, a set, etc.)  This will 
;;allow trivial solution representation definitions, as well as constraint 
;;definitions later on.

;;Ideally, we want a high-level means to map an n-dimensional point of 
;;values, ranging between [0.0 1.0], into a solution representation.
;;That way, we can just write a single normalized neighborhood function 
;;that can turn into a discrete solution.

;;Our primary solution representation will be as an abstract vector of doubles.
;;These protocols allow us to define efficient encoders and decoders.
(defprotocol IDoubleVectorEncoder
  (encode-doublevector [e s] "Maps s to an IDoubleVector."))
(defprotocol IDoubleVectorDecoder
  (decode-doublevector [d dvec] "Maps an IDoubleVector to another domain."))

;;We will use a supplementary library, implemented in __collective.vectors__ to 
;;handle defining and manipulating arbitrary vectors in an efficient manner.

;;Solution Perturbation (I said Perturbation, sicko!) 
;;===================================================

;;At the highest level of abstraction, we want to define means for altering or 
;;changing solution representations to facilitate the search process.  One 
;;practical and general means of perturbing solutions, particulalry solutions
;;with and encoding to normalized vectors, is to add an n-dimensional float 
;;vector to the normalized vector representation of the solution.  

;;For some types, we want to allow a means for quickly munging through 
;;perturbations.  For purely numerical represenations, for instance, we might 
;;just be performing primitive arithmetic to perturb the solution, if the 
;;solution representation is a double vector as well.  In that case, applying 
;;native operations would be much much faster, and facilitated via
;;internal-perturb.  If implementors define solutions that satisfy the protocol, 
;;the encoding/decoding process will be bypassed.
(defprotocol IPerturbable 
  (internal-perturb [x deltas] 
    "Like internal-reduce. Implementations can choose how to perturb.")) 

(defmacro with-double-encoding
  "Uses encoder e, which satisifes IDoubleVectorEncoder and IDoubleVectorDecoder
   to provide local bindings for encoding and decoding."
  [e body]
  `(let [~'*encode* (partial encode-doublevector ~e)
         ~'*decode* (partial decode-doublevector ~e)]
     ~body))

(defn perturb-solution
  "Perturbs solution s by supplying the associated double vector via 
   deltas.  Returns the newly perturbed representation.  s must support either 
   IPerturbable, or both IDoubleVectorEncoder and IDoubleVectorDecoder."
  [s deltas & {:keys [encoding] :or {encoding s}}]
  (if (satisfies? IPerturbable s) 
    (internal-perturb s deltas) ;use fast version if we can 
    (with-double-encoding encoding  ;else coerce to doublevec.
      (-> (*encode* s)
          (vec-add deltas)
          (*decode*)))))
 
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

;;Normalization Primitives
;;========================
;;Operations for projecting various types to and from a normal range.

(defn ^double numeric->normal
  "Returns a function that normalizes input values based on the width between
   lower and upper bounds."
  [lower upper]
  (let [w (double (- upper lower))
        f (float-clamp 0.0 1.0)]    
     (^double fn [x]  (f (/ (- x lower) w)))))  

(defn normal->numeric
  "Returns a function that scales normalized input values to the range between 
   lower and upper."
  [lower upper & {:keys [int? step-width]}]
  (let [w (double (- upper lower))
        clamped (if int? 
                  (int-clamp lower upper :step-width step-width) 
                  (float-clamp lower upper :step-width step-width))]
    (^double fn [^double x] (clamped (+ lower (* x w))))))

(defn normal->category
  "Returns a function that maps double inputs to discrete elements of a 
   set."
  [xs]
  (let [ordered (vec (distinct (seq xs)))
        norm    (double  (count ordered))]
    (fn [^double x] (nth ordered (Math/floor (* x norm))))))


;;Note: this guy eagerly enumerates the categories; we actually don't 
;;want that for combinatorial domains.  As such, we'll keep the categorical
;;and combinatorial domains separate, even though they're both VERY similar.

(defn category->normal
  "Returns a function that maps discrete elements of a categorical
   set to doubles."
  [xs]
  (let [ordered (into {} (map-indexed  vector (vec (distinct (seq xs)))))
        norm    (double  (count ordered))
        inverse (into {} (for [[k v] ordered] [v (double (/ k norm))]))]
    (fn [x] (get inverse x))))


(defn normal->combination 
  "Returns a function that maps double inputs to discrete elements of a 
   combinatoric map."
  [cmap]
  (let [norm (double (count cmap))]
    (fn [^Double x] (get cmap (Math/floor (* x norm)))))) 

(defn combination->normal
  "Returns a function that maps discrete elements of a combinatoric map to 
   normalized values."
  [cmap]
  (let [idx->normal (numeric->normal 0 (count cmap))] 
    (^double fn [xs] (idx->normal (elements->key cmap xs)))))

;;Normalization
;;=============

;;A normalizer is something that knows how to project values to and from 
;;a normalized range [0.0 1.0], and vice-versa.  We represent complex solutions
;;in a generic, normalized vector by composing normalizers across multiple 
;;domains.
(defprotocol INormalizer
  (to-normal [izer n] 
    "Projects n into a normal value between [0.0 1.0]")
  (from-normal [izer n] 
     "Projects a normalized value n into an underlying domain")
  (normalizer-spec [izer] 
     "Returns the rangespec(s) - that define normalization."))  

;;We have allowing maps to serve as specifications for converting to and from 
;;normalized float arrays.

;;We can probably just implement this in general for maps!
;;========================================================
(defn normals->map
  "Given a specification for a representation, returns a function that maps a 
   double vector to the solution domain."
  [spec]
  (let [ks (into [] (keys spec))]
	  (fn [dvec]
     (loop [acc (transient spec)
            ys  ks
            idx 0]
       (if (empty? ys) (persistent! acc)
           (let [k (first ys)
                 n (get spec k)]
             (recur (assoc! acc k (from-normal n (vec-nth dvec idx)))
                    (rest ys)
                    (inc idx))))))))

(defn map->normals
  "Given a specification for a representation, returns a function that maps 
   the solution domain to a double vector."
  [spec]
  (let [ks (into [] (keys spec))       
        a  (double-array (count ks))
        base-vec (get-empty-vec (count ks))]
	  (fn [s]
     (loop [acc base-vec
            ys  ks
            idx 0]
       (if (empty? ys) acc
         (let [k (first ys)
               n (get spec k)]
           (recur (set-vec acc idx (to-normal n (get s k))) 
                  (rest ys) 
                  (inc idx))))))))

(defn normals->vec
  "Given a specification for a representation, returns a function that maps 
   a double vector to the solution domain."
  [spec]
  (fn [dvec]
    (loop [acc (transient [])
           fs  spec
           idx 0]
      (if (empty? fs) (persistent! acc)
        (let [n (first fs)]
          (recur (conj! acc (from-normal n (vec-nth dvec idx)))
                 (rest fs)
                 (unchecked-inc idx)))))))

(defn vec->normals
  "Given a specification for a representation, returns a function that maps 
   the solution domain to double vectors."
  [spec]
  (let [base-vec (get-empty-vec (count spec))]
	  (fn [s]
     (loop [acc base-vec 
            fs  spec
            vs  s
            idx 0]
       (if (empty? fs) acc
         (let [n (first fs)]
           (recur (set-vec acc idx (to-normal n (first vs))) 
                  (rest fs) 
                  (rest vs) 
                  (unchecked-inc idx))))))))  

(defn type-map
  "Aux function for dispatching on normalization types."
  [t] 
  (cond (float? t)   :float      
        (integer? t) :int
        (map? t)     :map-spec
        (set? t)     :set
        (combinatoric-map? t) :combination
        (vector-range? t) :vec-range
        (vector? t)  :vec-spec
        :else        nil))

;;Default implementations for normalizing doubles, ints, and categorical data, 
;;so we have a simplified normalization interface.  These implementations will 
;;be composed in map-based specifications to conveniently describe solution 
;;representations.  We also handle combinatorial domains effeciently.

(defmulti normalizer
  "Builds a function that, based on the args provided, creates a normalization 
   scheme for encoding and decoding values.  Can take a pair of float or integer
   args.  If ints are provided, the normalized values will be clamped to 
   integer values.  If a sequence of values is provided, the normalizer will 
   map normal values to distinct elements of the sequence, and vice versa.  
   Maps are intepreted as specifcations for multi-dimensional solutions, with 
   each entry being a valid input to the normalizer function."
  (fn [x & [y & rest]] 
    (let [res (if y [(type-map x) (type-map y)]
                    [(type-map x)])]
      (assert (if (= (count res) 2) (= (first res) (second res)) true)
        (str "Types must be identically integer-based or float based for ranges"
             res))
      res)))

(defn make-normalizer [normal de-normal & {:keys [spec]}]
  (reify INormalizer
    (to-normal    [izer s] (normal s))
    (from-normal [izer n]  (de-normal n))
    (normalizer-spec [izer] spec) 
    IDoubleVectorEncoder
    (encode-doublevector [izer dv] (normal dv))
    IDoubleVectorDecoder
    (decode-doublevector [izer dv] (de-normal dv))))

;;Floating point values are clamped to the float range [x y step].
(defmethod normalizer [:float :float] [x & [y step]]
  (make-normalizer (numeric->normal x y) 
                   (normal->numeric x y :step-width step)
                   :spec {:float [x y step]}))

;;Integers are clamped to the integer range [x y step].
(defmethod normalizer [:int :int] [x & [y step]]
  (make-normalizer (numeric->normal x y)
                   (normal->numeric x y :int? true :step-width step)
                   :spec {:int [x y step]})) 

;;Collections are inferred to be categorical data.
(defmethod normalizer [:set]  [xs & rest]
  (make-normalizer (category->normal xs) 
                   (normal->category xs)
                   :spec {:category xs}))

;;__TODO__ Figure allow a caching scheme, to push the combinations into 
;;categorical data for small-enough combinations.  There's a cutoff where it's 
;;probably more efficient.  I'm assuming 100 for now...although that may change
;;to something much larger (maybe 1000?) after profiling.

;;Combinations are like categorical data, but they are not enumerated eagerly. 
(defmethod normalizer [:combination]  [xs & rest]
  (if (<= (count xs) 100)
    (let [cached (vec (vals xs))] ;enumerate the combinations for small domains.         
      (make-normalizer (category->normal cached) 
                       (normal->category cached)
                       :spec {:category cached}))
    ;;otherwise use the potentially gigantic, but efficient combinatorial map.
    (make-normalizer (combination->normal xs) 
                     (normal->combination xs)
                     :spec {:combination xs})))

;;vectors are implicit range specifications.
(defmethod normalizer [:vec-range]  [xs & rest]
  (normalizer (first xs) (second xs) (get xs 2)))

;;Maps serve as composite specifications for an n-dimensional array of 
;;normalized values.  Each key in the spec maps to a float-range, an integer 
;;range, or a collection of categorical data.  In theory, we could nest maps 
;;within maps...
(defmethod normalizer [:map-spec] [spec & rest]
  (let [normalizer-map 
        (reduce  (fn [acc [k v]] 
                   (->> (normalizer v)
                     (assoc acc k))) spec (for [[k v] spec] [k (as-range v)]))]
    (make-normalizer (map->normals normalizer-map) 
                     (normals->map normalizer-map)
                     :spec {:map-spec spec})))

;;annotated vectors are n-dimensional solution specifications.
(defmethod normalizer [:vec-spec]  [xs & rest]
  (let [normalizer-vec (into [] (map normalizer xs))]
    (make-normalizer
      (vec->normals normalizer-vec)
      (normals->vec normalizer-vec)
      :spec {:vec-spec xs})))

;;Exploring Domains
;;=================
(defn domain-to-normals [n xs] (map (partial to-normal n)  xs))
(defn normals->domain   [n normals] (map (partial from-normal n) normals))

(defn vec-filler [length v] (into [] (take length (repeat v))))
(defn map-filler [m v] (reduce (fn [acc k] (assoc acc k v)) m (keys m)))
(defn walk-domain
  "Returns a sequence of walks across the normalized domain of 0.0 to 1.0, by 
   the supplied step-width, for normalizer n.  Projects the normalized value 
   onto the n-dimensional range defined by the normalizer specification.
   For primitive normalizers, returns a seq of [n x], where x is the projection
   of the normal value n onto range x.  For vector normalizers, returns a seq 
   of [n [x1 x2 x3 ...]] where xs are the n-dimensional projection of n onto 
   xs.  Map normalizers return [n {:k1 x1 :k2 x2 :k3 x3}], where each k is the 
   dimensional component of the map, and x is the normalized projection of n 
   onto the domain of the dimension."
  [n & [step-width]]
  (let [normals (range 0.0 1.0 (or step-width 0.1))
        [normal-tag spec] (first (normalizer-spec n)) ;weak.        
        f  (partial from-normal n)]
    (map (fn [x] [x (f x)])
         (case normal-tag
           :vec-spec (map (partial vec-filler (count spec)) normals)
           :map-spec (map (partial map-filler spec) normals)
           normals))))                 

(defn normal-dimensions
  "Given a multi-dimensional normalization, returns the count of dimensions."
  [norm]
  (let [[normal-tag spec] (first (normalizer-spec norm))]
    (case normal-tag
      (:vec-spec :map-spec) (count spec)
      (throw (Exception. 
               (str "Requires a normalization spec with one or more dimensions:"
                    spec))))))

(defn random-normal-vector
  "Creates a double vector of random normalized points, based off a template
   vector." 
  [base-vec]
  (map-vec base-vec (fn [^double x] (rand)) )) 

(defn basis-vector 
  "Computes a feasible vector for the required dimensionality of the 
   normalization specs associated with normalizer."
  [normalizer]
  (random-normal-vector (get-empty-vec (normal-dimensions normalizer))))

;;Defining Solution Representations
;;=================================

(defmacro defsolution
  "Given a range specification, defines a record that implements INormalizer 
   and vector en/de-coding.  spec should be a map of {:field-name range-exp...}
   or a vector of [field-name range-expr], where range expression is any 
   expression that evaluates to something parse-able with as-range, typically 
   [from to] | [from to step] | #{x y z}."
  [name spec]
  (let [binds      (cond (map? spec)    (seq spec)
                     (vector? spec) (partition 2 spec)
                     :else (throw 
                             (Exception. "Spec must be a vector or a map")))          
        fields     (map first   binds)
        field-keys (map keyword fields)
        ranges     (map second  binds)
        normalizer-name (symbol (str name "-normalizer"))
        rand-name  (symbol (str "random-" name "!")) 
        spec-map   (apply array-map (interleave field-keys ranges))]
    `(do
       (def ~normalizer-name (normalizer ~spec-map)) 
       (defrecord ~name [~@fields]
         INormalizer 
         (~'to-normal   [~'izer ~'s] (to-normal ~normalizer-name ~'s))
         (~'from-normal [~'izer ~'n] (from-normal ~normalizer-name ~'n))
         (~'normalizer-spec [~'izer]  ~normalizer-name) 
         IDoubleVectorEncoder
         (~'encode-doublevector [~'izer ~'dv] 
           (to-normal ~normalizer-name ~'dv))
         IDoubleVectorDecoder
         (~'decode-doublevector [~'izer ~'dv] 
           (from-normal ~normalizer-name ~'dv)))
       (let [basis# (basis-vector ~normalizer-name)]
         (defn ~rand-name [] 
           (from-normal ~normalizer-name (random-normal-vector basis#)))))))

;;Testing
;;=======
(comment 
(def cities #{:SanAntonio :Houston :Austin})

(def common-ranges 
  {:cities #{:SanAntonio :Houston :Austin}
   :birth-to-now  [1981 2013]
   :tank-capacity [0.0 105.0]})

(def city-pairs (combinatoric-map cities 2))

(def repspec 
  (with-ranges common-ranges 
    (as-range {:year     :birth-to-now 
               :widgets  [0 100]
               :gallons  :tank-capacity  
               :city     :cities})))
(def vecspec 
  (with-ranges common-ranges
    (as-range [:birth-to-now               
               :tank-capacity])))
(def cityspec 
  (with-ranges common-ranges
    (as-range [:cities])))

(def sol (normalizer repspec))
(def simple-spec {:year [1981 2013]})
(def simple-spec-fives {:year [1981 2013 5]})
(def simple-sol (normalizer simple-spec))
(def simple-sol-fives (normalizer simple-spec-fives))
(def vec-normal (normalizer vecspec))
(def cat-normal (normalizer cityspec))

(def lunch-locations 
  #{:bozellis
    :px
    :golf-course
    :house-of-dynasty
    :the-greek
    :indian
    :pho})

(def lunch-members #{:john :rick :tom})
(def parties (combinatoric-map lunch-members 2))  

(def weekdays #{:monday :tuesday :wednesday :thursday :friday})

(defsolution lunch 
  [loc     lunch-locations 
   members lunch-members 
   day     weekdays])

(clojure.pprint/pprint 
  (with-double-encoding lunch-normalizer 
    (->> (iterate random-normal-vector (basis-vector sol))
      (map *decode*)
      (take 10))))
)