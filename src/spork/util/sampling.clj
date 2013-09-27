;A generic namespace for drawing samples from populations.
(ns spork.util.sampling
  (:require [spork.util [stats :as stats]]))

;;This rose from the "stochastic demand generation" problem that was presented
;;in a somewhat convoluted solution in VBA...
;;The basic process is this: 
;;  We have a population of records P.
;;  We want to define functions that sample from the population, according to 
;;    ad-hoc constraints, to generate samples of records.  These functions are
;;    record-generators, and they typically involve identifying a set of records,
;;    the "template" population, and then modifying the template according to 
;;    some function.  Modification means mapping a function to the template 
;;    population that returns a sampled population (i.e. transforming template
;;    or original records into sampled records).  
;;  We then want to define a sample as a function of a population and a sequence of
;;  or [record-generator, frequency] pairs, that reduces the sequence of one 
;;  generator pairs by applying the record-generator to the population, frequency 
;;  times, accumulating the resulting sample-records.

;;  There is an additional, post-processing phase, where we validate the sample.
;;  A valid sample is a set of records in which:
;;    A set of dependencies between records from the original population P are 
;;    enforced, in that if record A from population P exists in sample S, and
;;   record B from population P is dependent upon A, then record B must also 
;;    exist in S, where 
;;    dependent:: record -> record -> boolean

;;    For two records x1, x2, which are members of an equivalence class, denoted
;;    by equivalent:: record -> record -> boolean
;;    x1 and x2 cannot "intersect", where 
;;      intersection:: record -> record -> boolean
;;    The "distance" between records identified as members of an equivalance 
;;      class  is either 0 or (distance x1 x2) >= (minimum-distance x1 x2) > 0
;;      where
;;      distance :: record -> record -> float
;;      minimum-distance:: record -> record -> float


;;__Patched__
;;replacement for rand-nth, uses random number seed if provided, else defers to 
;;rand-nth 
(defn sample-nth [xs] (nth xs (int (* (stats/*rand*) (count xs)))))


;;Special operators for our temporal records...
(defn record->segment
  "Convert a map into a vector of [start duration]"
  [r] [(:start r) (+ (:start r) (:duration r))])

(defn segment->record
  "Convers a vector of [start end] into a record of {start duration}."
  [seg]
  {:start (first seg) :duration (- (second seg) (first seg))})

;;Functions for dealing with abstract 1D line segments, as represented by vector
;;pairs, where each entry in the vector is a point on a shared axis.

(defn overlap?
  "Determine if two segments, or 2 coordinate pairs, overlap."
  ([s1 e1 s2 e2] (and (<= s1 e2) (>= e1 s2)))
  ([seg1 seg2] (overlap? (first seg1) (second seg1) 
                         (first seg2) (second seg2))))
(defn segment-distance
  "Compute the distance between two segments, where overlapping segments have
   0 distance, and non-overlapping segments have distance equal to the minimum
   space between exterior points."
  [[s1 e1] [s2 e2]] 
  (if (overlap? s1 e1 s2 e2)   0
    (if (< s1 s2)
      (- s2 e1)
      (- s1 e2))))

(defn segment-encloses?
  "Returns true if segment1 encloses segment2, which means that segment2 lies 
   entirely inside of segment1, end-points inclusive."
  [[s1 e1] [s2 e2]]
  (and (<= s1 s2) (>= e1 e2)))

(defn segment-intersects?
  "Predicate to determine if segment s is defined at t."
  [s t] (segment-encloses? s [t t]))

(defn clip-segment
  "Attempts to project target-segment onto base-segment, were regions of 
   target-segment outside of base-segment are truncated, or clipped, to the 
   bounds of the base-segment."
  [base-segment target-segment]
  (cond (segment-encloses? base-segment target-segment) target-segment
        (overlap? base-segment target-segment) 
          (let [[s1 e1] base-segment 
                [s2 e2] target-segment]
            [(min e1 (max s2 s1))
             (max s1 (min e1 e2))])
        :else nil))

;;Operations on Temporal Records
;;==============================

;;For records that contain a :start and :duration field, we can view them as 
;;line segments on the temporal axis.
;;__TODO__ Move these operations to spork.util.temporal

(defn record-distance [r1 r2] 
  (apply segment-distance (map record->segment) [r1 r2]))

(defn stretch-to [rec1 rec2]
  (assoc rec1 :duration (- (:start rec2) (:start rec1))))

(defn lengthen [length rec]
  (assoc rec :duration (+ (:duration rec) length)))

(defn scale [alpha rec]
  (assoc rec :duration (* (:duration rec) alpha)))

(defn group [group-key rs] 
  (map #(assoc % :group group-key) rs)) 

(defn end-time [r]
  (+ (:start r) (:duration r)))

(defn unbounded-segment? [s] 
  (= s [Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY]))

(defn record-segment? [r]
  (and (map? r) 
       (contains? r :start)
       (contains? r :duration)))

(defn translate
  "Shifts record along its temporal axis by delta."
  [delta rec]
  (assoc rec :start (- (:start rec) delta)))

(defn move-before
  "Translates record1 to occur directly before record2.  Record1 will not 
   overlap record2."
  ([rec1 rec2 separation]
    (assoc rec1 :start (- (:start rec2) (+ separation (:duration rec1)))))
  ([rec1 rec2]
    (move-before rec1 rec2 1)))

(defn lead
  "Translates record1 to end directly on the start of record2, so that record1
   overlaps record2 by 1 unit of time.  Treats record 2 as fixed, and returns 
   transformed version of record1.  Record1 is the 'precursor to record2."
  [rec1 rec2]
  (move-before rec1 rec2 0))
  
(defn move-after
  "Translates record2 to occur directly after record1.  Record2 will not overlap
   record1."
  ([rec1 rec2 separation] 
    (assoc rec2 :start (+ separation (:start rec1) (:duration rec1))))
  ([rec1 rec2] (move-after rec1 rec2 1)))

(defn follow 
  "Translates record2 to start directly on the end of record1, so that record2
   overlaps record1 by 1 unit of time.  Record2 is the 'successor to record2.
   Identical to precursor in function, except record1 is treated as fixed, 
   while record2 moves.  Returns a shifted value for record2.  Useful for 
   constructing 'trailing sets of events that must overlap by a single unit of 
   time."
  [rec1 rec2]
  (move-after rec1 rec2 0))

(defn subsume
  "Translates and scales record 1 to accomodate the time segment of both record
   one and record2."
  [rec1 rec2]
  (let [tstart (min (map :start [rec1 rec2]))
        tfinal (max (map end-time [rec1 rec2]))]
    (merge {:start tstart :duration (- tfinal tstart)}
           rec1)))

(defn split-records-with
  "Splits any instances of records in xs, which intersect time t, into 2 or more
   new records by applying split-func to the intersecting records.  Returns the 
   split records concatenated with the orginal, un-split records."
  [t split-func xs]
  (reduce (fn [acc x] (if (segment-intersects? (record->segment x) t)
                        (reduce conj acc (split-func t x))
                        (conj acc x))) [] xs))

(defn split-record
  "Splits record at time t returning a pair of records, [l r], where l ends just 
   before t, and r begins on t.  Caller may supply a distance parameter to 
   determine the distance from t."
  ([t sep record] (let [seprec {:start t :duration sep}
                        post (- (end-time record)
                                (end-time seprec))]
                    [(stretch-to record seprec) 
                     (lengthen (* -1 post) (follow seprec record))]))
  ([t record] (split-record t 1 record)))

(defn split-records
  "Default record-splitting function.  Uses split record, with an optional 
   separation time uniformly applied around the split time t, to bifurcate 
   any records in xs that are defined over time t."
  [t xs] (split-records-with t split-record xs)) 

(defn merge-stochastic
  "Given a map m, creates a merge function that will associate values from 
   m into another map, record, with the possibility of evaluating any values 
   in m that are no-argument functions (assumably with side-effects). Intended
   to support drawing record fields from distributions of values."
  [m]
  (fn [record] 
    (reduce conj record (for [[k func-val] m] 
                          [k (if (fn? func-val) 
                               (func-val)
                               func-val)]))))

(defn following-recs [rs]
  (reduce (fn [acc x] 
            (conj acc (follow (last acc) x))) [(first rs)] (rest rs)))

(defn preceding-recs [rs]
  (reduce (fn [acc x] 
            (conj acc (lead x (last acc)))) [(first rs)] (rest rs)))

;;Currently on hold.
;(defn subsume-by
;;  "Allows a user-supplied function to determine the order in which a set of 
;;   records should be merged.  f should be a function that returns a value 
;;   amenable to comparison, using clojure.core/compare."
;;  [f xs])



;;Towards a Language for Describing Sampling Rules
;;================================================

;;We define a little language for building abstract syntax trees that correspond
;;to a computation that...for now....produces a vector of records.

;;The idea is to allow users to build an abstract syntax tree using this 
;;language, which describes a potentially highly complex rule for sampling 
;;from a corpus of records.  Rather than use macros, I implemented specific 
;;data types to provide a "node" for each bit of the language, and provide 
;;a set of combinators that allow us to compose these nodes into a directed 
;;acyclic graph that represents the abstract syntax tree of the rule.  

;;This approach is used throughout computer science, to provide a flexible, 
;;data-driven way to define complex rules.  We gain quite a bit of leverage, 
;;in that we can compose arbitrarily complex rules from simple rules, and 
;;that we can extend the language as-needed by added new primitives and 
;;means of combination.  

;;While adding a layer of complexity (due to the seperate node structures), the 
;;topology for a ruleset is independent of any corpus of samples.  As a result,
;;the composite rule is just another chunk of data, which can be applied in the 
;;context of a sampling evaluation, against ANY corpus of samples.

;;Additionally, since the sampling rule is actually a structure, it is easy to 
;;visualize and debug (unlike a compiled function).  We can even pipe it into 
;;an interactive graph-based program and allow users to visually define their 
;;own sampling rules, or, as-is currently the practice, to define rules using 
;;standard clojure data structures like vectors and maps.  

;;Later, to actually use the sampling rule to derive a sample from some data, 
;;we will define a function that evaluates the sampling rule __in the context__
;;of the corpus of samples.  The effect is that the rule structure is traversed
;;from some root node (possibly specified by the caller), and the leaves of the
;;traversal result in a sampling action.  The sampling action at a particular 
;;leaf node is declaratively defined by the "path" of nodes leading up to 
;;the leaf node.  So we have a simple tree-walker that visits the leaf nodes, 
;;applies the rule to the corpus, and returns all of the samples.

;;__Note__ After reconsidering the initial implementation, I may ditch the 
;;separate node structures in favor of just using lists and macros (more of a 
;;Lispy style, and more elegant).  To be determined...


;;Operators
;;=========

(defn chain
  "chain - takes a node list and creates a node that produces a list of nodes 
  that are chained together so that the end of one node is 'before the next.  
  Returns the concatenated, chained nodes, where chaining is defined by f.
  Default behavior ensures that any nodes in the list follow each other so that 
  their start and end time (+ start duration) overlap."
  ([chain-func nodes]
    (fn [ctx] 
      (chain-func (map (fn [nd] (nd ctx)) nodes))))
  ([nodes] (chain following-recs nodes)))

(defn choice
  "choice - takes a node list, and creates a node that will randomly return 
   one of the nodes in the list.  Probability of selecting a node is even, 
   unless an alternate probabilities are provided."
  ([sample-func nodes]
    (fn [ctx] ((sample-func nodes) ctx)))
  ([nodes] (choice sample-nth nodes)))

;(defn viable-weights? [pdf-map]
;  (or (every? integer? (vals pdf-map))
;      (and (every? float? (vals pdf-map)) 
;           (= 1.0 (float (reduce + (vals pdf-map))))))) 
        
;;Patched to use rand binding in stats.rand 

(defn weighted-choice
  "Identical to choice, except it takes a map of node->probability densities.
   Where the keys are resolvable nodes, and the densities are the probabilities
   from [0 1], that a node will be chosen.  Densities must sum to 1.0 to be
   valid."
  [pdf-map]
  (assert (= 1.0 (float (reduce + (vals pdf-map)))) 
          (str "Probability densities must sum to 1.0"))
  (let [nodes  (keys pdf-map) 
        choose (fn [] (loop [r (stats/*rand*)
                             xs nodes
                             ds (vals pdf-map)]
                        (cond (= (count xs) 1)  (first xs)
                              (<= r (first ds)) (first xs)
                              :else (recur (- r (first ds)) 
                                           (rest xs) (rest ds)))))]
    (fn [ctx] ((choose) ctx))))

(defn concatenate
  "Takes an arbitray number of nodes, applies them to a context, and 
   concatenates the result into a vector."
  [nodes]
  (fn [ctx] (reduce conj [] (map (fn [f] (f ctx)) nodes))))

(defn replications
  "replicate - takes an positive integer, n, and performs n traversals of the 
   children, implicitly concatenating the results." 
  [n nodes]
  (let [rep (fn [ctx f]  (reduce conj [] (map (fn [i] (f ctx)) (range n))))]
    (fn [ctx] 
      (vec (map (partial rep ctx) (if (sequential? nodes) nodes [nodes]))))))

(defn transform
  "given a function, applies the function to each child, returning the
   result.  basically map."
  [f node]
  (fn [ctx] (f (node ctx))))

(defn merge-record
  "Given a record of values, merges the record with each node, which assumably
   resolves to a record expression."
  [rec nodes]
  (transform #(merge % rec) nodes))


(defn truncate-record
  "Uses the boundary set by base-record to truncate target-record.
   If target record can be fit into the bounds of base, a modified target-record
   is returned.  Otherwise, nil."
  [base-record target-record]
  (assert (every? record-segment? [base-record target-record])
          (str "Every record must have :start and :duration"))               
  (let [bounds (record->segment base-record)
        target (record->segment target-record)]
    (if-let [new-bounds (clip-segment bounds target)]
      (merge target-record  (segment->record  new-bounds))
      nil)))

(defn with-constraints
  "Uses a pre-baked set of constraints, as provided by constraint-map, to filter
   the result of any generated nodes.  I'd like to change this, but for now 
   it'll be somewhat predefined constraints."
  [{:keys [tstart tfinal duration-max seed] :as constraint-map} nodes]
  (let [tstart       (or tstart 0)
        tfinal       (or tfinal Double/POSITIVE_INFINITY)
        duration-max (or duration-max Double/POSITIVE_INFINITY)
        seed         (or seed (rand-int Integer/MAX_VALUE))
        global-bounds [tstart tfinal]
        constrain    (if (unbounded-segment?  global-bounds)
                       identity
                       #(map (partial truncate-record 
                                      (segment->record global-bounds)) %))]
    (fn [ctx]
      (->> (stats/with-seed seed (flatten (nodes ctx)))
           (constrain) 
           (filter (complement nil?))))))    

(defn merge-context
  "Merges the value of rec into the context, effectively altering the evaluation
   context."
  [rec nodes]
  (fn [ctx] (nodes (merge rec ctx)))) 

(defn sample-context
  "Given an evalution context, typically a map of {rule-name rule|value}.  
   Value may be another rule (a node), a rule-name (a keyword), or a primitive
   value.  In this case, primitive values are any other data type, but are 
   typically sequences of clojure map strucures (i.e. records) in practice."
  [ctx nodes] (nodes ctx))

(defn compose
  "Returns a sampler that samples n1 with a context, then samples n2 with the 
   resulting context of n1's sampling.  Akin to function composition."
  [n1 n2]
  (fn [ctx] (n2 (n1 ctx))))


;;Data Structures for Nodes
;;=========================
(defn node-type [nd] (get nd :node-type))
(defn node-data [nd] (get nd :node-data))

;;Node constructors.  We define different abstract nodes from a general node, 
;;which is just a clojure map with a type key.  Each node correponds to 
;;primitives or operators in our sampling language.  We add more features to 
;;the language by adding more nodes, and implementing their behavior when 
;;traversed.
(defn ->node  [type data] 
  {:node-type type :node-data data})
(defn ->chain [nodes]    
  (->node :chain {:children nodes}))
(defn ->replications [n nodes] 
  (->node :replications {:reps n :children nodes}))
(defn ->choice [nodes]    
  (->node :choice {:children nodes}))
(defn ->transform    
  [f nodes]  
  (->node :transform {:f  f :children nodes})) 
(defn ->concatenate  
  [nodes]    
  (->node :concatenate {:children nodes}))
(defn ->constrain    
  [constraints nodes] 
  (->node :constrain {:constraints constraints :children nodes}))

;;Evaluation Semantics for Sampling Rules
;;=======================================

;;To evaluate our syntax tree, we define a multimethod called __sample-node__ .
;;__sample-node__ is defined for each type of node in our language, and 
;;enforces the unique evaluation semantics of the node during traversal.

;;__Note:__ Much of this could be implemented using a custom lisp reader and 
;;evaluator, and would be more elegant.  There is some utility to retaining 
;;the classical "node" approach, in that we can see the node data and debug it
;;visually.  We could do the same with lists of symbols, and may yet.

(defmulti  sample-node
  "A generic method for traversing a sample-tree, collecting records at each 
   step."
  (fn [node ctx] (node-type node)))

(defn lift-children
  "Helper function to allow us to ensure that values we need to pass the 
   rendering context to are able to be evaluated.  Things that are keywords 
   or functions are fine already, where anything else - like a node - needs to 
   be lifted using node rendering.  The only reason for this is to allow 
   inlining node definitions anywhere in the sample graph, but it's useful 
   enough to justify the overhead."
  [xs] 
  (into [] (map (fn [x] (if (or (keyword? x) (fn? x)) x 
                          (fn [ctx] (sample-node x ctx)))) xs)))

(defn lift [x] (fn [ctx] (sample-node x ctx)))
(defn node? [x] (and (map? x) (contains? x :node-type)))

;;The default behavior for sampling a node is to determine if the node is 
;;a primitive atom (leaf/data),a singleton node (leaf), or  a composite node 
;;(branch).  Primitive elements, like numbers, strings, keywords, and symbols, 
;;are first looked up in the current context.  If the result is not a node, 
;;the data is returned, otherwise, the newly-fetched node is sampled 
;;recursively.  Sequences of nodes (composite nodes), are sampled sequentially.
(defmethod sample-node :default  [node ctx]
  (cond (sequential? node) (map #(sample-node % ctx) node) 
        (or (keyword? node) 
            (symbol? node) 
            (number? node) 
            (string? node)) 
          (let [res (get ctx node)]
            (if res (sample-node res ctx) ;continue sampling... 
              (or res (when ((complement node?) node) node))))
        :else (if-let [remaining (node-data node)]
                (sample-node remaining ctx)
                node)))

;;Each implementation of sample-node invokes the previously defined combinators
;;on its child or children, ensuring that the children are sampled or "lifted" 
;;as necessary for evaluation.  This is an area that feels a bit redundant, 
;;since we're just wrapping previously-defined functions from above.

(defmethod sample-node :leaf     [node ctx] (get ctx (node-data node)))
(defmethod sample-node :chain    [node ctx] ((chain (node-data node)) ctx))
(defmethod sample-node :choice   [node ctx]
  (let [data (:children (node-data node))]
    (if (map? data) 
      (let [rendered-map (zipmap (lift-children (keys data))
                                                  (vals data))]                                             
        (sample-node ((weighted-choice rendered-map) ctx) ctx))
      (sample-node ((choice (lift-children data)) ctx) ctx))))

(defmethod sample-node :transform [node ctx]
  (let [{:keys [f children]} (node-data node)
        func (if (map? f) 
               (merge-stochastic f)
               f)]
      ((transform func (lift children)) ctx)))

(defmethod sample-node :replications [node ctx]
  (let [{:keys [reps children]} (node-data node)]
    ((replications reps (lift-children children)) ctx)))

(defmethod sample-node :constrain [node ctx]
  (let [{:keys [constraints children]} (node-data node)]
    ((with-constraints constraints (lift children)) ctx)))

(defmethod sample-node :concatenate [node ctx]
  ((concatenate (node-data node)) ctx))

;;Sampling API
;;============

(defn sample-from
  "Convenience function for executing complex queries.  Useful for building 
   queries inline, using the ->> threading macro."
  [ctx query]
  (sample-node query ctx))


;;testing
(comment
  ;A population of records from which to sample.       
(def example-population-table
  [{:group "LandPrey" :start 1 :duration 1000 :SRC "Donkey"  :quantity 2}
   {:group "LandPrey" :start 1 :duration 1000 :SRC "Rabbit"  :quantity 5}
   {:group "LandPrey" :start 1 :duration 1000 :SRC "Mouse"   :quantity 20}
   {:group "AirPrey"  :start 1 :duration 150  :SRC "Sparrow" :quantity 10}
   {:group "AirPrey"  :start 1 :duration 150  :SRC "Finch"   :quantity 30}
   {:group "WaterPrey" :start 1 :duration 1000 :SRC "Minnow" :quantity 100}
   {:group "LandPred" :start 1 :duration 300  :SRC "Fox"     :quantity 1}
   {:group "LandPred" :start 1 :duration 300  :SRC "Wolf"    :quantity 5}
   {:group "AirPred"  :start 1 :duration 500  :SRC "Hawk"    :quantity 15}
   {:group "AirPred"  :start 1 :duration 500  :SRC "Eagle"   :quantity 7}
   {:group "WaterPred" :start 1 :duration 2000 :SRC "Shark"  :quantity 1}])
         
(def population-context 
  (->>  example-population-table
    (group-by (comp keyword clojure.string/lower-case :group))))
        

(def simple-sim (->replications 10 
                  [(->transform 
                     #(map (merge-stochastic 
                             {:start (stats/uniform-dist 0 20000)
                              :duration (stats/normal-dist 500 150)}) %)
                     (->choice (vec (keys population-context))))]))
;These are our primitive nodes....
;If we don't have a set of primitive nodes, we need a way to generate them.
(def p1 {:bar1 {:name "bar1" :start 10 :duration 1}
         :bar2 {:name "bar2" :start 11 :duration 10}
         :bar3 {:name "bar3" :start 21 :duration 5}
         :bill {:name "bill" :start 30 :duration 30}
         :cat  {:name "cat" :start 2  :duration 1}
         :qux  {:name "qux" :start 50 :duration 1000}})
;let's create a set of grouped nodes...

;Rules for composing primitive nodes, which in turn create new nodes.
(def p2 {:bar (->chain  [:bar1 :bar2 :bar3])
         :baz (->choice [:bill 
                         (->transform 
                           {:start    (stats/exponential-dist 10)
                            :duration (stats/exponential-dist 2000)}
                           :cat)])
         :foo (->transform  {:start (stats/normal-dist 10 1)}
                 (->choice  {:bar (/ 1 3)
                             :baz (/ 1 3)
                             :qux (/ 1 3)}))})

;Rules for composing previous nodes into a case node.
(def p3 {:case1 (->concatenate 
                  [(->replications 2  :foo)
                   (->replications 10 :qux)
                   (->replications 3  :baz)])})

;Rules for composing everything into a sample. 
(def p4 {:sample (->replications 3 [(->constrain {:tfinal 5000 
                                                  :duration-max 5000}
                                                 :case1)])})
;;We can compose p1..p4 into a database of rules just using clojure.core/merge
(def sample-graph (merge p1 p2 p3 p4))

(sample-from sample-graph :case1)

;(def ptest 
;  {:bill {:name "bill" :start 30 :duration 30}
;   :cat  {:name "cat" :start 2  :duration 1}   
;   :baz (->choice [:bill 
;                   (->transform 
;                     {:start    (stats/exponential-dist 10)
;                      :duration (stats/exponential-dist 2000)}
;                     :cat)])})

;nodes take the form {:node-type some-type :node-data  some-data}
(def sampler1 
  (->node :start 
    (->replications 3 :bar1)))
  
(def sampler2 
  (->node :start 
    (->replications 3
       (->choice  [:bar1
                   :bar2
                   :bar3]))))

(def nested-choices 
  (->choice  [(->choice [:qux :cat :bill])
              :bar2
              :bar3]))

(def weighted-choices
  (->choice  {:qux 0.25 
              :cat 0.25
              :bill 0.25
              (->choice [:bar1 :bar2 :bar3]) 0.25}))
(def sampler3 
  (->node :start 
    (->replications 3
       (->choice  [(->choice [:bill :cat :qux])
                   :bar2
                   :bar3]))))

;sample from a pdf...
(def simple-graph {:foo {:name "Foo!"}
                   :bar {:name "Bar!"}
                   :baz {:name "Baz!"}})

(def simple-nodes (keys simple-graph))

(def choices (weighted-choice {:foo 0.25 :bar 0.50 :baz 0.25}))
(def even-choices (choice (keys simple-graph)))
(def samples (frequencies (take 1000 (repeatedly #(choices simple-graph)))))
;=>{{:name "Bar!"} 514, {:name "Baz!"} 242, {:name "Foo!"} 244}
(def even-samples 
  (frequencies (take 1000 (repeatedly #(even-choices simple-graph)))))
;=>{{:name "Bar!"} 322, {:name "Baz!"} 347, {:name "Foo!"} 331}

(defn time-stamp [nodes] 
  (merge-record {:time (System/currentTimeMillis)} nodes))

(def timed-result (->> simple-graph 
                    ((add-time simple-nodes))))

(def multiple-results (->> (choice [:foo :bar])
                        (compose (time-stamp simple-nodes))
                        (replications 2)                                                                                
                        (sample-context simple-graph)))
                                            
)





