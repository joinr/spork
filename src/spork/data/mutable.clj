;;A set of idiomatic wrappers for mutable datastructures from
;;java.util.  These collections are treated like transients, allowing
;;for conj!. 
;; Work in progress.
(ns spork.data.mutable  
;  (:refer-clojure :exclude [conj persistent!])
  (:require [spork.protocols [core :as generic]])
  (:import  [java.util ArrayList PriorityQueue ArrayDeque HashMap]))

;;#Additional Reducers 
;;These haven't made it into clojure.core yet, they probably will in
;;1.7  .  I hacked together a couple of useful ones, like range.
(in-ns 'clojure.core.reducers)

;;we're going to add in iterate, range, and friends

(doseq [s    '[range
               iterate
               map-indexed]]
  (ns-unmap *ns* s))  

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

(in-ns 'spork.data.mutable)


(extend-type  java.util.HashMap 
  clojure.core.protocols/IKVReduce 
  (kv-reduce [amap f init] 
    (let [^java.util.Set es (.entrySet ^java.util.HashMap amap)
          ^java.util.Iterator it (.iterator es)]
      (loop [acc init]
        (if (reduced? acc) @acc
            (if (.hasNext it)
              (let [^java.util.Map$Entry e (.next it)]
                (recur (f acc (.getKey e) (.getValue e))))
              acc)))))
  clojure.core.protocols/CollReduce  
  (coll-reduce 
    ([coll f]     
       (let [^java.util.Set es (.entrySet ^java.util.HashMap coll)
             ^java.util.Iterator it (.iterator es)]
         (when (.hasNext it) 
           (loop [acc (.next it)]
             (if (reduced? acc) @acc
                 (if (.hasNext it)
                   (let [^java.util.Map$Entry e (.next it)]
                     (recur (f acc  e)))
                   acc))))))
    ([coll f val]
       (let [^java.util.Set es (.entrySet ^java.util.HashMap coll)
             ^java.util.Iterator it (.iterator es)]
         (loop [acc val]
           (if (reduced? acc) @acc
               (if (.hasNext it)
                 (let [^java.util.Map$Entry e (.next it)]
                   (recur (f acc  e)))
                 acc)))))))

;;Some notes on performance....
;;We can mimic field access by creating an interface that correponds
;;to the type.  The only difference is that the interface methods
;;cannot contain '-'.....so what's a good way around this? 

;;We have a mechanism to generate random interfaces for our 
;;types...specifically our mutable types...and from there 
;;we get native access speeds through the interface.

;;What we really want is a unified way to represent field access...
;;in clojure, this is typically .-the-field 
;;Well, we could have a macro called get-field....
;;that takes a type, sees if it's mutable, and exposes public fields.
;;Another option is to wrap the mutable in an object that has public
;;fields.  Then you get 2 allocations per object though...ugh...

;;There's always the good old with-slots form...
;;(with-slots [obj] [slot1 slot2 slot3] ...)
;;If a tag is provided, we can see if the type is a mutable.
;;If it is, we can replace the slot names with appropriate 
;;mutable alternatives at compile time.

(declare mutable? slot->field slot->accessor)

;;(defn slot->field [slot]   (symbol (str \. slot)))
;;given a form the-slot-at-blah, we need to turn it into camelcase
;;theSlotAtBlah
;; (defn slot->accessor [slot] 
;;   (let [xs (clojure.string/split (str slot) #"-")]
;;     (->> (map clojure.string/capitalize (rest xs))
;;          (into [(first xs)])
;;          (clojure.string/join)
;;          (symbol))))

;; (defn slot-binder [f] 
;;   (fn [slot] [slot (f slot)]))

;;This is a throwback to CommonLisp :)
;; (defmacro with-fields [obj slots & expr]
;;   (let [the-hint (:tag (meta obj))]
;;     `(let [~@(flatten (map (slot-binder (if (mutable? the-hint) 
;;                                           slot->slot-accessor slot->field))
;;                            slots))]
;;        ~@expr)))      

;; (let [hyphen #"-"]
;;   (defn invalid-field? [field]
;;     (re-find hyphen (str field))))

(defn ^ArrayList make-array-list [] (ArrayList.))
(defn ^ArrayList array-list [xs] 
  (reduce (fn [^ArrayList acc x] (doto acc (.add x))) (make-array-list) xs))

(defmacro hget [hint coll k]
  `(.get ~(with-meta coll {:tag hint})  ~k))
(defmacro hput [hint coll k v]
  `(doto ~(with-meta coll {:tag hint})  (.put ~k ~v)))
(defmacro hadd [hint coll v]
  `(doto ~(with-meta coll {:tag hint}) (.add ~v)))

;;Unifies mutable abstractions...
;;Protocol dispatch cost is not terrible.
(defprotocol IFastAccess
  (fast-get  [coll k])
  (fast-add  [coll v])
  (fast-drop [coll k]))
(defprotocol IFastHashAccess
  (fast-put  [coll k v]))

(defprotocol IMutable
  (mutable   [c])
  (immutable [c]))

;;Debatable as to how much faster this is...
(in-ns 'clojure.core)

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined. *Modified to account for mutable structures." 
  {:added "1.0"
   :static true}
  [to from]
  (if (instance? clojure.lang.IEditableCollection to)
    (with-meta (persistent! (reduce conj! (transient to) from)) (meta to))
    (if (instance? spork.data.mutable.IMutable to)
      (reduce conj!(spork.data.mutable/mutable to) from)
      (reduce conj  to from))))
(in-ns 'spork.data.mutable)

(extend-type ArrayList 
  IFastAccess 
  (fast-get [coll k]   (hget ArrayList coll k))
  (fast-add [coll v]   (hadd ArrayList coll v))
  (fast-drop [coll k]  (doto ^java.util.ArrayList coll (.remove k)))
  IMutable
  (mutable [m] m)
  (immutable [m] (into [] m)))

(extend-type   HashMap 
  IFastAccess
  (fast-get [coll k]   (hget HashMap coll k))
  (fast-add [coll v]   (hput HashMap coll (first v) (second v)))
  (fast-drop [coll k]  (doto ^java.util.HashMap coll (.remove k)))
  IFastHashAccess
  (fast-put [coll k v] (hput HashMap coll k v))
  IMutable
  (mutable   [m] m)
  (immutable [m] (into {} m)))  

(definline get-arraylist [l n]
  `(.get ~(with-meta l {:tag 'java.util.ArrayList}) ~n))
(definline push-arraylist [l x]
  `(doto  ~(with-meta l {:tag 'java.util.ArrayList}) (.add ~x)))

(definline jassoc [m k v]
  `(doto ~(with-meta m {:tag 'java.util.HashMap})
     (.put ~k ~v)))

(defn ^ArrayDeque make-queue [] (ArrayDeque.))
(defn ^ArrayDeque queue [xs] 
  (reduce (fn [^ArrayDeque acc x] (doto acc (.add x))) (make-queue) xs))

(definline mutable-list? [coll]  `(= (type ~coll) java.util.ArrayList))

(defn entry-comparer [l r] 
  (let [pl (generic/entry-priority l)
        pr (generic/entry-priority r)]
    (cond (< pl pr) -1 
          (> pl pr) 1
          :else 0)))   

(defn ^ArrayList  add-list   [^ArrayList l obj]  (doto l  (.add obj)))
(defn ^ArrayDeque add-q      [^ArrayDeque q obj]  (doto q (.add obj)))

(defn ^PriorityQueue make-pq [] (PriorityQueue. 11 entry-comparer))
(defn ^PriorityQueue pq [xs] 
  (reduce (fn [^PriorityQueue acc x]   
            (doto acc (.add x))) (make-pq) xs))
            
(defn ^PriorityQueue add-pq  [^PriorityQueue q obj]  (doto q (.add obj)))
(defn ^PriorityQueue pop-pq  [^PriorityQueue q    ]  (do (.poll q) q))


(defn flatten-bindings [xs]
  (reduce (fn [acc [k v]] 
            (-> acc (conj k) (conj v))) [] xs))

(defn field-setter [fld hints v]
  (let [prims     (set '[long int boolean double char float object])       
        hint      (get hints fld)
        f         (symbol (str fld))]
       (cond (and hint (contains? prims hint))
             `(set! ~f (~hint ~v))
             hint 
             `(set! ~f ~(with-meta v {:tag hint}))
             :else
             `(set! ~f ~v))))

;;Tag protocol
(defprotocol IMutableContainer)


;;Defines a mutable container.  Ueser may supply their own
;;implementations of persistent, deref, conj, and without in the
;;specs.
(defmacro defmutable [name fields & specs]
  (let [flds        (mapv (fn [sym] (vary-meta (symbol sym) merge {:unsynchronized-mutable true})) fields)
        fld-hints   (zipmap flds (map (comp :tag meta) flds))
        keyfields   (mapv keyword fields)
        field-symbs (map (fn [sym] (vary-meta sym dissoc :tag)) fields)
        the-value   (gensym "the-value")
        setters     (flatten-bindings (map (fn [s] [(keyword s) (field-setter s fld-hints the-value)]) flds))
        getters     (flatten-bindings (map (fn [s] [(keyword s) s]) field-symbs))
        fieldmap    (zipmap keyfields field-symbs)
        user-supplied  (reduce (fn [acc symb]
                                 (if (coll? symb)
                                   (case (str (first symb)) 
                                     "persistent" (conj acc :persistent)
                                     "deref"      (conj acc :deref)
                                     "conj"       (conj acc :conj)
                                     "without"    (conj acc :without)
                                     acc)
                                   acc))  #{} specs)]
    `(deftype ~name ~flds 
         ~@specs
         spork.data.mutable.IMutableContainer
         clojure.lang.ITransientMap  
         (~'valAt [this# k#] 
           (case k# 
             ~@getters
             (throw (Error. (str "Invalid field: " k#)))))
         (~'valAt [this# k# not-found#] 
           (case k# 
             ~@getters
             (throw (Error. (str "Invalid field: " k#)))))
         (~'assoc [this# k# ~the-value]
           (do (case k#
                 ~@setters
                 (throw (Error. (str "Invalid field: " k#))))                                      
               this#))  
         ~@(when (not (contains? user-supplied :conj))
               `((~'conj [this# e#]    
                         (let [[k# v#] e#]      
                           (.assoc this# k# v#)))))  
         ~@(when (not (contains? user-supplied :without))
               `((~'without [this# k#]    
                    (throw (Error. (str "Cannot dissoc from a mutable container: " k#))))))       
         ~@(when (not (contains? user-supplied :persistent))
               `((~'persistent [this#]    
                               ~fieldmap)))
         ~@(when (not (contains? user-supplied :deref))
               `(clojure.lang.IDeref                 
                 (~'deref [this#] ~fieldmap))))))

(deftype mutlist [^java.util.ArrayList m] 
  clojure.lang.ITransientVector  
  (assocN [this i val] (do (.add m i val) this))
  (pop    [this]       (do (.remove m (unchecked-dec (.size m))) this))
  (nth    [this k]     (.get m k))
  (nth    [this k not-found] (or (.get m k) not-found))
  (assoc  [this k v]   (do  (.add m k v) this))
  (conj    [this v]    (do (.add m v) this))
  (persistent [this]   (into [] m))
  clojure.lang.Seqable
  (seq [this] (seq m))
  clojure.lang.Counted 
  (count [coll] (.size m))
  IFastAccess 
  (fast-get [coll k]   (.get m k))
  (fast-add [coll v]   (do (.add  m v) coll))
  (fast-drop [coll k]  (do  (.remove m k) coll))
  IMutable
  (mutable   [this] this)
  (immutable [this] (into [] m))
  clojure.lang.IDeref
  (deref [this] m))

(deftype mutmap [^java.util.HashMap m] 
  clojure.lang.ITransientMap  
  (valAt [this k]           (.get m k))
  (valAt [this k not-found] (or (.get m k) not-found))
  (assoc [this k v] (do  (.put m k v) this))
  (conj  [this e]    
    (let [[k v] e]      
      (do (.put m k v) this)))
  (without [this k]   (do (.remove m k) this))
  (persistent [this] (into {}  m))
  clojure.lang.Seqable
  (seq [this] (seq m))
  clojure.lang.Counted 
  (count [coll] (.size m))
  IFastAccess 
  (fast-get  [coll k]  (.get m k))
  (fast-add  [coll v]  (do  (.put  m (first v) (second v)) coll))
  (fast-drop [coll k]  (do  (.remove m k) coll))
  IFastHashAccess
  (fast-put [coll k v] (do (.put m k v) coll))
  IMutable
  (mutable [this] this)
  (immutable [this] (into {} m))
  clojure.lang.IDeref
  (deref [this] m))

(defn ^mutmap ->mutmap [& xs]  
  (mutmap.  (reduce (fn [m [k v]] (fast-put m k v)) (java.util.HashMap.)  xs)))

(defn ^mutlist ->mutlist [& xs]  
  (mutlist. (reduce (fn [^java.util.ArrayList m k] (doto m (.add k)))  (java.util.ArrayList.)  xs)))

(extend-protocol IMutable
  clojure.lang.PersistentVector
  (mutable [v]   (mutlist. (reduce fast-add (java.util.ArrayList.) v)))
  (immutable [v] v)
  clojure.lang.PersistentList$EmptyList
  (mutable [v]   (mutlist. (java.util.ArrayList.)))
  (immutable [v] v)
  clojure.lang.PersistentList
  (mutable [v]    (mutlist. (reduce fast-add (java.util.ArrayList.) v)))
  (immutable [v]   v)
  clojure.lang.PersistentArrayMap
  (mutable [v]   (mutmap. (reduce-kv fast-put (java.util.HashMap.) v)))
  (immutable [v] v)
  clojure.lang.PersistentHashMap
  (mutable   [v] (mutmap. (reduce-kv fast-put (java.util.HashMap.) v)))
  (immutable [v] v))


;; (defmacro defarralias [name hint fields]
;;   (let [flds        (mapv (fn [sym] (vary-meta (symbol sym) merge {:tag hint})) fields)
;;         fld-hints   (zipmap flds (map (comp :tag meta) flds))
;;         hints       '{long longs int ints double doubles float floats char object}
;;         keyfields   (mapv keyword fields)
;;         field-symbs (map (fn [sym] (vary-meta sym dissoc :tag)) fields)
;;         the-value   (gensym "the-value")
;;         setters     (flatten-bindings (map-indexed (fn [i s] [(list (keyword s) i) (field-setter s fld-hints the-value)]) flds))
;;         getters     (flatten-bindings (map-indexed (fn [i s] [(list (keyword s) i) s]) field-symbs))
;;         arrsymb     (with-meta 'backing-array {:tag hint})
;;         ctor        (symbol (str "->" name))
;;         fieldmap    (zipmap keyfields field-symbs)]
;;    `(do
;;       (declare ~ctor)
;;       (deftype ~name [~arrsymb]
;;         ~@specs
;;         clojure.lang.ITransientMap  
;;         (~'valAt [this# k#] 
;;           (case k# 
;;             ~@getters
;;             (throw (Error. (str "Invalid field: " k#)))))
;;         (~'valAt [this# k# not-found#] 
;;           (case k# 
;;             ~@getters
;;             (throw (Error. (str "Invalid field: " k#)))))
;;         (~'assoc [this# k# ~the-value]
;;           (do (case k#
;;                 ~@setters
;;                 (throw (Error. (str "Invalid field: " k#))))                                      
;;               this#))  
;;         (~'conj [this# e#]    
;;           (let [[k# v#] e#]      
;;             (.assoc this# k# v#)))  
;;         (~'without [this# k#]    
;;           (throw (Error. (str "Cannot dissoc from a mutable container: " k#))))
;;         (~'persistent [this#] ~fieldmap)
;;         clojure.lang.IDeref
;;         (~'deref [this#] ~arrsymb))
;;       (defn ~(with-meta ctr {:tag name}) 
;;         [~@(map (fn [s] (with-meta  
      


  ;; ArrayDeque
  ;; (conj [o v] (add-q o v))
  ;; (persistent [o] (into [] (iterator-seq o)))
  ;; PriorityQueue
  ;; (conj [o v] (add-pq o v))
  ;; (persistent [o] (into [] (map first (iterator-seq o))))

