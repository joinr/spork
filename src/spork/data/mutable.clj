;;A set of idiomatic wrappers for mutable datastructures from
;;java.util.  These collections are treated like transients, allowing
;;for conj!. 
;; Work in progress.
(ns spork.data.mutable  
;  (:refer-clojure :exclude [conj persistent!])
  (:require [spork.protocols [core :as generic]])
  (:import  [java.util ArrayList PriorityQueue ArrayDeque HashMap]))

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
;;Tag protocol
(defprotocol IMutableContainer)
(defprotocol IMutable
  (mutable [ob])
  (immutable [obj]))

(declare mutable? slot->field slot->accessor)

(defn ^ArrayList make-array-list [] (ArrayList.))
(defn ^ArrayList array-list [xs] 
  (reduce (fn [^ArrayList acc x] (doto acc (.add x))) (make-array-list) xs))


(defmacro as [type obj]
  (let [typed (with-meta (gensym "obj") {:tag type})]
    `(let [~typed ~obj]
       ~typed)))

(extend-protocol IMutable  
  ArrayList
  (mutable [m] m)
  (immutable [m] (into [] m))
  HashMap 
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

(defn ^ArrayList  add-list   [^ArrayList l  obj]   (doto l  (.add obj)))
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
  clojure.lang.IPersistentVector  
  (assocN [this i val] (do (.add m (int i) val) this))
  (pop    [this]       (do (.remove m (unchecked-dec (.size m))) this))
  (nth    [this k]     (.get m k))
  (nth    [this k not-found] (or (.get m k) not-found))
  (assoc  [this k v]   (do  (.add m (int k) v) this))
  (cons [this k] (do (.add m k) this))
  (empty [this] (mutlist. (java.util.ArrayList.)))
; (conj    [this v]    (do (.add m v) this))
; (persistent [this]   (into [] m))
  clojure.lang.Seqable
  (seq [this] (seq m))
  clojure.lang.Counted 
  (count [coll] (.size m))
  IMutable
  (mutable   [this] this)
  (immutable [this] (into [] m))
  java.util.Collection
  (add [this e] (do (.add m e) this))
  (addAll [this c] (do (.addAll m c) this))
  (clear [this] (do (.clear m) this))
  (contains [this o] (.contains m o))
  (containsAll [this c] (.containsAll m c))
  (equals [this o] (.equals m o))
  (hashCode [this] (.hashCode m))
  (isEmpty [this] (.isEmpty m))
  (iterator [this] (.iterator m))
  (remove [this o] (do (.remove m o) this))
  (removeAll [this c] (do (.removeAll m c) this))
  (retainAll [this c] (do (.retainAll m c) this))
  (size [this] (.size m))
  (^objects toArray [this] (.toArray m))
  (^objects toArray [this ^objects arr]  (.toArray m arr))
  )

(defprotocol IUpdateable
  (update- [o k f]))

(definline update! [m k f]
  (let [m (with-meta m {:tag 'spork.data.mutable.IUpdateable})]
    `(.update- ~m ~k ~f)))

(definline tupdate [m k f]
  (let [m (with-meta m {:tag 'clojure.lang.ITransientAssociative})
        ml (with-meta m {:tag 'clojure.lang.ILookup})]
    `(.assoc ~m ~k (~f (.valAt ~ml ~k)))))

(defmacro entry? [e]  `(instance? java.util.Map$Entry ~e))
(deftype mutmap [^java.util.HashMap m]
  IUpdateable
  (update- [o k f]
    (do (.put m k (f (.get m k)))
        o))
  clojure.lang.IPersistentMap
  (valAt [this k]           (.get m k))
  (valAt [this k not-found] (or (.get m k) not-found))
  (assoc [this k v] (do  (.put m k v) this))
  (cons  [this e]
    (if (entry? e)
      (let [^java.util.Map$Entry e e]
        (do (.put m (.getKey e) (.getValue e)) this))
      (let [[k v] e]      
        (do (.put m k v) this))))
  (without [this k]   (do (.remove m k) this))
  (entryAt [this k]   (when-let [v (.get m k)]
                        (clojure.lang.MapEntry. k v)))
  clojure.lang.Seqable
  (seq [this] (seq m))
  clojure.lang.Counted 
  (count [coll] (.size m))
  IMutable
  (mutable [this] this)
  (immutable [this] (into {} m))
  java.util.Map
  (put    [this k v] (do (.put m k v) this))
  (putAll [this c] (do (.putAll m c) this))
  (clear  [this] (do (.clear m) this))
  (containsKey   [this o] (.containsValue m o))
  (containsValue [this o] (.containsValue m o))
  (entrySet [this] (.entrySet m))
  (keySet   [this] (.keySet m))
  (get [this k] (.get m k))
  (equals [this o] (.equals m o))
  (hashCode [this] (.hashCode m))
  (isEmpty [this] (.isEmpty m))
  (remove [this o] (do (.remove m o) this))
  (values [this] (.values m))
  (size [this] (.size m))
  clojure.core.protocols/IKVReduce 
  (kv-reduce [amap f init] 
    (let [^java.util.Set es (.entrySet ^java.util.HashMap m)
          ^java.util.Iterator it (.iterator es)]
      (loop [acc init]
        (if (reduced? acc) @acc
            (if (.hasNext it)
              (let [^java.util.Map$Entry e (.next it)]
                (recur (f acc (.getKey e) (.getValue e))))
              acc)))))
  clojure.core.protocols/CollReduce  
  (coll-reduce [coll f]     
       (let [^java.util.Set es (.entrySet ^java.util.HashMap m)
             ^java.util.Iterator it (.iterator es)]
         (when (.hasNext it) 
           (loop [acc (.next it)]
             (if (reduced? acc) @acc
                 (if (.hasNext it)
                   (let [^java.util.Map$Entry e (.next it)]
                     (recur (f acc  e)))
                   acc))))))
  (coll-reduce [coll f val]
       (let [^java.util.Set es (.entrySet ^java.util.HashMap m)
             ^java.util.Iterator it (.iterator es)]
         (loop [acc val]
           (if (reduced? acc) @acc
               (if (.hasNext it)
                 (let [^java.util.Map$Entry e (.next it)]
                   (recur (f acc  e)))
                 acc))))))


(deftype txmutmap [^:unsynchronized-mutable ^long tx ^java.util.HashMap m
                   ^:unsynchronized-mutable ^int _hash]
  IUpdateable
  (update- [o k f]
    (do (.put m k (f (.get m k)))
        o))
  clojure.lang.IPersistentMap
  (valAt [this k]           (.get m k))
  (valAt [this k not-found] (or (.get m k) not-found))
  (assoc [this k v] (do  (.put m k v) this))
  (cons  [this e]
    (if (entry? e)
      (let [^java.util.Map$Entry e e]
        (do (.put m (.getKey e) (.getValue e)) this))
      (let [[k v] e]      
        (do (.put m k v) this))))
  (without [this k]   (do (.remove m k) this))
  clojure.lang.Seqable
  (seq [this] (seq m))
  clojure.lang.Counted 
  (count [coll] (.size m))
  IMutable
  (mutable [this] this)
  (immutable [this] (into {} m))
  java.util.Map
  (put    [this k v] (do (set! tx (unchecked-inc tx))
                         (.put m k v) this))
  (putAll [this c] (do (set! tx (unchecked-inc tx))
                       (.putAll m c) this))
  (clear  [this] (do (set! tx 0)
                     (.clear m) this))
  (containsKey   [this o] (.containsValue m o))
  (containsValue [this o] (.containsValue m o))
  (entrySet [this] (.entrySet m))
  (keySet   [this] (.keySet m))
  (get [this k] (.get m k))
  (equals [this o] (.equals m o))
  (hashCode [this] (.hashCode m))
  (isEmpty [this] (.isEmpty m))
  (remove [this o] (do (set! tx (unchecked-inc tx))
                       (.remove m o) this))
  (values [this] (.values m))
  (size [this] (.size m)))

;; (deftype mutsortedmap [^java.util.TreeMap m]
;;   IUpdateable
;;   (update- [o k f]
;;     (do (.put m k (f (.get m k)))
;;         o))
;;   clojure.lang.IPersistentMap
;;   (valAt [this k]           (.get m k))
;;   (valAt [this k not-found] (or (.get m k) not-found))
;;   (assoc [this k v] (do  (.put m k v) this))
;;   (cons  [this e]
;;     (if (entry? e)
;;       (let [^java.util.Map$Entry e e]
;;         (do (.put m (.getKey e) (.getValue e)) this))
;;       (let [[k v] e]      
;;         (do (.put m k v) this))))
;;   (without [this k]   (do (.remove m k) this))
;;   clojure.lang.Sorted
  
;;   clojure.lang.Seqable
;;   (seq [this] (seq m))
;;   clojure.lang.Counted 
;;   (count [coll] (.size m))
;;   IMutable
;;   (mutable [this] this)
;;   (immutable [this] (into {} m))
;;   java.util.SortedMap
;;   (put    [this k v] (do (.put m k v) this))
;;   (putAll [this c] (do (.putAll m c) this))
;;   (clear  [this] (do (.clear m) this))
;;   (containsKey   [this o] (.containsValue m o))
;;   (containsValue [this o] (.containsValue m o))
;;   (entrySet [this] (.entrySet m))
;;   (keySet   [this] (.keySet m))
;;   (get [this k] (.get m k))
;;   (equals [this o] (.equals m o))
;;   (hashCode [this] (.hashCode m))
;;   (isEmpty [this] (.isEmpty m))
;;   (remove [this o] (do (.remove m o) this))
;;   (values [this] (.values m))
;;   (size [this] (.size m)))

(defmacro  indexOf
  ([arr k off]
   (let [xs  (with-meta (gensym "Arr")   {:tag 'objects})]
     `(let [~xs     ~arr
            bound#  (alength ~xs)]
        (loop [idx# ~off]
          (if (>= idx# bound#) -1
              (if (identical? (aget ~xs idx#) ~k) idx#
                  (recur (unchecked-inc (unchecked-inc idx#)))              
                  ))))))  
  ([arr k] `(indexOf ~arr ~k 0)))


(deftype mutarrmap [^:long ^:unsynchronized-mutable cnt ^objects xs]  
  IUpdateable
  (update- [o k f]
    (let [bound  (unchecked-multiply cnt 2)]
        (loop [idx 0]
          (if (== idx bound) nil
              (if (identical? (aget xs idx) k)
                (let [vidx (unchecked-inc idx)]
                  (do (aset xs vidx
                            (f (aget xs vidx)))
                      o))
                  (recur (unchecked-add idx 2))              
                  )))))
  clojure.lang.IPersistentMap
  (valAt [this k]
    (let [bound  (unchecked-multiply cnt 2)]
        (loop [idx 0]
          (if (== idx bound) nil
              (if (identical? (aget xs idx) k) (aget xs (unchecked-inc idx))
                  (recur (unchecked-add idx 2))              
                  )))))
  (valAt [this k not-found]
    (let [bound  (unchecked-multiply cnt 2)]
      (loop [idx 0]
        (if (== idx bound) not-found
            (if (identical? (aget xs idx) k) (aget xs (unchecked-inc idx))
                (recur (unchecked-add idx 2))              
                  )))))
  (assoc [this k v]
    (let [idx (indexOf xs k)]
      (if (not (neg? idx))
        (do (aset xs (unchecked-inc idx) v)
            this)
        (if (< cnt 16)
          (let [idx (* cnt 2)
                _ (aset xs idx k)
                _ (aset xs (unchecked-inc idx) v)
                _ (set! cnt (unchecked-inc cnt))]
              this)
          (reduce (fn [m idx]
                    (let [idx (* idx 2)
                          ]
                      (assoc m 
                             (aget xs idx)
                             (aget xs (unchecked-inc idx)))))
                  (mutmap. (java.util.HashMap.)) (range 16))))))            
  (cons  [this e]
    (if (entry? e)
      (let [^java.util.Map$Entry e e]
        (.assoc this  (.getKey e) (.getValue e)))
      (let [[k v] e]      
        (.assoc this k v))))
  (without [this k]
    (if (pos? cnt)
      (let [idx (indexOf xs k)]
        (if (not (neg? idx))
          (do (aset xs idx nil)
              (aset xs (unchecked-inc idx) nil)
              (set! cnt (unchecked-dec cnt))
              this)
          this))
      this))
  clojure.lang.Seqable
  (seq [this]
    (map (fn [idx] (let [idx (* idx 2)]
                     (clojure.lang.MapEntry.
                      (aget xs idx)
                      (aget xs (unchecked-inc idx)))))
         (range cnt)))
  clojure.lang.Counted 
  (count [coll] cnt)
  IMutable
  (mutable [this]    this)
  (immutable [this] (into {} (seq this)))
  java.util.Map
  (put    [this k v] (.assoc this k v))
  (putAll [this c] (throw (Exception. "op not supported yet")))
  (clear  [this] (dotimes [i cnt]
                   (do (aset xs i nil)
                       (aset xs (unchecked-inc i) nil))
                   this))
  (containsKey   [this o] (pos? (indexOf xs o)))
  (containsValue [this o] (pos? (indexOf xs o 1)))
  (entrySet [this]  (seq this))
  (keySet   [this] (map key (seq this)))
  (get [this k] (.valAt this  k))
  (equals [this o] (.equals xs o))
  (hashCode [this] (.hashCode xs))
  (isEmpty [this]  (zero? cnt))
  (remove [this o] (.without this o))
  (values [this] (map val (seq this)))
  (size [this] cnt))

(defn ^mutarrmap ->mutarrmap [& xs]
  (into (mutarrmap. 0 (object-array 32))
                    (reduce (fn [^java.util.Map m e]
                              (if (entry? e)
                                (let [^java.util.Map$Entry e e]
                                  (doto m (.put (.getKey e) (.getValue e))))
                                (let [[k v] e]
                                  (doto m (.put  k v))))) (java.util.HashMap.)  xs)))
(defn ^mutmap ->mutmap [& xs]  
  (into (mutmap. (java.util.HashMap.)) xs))

(defn hashmap->mutmap [hm] (mutmap. hm))

(defn ^txmutmap ->txmutmap [& xs]  
  (into (txmutmap. 0 (java.util.HashMap.) -1) xs))

(defn ^mutlist ->mutlist [& xs]  
   (mutlist. (reduce (fn [^java.util.ArrayList m k] (doto m (.add k)))  (java.util.ArrayList.)  xs)))

(extend-protocol IMutable
  clojure.lang.PersistentVector
  (mutable [v]   (into (->mutlist) v))
  (immutable [v] v)
  clojure.lang.PersistentList$EmptyList
  (mutable [v]   (->mutlist))
  (immutable [v] v)
  clojure.lang.PersistentList
  (mutable [v]    (into (->mutlist) v))
  (immutable [v]   v)
  clojure.lang.PersistentArrayMap
  (mutable [v]   (into (->mutmap) v))
  (immutable [v] v)
  clojure.lang.PersistentHashMap
  (mutable   [v] (into (->mutmap) v))
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

(comment ;testing
  ;;So, the surface analysis is that we do get a decent improvement.
  ;;Can we create lightweight structs, so we're not 
  ;;~71
  (let [inner  (atom {:a 2 :b 3}) outer {:inner inner} ]
    (time (dotimes [i 1000000] (swap! inner (fn [^clojure.lang.Associative m] (.assoc  m :a 3))))))
  ;;assoc-in kills us due to resolving the nested path
  ;;~1114
  (let [inner  {:a 2 :b 3} outer {:inner inner} ]
    (time (dotimes [i 1000000] (assoc-in outer [:inner :a] 3))))
  ;;94~
  (let [inner  {:a 2 :b 3} outer {:inner inner} ]
    (time (dotimes [i 1000000] (.assoc ^clojure.lang.Associative
                                       outer
                                       :inner
                                       (.assoc ^clojure.lang.Associative inner :a 3))))) 
  ;;~42/~28 for a mutarr map
  (let [inner  (->mutarrmap (seq {:a 2 :b 3})) outer {:inner inner} ]
    (time (dotimes [i 1000000] (.assoc ^clojure.lang.Associative inner :a 3))))

  ;;lookup tests
  ;;~30 ms
  (let [inner (mutable {:a 2 :b 3})]
    (time (dotimes [i 1000000] (.valAt ^clojure.lang.ILookup inner :b))))
  ;;~17ms 
  (let [inner  {:a 2 :b 3}]
    (time (dotimes [i 1000000] (.valAt ^clojure.lang.ILookup inner :b))))

  ;;however, our use case puts the assoc map behind an atom, for mutation.
  ;;~34
  (let [inner  (atom {:a 2 :b 3})]
    (time (dotimes [i 1000000] (.valAt ^clojure.lang.ILookup @inner :b))))

  ;;~25ms
  (let [inner  (atom {:a 2 :b 3})]
    (time (dotimes [i 1000000] (.valAt ^clojure.lang.ILookup
                                       (.deref ^clojure.lang.Atom inner) :b))))
  ;;So with a type hinted atom, we're about 5 ms slower using our mutable arrays for
  ;;reading....however....
  ;;we're 2x as slow at writing.  Since we perform multiple writes when updating,
  ;;I suppose it makes sense to allow this use case.
  
  ;;reads are 2xfaster with the arraymap....
  ;;probably the best of both worlds is just a wrapped object array.
  ;;we've got a small, possibly mutable array of fields to work with.
  ;;easiest just to blast over the entries.
  ;;if we grow beyond 16, we can graduate to a mutmap...
  )



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


 
