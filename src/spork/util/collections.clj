;;#Operations for working with mutable references
;;particularly working with pieces of state in a nested associative
;;structure.   These functions make it significantly easier to
;;work with the pre-baked mutable implementations of
;;order/unorderd maps and collections that exist in the
;;host platform.  Experimental.  Adoption with the general
;;population is probably unlikely, since we introduce
;;potentially uncontrolled mutation that would upset
;;a lot of folks; however, for cases where we desire
;;ephemeral collections backing pure functions, the
;;functionality here makes it nicer to work with.
(ns spork.util.collections
  (:refer-clojure :exclude
     [into update update-in assoc-in assoc 
      dissoc dissoc-in conj disj contains?]))

;;Generic operations defined over persistent, transient,
;;and mutable data types.

(defmacro as [type obj]
  (let [typed (with-meta (gensym "obj") {:tag type})]
    `(let [~typed ~obj]
       ~typed)))

(definline assoc-t [m k v]
  `(.assoc    (as  clojure.lang.ITransientAssociative ~m) ~k ~v))
(definline assoc-p  [m k v]
  `(.assoc    (as clojure.lang.Associative ~m) ~k ~v))
(definline assoc-m  [m k v]
  `(doto (as java.util.Map ~m)                 (.put  ~k ~v)))

(definline assoc [m k v] 
  ` (cond (instance? clojure.lang.ITransientAssociative ~m) (.assoc    (as  clojure.lang.ITransientAssociative ~m) ~k ~v)
          (instance? clojure.lang.Associative ~m)           (.assoc    (as clojure.lang.Associative ~m) ~k ~v)
          :else (doto (as java.util.Map ~m)                 (.put  ~k ~v))))

(definline conj-t [coll v]
  `(.conj (as clojure.lang.ITransientCollection  ~coll) ~v))
(definline conj-p [coll v]
  `(if (nil? ~coll) (cons ~coll nil)
       (.cons  (as clojure.lang.IPersistentCollection ~coll) ~v)))
(definline conj-m [coll v]
  `(if (nil? ~coll) (doto (java.util.ArrayList.) (.add ~v))
                   (doto (as java.util.Collection ~coll) (.add  ~v))))

(definline conj-hm [coll v]
  (let [idxd (with-meta (gensym "indexed") {:tag 'clojure.lang.Indexed})]
    `(cond (instance? java.util.Map$Entry ~v)          
             (if (nil? ~coll) (doto (java.util.HashMap.) (.put (key ~v) (val ~v)))
                 (doto (as java.util.Map ~coll)   (.put (key ~v) (val ~v))))            
           (instance? clojure.lang.Indexed ~v)
             (let [~idxd ~v]
               (if (nil? ~coll) (doto (java.util.HashMap.) (.put (.nth ~idxd 0 ) (.nth ~idxd 1)))
                   (doto (as java.util.Map ~coll)   (.put (.nth ~idxd 0) (.nth ~idxd 1)))))
           :else
             (if (nil? ~coll) (doto (java.util.HashMap.) (.put (first ~v) (second ~v)))
                 (doto (as java.util.Map ~coll)   (.put (first ~v 0) (second ~v 1)))))))

(definline conj [coll v] 
  `(cond (instance? clojure.lang.ITransientCollection ~coll)  (.conj (as clojure.lang.ITransientCollection  ~coll) ~v)
         (instance? clojure.lang.IPersistentCollection ~coll) (if (nil? ~coll) (cons ~coll nil)
                                                                  (.cons  (as clojure.lang.IPersistentCollection ~coll) ~v))
         (instance? java.util.Collection ~coll)
               (if (nil? ~coll) (doto (java.util.ArrayList.) (.add ~v))
                   (doto (as java.util.Collection ~coll) (.add  ~v)))
          :else  (conj-hm ~coll ~v)))

(definline dissoc-t [m k]
  `(.without  (as clojure.lang.ITransientMap ~m) ~k)
  )
(definline dissoc-p [m k]
  `(.without  (as clojure.lang.IPersistentMap  ~m) ~k)
  )
(definline dissoc-m [m k]
  `(doto  (as java.util.Map ~m) (.remove ~k))
  )

(definline dissoc [m k] 
  `(cond (instance? clojure.lang.ITransientAssociative ~m) (.without  (as clojure.lang.ITransientMap ~m) ~k)
         (instance? clojure.lang.IPersistentMap ~m)        (.without  (as clojure.lang.IPersistentMap  ~m) ~k)
         :else (doto  (as java.util.Map ~m) (.remove ~k))))

(definline disj-t [m v]
  `(.disjoin (as clojure.lang.ITransientSet ~m)  ~v))
(definline disj-p [m v]
  `(.disjoin (as clojure.lang.IPersistentSet ~m) ~v))
(definline disj-m [m v]
  `(doto  (as java.util.Collection ~m) (.remove ~v)))

(definline disj [m v] 
  `(cond (instance? clojure.lang.ITransientSet ~m)   (.disjoin (as clojure.lang.ITransientSet ~m)  ~v)
         (instance? clojure.lang.IPersistentSet ~m)  (.disjoin (as clojure.lang.IPersistentSet ~m) ~v)
         :else (doto  (as java.util.Collection ~m) (.remove ~v))))

(defn assoc-in
  "Replacement for assoc-in, works on both transients and persistents."
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defn into 
  ([to from]
   (cond (instance? clojure.lang.IEditableCollection to)
            (with-meta (persistent! (reduce conj-t (transient to) from)) (meta to))
          (instance? clojure.lang.IPersistentCollection to)
            (reduce conj-p to from)
          (instance? java.util.Collection to)
            (reduce conj-m to from)
          :else
            (reduce conj-hm to from)))  
  ([to xform from]
   (cond (instance? clojure.lang.IEditableCollection to)         
           (with-meta (persistent! (transduce xform (completing conj-t) (transient to) from))
             (meta to))
         (instance? clojure.lang.IPersistentCollection to)
             (transduce xform (completing conj-p) to from)
         (instance? java.util.Collection to)
             (transduce xform (completing conj-m) to from)
         :else
             (transduce xform (completing conj-hm) to from))))

(defmacro update-1
  [m k f & args]
  `(assoc ~m ~k (~f (get ~m ~k) ~@args)))

(defn update
  ([m k f]
   (assoc m k (f (get m k))))
  ([m k f x]
   (assoc m k (f (get m k) x)))
  ([m k f x y]
   (assoc m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc m k (apply f (get m k) x y z more))))

(extend-protocol clojure.core.protocols/IKVReduce
  java.util.Map
  (kv-reduce [obj f init]
    (reduce (fn [acc ^java.util.Map$Entry e]
              (f acc (.getKey e) (.getValue e)))
            init (.entrySet ^java.util.Map obj)))
  java.util.HashMap
  (kv-reduce [obj f init]
    (reduce (fn [acc ^java.util.Map$Entry e]
              (f acc (.getKey e) (.getValue e)))
            init (.entrySet ^java.util.HashMap obj)))
  java.util.LinkedHashMap
  (kv-reduce [obj f init]
    (reduce (fn [acc ^java.util.Map$Entry e]
              (f acc (.getKey e) (.getValue e)))
            init (.entrySet ^java.util.LinkedHashMap obj)))
  java.util.concurrent.ConcurrentHashMap
  (kv-reduce [obj f init]
    (reduce (fn [acc ^java.util.Map$Entry e]
              (f acc (.getKey e) (.getValue e)))
            init (.entrySet ^java.util.concurrent.ConcurrentHashMap obj)))
  java.util.TreeMap
  (kv-reduce [obj f init]
    (reduce (fn [acc ^java.util.Map$Entry e]
              (f acc (.getKey e) (.getValue e)))
            init (.entrySet ^java.util.TreeMap obj))))

(defn update-in
  "Replacement for update-in, works on both transients and persistents."
  ([m [k & ks] f & args]
   (if ks
     (assoc m k (apply update-in (get m k) ks f args))
     (assoc m k (apply f (get m k) args)))))

;;if we know the structure is mutable, we don't need to call put.
(defmacro deep-update
  "Replacement for update-in, but without the function call overhead.
   If the key-path is composed of literals, this is about 
   3 times faster then assoc-in."
  [m [k & ks] f & args]
   (if ks
     `(assoc ~m ~k (deep-update (get ~m ~k) ~ks ~f ~@args))
     `(assoc ~m ~k (~f (get ~m ~k) ~@args))))

(defmacro deep-update!
  "Replacement for update-in, but without the function call overhead.
   If the key-path is composed of literals, this is about 
   3 times faster than assoc-in."
  [m ks f & args]
  (let [k     (last ks)
        ppath (vec (butlast ks))]
    `(let [m# ~m
           parent# (get-in m# ~ppath)]
       (do (update parent# ~k  ~f ~@args)
           m#))))

(definline contains? [m k]
    `(not (identical? (get ~m ~k :not-found) :not-found)))

;;one of the cool things we can do

;;recursively convert an immutable collection into a mutable collection.  This is not a
;;transient collection...
(defn hmap [m]
  (reduce-kv (fn [acc k v]
               (if (map? v)
                 (assoc acc k (hmap v))
                 (assoc acc k v))) (java.util.HashMap.) m))

(defn unhmap [hm]
  (persistent!
   (reduce-kv (fn [acc k v]
                (if (map? v)
                  (assoc! acc k (unhmap v))
                  (assoc! acc k v))) (transient {})  hm)))
(comment    
(defn immutable! [coll])
(defn mutable!   [coll])
)

(comment ;testing


  )
