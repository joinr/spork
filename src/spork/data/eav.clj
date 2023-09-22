(ns spork.data.eav)

;;so testing in java land proves we can get a fast mutable store with pointer entries stored in java maps.
;;so let's do that.

;;alternate encoding....
;;row-store with lightweight indices.

;;{:entities {e1 {a1 v1 a2 v2}} :attributes {:a1 #{e1}}}
;;so we ditch the pointer type entirely.
;;each map is trivially serialized.
;;no joins.
;;column indices are kept up to date with get/put on the entity.
;;get a map projection of the column easily.
;;so invert the relationship.
;;makes persistence trivial.
;;fast writes on entities.
;;queries still fast.

(defprotocol IAEVStore
  (entities [this])
  (attributes [this])
  (get-entity [this k])
  (get-attribute [this k]))

(defprotocol IEAV
  (get-e [this])
  (get-a [this a]))

;;maybe obe.
(defprotocol IBox
  (val-      [this])
  (setValue- [this v]))

(def ^java.util.function.Function create-set
  (reify java.util.function.Function
    (apply [this k] (java.util.HashSet.))))

(defn get-index ^java.util.HashSet [^java.util.HashMap m k]
  (.computeIfAbsent m k create-set))

(defmacro on-growth
  [m expr  & body]
  `(let [size# (.size ~m)
         res#      ~expr
         new-size# (.size ~m)]
     (when (<  size# new-size#)
       ~@body)
     res#))

(defmacro on-loss
  [m expr  & body]
  `(let [size# (.size ~m)
         res#      ~expr
         new-size# (.size ~m)]
     (when (> size# new-size#)
       ~@body)
     res#))

(def replace-val
  (reify java.util.function.BiFunction
    (apply [this k v]
      v)))

(defn update-index [k v])

;;entity is just a wrapped hashmap.
(deftype EntityRecord [id  ^java.util.HashMap attributes ^java.util.HashMap AE]
  IEAV
  (get-e [this]    id)
  (get-a [this a] (.get this a))
  java.util.Map
  (get [this k] (.get attributes k))
  (getOrDefault [this k default] (.getOrDefault attributes k default))
  ;;want to hook an update to the set if the key is novel.
  ;;it's novel if the map size changed.
  (put [this k v]
    (on-growth attributes
               (.put attributes k v)
               (let [idx (get-index AE k)]
                 (.add idx id))))
  (putIfAbsent [this k v]  ;;slowish path.
    (on-growth attributes
               (.putIfAbsent attributes k v)
               (let [idx (get-index AE k)]
                 (.add idx id))))
  (putAll [this m]
    (reduce (fn [acc ^java.util.Map$Entry e]
              (.put this (.getKey e) (.getValue e))) nil m))
  (remove [this k]
    (on-loss attributes
             (.remove attributes k)
             (let [idx (get-index AE k)
                   _   (.remove idx id)]
               (when (zero? (.size idx)) (.remove AE k)))))
  (remove [this k v]
    (on-loss attributes
             (.remove attributes k v)
             (let [idx (get-index AE k)
                   _   (.remove idx id)]
               (when (zero? (.size idx)) (.remove AE k)))))
  (size [this] (.size attributes))
  (containsKey   [this k] (.containsKey attributes k))
  (containsValue [this v] (some #(= v %) (vals this)))
  (entrySet [this] (.entrySet attributes))
  (equals [this o] (or (identical? this o)
                       (clojure.lang.APersistentMap/mapEquals attributes o)))
  clojure.lang.Seqable
  (seq [this]  (->> (.iterator (.entrySet attributes))
                    iterator-seq))
  #_#_
  clojure.lang.IKVReduce
  (kvreduce [f init]
    (let [it (.iterator (.entrySet m))]
      (loop [acc init]
        (if (.hasNext it)
          (let [^java.util.Map$Entry kv (.next it)
                res (f acc (.getKey kv) (.val- (.getValue kv)))]
            (if (reduced? res)
              @res
              (recur res)))
          acc))))
  clojure.lang.IReduce
  (reduce [this f]
    (->> (.entrySet attributes)
         (reduce f)))
  (reduce [this f init]
    (->> (.entrySet attributes)
         (reduce f init )))
  clojure.lang.IHashEq
  (hasheq [this]   (hash-unordered-coll (.seq this)))
  (hashCode [this] (clojure.lang.APersistentMap/mapHash attributes)) ;;probably change this, since we need to deref.
  #_
  (equiv  [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
          :else nil))
  (keySet [this]  (.keySet attributes))
  (values [this]  (.values attributes)) ;;maybe suboptimal.
  (isEmpty [this] (.isEmpty attributes))
  clojure.lang.Associative
  (assoc [this k v] (.put this k v) this)
  (entryAt [this k]
    (let [res (.getOrDefault attributes k ::not-found)]
      (when-not (identical? res ::not-found)
        (clojure.lang.MapEntry. k res))))
  clojure.lang.ILookup
  (valAt [this k]           (.get attributes k))
  (valAt [this k not-found] (.getOrDefault attributes k not-found))
  Iterable
  (iterator [this] (.iterator (.entrySet attributes))))

(defrecord RecordStore [^java.util.Map entities ^java.util.Map AE])

(defn ->record-store [] (RecordStore. (java.util.HashMap.) (java.util.HashMap.)))

(defn ->entity-record [store id]
  (let [e (EntityRecord. id (java.util.HashMap.) (:AE store))
        ^java.util.Map
        entities (:entities store)]
    (.put entities id e)
    e))

(deftype Pointer [^:unsynchronized-mutable v]
  clojure.lang.IDeref
  (deref [this] v)
  IBox
  (val- [this] v)
  (setValue- [this v2] (set! v v2) this))

(defn acquire-pointer ^Pointer [^java.util.Map m k]
  (if-let [p (.get m k)]
    p
    (let [new-pointer (Pointer. nil)
          _           (.put m k new-pointer)]
      new-pointer)))

(defn remove-ae [store a e] store)
(defn remove-ea [store e a] store)

(defn pointer-iterator ^java.util.Iterator [^java.util.Map m]
  (let [^java.util.Iterator it (.iterator (.entrySet m))]
    (reify java.util.Iterator
      (hasNext [this] (.hasNext it))
      (next [this]    (.val- ^Pointer (.getValue ^java.util.Map$Entry (.next it)))))))

(deftype EntityMap [^spork.data.eav.IAEVStore store id ^java.util.HashMap attributes]
  IEAV
  (get-e [this]    id)
  (get-a [this a] (.get this a))
  java.util.Map
  (get [this k]
    (when-let [^Pointer v (.get attributes k)]
      (.val- v)))
  (getOrDefault [this k default]
    (if-let [^Pointer v (.get attributes k)]
      (.val- v)
      default))
  (put [this k v]
    (let [^Pointer p (acquire-pointer attributes k)]
      (.setValue- p v)))
  (putIfAbsent [this k v]
    (if-let [p (.get attributes k)]
      (.val- ^Pointer p)
      (let [new-pointer (Pointer. nil)]
        (.put attributes k new-pointer)
        (.setValue- new-pointer v)
        nil)))
  (putAll [this m])
  (remove [this k]
    (when-let [^Pointer p (.get attributes k)]
      (.remove attributes k)
      (remove-ae store k id)
      (.val- p)))
  (remove [this k v]
    (when-let [^Pointer p (.get attributes k)]
      (when (= (.val- p) v)
        (.remove attributes k)
        (remove-ae store k id)
        v)))
  (size [this] (.size attributes))
  (containsKey   [this k] (.containsKey attributes k))
  (containsValue [this v] (some #(= v %) (vals this)))
  (entrySet [this]
    (reduce-kv (fn [^java.util.HashSet acc k ^Pointer v]
                 (doto acc (.add (clojure.lang.MapEntry. k (.val- v)))))
               (java.util.HashSet.)
               attributes))
  (equals [this o] (or (identical? this o)
                       (clojure.lang.APersistentMap/mapEquals this o)))
  clojure.lang.Seqable
  (seq [this]  (->> (.iterator (.entrySet attributes))
                    iterator-seq
                    (map (fn [^java.util.Map$Entry e]
                           (clojure.lang.MapEntry. (.getKey e) (.val- ^Pointer (.getValue e)))))))
  #_#_
  clojure.lang.IKVReduce
  (kvreduce [f init]
    (let [it (.iterator (.entrySet m))]
      (loop [acc init]
        (if (.hasNext it)
          (let [^java.util.Map$Entry kv (.next it)
                res (f acc (.getKey kv) (.val- (.getValue kv)))]
            (if (reduced? res)
              @res
              (recur res)))
          acc))))
  clojure.lang.IReduce
  (reduce [this f]
    (->> (.entrySet attributes)
         (eduction (map (fn [^java.util.Map$Entry kv] (clojure.lang.MapEntry. (.getKey kv) (.val- ^Pointer (.getValue kv))))))
         (reduce f)))
  (reduce [this f init]
    (->> (.entrySet attributes)
         (eduction (map (fn [^java.util.Map$Entry kv] (clojure.lang.MapEntry. (.getKey kv) (.val- ^Pointer (.getValue kv))))))
         (reduce f init )))
  clojure.lang.IHashEq
  (hasheq [this]   (hash-unordered-coll (.seq this)))
  (hashCode [this] (clojure.lang.APersistentMap/mapHash attributes)) ;;probably change this, since we need to deref.
  #_
  (equiv  [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
          :else nil))
  (keySet [this]  (.keySet attributes))
  (values [this]  (iterator-seq (pointer-iterator attributes))) ;;maybe suboptimal.
  (isEmpty [this] (.isEmpty attributes))
  Iterable
  (iterator [this] (pointer-iterator attributes)))

(defn ->entity [store id]
  (EntityMap. store id (java.util.HashMap.)))

;; ;;semantics for a mutable passmap are that we willingly mutate the underlying
;; ;;data structure (the db).  so we have a map of {e {a v}} and a set of #{a1 a2 a3...} keys
;; ;;that tells us which entries the entity has.  In this case, we have more of a reference
;; ;;that we can mutate directly.  Downside is that lookups and updates are in a nested map.
;; ;;Is this faster than having a local mutable hashmap?  Probably since we don't need to
;; ;;compute joins and stuff....
;; ;;this represents a cursor into the underlying 2d map.
;; (deftype PointerEntry [^:unsynchronized-mutablev k ^:unsynchronized-mutable v]
;;   clojure.lang.IMapEntry
;;   (key [this] k)
;;   (val [this] v)
;;   clojure.lang.Indexed
;;   (nth [this n] (case n 0 k 1 v (throw (ex-info "invalid index" {:in n}))))
;;   (nth [this n not-found] (case n  0 k 1 v not-found))
;;   clojure.lang.Seqable
;;   (seq [this] (seq [k v]))
;;   clojure.lang.Counted
;;   (count [this] 2)
;;   java.util.Map$Entry
;;   (getKey [this] k)
;;   (getValue [this] v)
;;   (setValue [this v2] (set! v v2) this)
;;   java.util.RandomAccess
;;   java.util.List ;;partial dgaf implementation.
;;   (get [this idx] (case idx 0 k 1 v))
;;   (size [this] 2)
;;   (hashCode [this]
;;     (-> (+ 31 (if k  (.hashCode k) 0))
;;         (* 31)
;;         (+ (if v  (.hashCode v) 0))))
;;   ;;hacky impl.....not intended for public consumption.
;;   (equals [this that]
;;     (cond (identical? this that) true
;;           (instance? PointerEntry that)
;;             (and (= k (.key ^PointerEntry that))
;;                  (= v (.val ^PointerEntry that)))
;;             ;;probably WRONG.  Need to handle collection cases and whatnot...
;;             :else
;;             (= (.hashCode this) (.hashCode that))))
;;   ;;not enamored with this but meh.
;;   (iterator [this]
;;     (let [^longs n (long-array 1)]
;;       (reify java.util.Iterator
;;         (hasNext [this] (< (aget n 0) 2))
;;         (next [this] (case (aget n 0)
;;                        0 k v))))))
