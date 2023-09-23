(ns spork.data.eav
  (:require [spork.data [derived :as derived]]))

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
;;need dual of this for attribute map.
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
  clojure.lang.IKVReduce
  (kvreduce [this f init]
    (let [it (.iterator (.entrySet attributes))]
      (loop [acc init]
        (if (.hasNext it)
          (let [^java.util.Map$Entry kv (.next it)
                res (f acc (.getKey kv) (.getValue kv))]
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

(defrecord RecordStore [^java.util.Map entities ^java.util.Map AE ^java.util.Map columns])

(defn ->record-store []
  (let [^java.util.Map
        entities (java.util.HashMap.)
        AE       (java.util.HashMap.)
        cols     (derived/->derived-map AE
                   (fn [a ^java.util.HashSet es]
                     (derived/->inverse-map a es entities
                           (fn make-entity [^java.util.Map EAVs e]
                             (let [enew (EntityRecord. e (java.util.HashMap.) AE)
                                   _    (.put entities e enew)]
                               enew)))))]
    (RecordStore. entities AE cols)))

(defn ->entity-record [store id]
  (let [e (EntityRecord. id (java.util.HashMap.) (:AE store))
        ^java.util.Map
        entities (:entities store)]
    (.put entities id e)
    e))


;;A separate columnstore implementation.
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

(defn pointer-iterator ^java.util.Iterator [^java.util.Map m]
  (let [^java.util.Iterator it (.iterator (.entrySet m))]
    (reify java.util.Iterator
      (hasNext [this] (.hasNext it))
      (next [this]    (.val- ^Pointer (.getValue ^java.util.Map$Entry (.next it)))))))


(defn put-ae [^spork.data.eav.IAEVStore store a e v]
  (let [^java.util.Map
        AEVs (.attributes store)
        ^java.util.Map
        EV   (or (.get AEVs a)
                 (let [attr (java.util.HashMap.)
                       _    (.put AEVs a attr)]
                   attr))
        _ (.put EV e v)]
    nil))

(defn remove-ae [^spork.data.eav.IAEVStore store a e]
  (let [^java.util.Map
        AEVs (.attributes store)]
    (when-let [^java.util.Map
               EV  (.get AEVs a)]
      (.remove EV e)
      (when (zero? (.size EV))
        (.remove AEVs a)))))


;;WIP
;; (defn put-ea [^spork.data.eav.IAEVStore store e a v]
;;   (let [^java.util.Map
;;         EAVs (.entities store)
;;         ^java.util.Map
;;         AV   (or (.get EAVs e)
;;                  (let [ent  (EntityRecord. id (doto ^java.util.Map (java.util.HashMap.) (.put a v)) (:AE store))
;;                        _    (.put EAVs e ent)]
;;                    ent))
;;         _ (.put EV e v)]
;;     nil))

#_
(deftype AttributeMap [^spork.data.eav.IAEVStore store a ^java.util.HashMap entities]
  java.util.Map
  (get [this k]
    (when-let [^Pointer v (.get entities k)]
      (.val- v)))
  (getOrDefault [this k default]
    (if-let [^Pointer v (.get entities k)]
      (.val- v)
      default))
  (put [this k v]
    (if-let [^Pointer p (.get entities k)]
      (.setValue- p v)
      (let [^Pointer p (Pointer. v)
            _ (.put entities k p)
            _ (put-ea store k a p)]
        nil)))
  (putIfAbsent [this k v]
    (if-let [p (.get entities k)]
      (.val- ^Pointer p)
      (let [^Pointer p (Pointer. v)
            _ (.put entities k p)
            _ (put-ea store k a p)]
        nil)))
  (putAll [this m] (reduce (fn [acc ^java.util.Map$Entry e]
                             (.put this (.getKey e) (.getValue e))) nil m))
  (remove [this k]
    (when-let [^Pointer p (.get entities k)]
      (.remove entities k)
      (remove-ea store k a)
      (.val- p)))
  (remove [this k v]
    (when-let [^Pointer p (.get entities k)]
      (when (= (.val- p) v)
        (.remove entities k)
        (remove-ea store k a)
        v)))
  (size [this] (.size entities))
  (containsKey   [this k] (.containsKey entities k))
  (containsValue [this v] (some #(= v %) (vals this)))
  (entrySet [this] (.entrySet (derived/->derived-map entities val-)))
  (keySet [this]  (.keySet entities))
  (values [this]  (iterator-seq (pointer-iterator entities))) ;;maybe suboptimal.
  (isEmpty [this] (.isEmpty entities))
  (equals [this o] (or (identical? this o)
                       (clojure.lang.APersistentMap/mapEquals this o)))
  clojure.lang.Seqable
  (seq [this]  (->> (.iterator (.entrySet entities))
                    iterator-seq
                    (map (fn [^java.util.Map$Entry e]
                           (clojure.lang.MapEntry. (.getKey e) (.val- ^Pointer (.getValue e)))))))
  clojure.lang.IKVReduce
  (kvreduce [this f init]
    (let [it (.iterator (.entrySet entities))]
      (loop [acc init]
        (if (.hasNext it)
          (let [^java.util.Map$Entry kv (.next it)
                res (f acc (.getKey kv) (.val- ^Pointer (.getValue kv)))]
            (if (reduced? res)
              @res
              (recur res)))
          acc))))
  clojure.lang.IReduce
  (reduce [this f]
    (->> (.entrySet entities)
         (eduction (map (fn [^java.util.Map$Entry kv] (clojure.lang.MapEntry. (.getKey kv) (.val- ^Pointer (.getValue kv))))))
         (reduce f)))
  (reduce [this f init]
    (->> (.entrySet entities)
         (eduction (map (fn [^java.util.Map$Entry kv] (clojure.lang.MapEntry. (.getKey kv) (.val- ^Pointer (.getValue kv))))))
         (reduce f init )))
  clojure.lang.IHashEq
  (hasheq [this]   (hash-unordered-coll (.seq this)))
  (hashCode [this] (clojure.lang.APersistentMap/mapHash entities)) ;;probably change this, since we need to deref.
  #_
  (equiv  [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
          :else nil))
  Iterable
  (iterator [this] (pointer-iterator entities)))

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
    (if-let [^Pointer p (.get attributes k)]
      (.setValue- p v)
      (let [^Pointer p (Pointer. v)
            _ (.put attributes k p)
            _ (put-ae store k id p)]
        nil)))
  (putIfAbsent [this k v]
    (if-let [p (.get attributes k)]
      (.val- ^Pointer p)
      (let [^Pointer p (Pointer. v)
            _ (.put attributes k p)
            _ (put-ae store k id p)]
        nil)))
  (putAll [this m] (reduce (fn [acc ^java.util.Map$Entry e]
                             (.put this (.getKey e) (.getValue e))) nil m))
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
  (entrySet [this] (.entrySet (derived/->derived-map attributes val-)))
  (keySet [this]  (.keySet attributes))
  (values [this]  (iterator-seq (pointer-iterator attributes))) ;;maybe suboptimal.
  (isEmpty [this] (.isEmpty attributes))
  (equals [this o] (or (identical? this o)
                       (clojure.lang.APersistentMap/mapEquals this o)))
  clojure.lang.Seqable
  (seq [this]  (->> (.iterator (.entrySet attributes))
                    iterator-seq
                    (map (fn [^java.util.Map$Entry e]
                           (clojure.lang.MapEntry. (.getKey e) (.val- ^Pointer (.getValue e)))))))
  clojure.lang.IKVReduce
  (kvreduce [this f init]
    (let [it (.iterator (.entrySet attributes))]
      (loop [acc init]
        (if (.hasNext it)
          (let [^java.util.Map$Entry kv (.next it)
                res (f acc (.getKey kv) (.val- ^Pointer (.getValue kv)))]
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
  Iterable
  (iterator [this] (pointer-iterator attributes)))

(defrecord MapStore [^java.util.Map entities ^java.util.Map attributes]
  IAEVStore
  (entities [this] entities)
  (attributes [this] attributes)
  (get-entity [this k] (.get entities k))
  (get-attribute [this k] (.get attributes k)))

(defn ->map-store []  (MapStore. (java.util.HashMap.) (java.util.HashMap.)))

(defn ->entity-map [store id]
  (let [e   (EntityMap. store id (java.util.HashMap.))
        ^java.util.Map
        entities (:entities store)]
    (.put entities id e)
    e))
