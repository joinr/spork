(ns spork.data.eav
  (:require [spork.data
             [derived :as derived]
             [mutable :as mut]]
            [spork.protocols.core :as proto]
            [spork.util.general :as gen]))

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
  (eav-entities [this])
  (eav-attributes [this])
  (eav-entity [this k])
  (eav-attribute [this k])
  (eav-acquire-entity [this k] [this k init])
  (eav-acquire-attribute [this a] [this a init]))

;;maybe obe.
(defprotocol IBox
  (val-      [this])
  (setValue- [this v]))

;;A separate columnstore implementation.
(deftype Pointer [^:unsynchronized-mutable v]
  clojure.lang.IDeref
  (deref [this] v)
  IBox
  (val- [this] v)
  (setValue- [this v2] (set! v v2) this))

(defprotocol IPointerMap
  (put-pointer    [m k ^Pointer v])
  (get-pointer    ^Pointer [m k])
  (remove-pointer [m k])
  (pointer-entries ^java.util.Iterator [m]))

(defprotocol IEAV
  (get-e [this])
  (get-a [this a]))

(defn seqable-map-hash [this]
  (clojure.lang.APersistentMap/mapHash
   (reify clojure.lang.IPersistentMap
     (seq [m] (cond
                (seqable? m) (seq this)
                (instance? java.lang.Iterable this)
                (iterator-seq (.iterator ^java.lang.Iterable this))
                :else (throw (ex-info "no idea how to hash!" {:in (type this)})))))))

;;hmmm, maybe this is janky.
(defn map-equals [^java.util.Map m1 obj]
  (if  (not (instance? java.util.Map obj)) false
       (let [^java.util.Map      obj     obj
             ^java.util.Iterator it      (.iterator (.entrySet m1))]
         (loop [acc true]
           (if (.hasNext it)
             (let [^java.util.Map$Entry nxt (.next it)]
               (if-let [v (.get obj (.getKey nxt))]
                 (if (= v (.getValue nxt))
                   (recur acc)
                   false)
                 false))
             acc)))))

;; static public boolean mapEquals(IPersistentMap m1, Object obj)
;; {
;;  if(m1 == obj) return true;
;;  if(!(obj instanceof Map))
;;  return false;
;;  Map m = (Map) obj;
 
;;  if(m.size() != m1.count())
;;  return false;
 
;;  for(ISeq s = m1.seq(); s != null; s = s.next())
;; 		      {
;; 		       Map.Entry e = (Map.Entry) s.first();
;; 		       boolean found = m.containsKey(e.getKey());
           
;; 		       if(!found || !Util.equals(e.getValue(), m.get(e.getKey())))
;; 			     return false;
;; 		       }
          
;; 	        return true;
;;           }
 

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
                       (#_clojure.lang.APersistentMap/mapEquals map-equals attributes o)))
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
  (hashCode [this] (.hashCode attributes)) ;;probably change this, since we need to deref.
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

(defn pointer-entry-iterator ^java.util.Iterator [^java.util.Map m]
  (let [^java.util.Iterator it (.iterator (.entrySet m))]
    (reify java.util.Iterator
      (hasNext [this] (.hasNext it))
      (next [this]    (let [e ^java.util.Map$Entry (.next it)]
                        (clojure.lang.MapEntry. (.getKey e) (.val- ^Pointer (.getValue e))))))))

(declare put-ae remove-ae put-ea remove-ea)

;;functor ops.


;;clearing semantics:
;;clearing an attribute map means we need to traverse every entity and remove the relation,
;;then remove the attribute map from the attributes.

;;clearing an entity means we need to traverse every attribute from the entity,
;;and remove the relation.  Do we keep the entity map?  Currently no.
;;The result of clearing is nil. There should be no reference left to either
;;entity map or attribute map.
(deftype AttributeMap [^spork.data.eav.IAEVStore store a ^java.util.HashMap entities]
  IPointerMap
  (get-pointer     [m k]   (.get entities k))
  (remove-pointer  [m k]   (.remove entities k))
  (put-pointer     [m k v] (.put entities k v))
  (pointer-entries [m]     (.iterator ^java.lang.Iterable (.entrySet  entities)))
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
  ;;remove all E V mappings, remove A from the store.  relationally destructive operation.
  (clear [this]
    (let [^java.util.Iterator it (.iterator (.entrySet entities))]
      (loop []
        (when-let [^java.util.Map$Entry ev (and  (.hasNext it) (.next it))]
          (let [e (.getKey ev)]
            (println [:removing e :from a])
            (.remove it)
            (remove-ea store e a)
            (recur))))
      (.remove ^java.util.Map (eav-attributes store) a));;drop from the attributes map.
      )
  (size [this] (.size entities))
  (containsKey   [this k] (.containsKey entities k))
  (containsValue [this v] (some #(= v %) (vals this)))
  (entrySet [this] (.entrySet (derived/->derived-map entities (fn [k v] (val- v)))))
  (keySet [this]  (.keySet entities))
  (values [this]  (iterator-seq (pointer-iterator entities))) ;;maybe suboptimal.
  (isEmpty [this] (.isEmpty entities))
  (equals [this o] (or (identical? this o)
                       (#_clojure.lang.APersistentMap/mapEquals map-equals this o)))
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
  (hashCode [this] (seqable-map-hash entities)) ;;probably change this, since we need to deref.
  #_
  (equiv  [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
          :else nil))
  Iterable
  (iterator [this] (pointer-entry-iterator entities))
  mut/IUpdateKV
  (update-kv [coll f]
    (gen/iter [^java.util.Map$Entry nxt (.pointer-iterator coll)]
      (let [k (.getKey nxt)
            ^Pointer p (.getValue nxt)]
        (.setValue- p (f k (.val- p)))))
    coll)
  (update-kv-where [coll pred f]
    (gen/iter [^java.util.Map$Entry nxt (.pointer-iterator coll)]
      (let [k (.getKey nxt)
            ^Pointer p (.getValue nxt)
            v          (.val- p)]
        (if (pred k v) (.setValue- p (f k (.val- p))))))
    coll)
  (update-kv-keep [coll f]
    (let [^java.util.Iterator it (.pointer-iterator coll)]
      (gen/iter [^java.util.Map$Entry nxt it]
        (let [k                        (.getKey nxt)
              ^Pointer p               (.getValue nxt)
              v                        (.val- p)]
          (if-let [res (f k v)]
            (.setValue- p res)
            (do (.remove it)
                (remove-ea store k a)))))
      coll))
  (update-kv-keep-where [coll pred f]
    (let [^java.util.Iterator it (.pointer-iterator coll)]
      (gen/iter [^java.util.Map$Entry nxt it]
        (let [k                        (.getKey nxt)
              ^Pointer p               (.getValue nxt)
              v                        (.val- p)]
          (when (pred k v)
            (if-let [res (f k v)]
              (.setValue- p res)
              (do (.remove it)
                  (remove-ea store k a))))))
          coll)))

(deftype EntityMap [^spork.data.eav.IAEVStore store id ^java.util.HashMap attributes]
  IPointerMap
  (get-pointer     [m k]   (.get attributes k))
  (remove-pointer  [m k]   (.remove attributes k))
  (put-pointer     [m k v] (.put attributes k v))
  (pointer-entries [m]     (.iterator ^java.lang.Iterable (.entrySet  attributes)))
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
  (clear [this]
    (let [^java.util.Iterator it (.iterator (.entrySet attributes))]
      (loop []
        (when-let [^java.util.Map$Entry av (and  (.hasNext it) (.next it))]
          (let [a (.getKey av)]
            (.remove it)
            (remove-ae store  a id)
            (recur))))
      (.remove ^java.util.Map (eav-entities store) id)))
  (size [this] (.size attributes))
  (containsKey   [this k] (.containsKey attributes k))
  (containsValue [this v] (some #(= v %) (vals this)))
  (entrySet [this] (.entrySet (derived/->derived-map attributes (fn [k v] (val- v)))))
  (keySet [this]  (.keySet attributes))
  (values [this]  (iterator-seq (pointer-iterator attributes))) ;;maybe suboptimal.
  (isEmpty [this] (.isEmpty attributes))
  (equals [this o] (or (identical? this o)
                       (#_clojure.lang.APersistentMap/mapEquals map-equals this o)))
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
  (hashCode [this] (seqable-map-hash this)) ;;probably change this, since we need to deref.
  #_
  (equiv  [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
          :else nil))
  Iterable
  (iterator [this] (pointer-entry-iterator attributes))
  (clone [this]
    (let [^java.util.HashMap
          m  (java.util.HashMap.)
          ^java.util.Iterator
          it (.iterator (.entrySet attributes))]
      (loop []
        (when (.hasNext it)
          (let [^java.util.Map$Entry e (.next it)]
            (.put m (.getKey e) (.val- ^Pointer (.getValue e)))
            (recur))))
      m))
  mut/IUpdateKV
  (update-kv [coll f]
    (gen/iter [^java.util.Map$Entry nxt (.pointer-iterator coll)]
      (let [k (.getKey nxt)
            ^Pointer p (.getValue nxt)]
        (.setValue- p (f k (.val- p)))))
    coll)
  (update-kv-where [coll pred f]
    (gen/iter [^java.util.Map$Entry nxt (.pointer-iterator coll)]
      (let [k (.getKey nxt)
            ^Pointer p (.getValue nxt)
            v          (.val- p)]
        (if (pred k v) (.setValue- p (f k (.val- p))))))
    coll)
  (update-kv-keep [coll f]
    (let [^java.util.Iterator it (.pointer-iterator coll)]
      (gen/iter [^java.util.Map$Entry nxt it]
        (let [k                        (.getKey nxt)
              ^Pointer p               (.getValue nxt)
              v                        (.val- p)]
          (if-let [res (f k v)]
            (.setValue- p res)
            (do (.remove it)
                (remove-ae store k id)))))
      coll))
  (update-kv-keep-where [coll pred f]
    (let [^java.util.Iterator it (.pointer-iterator coll)]
      (gen/iter [^java.util.Map$Entry nxt it]
        (let [k                        (.getKey nxt)
              ^Pointer p               (.getValue nxt)
              v                        (.val- p)]
          (when (pred k v)
            (if-let [res (f k v)]
              (.setValue- p res)
              (do (.remove it)
                  (remove-ae store k id))))))
      coll)))

;;bidirectional pointer ops.
(defn put-ae [^spork.data.eav.IAEVStore store a e pv]
  (let [^java.util.Map
        AEVs (.eav-attributes store)
        ^spork.data.eav.IPointerMap
        EV   (or (.get AEVs a)
                 (let [attr (AttributeMap.  store a (java.util.HashMap.))
                       _    (.put AEVs a attr)]
                   attr))
        _ (.put-pointer EV e pv)]
    nil))

(defn remove-ae [^spork.data.eav.IAEVStore store a e]
  (let [^java.util.Map
        AEVs (.eav-attributes store)]
    (when-let [^spork.data.eav.AttributeMap
               EV  (.get AEVs a)]
      (.remove-pointer EV e)
      (when (zero? (.size EV))
        (.remove AEVs a)))))

(defn put-ea [^spork.data.eav.IAEVStore store e a pv]
  (let [^java.util.Map
        EAVs (.eav-entities store)
        ^EntityMap
        AV   (or (.get EAVs e)
                 (let [ent  (EntityMap. store e (java.util.HashMap.))
                       _    (.put EAVs e ent)]
                   ent))
        _ (.put-pointer AV a pv)
        _ (.put AV ::name e)]
    nil))

(defn remove-ea [^spork.data.eav.IAEVStore store e a]
  (let [^java.util.Map
        EAVs (.eav-entities store)]
    (if-let [^EntityMap AV (.get EAVs e)]
      (do (.remove-pointer AV a)
          (when (zero? (.size AV))
            (.remove EAVs e)))
      nil)))

;;need to check for collisions....assumes new entity.
(defn ->entity-map
  ([store id init]
   (let [e   (EntityMap. store id (java.util.HashMap.))
         ^java.util.Map
         entities (:entities store)]
     (.put entities id e)
     (.putAll ^EntityMap e init)
     e))
  ([store id] (->entity-map store id {::name id})))

(defrecord MapStore [^java.util.Map entities ^java.util.Map attributes]
  IAEVStore
  (eav-entities [this] entities)
  (eav-attributes [this] attributes)
  (eav-entity [this k] (.get entities k))
  (eav-attribute [this k] (.get attributes k))
  (eav-acquire-entity [this k]
    (or (.get entities k)
        (->entity-map this k)))
  (eav-acquire-entity [this k init]
    (or (.get entities k)
        (->entity-map this k init)))
  (eav-acquire-attribute [this a]
    (or (.get attributes a)
        (let [attr (AttributeMap. this a (java.util.HashMap.))]
          (.put attributes a attr)
          attr)))
  (eav-acquire-attribute [this a init]
    (or (.get attributes a)
        (let [attr (AttributeMap. this a (java.util.HashMap.))]
          (.put attributes a attr)
          (.putAll attr init)
          attr))))

(defn ->map-store []  (MapStore. (java.util.HashMap.) (java.util.HashMap.)))




;;testing
(comment
  (def other-store (->map-store))
  (def other-ent   (->entity-map other-store "bilbo"))
  (.put ^java.util.Map other-ent :name "Baggins")
  (-> other-store :attributes :name (.put "samwise" "gamgee"))
  ;;simple cloning semantics.
  (let [old (-> other-store :entities (get "bilbo"))
        m (doto (java.util.HashMap.) (.putAll old))
        _ (.put old :version 2)] [m old])
  #_
  [{:name "Baggins", :spork.data.eav/name "bilbo"}
   {:name "Baggins", :spork.data.eav/name "bilbo", :version 2}]
  ;;cloning small entities is much faster than putAll.
  ;;(let [old (-> other-store :entities (get "bilbo")) ] (c/quick-bench  (.clone ^EntityMap old)))
  ;;Execution time mean : 79.934234 ns
  )
