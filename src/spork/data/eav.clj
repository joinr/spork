(ns spork.data.eav)

;;so testing in java land proves we can get a fast mutable store with pointer entries stored in java maps.
;;so let's do that.

(defprotocol IAEVStore
  (entities [this])
  (attributes [this])
  (get-entity [this k])
  (get-attribute [this k]))

(defprotocol IEAV
  (get-e [this])
  (get-a [this a]))

(defprotocol IBox
  (val-      [this])
  (setValue- [this v]))

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
