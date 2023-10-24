(ns spork.data.derived)
;;need a derived set type too.
;;we have a common case where we are storing entries of
;;[k [k v]]] instead of [k v], or some other storage type
;;that we'd like to e.g. deref or treat transparently...
;;since maps operate on entry sets, havinga function that
;;lazily lifts this stuff would be useful in construction.


;;like clojure.core/hash-ordered-coll but for hashCode
;;computations for equals and friends.  e.g. no murmur3 mixing.
(defn hashcode-ordered-coll ^long [xs]
  (reduce (fn ^long [^long acc ^Object x]
            (unchecked-int ^long (unchecked-add (unchecked-multiply 31 acc) (.hashCode x)))) 1 xs))

(defn hashcode-unordered-coll ^long [xs]
  (reduce (fn ^long [^long acc ^Object x]
            (unchecked-int ^long (unchecked-add acc (.hashCode x)))) 0 xs))

;;wouldn't rely on these for hashing and equality just yet...
(deftype WrappedEntrySet [^java.util.Map m f]
  java.util.Set
  (add [this v] (throw (ex-info "Read only set" {:in v})))
  (addAll [this coll] (throw (ex-info "Read only set" {:in coll})))
  (remove    [this k] (throw (ex-info "Read only set" {:in k})))
  (removeAll [this xs] (throw (ex-info "Read only set" {})))
  (isEmpty [this] (.isEmpty m))
  (clear  [this]  (throw (ex-info "Read only set" {})))
  (size   [this]  (.size m))
  (equals [this that] (or (identical? this that)
                          (clojure.lang.APersistentSet/setEquals this that)))
  (hashCode [this]  (hashcode-unordered-coll this)) ;;will yield different hashCode.  java.util.HashMap.Node
  ;;hashes by int: K ^ V, clojure hashes Map entry as vector, wish is hashcode-ordere-coll.
  (contains [this  e]
    ;;this is weak containmant.  we don't have access to the exact entry from juh, since nodes
    ;;aren't exposed. it depends on how equals is implemented for entries in java land.
    (when-let [v (.get m (.getKey ^java.util.Map$Entry e))]
      (= (f v) (.getValue ^java.util.Map$Entry e))))
  (containsAll [this coll]
    (reduce (fn [l v]
              (if-not (.contains this v)
                (reduced false)
                true)) true coll))
  (iterator [this] (.iterator ^java.lang.Iterable (eduction (map f) (.entrySet m)))))

#_
(deftype WrappedEntry [^java.util.Map$Entry e ^clojure.lang.IFn on-get ^clojure.lang.IFn on-put]
  java.util.Map$Entry
  (getValue [this] )
  (setValue [this] ))

(deftype DerivedMap [^java.util.Map m ^clojure.lang.IFn on-get ^clojure.lang.IFn on-put]
  java.util.Map
  (containsKey [this k] (.containsKey m k))
  (get [this k] (when-let [res (.get m k)]
                  (on-get k res)))
  (getOrDefault [this k default]
    (if-let [res (.get m k)]
      (on-get k res)
      default))
  (put [this k v] (.put m k (on-put k v)))
  (putAll [this m]
    (reduce (fn [_ ^java.util.Map$Entry e]
              (let [k (.getKey e)]
                (.put this k (on-put k (.getValue e)))) nil m)))
  (size [this] (.size m))
  (keySet [this]   (.keySet m))
  (entrySet [this] (WrappedEntrySet. m (fn on-entry [^java.util.Map$Entry kv]
                                         (let [k (.getKey kv)]
                                           (clojure.lang.MapEntry. k  (on-get k (.getValue kv)))))))
  (values [this]   (vec (eduction (map (fn [^java.util.Map$Entry kv]
                                         (on-get (.getKey kv) (.getValue kv)))) (.entrySet m))))
  (hashCode [this] (hashcode-unordered-coll (.entrySet this))))



(deftype InverseEntrySet [a ^java.util.Set es ^java.util.Map EAVs]
  java.util.Set
  (add [this v] (throw (ex-info "Read only set" {:in v})))
  (addAll [this coll] (throw (ex-info "Read only set" {:in coll})))
  (remove    [this k] (throw (ex-info "Read only set" {:in k})))
  (removeAll [this xs] (throw (ex-info "Read only set" {})))
  (isEmpty [this] (.isEmpty es))
  (clear  [this]  (throw (ex-info "Read only set" {})))
  (size   [this]  (.size es))
  (equals [this that] (or (identical? this that)
                          (clojure.lang.APersistentSet/setEquals this that)))
  (hashCode [this]  (hashcode-unordered-coll this)) ;;will yield different hashCode.  java.util.HashMap.Node
  ;;hashes by int: K ^ V, clojure hashes Map entry as vector, wish is hashcode-ordere-coll.
  (contains [this  e]
    (let [k (.getKey ^java.util.Map$Entry e)]
      (if-let [^java.util.Map ent (.get EAVs e)]
        (= (.get ent a) (.getValue ^java.util.Map$Entry e))
        false)))
  (containsAll [this coll]
    (reduce (fn [l v]
              (if-not (.contains this v)
                (reduced false)
                true)) true coll))
  (iterator [this] (.iterator ^java.lang.Iterable
                              (eduction (map (fn [e]
                                               (let [^java.util.Map ent (.get EAVs e)]
                                                 (clojure.lang.MapEntry. e (.get ent a))))) es))))

(deftype InverseMap [a ^java.util.Set es ^java.util.Map EAVs make-e]
  java.util.Map
  (containsKey [this k] (.contains es k))
  (get [this k]
    (when-let [^java.util.Map e (.get EAVs a)]
      (.get e a)))
  (getOrDefault [this k default]
    (or (.get this k) default))
  (put [this k v]
    (if-let [^java.util.Map e (.get EAVs k)]
      (.put e a v)
      (let [^java.util.Map
            enew (make-e EAVs k)]
        (.put enew a v))))
  (remove [this k] )
  (putAll [this m]
    (reduce (fn [_ ^java.util.Map$Entry e]
              (let [k (.getKey e)]
                (.put this k (.getValue e))) nil m)))
  (size [this] (.size es))
  (keySet [this]  es)
  (entrySet [this] (InverseEntrySet. a es EAVs))
  (values [this]   (into [] (map (fn [e] (.get ^java.util.Map (.get EAVs e) a))) es))
  (hashCode [this] (hashcode-unordered-coll (.entrySet this))))

(defn ->inverse-map ^InverseMap [a es EAVs make-entity]
  (InverseMap. a es EAVs make-entity))

(defn ->derived-map
  (^DerivedMap [m on-get on-put] (DerivedMap. m on-get on-put))
  (^DerivedMap [m on-get] (->derived-map m on-get (fn  [k v] v))))
