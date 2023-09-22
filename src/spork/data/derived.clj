(ns spork.data.derived)
;;need a derived set type too.
;;we have a common case where we are storing entries of
;;[k [k v]]] instead of [k v], or some other storage type
;;that we'd like to e.g. deref or treat transparently...
;;since maps operate on entry sets, havinga function that
;;lazily lifts this stuff would be useful in construction.


(defn collhash ^long [xs]
  (reduce (fn [^long acc ^Object x]
            (+ (* 31 acc) (.hashCode x))) 1 xs))

(deftype WrappedEntrySet [^java.util.Map m f]
  java.util.Set
  (add [this v] (throw (ex-info "Read only set" {:in v})))
  (addAll [this coll] (throw (ex-info "Read only set" {:in coll})))
  (remove    [this k] (throw (ex-info "Read only set" {:in k})))
  (removeAll [this]   (throw (ex-info "Read only set" {})))
  (isEmpty [this] (.isEmpty m))
  (clear  [this]  (throw (ex-info "Read only set" {})))
  (size   [this]  (.size m))
  (equals [this that] (or (identical? this that)
                          (clojure.lang.APersistentSet/setEquals this that)))
  (hashCode [this]  (coll-hash (.iterator this)))
  (contains [this ^java.util.Map$Entry v]
    ;;this is weak containmant.  we don't have access to the exact entry from juh, since nodes
    ;;aren't exposed. it depends on how equals is implemented for entries in java land.
    (when-let [res (.get m (.getKey k))]
      (= (f res) (.getValue v))))
  (containsAll [coll]
    (reduce (fn [l v]
              (if-not (.contains this v)
                (reduced false)
                true)) true coll))
  (iterator [this] (.iterator (eduction (map f) entries))))

(deftype DerivedMap [^java.util.Map m ^clojure.lang.IFn v]
  java.util.Map
  (get [this k] (when-let [res (.get m k)]
                  (f res)))
  (get [this k default]
    (if-let [res (.get m k)]
      (f res)
      default))
  (put [this k v] (throw (ex-info "derived map is ReadOnly!" {:in [k v]})))
  (size [this] (.size m))
  (keySet [this]   (.keySet m))
  (entrySet [this] (WrappedEntrySet. m f))
  (values [this]   (vec (eduction (map (fn [^java.util.MapEntry kv] (f (.getValue kv)))) (.entrySet m))))
  (iterator [this]))
