;;A quickly hacked together implementation of a
;;dumb, lazy map based on wrapped a (delay ...)
;;expression that produces a map.  Intended for
;;coarse-grained delay of work, while maintaining
;;seamless interop with clojure's idioms.
(ns spork.data.lazymap)

(defn mapEquals [^clojure.lang.IPersistentMap m1 obj]
  (clojure.lang.APersistentMap/mapEquals m1 obj))

(defmacro with-lazy-map [thunk lm symb & body]
  (let [the-map (with-meta symb {:tag 'clojure.lang.IPersistentMap})]
    `(do (when (not (realized? ~thunk))
           (set! ~lm (force ~thunk)))
       (let [~the-map ~lm]
         ~@body))))

(deftype LazyMap [^clojure.lang.Delay thunk
                  ^:unsynchronized-mutable ^clojure.lang.IPersistentMap m]
  clojure.lang.IHashEq
  (hasheq [this] (with-lazy-map thunk m the-map
                   (.hasheq the-map)))
  (hashCode [this]
    (with-lazy-map thunk m the-map
      (.hashCode the-map)))
  (equals [this o] (clojure.lang.APersistentMap/mapEquals this o))
  (equiv  [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
              :else nil))  
  clojure.lang.IObj
  (meta     [this]    (with-lazy-map thunk m the-map (.meta ^clojure.lang.IObj the-map)))
  (withMeta [this xs] (with-lazy-map thunk m the-map (with-meta  the-map xs)))
  clojure.lang.IPersistentMap
  (valAt [this k] (with-lazy-map thunk m the-map (.valAt the-map k)))
  (valAt [this k not-found]
    (if-let [res (.valAt this k)]
      res
      not-found))
  (entryAt [this k] (with-lazy-map thunk m the-map (.entryAt the-map k)))   
  (assoc [this k v] (with-lazy-map thunk m the-map (.assoc the-map k v)))
  (cons  [this e]   (with-lazy-map thunk m the-map (.cons the-map e)))
  (without [this k] (with-lazy-map thunk m the-map (.without the-map k)))
  clojure.lang.Seqable
  (seq [this] (with-lazy-map thunk m the-map (seq the-map)))
  clojure.lang.Counted
  (count [this] (with-lazy-map thunk m the-map (.count the-map)))
  java.util.Map ;;some of these aren't correct....might matter.
  (put    [this k v]  (.assoc this k v))
  (putAll [this c] (with-lazy-map thunk m the-map (.putAll ^java.util.Map the-map c)))
  (clear  [this] {})
  (containsKey   [this k]
    (with-lazy-map thunk m the-map (.containsKey ^java.util.Map the-map k)))
  (containsValue [this o]
     (with-lazy-map thunk m the-map (.containsValue ^java.util.Map the-map o)))
  (entrySet [this] (with-lazy-map thunk m the-map  
                          (.entrySet ^java.util.Map the-map))) 
  (keySet   [this] (with-lazy-map thunk m the-map 
                         (.keySet ^java.util.Map the-map)))   
  clojure.core.protocols/IKVReduce
  (kv-reduce [this f init]
    (with-lazy-map thunk m the-map
      (reduce-kv  f init the-map))))

(defmacro lazy-map [& expr]
  `(LazyMap. (delay ~@expr) nil)) 

;;caveats:
;;If you call (type ...) on this, or otherwise
;;invoke (meta ...), you will force the map to
;;eval.  Other than that, we should be golden.
;;So, (class ...) is good.
