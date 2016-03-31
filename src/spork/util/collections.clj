;;#Operations for working with mutable references
;;particularly working with pieces of state in a nested associative
;;structure.
(ns spork.util.collections)
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

(definline assoc-any [m k v] 
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

(definline conj-any [coll v] 
  `(cond (instance? clojure.lang.ITransientCollection ~coll)  (.conj (as clojure.lang.ITransientCollection  ~coll) ~v)
         (instance? clojure.lang.IPersistentCollection ~coll) (if (nil? ~coll) (cons ~coll nil)
                                                                  (.cons  (as clojure.lang.IPersistentCollection ~coll) ~v))
         :else (if (nil? ~coll) (doto (java.util.ArrayList.) (.add ~v))
                   (doto (as java.util.Collection ~coll) (.add  ~v)))))

(definline dissoc-t [m k]
  `(.without  (as clojure.lang.ITransientMap ~m) ~k)
  )
(definline dissoc-p [m k]
  `(.without  (as clojure.lang.IPersistentMap  ~m) ~k)
  )
(definline dissoc-m [m k]
  `(doto  (as java.util.Map ~m) (.remove ~k))
  )

(definline dissoc-any [m k] 
  `(cond (instance? clojure.lang.ITransientAssociative ~m) (.without  (as clojure.lang.ITransientMap ~m) ~k)
         (instance? clojure.lang.IPersistentMap ~m)        (.without  (as clojure.lang.IPersistentMap  ~m) ~k)
         :else (doto  (as java.util.Map ~m) (.remove ~k))))

(definline disj-t [m v]
  `(.disjoin (as clojure.lang.ITransientSet ~m)  ~v))
(definline disj-p [m v]
  `(.disjoin (as clojure.lang.IPersistentSet ~m) ~v))
(definline disj-m [m v]
  `(doto  (as java.util.Collection ~m) (.remove ~v)))

(definline disj-any [m v] 
  `(cond (instance? clojure.lang.ITransientSet ~m)   (.disjoin (as clojure.lang.ITransientSet ~m)  ~v)
         (instance? clojure.lang.IPersistentSet ~m)  (.disjoin (as clojure.lang.IPersistentSet ~m) ~v)
         :else (doto  (as java.util.Collection ~m) (.remove ~v))))

(defn assoc-in-any
  "Replacement for assoc-in, works on both transients and persistents."
  [m [k & ks] v]
  (if ks
    (assoc-any m k (assoc-in-any (get m k) ks v))
    (assoc-any m k v)))

(defn into-any 
  ([to from]
   (cond (instance? clojure.lang.IEditableCollection to)
            (with-meta (persistent! (reduce conj-t (transient to) from)) (meta to))
          (instance? clojure.lang.IPersistentCollection to)
            (reduce conj-p to from)
          :else (reduce conj-m to from)))
  ([to xform from]
   (cond (instance? clojure.lang.IEditableCollection to)         
           (with-meta (persistent! (transduce xform conj-t (transient to) from))
             (meta to))
         (instance? clojure.lang.IPersistentCollection to)
           (transduce xform conj-p to from)
         :else (transduce xform conj-m to from))))

(defn update-any
  ([m k f]
   (assoc-any m k (f (get m k))))
  ([m k f x]
   (assoc-any m k (f (get m k) x)))
  ([m k f x y]
   (assoc-any m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc-any m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc-any m k (apply f (get m k) x y z more))))

(defn update-in-any
  "Replacement for update-in, works on both transients and persistents."
  ([m [k & ks] f & args]
   (if ks
     (assoc-any m k (apply update-in-any (get m k) ks f args))
     (assoc-any m k (apply f (get m k) args)))))

(definline contains-any? [m k]
    `(not (identical? (get ~m ~k :not-found) :not-found)))
