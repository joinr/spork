;;Implementation of an auxillary map that is designed to
;;work with an assumably large database, upon which the
;;fields and values of the map are drawn.  This is
;;designed specifically for the use-case of a
;;column-based entitystore, where columns are
;;mappings of entityID to a particular field value.
;;Still, this will work with any abstract mapping,
;;where the "database" is an associative structure
;;of the type {field {id field-value}}, such that
;;(get some-passmap field) will use the backing
;;store to find the field associated with the entity -
;;basically perform a join.
;;As an optimization, we defer joins until necessary,
;;and we cache joins into a persistentmap.  "updates"
;;like assoc, dissoc, etc,. manifest as operations on
;;the cached map, so that the lazy-map will, over time,
;;build up a new map on-demand, and avoid joining all
;;fields of the entity.
(ns spork.data.passmap)

;;We care about adds and drops....right?
;;If we just always assoc a sentinel value when we merge the map,
;;like ::dropped, we can check for it on entity merge...
;;Original design had the entity map diffing based on the fields.
;;We could alternately use a different type of mapentry...
;;Or maintain a hashmap of altered fields, and how they're altered.
;;Having a single mutable hashmap that's shared by all ancestors
;;of the fields could be useful....
;;Basically, since the entity was created, we have a journal of
;;all the modifications that occurred relative to the initial
;;pull....Can we handle dissocing better?  Maybe, explicitly
;;handle dissocing....if we assoc ::drop to the map then
;;on merge it'll get dropped....maybe wrap an acessor around it?
;;Most of the time, we just assoc nil anyway, but sometimes we
;;dissoc...we could always dissoc the db....to indicate inconsistency
;;with the original db, so that on merge, we can compute a diff.
;;alternately, we can define adds and drops...
;;right now, m is === adds, we don't keep track of drops.
;;If we drop a field and add it later, then what...
;;Is there a better way to do a diff?
;;We know from the fields in m which fields were actually
;;read.  We also know, which fields have changed if we
;;keep track of altered.  Currently, we infer that if
;;there's an altered field and not a field in m, that the
;;alteration implies a drop....

;;a passthrough map...a map that references a db of fields in the background to implement its map operations.
;;if we make the assumption that the db contains all possible fields for m, then db is just
;;the means for computing fields in m.  When we drop items from the passmap,
;;we assoc a sentinel value to it, rather than dropping outright (note...we're still
;;paying the cost of associating....)

;;implementing hashing for this guy.  Hashing will force us to flush the
;;backing map, effectively building up the entire map.  At that point, we
;;don't need the backing map anymore...  So it gets ditched.  The new map
;;now fully shadows/contains the old.

;;Cool..for map implementations, we can cheat and use APersistetMap
;;to carry us through...
(defn mapEquals [^clojure.lang.IPersistentMap m1 obj]
  (clojure.lang.APersistentMap/mapEquals m1 obj))
        

(defmacro some-set [x]
  `(if (identical? ~x #{}) nil
       ~x))

(defmacro join! [db-keys db]
  `(do (doseq [k# (or (some-set ~db-keys) (keys ~db))]
         (.entryAt ~'this k#)) ;"forces" the join
       (set! ~db-keys #{})
       (set! ~db nil)))

(deftype PassMap [id
                  ^:unsynchronized-mutable  ^clojure.lang.IPersistentMap m
                  ^:unsynchronized-mutable  ^clojure.lang.IPersistentMap db
                  ;;the original keys in the database, what we're lazily passing through.                  
                  ^:unsynchronized-mutable  ^clojure.lang.IPersistentSet db-keys
                  ]
  clojure.lang.IHashEq
  (hasheq [this]   (if-not db (.hasheq ^clojure.lang.IHashEq m)
                       ;;we need to go ahead and do an eager join with the db.
                           (do  (join! db-keys db)
                                (.hasheq ^clojure.lang.IHashEq m))))
  (hashCode [this]
    (if-not db (.hashCode ^clojure.lang.IHashEq m)
                       ;;we need to go ahead and do an eager join with the db.
            (do  (join!  db-keys db)
                 (.hashCode ^clojure.lang.IHashEq m))))
  (equals [this o] (clojure.lang.APersistentMap/mapEquals this o))
  (equiv  [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
              :else nil))  
  clojure.lang.IObj
  (meta     [this]    (.meta ^clojure.lang.IObj m))
  (withMeta [this xs] (PassMap. id (with-meta ^clojure.lang.IObj m xs) db db-keys))
  clojure.lang.IPersistentMap
  (valAt [this k]
    (let [^clojure.lang.MapEntry res (.entryAt m k)]
      (if res (.val res)
        (if-let [res  (.valAt  ^clojure.lang.IPersistentMap (.valAt db k {}) id)]
          (do ;(println :caching k)
              (set! m (.assoc m k res))
              res)
          (do ;(println :nilcache)
              (set! m (.assoc m k nil))
              nil)))))                            
  (valAt [this k not-found]
    (if-let [res (.valAt this k)]
      res
      not-found))
  (entryAt [this k] (if-let [res (.entryAt m k)]
                      res
                      (when-let [k (if  (some-set db-keys) (db-keys k)
                                         k)]                                        
                        (when-let [^clojure.lang.MapEntry res (.entryAt ^clojure.lang.IPersistentMap (.valAt db k {}) id)]
                          (do (set! m (.assoc m k (.val res)))
                              (clojure.lang.MapEntry. k (.val res))                              
                              )))))
  (assoc [this k v]   (PassMap. id (.assoc m k v)  db db-keys))
  (cons  [this e]     (PassMap. id (.cons m e)     db  db-keys))
  (without [this k]   (PassMap. id (.without m k) (.without db k) (.disjoin db-keys k)))
  clojure.lang.Seqable
  (seq [this] (concat (seq m)
                      (filter identity
                              (map (fn [^clojure.lang.MapEntry e]
                                     (if (contains? m (.key e))
                                       nil
                                       (.entryAt this (.key e)))) db))))
  clojure.lang.Counted
  (count [this]      (do (when db (join! db-keys db)) (.count m)))
  java.util.Map ;;some of these aren't correct....might matter.
  (put    [this k v]  (.assoc this k v))
  (putAll [this c] (PassMap. id (.putAll ^java.util.Map m c) db db-keys))
  (clear  [this] (PassMap.  id {} nil #{}))
  (containsKey   [this k]
    (or (.containsKey ^java.util.Map m k)
        (and db
             (when-let [k (if  (some-set db-keys) (db-keys k)
                               k)]            
               (.containsKey ^java.util.Map db k)))))
  (containsValue [this o] (throw (Exception. "containsValue not supported")))
  (entrySet [this]   (do  (when db (join!  db-keys db))
                          (.entrySet ^java.util.Map m))) 
  (keySet   [this]   (do (when db (join!  db-keys db)) 
                         (.keySet ^java.util.Map m)))   
  clojure.core.protocols/IKVReduce
  (kv-reduce [this f init]
    (reduce-kv (fn [acc k v]
                 (if (contains? m k)
                   acc
                   (if-let [^clojure.lang.MapEntry e (.entryAt this k)]
                     (f acc (.key e) (.val e))
                     acc))) (reduce-kv f init m) db))
  )


(defn lazy-join
  ([source k] (PassMap. k {} source #{}))
  ([source k keyset]  (PassMap. k {} source keyset)))

;;testing 
