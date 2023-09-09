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
(ns spork.data.passmap
  (:require [spork.data [mutable :as mutable]]
            [spork.util [general :as gen]]
            [ham-fisted.api :as hf]
            [ham-fisted.lazy-noncaching :as ln]))

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
  `(do  (doseq [k# (or (some-set ~db-keys) (keys ~db))]
         (.entryAt ~'this k#)) ;"forces" the join
       (set! ~db-keys #{})
       (set! ~db nil)))

(defmacro get!
  "This is a quick performance hack to allow direct method invocation
   with a type-hinted java.util.Map object.  If we didn't do this,
   using the intermediate let binding, expressions aren't hinted and
   we wind up with reflections.  It may seem like overkill, but
   this ends up being ~3x faster than using clojure.core/get,
   which is still quite fast.  Still, we're on a hot path,
   so I'm using this to provide compatibility between
   persistent maps and hashmaps."
  [m k]
  (let [the-map (with-meta (gensym "the-map")  {:tag 'java.util.Map})]
    `(let [~the-map ~m]
       (.get ~the-map ~k))))


;;ported from clojure.lang.APersistentMap
(defn- map-equiv [this o]
  (cond (not (instance? java.util.Map o)) false
        (and (instance? clojure.lang.IPersistentMap o)
             (not (instance? clojure.lang.MapEquivalence o))) false
        :else (let [^java.util.Map m o]
                (and (= (.size m) (count this))
                     (every? (fn [k]
                               (and (.containsKey m k)
                                    (= (this k) (.get m k)))) (keys this))))))

(deftype PassMap [id
                  ^:unsynchronized-mutable  ^clojure.lang.IPersistentMap m
                  ^:unsynchronized-mutable  ^clojure.lang.IPersistentMap db
                  ;;the original keys in the database, what we're lazily passing through.
                  ^:unsynchronized-mutable  ^clojure.lang.IPersistentSet db-keys
                  ^boolean mutable
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
  (withMeta [this xs] (PassMap. id (with-meta ^clojure.lang.IObj m xs) db db-keys mutable))
  clojure.lang.IPersistentMap
  (valAt [this k]
    (let [^clojure.lang.MapEntry res (.entryAt m k)]
      (if res (.val res)
          (if-let [res  (.get  ^java.util.Map (.valAt db k {}) id)]
            (do (set! m (.assoc m k res))
                res)
          (do (set! m (.assoc m k nil))
              nil)))))
  (valAt [this k not-found]
    (if-let [res (.valAt this k)]
      res
      not-found))
  (entryAt [this k] (if-let [res (.entryAt m k)]
                      res
                      (when-let [k (if  (some-set db-keys) (db-keys k)
                                        k)]
                        (when-let [v (get! (or (get! db k) {}) id)]
                          (do (set! m (.assoc m k v))
                              (clojure.lang.MapEntry. k v)
                              )))))
  (assoc [this k v]   (PassMap. id (.assoc m k v)  db db-keys mutable))
  (cons  [this e]     (PassMap. id (.cons m e)     db  db-keys mutable))
  (without [this k]   (PassMap. id (.without m k) (if-not mutable (.without db k) db) (.disjoin db-keys k) mutable))
  clojure.lang.Seqable
  (seq [this] (concat (seq m)
                      (filter identity
                              (map (fn [^java.util.Map$Entry e]
                                     (if (.containsKey ^clojure.lang.IPersistentMap m (.getKey e))
                                       nil
                                       (.entryAt this (.getKey e)))) db))))
  clojure.lang.Counted
  (count [this]      (do (when db (join! db-keys db)) (.count m)))
  java.util.Map ;;some of these aren't correct....might matter.
  (put    [this k v]  (.assoc this k v))
  (putAll [this c] (PassMap. id (.putAll ^java.util.Map m c) db db-keys mutable))
  (clear  [this] (PassMap.  id {} nil #{} mutable))
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
    (#_reduce-kv
     gen/kvreduce (fn [acc k v]
                    (if (.containsKey ^clojure.lang.IPersistentMap m k)
                      acc
                      (if-let [^clojure.lang.MapEntry e (.entryAt this k)]
                        (f acc (.key e) (.val e))
                        acc))) (#_reduce-kv gen/kvreduce f init m) db)))

;;lame aux function to convert a nested map (aev datastore in our model)
;;into mutable map of mutable maps.
;;note - we can delay mutation too, under this model we pay for every one.
;;we can delay until we write and leave the persistent maps alone
;;until we actually mutate them.  need to see what kind of
;;cost model we're looking at too.  if it costs more to mutate
;;the aev map, then we go back to maintaining a mutable local hashmap
;;and a local keyset. hmm
(defn mutable2d [m]
  (let [hm (hf/mut-hashtable-map m)]
    (reduce-kv (fn [acc k v]
                 (assoc! acc k (hf/mut-hashtable-map v))) hm hm )))

;;semantics for a mutable passmap are that we willingly mutate the underlying
;;data structure (the db).  so we have a map of {e {a v}} and a set of #{a1 a2 a3...} keys
;;that tells us which entries the entity has.  In this case, we have more of a reference
;;that we can mutate directly.  Downside is that lookups and updates are in a nested map.
;;Is this faster than having a local mutable hashmap?  Probably since we don't need to
;;compute joins and stuff....
;;this represents a cursor into the underlying 2d map.
(deftype PassMapMut [id
                     ^:unsynchronized-mutable  ^java.util.Map db
                     ;;the original keys in the database, what we're lazily passing through.
                     ^:unsynchronized-mutable  ^java.util.Set db-keys
                     ^:unsynchronized-mutable   _meta]
  clojure.lang.IHashEq  ;;probably not going to be used....
  (hasheq [this]   (hash-unordered-coll (.seq this)))
  (hashCode [this] (clojure.lang.APersistentMap/mapHash this))
  (equals [this o] (or (identical? this o)
                       (clojure.lang.APersistentMap/mapEquals this o)))
  #_
  (equiv  [this o] (or (identical? this o) (map-equiv this o)))
  clojure.lang.IObj
  (meta     [this]    _meta)
  (withMeta [this xs] (set! _meta xs) this)
  clojure.lang.ITransientAssociative2
  clojure.lang.ITransientMap
  (valAt [this k]
    (when-let [inner (db k)]
      (inner id)))
  (valAt [this k not-found]
    (if-let [inner (db k)]
      (inner id not-found)
      not-found))
  (entryAt [this k]
    (when-let [inner ^clojure.lang.ITransientAssociative2 (db k)]
      (.entryAt inner id)))
  (assoc [this k v]
    (if (db-keys k)
      (let [inner (db k)]
        (set! db (assoc! db k (assoc! inner id v))))
      (do (set! db-keys (conj! db-keys k))
          (set! db      (assoc! db k (hf/mut-hashtable-map (hf/obj-ary id v))))))
    this)
  (conj  [this e]
    (cond (instance? java.util.Map$Entry e)
            (.assoc this (key e) (val e))
          (vector? e)
            (.assoc this (e 0) (e 1))
            :else (try (reduce (fn [_ kv] (.assoc this (key kv) (val kv))) nil e)
                       (catch Exception e
                         (throw (ex-info "conj expectes a MapEntry, a 2-element vector, or a collection of MapEntries"
                                         {:caught e})))))
    this)
  (without [this k]
    (when (db-keys k)
      (let [inner (db k)]
        (let [res (dissoc! inner id)]
          (when (zero? (count res)) (set! db (dissoc! db k))))))
    this)
  clojure.lang.Seqable
  (seq [this] (map (fn [k] (clojure.lang.MapEntry. k (.get ^java.util.Map (db k) id))) db-keys))
  clojure.lang.Counted
  (count [this]   (.size db-keys))
  java.util.Map ;;some of these aren't correct....might matter.
  (put    [this k v]  (.assoc this k v) v)
  (putAll [this c]
    (reduce (fn [_ kv] (.conj this kv )) nil c))
  (clear  [this]
    (set! db-keys (.clear db-keys))
    (set! db (.clear db))
    this)
  (containsKey   [this k] (and (db-keys k) true))
  (containsValue [this o]
    (reduce (fn [acc k]
              (if (= (.valAt this k) o)
                (reduced true)
                acc)) false db-keys))
  (entrySet [this]  (set (.seq this)))
  (keySet   [this]  db-keys)
  clojure.core.protocols/IKVReduce
  (kv-reduce [this f init]
    (reduce (fn [acc k]
              (f acc k (.valAt this k))) init db-keys)))

;;we want to implement a couple of variants of passmaps.
;;simplest variant is to preserve existing semantics, but use a mutable store for the entity entries

(defn lazy-join
  ([source k] (PassMap. k {} source #{} false))
  ([source k keyset]  (PassMap. k {} source keyset false)))

;;quick hack to allow wrapped hashmaps; re-uses the existing
;;passmap code.  Doesn't alter the underlying hashmap on .dissoc.
(defn lazy-join-mutable
  ([source k] (PassMap. k {} (mutable/hashmap->mutmap source) #{} true))
  ([source k keyset]  (PassMap. k {} (mutable/hashmap->mutmap source) keyset true)))

;;testing 

;;the purpose of having a reference like this...
;;is that we have a row into a columnar db...
;;The current entity-based idiom is that we
;;have a persistent reference to the rows in the db.
;;We can munge on that reference all we want, then
;;commit the result after we're done.  For the single-threaded
;;version, this works fine.

;;If we creata a mutable reference, what does it mean to modify
;;fields on the ref?  If we assoc a new field, do we modify the
;;underlying db?  Allow adding column entries?  Allow removing
;;column entries?  This is turning into an in-memory database..
;;with similar semantics.  Naive implementation just says:
;;mutate the entries directly.  It's up to the caller to
;;implement said functionality over top of this  responsibly.

;;Looking at a mutable view of entries in a database...debating
;;whether this fits our semantics...Currently, assumes non-concurrent
;;access...so this "can" work.

;;assuming we're not doing concurrent access...
;;just using mutation as an optimization.
;;we don't need locks and the like.
;;alternately...
;;we can have a model where new fields are appended to the
;;entity ref...dropped fields are mutably removed....
;;updated fields are passed through?

;;In the case where we're operating on a single entity,
;;I think this works okay.

;;Alternately, we just disallow row operations and
;;force everything to delegate to the store via
;;add/remove entry?





;;Updated thoughts on buffering the underlying db...
;;Semantics follow:
;;We maintain the local cache, the db, and a set of db-keys.
;;  If a key is dropped from the db,
;;     future assoc/dissoc should be routed to the cache...
;;     the db-keys should remove the dropped key.
;;  If a key is added,
;;     the key should be modified in the cache.
;;  IF a key is read
;;     the key should be looked up in the cache,
;;     then - if not dropped from the db -
;;      looked up in the db.

;;So, if we add an extra bit of info
