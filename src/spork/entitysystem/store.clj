;;An implementation of an entity store, based on an entity-component
;;architecture.    Might rename this to "CENTS" "Component Entity System"
(ns spork.entitysystem.store
  (:require [clojure.core.reducers :as r]))

;A component-based architecture is a collection of Domains, Systems, Components,
;and entities.  
;
;The central idea behind the architecture is to find a way to decompose the 
;traditional hierarchical, intermixed data and functionality present in many 
;object-oriented designs along orthogonal, domain-specific lines.  
;In the existing simulation framework for Marathon, entities are instances of 
;unitdata or demanddata classes.  They have a significant amount of 
;functionality associated with (and stuck IN) the class.  There is a sense of 
;modularity in the sense that unit policy, behavior, etc. are also objects with 
;independent functions.  However, the design suffers from extensibility due to
;the encapsulation of domain-specific data and functionality, particularly where 
;more than one domain may be accessed.   We have to define the relation between 
;unitdata and policy explicitly in the unitdata class…indiciating that all 
;unitdata objects (and hence unit entities) must have a policy.  We also see 
;data duplication due to encapsulation inside of classes.  Encapsulation also 
;limits the visibility of state data (by design) unless everything is made 
;public.  No longer can we push around collections of primitive data, we must 
;access public members of a class instance (even if we just need a simple 
;container for the class data).  As a result, our notion of classes will become 
;harder to maintain and extend over time….even with less-primitive OOP 
;facilities than in VBA, developing a deep inheritance hierarchy in an OOP 
;system inexorably leads to an inflexible and unwieldy design.
;
;A domain is an orthogonal aspect of the simulated environment upon which 
;reality is partitioned and modeled. To describe a component-based architecture, 
;we probably need to know what components are.   We’ll do a bit of mutual 
;recursion, and rely on another definition to give insight into what a component 
;is.  Components are derived from Domains….so what’s a Domain? Domains are the 
;orthogonal aspects of a simulated environment upon which the environment is 
;partitioned and modeled.   In other words, domains represent clear boundaries 
;between functionality, and enable us to abstract a complicated, tangled 
;environment into a set of unique domains with clearly defined operations for
;each domain, and across said domains.  Domains represent our understanding of
;the unique properties of the environment. 
;
;A component is a collection of functionally-similar, or domain-specific data 
;(traditionally not functions or methods!)  Strictly speaking, components are 
;domain-specific data.   Components derive their meaning from domains because 
;they represent an implicit encoding of domain-specific information.  We can 
;infer from the set of components, which domains are covered by the environment, 
;without having to explicitly state the domains.  This allows for a grass-roots 
;approach to design, in which domains are defined in small pieces, and composed.  
;The smaller and more specific the component, the more obvious the domain 
;context, and the easier it is to factor out duplicate functionality into truly 
;unique data (which reinforces our desire to maintain orthogonal domains).
    
;components define a unique domain, and some data associated with the domain.
;in most setups, data is statically typed, so that the components are homogenous
;we'll follow that practice, but nothing is technically stopping us from having
;heterogenous data across a component.
(defprotocol IComponent 
  (component-domain- [x] "Returns the logical domain of the component")
  (component-data-   [x] "Returns the values associated with the component."))

(defn component-domain [^clojure.lang.Indexed obj] (.nth obj 0))
(defn component-data [^clojure.lang.Indexed obj]   (.nth obj 1))

(extend-protocol IComponent
  clojure.lang.PersistentArrayMap
  (component-domain [x] (:domain x))
  (component-data [x]   (:data x)))

(extend-protocol IComponent
  clojure.lang.MapEntry
  (component-domain [x] (.key x))
  (component-data [x]   (.val x)))

(definline ->component   [domain data]  `(clojure.lang.MapEntry. ~domain ~data))
(defn as-component  [domain data]  (->component  domain  data))

;;Entity Definition
;;=================

(defprotocol IEntity 
  "A protocol for defining fundamental operations on entities."
  (entity-name       [e] "Get the unique name of entity e")
  (conj-component    [e c] "Conjoin a component onto the entity")
  (disj-component    [e c] "Disjoin a component from the entity")
  (get-component     [e domain] "Return a component from the entity that matches the domain")
  (entity-components [e] "Get the unique components that define entity e")
  (entity-domains  [e] "Return the domains the entity is defined over."))

(defprotocol IAlteredKeys
  "A protocol for finding changes in an entity reference, for efficient committing."
  (altered-keys [e]))

;;TODO- lazily load components so the entity
;;type is more of a cursor.  For now, the
;;performance hit isn't significant for the
;;workload...

;;For lazy loading of components...
;;we know the entity has domains in the store..
;;initially the store is our backing map...
;;We can build the cache incrementally on read..
;;when we do a valat, we lookup the component in the
;;background...

;;a lazy reference to an entity; basically a lazy
;;map.  Derived from a backing entity store; fields
;;are only accessed on-demand, and cached.  Modified
;;fields are stored for differential purposes.  This
;;lets us use the entityref as a flyweight accessor
;;for limited sets of components, but having access
;;to all the components registered to the entity.
;;Maintains a set of altered keys....note that
;;altered keys are shared amongst descendants.
;;So, we can tell, upon commitment, which keys to update.
;;If a key is altered and absent in the entity map,
;;then we dissoc it from the store.  If it's present
;;in the map, then we assoc it.  This should save
;;us a significant portion of time when we go
;;through the map-based update/commit cycle.  Reads
;;don't cost us anything on commit; we only commit
;;entries linear in the size of assoc/dissoc on the
;;map reference.

;;As we read values, we populate m implictly.  If we assoc values, we populate m.  if we dissoc, we
;;dissoc from m and (or we assoc a sentinel onto m?)
;;Worst case, we load values from db lazily.  This way, we don't build the entire map at once if
;;we don't need it.  Can we leverage any properties to encode the map so we already have information
;;localized?

(defprotocol IGetmap (getmap [obj]))

;;note sure is we want to do this...
;;a lot of times, we only want to 


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
(deftype passmap [id ^:unsynchronized-mutable ^clojure.lang.IPersistentMap m
                  ^clojure.lang.IPersistentMap db
                  ]
  ;; clojure.lang.IHashEq
  ;; (hasheq [this]   (if realized
  ;;                    (.hasheq ^clojure.lang.IHashEq m)
  ;;                    (set! m 
  ;; (hashCode [this] (.hashCode ^clojure.lang.IHashEq m))
  ;; (equals [this o] (or (identical? this o) (.equals ^clojure.lang.IHashEq m o)))
  ;; (equiv [this o]
  ;;   (cond (identical? this o) true
  ;;         (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
  ;;         (or (instance? clojure.lang.Sequential o)
  ;;             (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
  ;;             :else nil))  
  clojure.lang.IObj
  (meta     [this]    (.meta ^clojure.lang.IObj m))
  (withMeta [this xs] (passmap. id (with-meta ^clojure.lang.IObj m xs) db))
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
                      (when-let [^clojure.lang.MapEntry res (.entryAt ^clojure.lang.IPersistentMap (.valAt db k {}) id)]
                        (do (set! m (.assoc m k (.val res)))
                            (clojure.lang.MapEntry. k (.val res))))))
  (assoc [this k v]   (passmap. id (.assoc m k v)  db))
  (cons  [this e]     (passmap. id (.cons m e) db))
  (without [this k]   (passmap. id (.without m k) (.without db k)))
  clojure.lang.Seqable
  (seq [this] (concat (seq m)
                      (filter identity
                              (map (fn [^clojure.lang.MapEntry e]
                                     (if (contains? m (.key e))
                                       nil
                                       (.entryAt this (.key e)))) db))))                             
  clojure.lang.Counted
  (count [coll]       (.count m))
  java.util.Map
  (put    [this k v]  (.assoc this k v))
  (putAll [this c] (passmap. id (.putAll ^java.util.Map m c) db))
  (clear  [this] (passmap.  id {} {}))
  (containsKey   [this o] (throw (Exception. "containsKey not supported")))
  (containsValue [this o] (throw (Exception. "containsValue not supported")))
  (entrySet [this] (.entrySet ^java.util.Map m))
  (keySet   [this] (.keySet ^java.util.Map m))
  IGetmap
  (getmap [this] m)
  )

(defn lazy-join [source k] (passmap. k {} source))
(definline has-entry? [m k]
  (let [m (with-meta m {:tag 'clojure.lang.IPersistentMap})]
    `(.entryAt ~m ~k)))

;;if you've only got a few fields, a lil is probably
;;okay....chances are we're not
;;So, some things I've learned...hashsets are pretty fast
;;for lookup/membership, but not necessarily traversal.
;;using set-reduce makes them faster though (they're much
;;faster than maps for lookups...so using naive persmaps
;;isn't a great idea, stick with sets...)

;;Note: since we lookup entities a lot...So far, I'm
;;idiomatically pulling entities together here.
;;Note: we only really care about dropped fields...
;;Associng costs us...since we end up path-copying and
;;stuff.  So, ideally we trade reads for writes...Read
;;more, write (copy) less...For the persistent data structures
;;this is fairly necessary.
(deftype entity [^:unsynchronized-mutable name
                 ^:unsynchronized-mutable domains
                 ^:unsynchronized-mutable components
                 ^clojure.lang.IPersistentMap m
                 ^clojure.lang.IPersistentSet altered
                 ]
  clojure.lang.IHashEq
  (hasheq   [this]   (.hasheq   ^clojure.lang.IHashEq m))
  (hashCode [this]   (.hashCode ^clojure.lang.IHashEq m))
  (equals   [this o] (or (identical? this o) (.equals ^clojure.lang.IHashEq m o)))
  (equiv    [this o]
    (cond   (identical? this o) true
            (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
              :else nil))  
  clojure.lang.IObj
  (meta     [this] (.meta ^clojure.lang.IObj m))
  (withMeta [this xs] (entity. name domains components
                               (with-meta ^clojure.lang.IObj m xs) altered))
  IEntity 
  (entity-name [e] (if name name
                       (do (set! name (.valAt m :name))
                           name)))
  (conj-component [e c] 
    (entity. name nil nil (.assoc m (component-domain c) (component-data c))
                          (.cons altered (component-domain c))))
  (disj-component [e c] 
    (entity. name domains components (.without m (component-domain c))
                                     (.cons altered (component-domain c))))
  (get-component [e domain]          (.valAt m domain))
  (entity-components [e]  (if components components
                              (do (set! components (into [] (seq  m)))
                                  components)))
  (entity-domains [e] (if domains domains
                          (do (set! domains (keys m))
                              domains)))
  clojure.lang.IPersistentMap
  (valAt [this k] (.valAt m k))
  (valAt [this k not-found] (.valAt m k not-found))
  (entryAt [this k] (.entryAt m k))
  (assoc [this k v]   (entity. nil nil nil (.assoc m k v)   (.cons altered k)))
  (cons  [this e]   
    (entity. nil nil nil (.cons m e)
             (if (map? e) (into altered (keys e))
                 (.cons altered (key e)))))
  (without [this k]   (entity. nil nil nil (.without m k) (.cons altered k)))
  clojure.lang.Seqable
  (seq [this] (seq m))
  clojure.lang.Counted
  (count [coll] (.count m))
  java.util.Map
  (put    [this k v]  (.assoc this k v))
  (putAll [this c] (entity. nil nil nil (.putAll ^java.util.Map m c)  m))
  (clear  [this] (entity. nil nil nil {} #{}))
  (containsKey   [this o] (.containsKey ^java.util.Map m o))
  (containsValue [this o] (.containsValue ^java.util.Map m o))
  (entrySet [this] (.entrySet ^java.util.Map m))
  (keySet   [this] (.keySet ^java.util.Map m))
  (get [this k] (.valAt m k))
  ;(equals [this o] (.equals ^java.util.Map m o))
  (isEmpty [this] (.isEmpty ^java.util.Map m))
  (remove [this o] (.without this o))
  (values [this] (.values ^java.util.Map m))
  (size [this] (.count m))
  clojure.core.protocols/IKVReduce
  (kv-reduce [coll f init] (reduce-kv f init m))
  clojure.core.protocols/CollReduce
  (coll-reduce [coll f init] (reduce m f init))
  (coll-reduce [coll f] (reduce m f))
  IAlteredKeys
  (altered-keys [m] (if (identical? altered #{}) nil altered))
  )

(extend-protocol IAlteredKeys
  clojure.lang.PersistentArrayMap
  (altered-keys [m] (keys m))
  clojure.lang.PersistentHashMap
  (altered-keys [m] (keys m)))
  

;;What we really want here is a flyweight entity container...

;;We can define entity-reductions which allow the dsl to extend for reduce...
;;(entity-merge ent {component val*}) => update the entries in the db via assoc
;;and friends.  Could further optimize via diffing and other stuff.


;;I think there's a better basic form than this for components.
;;Components are entries, which is great; However, we want entities
;;to be collections of entries (i.e. maps).
(extend-protocol IEntity 
  nil 
  (entity-name [n] nil)
  (entity-components [n] nil)
  (entity-domains [e] nil)
  clojure.lang.PersistentArrayMap
  (entity-name [e] (.valAt e :name))
  (conj-component [e c] 
    (.assoc e (component-domain c) 
              (component-data c)))
  (disj-component [e c]     (.without e (component-domain c)))
  (get-component [e domain] (.valAt e  domain))
  (entity-components [e] (seq e))
  (entity-domains [e] (keys e))
  clojure.lang.PersistentHashMap
  (entity-name [e] (.valAt e :name))
  (conj-component [e c] 
    (.assoc e (component-domain c) 
              (component-data c)))
  (disj-component [e c]     (.without e (component-domain c)))
  (get-component [e domain] (.valAt e  domain))
  (entity-components [e] (seq e))
  (entity-domains [e] (keys e)))

;(def empty-entity-ref (entityref. nil nil nil {} #{}))
(def empty-entity (entity. nil nil nil {} #{}))


(defn conj-components
  "Conjoins each component in cs to the components in ent ."
  [ent cs]
  (reduce (fn [e c] (conj-component  e  c)) ent cs))

(defn build-entity
  "Assembles an entity record from one or more component records."
  [name components]
  (conj-components (entity. name nil nil {} #{})  components))

(defn keyval->component
  "Converts key/value pairs into components.  Allows a simple shorthand
   for composing entities, in that entity components can be contained in 
   a map."
  ([k v]
    (cond (and (satisfies? IComponent v) (not (map? v)))  v
          (and (map? v) 
               (contains? v :domain) 
               (contains? v :components))
          (->component (:domain v) (:components v))
          :else (->component k v)))
  ([keyval] (keyval->component (first keyval) (fnext keyval))))

(defn entity-from-map [m]
  "Allows shorthand definition of entities from simple map structures.
   Keys in the map correspond to component domains of the entity."
  (entity. nil nil nil m #{}))
                  
(defn entity->components
  "Retrieve the component data associated with an entity."
  [ent]  (entity-components ent))

(defn entity->domains
  "Retrieve the domains that the entity is a member of, where 
   domains are the names of components."
  [ent] (keys (entity-components ent)))

(defn entity->entries
  "Converts the entity into a sequene of records, which can be easily added to
   an entity store.  This is the entity in its non-reified form, distributed
   across one or more components."
  [ent]
  (let [id (get ent :name)]
    (reduce-kv 
         (fn [acc k v] (conj acc [id k v])) 
         []
         (entity-components ent))))

;;Fix this variadic arg.  It's slowdown city.
(defn merge-entity
  "Similar to the merge operation on maps, only for entities.  When entities 
   are merged, the components of the first entity are conjoined with the second, 
   and a 'merged' entity is produced. The merged entity has components across 
   the union of domains from both entities.  The component data in each domain 
   corresponds to the last component data found.  This means entity2's data will 
   be returned in cases where the domains overlap."
  ([e1 e2 id]
   (let [m (into {}
                     (concat (entity-components e1)
                             (entity-components e2)))]
     (entity. id nil nil m (set (keys m)))))
  ([e1 e2] (merge-entity  e1 e2 (keyword (gensym "merged")))))
  
(defn ent-seq? [entcoll] (satisfies? IEntity (first (seq entcoll))))
            
(defn merge-entities 
  "Merges multiple entities.  For overlapping domains, the right-most, or last 
   entity's data is returned in the merge."
  [entcoll & {:keys [id] :or {id :merged-entity}}] 
  (if (ent-seq? entcoll)
    (reduce (fn [e1 e2] (merge-entity e1 e2 id)) entcoll)
    (throw (Exception. "Expected a collection of entities."))))
                    
(defn get-info
  "Get a quick summary of the entity, i.e. its components..."
  [ent]
  [(entity-name ent) (entity->domains ent)])


;;Entities live in Entity Stores
;;==============================

(defprotocol IEntityStore
  "The entitystore is an abstract protocol for defining functionality necessary 
   to manage entities, as defined by collections of component data.  Due to the 
   independent nature of component data (each component maps 1:1 to a specific, 
   orthogonal domain of information, and is self contained), our entity store 
   could be implemented a number of ways.  The obvious implementation is as a 
   single hash-map database on a single machine.  However, we could easily 
   distribute the component maps across multiple nodes, effectively distributing
   our store and allowing for parallel querying/updating functions.  Finally, 
   we could implement the entitystore using a persistent database backend, if it
   makes sense to do that. "
  (add-entry   [db id domain data] "Associate a record of data /w entity id")
  (drop-entry  [db id domain] "Drop record for id from component")
  (get-entry   [db id domain] "Fetch the component record for ent id")
  (domains [db]   "Map of domain -> (id -> component), the database.")
  (components-of  [db id]  "derive components id contains")
  (domains-of     [db id]  "set of domains id intersects")
  (get-entity [db id] "Returns an IEntity associated with id")
  (conj-entity    [db id components] "Add an entity to the database.")   
  (entities [db] "Return a map of {entityid #{components..}}"))

;  (alter-entity [db id f] "Alter an entity's components using f.  f
;  should take an entity and return a set of components that change.")

;;Traversing our component set is much faster this way, about 2x.  weird...
;; (definline i-reduce [f init coll]
;;   (let [xs (with-meta (gensym "xs") {:tag  'clojure.lang.PersistentHashSet})]
;;     `(let [~xs       ~coll
;;            it#      (.iterator ~xs)
;;            bound#   (count ~xs)]
;;        (loop [idx# 0
;;               acc# ~init]
;;          (cond  (reduced?  acc#) @acc#
;;                 (== idx# bound#) acc#
;;                 :else 
;;                 (let [nxt# (.next it#)]
;;                   (recur (unchecked-inc idx#)
;;                          (~f acc# nxt#))))))))

;; (definline set-reduce [f init coll]
;;   (let [xs (with-meta (gensym "xs") {:tag  'clojure.lang.PersistentHashSet})]
;;     `(let [~xs       ~coll
;;            it#      (.iterator ~xs)
;;            bound#   (.count ~xs)]
;;        (loop [idx# 0
;;               acc# ~init]
;;          (cond  (reduced?  acc#) @acc#
;;                 (== idx# bound#) acc#
;;                 :else 
;;                 (let [nxt# (.next it#)]
;;                   (recur (unchecked-inc idx#)
;;                          (~f acc# nxt#))))))))

;; (definline set-reduce2 [f init coll]
;;   (let [xs (with-meta (gensym "xs") {:tag  'clojure.lang.PersistentHashSet})]
;;     `(let [~xs       ~coll
;;            it#      (.iterator ~xs)]          
;;        (loop [idx# 0
;;               acc# ~init]
;;          (cond  (reduced?  acc#) @acc#
;;                 (.hasNext it#)
;;                 (let [nxt# (.next it#)]
;;                   (recur (unchecked-inc idx#)
;;                          (~f acc# nxt#)))
;;                 :else acc#)))))

;;Interesting note:
;;Most entities will actually be very small sets of components.
;;If we go row-based, we get all the data stored, for free, packed
;;and ready to go at the record level.  We can even mutate it.
;;Additionally, we lose the requirement to have a separate index
;;for the entity.  The components are the keys....so...they're
;;precomputed and hashed, and faster to iterate.
;;We can still get select-queries by iterating over rowsets...
;;Changing an entry costs 2x assoc operations, however merges are
;;much much faster, since we can merge the whole entity at once...
;;If we do all our work on the previous entity, then the
;;merge is an O(1) swap out of the new entity for the old record.
;;Otherwise, we go through our assoc overhead updating the entries that
;;changed on merge.  This means we have to keep track of the components
;;that changed.  Note: if we are lazily loading, we get this quality
;;for free though...

;;What are we using the component set for?  Joins...
;;The alternative is to maintain independent sets of component membership
;;and use the indices for joins.  Same difference (we already do this
;;in the columnar version).  Also, if we want access to a single component,
;;or a couple of named components, we can select the columns directly
;;and operate on them.


;;Wonder if we can define keypaths...
;;for instance, they use :path/to/blah
;;as a keyword...
;;dunno how they cache the path for it...

;EntityStore is the default implementation of our protocol, and it uses maps 
;to maintain records of component data, keyed by entity ID, as well as a map of
;entities to the set of components they are associated with.  
(defrecord EntityStore [^clojure.lang.IPersistentMap entity-map
                        ^clojure.lang.IPersistentMap domain-map]
  IEntityStore
  (add-entry [db id domain data] ;;this gets called a lot....
    (let [doms  (.valAt entity-map id #{})]
      (if (doms domain)
        (EntityStore.
          entity-map
          (.assoc ^clojure.lang.Associative domain-map domain ^clojure.lang.IPersistentMap (assoc (.valAt domain-map domain {}) id data)))
        (EntityStore.
         (.assoc ^clojure.lang.Associative entity-map id
                 ^clojure.lang.PersistentHashSet (.cons ^clojure.lang.PersistentHashSet doms domain))
         (.assoc ^clojure.lang.Associative domain-map domain
                 ^clojure.lang.IPersistentMap (assoc (.valAt domain-map domain {}) id data))))))
  (drop-entry [db id domain]
    (or  (when-let [^clojure.lang.PersistentHashSet ent  (.valAt entity-map id)]
           (when (ent domain) ;entity exists, and has the domain...
             (let  [^clojure.lang.IPersistentMap m     (.valAt domain-map domain)
                    cnext (.assoc ^clojure.lang.Associative domain-map domain
                                  (.without m id))
                    enext (if (== (.count ent) 1)
                            (.without entity-map id)
                            (.assoc ^clojure.lang.Associative entity-map id (disj ent domain)))]
               (EntityStore. enext cnext))))
         db))
  (get-entry     [db id domain]
    (when-let [m (.valAt ^clojure.lang.IPersistentMap  domain-map domain)]
      (.valAt ^clojure.lang.IPersistentMap m id)))
  (entities [db]  entity-map)
  (domains [db]   domain-map)
  (domains-of     [db id]  (.valAt entity-map id))
  (components-of  [db id]
    ;; (set-reduce (fn [^clojure.lang.IPersistentMap acc dom]
    ;;               (.assoc acc dom
    ;;                       (.valAt ^clojure.lang.IPersistentMap (.valAt domain-map dom)
    ;;                               id)))  ;;get-in is slowish; changed to direct method calls.
    ;;             {} (get entity-map id))
    (lazy-join domain-map id)
    )  
  ;;We want to avoid large joins....hence, getting an entity reference that lazily loads and
  ;;caches values, so we only have to pay for what we load.
  (get-entity [db id]
    (when-let [comps (.components-of db id)]
      (entity. id nil nil comps #{})))
  (conj-entity     [db id components] 
      (if (map? components) 
        (reduce-kv (fn [^EntityStore acc dom dat]
                     (.add-entry acc id dom dat))
                   db components)
        (reduce (fn [^EntityStore acc domdat] (.add-entry acc id (first domdat) (second domdat)))
                db components))))

;;Beginnings of a mutable entity store.  Note: this is about 6x faster than our
;;persistent counterpart, but it's 
(deftype MapEntityStore [^java.util.HashMap entity-map
                         ^java.util.HashMap domain-map]
  IEntityStore
  (add-entry [db id domain data] ;;this gets called a lot....
    (let [^java.util.HashSet
          domains (or (.get entity-map id)
                      (let [res (java.util.HashSet.)
                            _ (.put entity-map id res)]
                        res))
          ^java.util.HashMap
          component (or (.get domain-map domain)
                        (let [res (java.util.HashMap.)
                              _  (.put domain-map domain res)]
                          res))]
      (do (when (not (.contains domains domain)) (.add domains domain))
          (.put component id data)
          db)))
  (drop-entry [db id domain]
    (if-let [^java.util.HashSet e (.get entity-map id)]
      (do (if (not (== 1 (.size e)))
            (.remove e domain)
            (.remove entity-map id))
          (let [^java.util.HashMap comps (.get domain-map domain)]
            (if (not (== 1 (.size comps)))
              (.remove comps id)
              (.remove domain-map domain)))
          db)
      (throw (Exception. (str "entitystore does not contiain an entry for " id)))))
  (get-entry     [db id domain]
    (when-let [^java.util.HashMap comps (.get domain-map domain)]
      (.get comps id)))
  (entities [db]  entity-map)
  (domains [db]   domain-map)
  (domains-of     [db id]  (.get entity-map id))
  (components-of  [db id]
    (lazy-join domain-map id)
    )  
  ;;We want to avoid large joins....hence, getting an entity reference that lazily loads and
  ;;caches values, so we only have to pay for what we load.
  (get-entity [db id]
    (when-let [comps (.components-of db id)]
      (entity. id nil nil comps #{})))
  (conj-entity     [db id components])
  clojure.lang.IDeref
  (deref [this] {:entity-map  entity-map
                 :domain-map domain-map})
  )

(comment 
;;rather than maintaining the domains explicitly, we can lazily compute them
;;on demand.  This could amortize our costs.  We can relook the domains
;;upon dirty entities.  If we're not adding to entities, most of the time
;;we're not caring about domains.  If we make a lot of changes, we can
;;update dirty domain info.
;;Maintain entity information in a recordset, rather than multiple tables...
(deftype RecordEntityStore [^java.util.HashMap   entity-map
                            ^java.util.HashMap   domains
                            ^java.util.ArrayList dirty]
  IEntityStore
  (add-entry [db id domain data] ;;this gets called a lot....
    (let [^java.util.HashMap  e (or (.get entity-map id)
                                         (let [res (java.util.HashMap.)
                                               _  (.put entity-map id res)]
                                           res))]
      (do 
          (.put e domain data)
          db)))
  (drop-entry [db id domain]
    (if-let [e (.get entity-map id)]
      (do (.remove ^java.util.HashMap e domain)
          db)
      (throw (Exception. (str "entitystore does not contiain an entry for " id)))))
  (get-entry     [db id domain]
    (when-let [ comps (.get entity-map id)]
      (.get ^java.util.HashMap comps domain)))
  (entities [db]  entity-map)
  (domains [db]   entity-map)
  (domains-of     [db id]  (keys (.get entity-map id)))
  (components-of  [db id]
    (.get entity-map id)
    )  
  ;;We want to avoid large joins....hence, getting an entity reference that lazily loads and
  ;;caches values, so we only have to pay for what we load.
  (get-entity [db id]
    (when-let [comps (.components-of db id)]
      (entity. id nil nil comps #{})))
  (conj-entity     [db id components])
  clojure.lang.IDeref
  (deref [this] {:entity-map  entity-map
                 })
  )
)

(def emptystore (->EntityStore {} {}))

(defn snapshot! [store]
   (reduce-kv (fn [acc domain m]
                (reduce-kv (fn [acc id data]
                            (add-entry acc id domain data)) acc m))
              emptystore (.domain-map store)))

(extend-protocol clojure.core.protocols/IKVReduce
  java.util.HashMap 
  (kv-reduce [obj f init]
    (reduce (fn [acc ^java.util.Map$Entry ent]
              (f acc (.getKey ent) (.getValue ent))) init obj)))

(declare mutate!)
(defn memptystore
  ([] (MapEntityStore. (java.util.HashMap.) (java.util.HashMap.)))
  ([store] (mutate! store)))

(defn mutate! [store]
  (reduce-kv (fn [acc domain m]
               (reduce-kv (fn [acc id data]
                            (add-entry acc id domain data)) acc m))
             (memptystore)
             (:domain-map store)))

(defn domain-keys [db]   (keys (domains db)))
(defn get-domain  [db d] (get (domains db) d))
(defn disj-entity [db id xs] 
  (reduce (fn [acc dom] (drop-entry acc id dom)) db xs))

;;can we create custom entities at runtime using reify?
;;How slow is this?  Fast enough for individual queries?
;;Another strategy is to create field-accessible entity
;;views...maybe cache the types....dunno, these are additional
;;optimizations that may or may not help us out....

;;The easiest thing to do is fire-and-forget though....
;;We have a read-cache of mutable values that we hit.
;;If our value is there, we're done.  If not,
;;we check the backing store.  If it exists, we
;;store it in the read-cache.  If not, we store it
;;in the read-cache as nil.
;;From there on, the read-cache helps us quickly deref
;;items - and store them, since associng goes onto the
;;read cache.
;;When we go to merge, any items that are in the read
;;cache that are also altered (currently tracked in the
;;altered cache) get merged as novel updates.  Additionally,
;;items in the read cache that are nil get dropped if
;;they were altered....
;;as if we dropped them from the component itself.
;;I think this satisfies the lazy loading requirement, and
;;the lazy commit requirement...
;;This also works with a mutable version, since we just
;;replace the readcache with a mutable map.

;;Todo
;;Replace this with more efficient algorithm.  Right now,
;;we're removing entry by entry.  It's probably faster to
;;dissoc the domain from the store, and walk all effected entities,
;;or better yet, collect all effected entities in batch,
;;then walk them (and dissoc the domains from the store).
(defn drop-domains [ces ds]
  (reduce  (fn [acc d]
             (reduce-kv (fn [acc ent _]
                          (drop-entry acc ent d))
                        acc (get-domain acc d)))
           ces ds))

(defn drop-domain [ces d]  (drop-domains ces [d]))

;; (comment ;testing

;; (def db (conj-entity emptystore 2 {:age 22 :name "some-entity"}))
;; (domains db) ;=>      {:age {2 22} :name {2 "some-entity"}}
;; (get-domain  db :age) => [:age {2 22}]
;; (get-entities db) => {2 #{:age :name}}

;; (get-entity db 2) => {id 2 {:age 22 :name "some-entity"}}
;; (domains-of db 2)    => #{:age :name}
;; (components-of db 2) => {:age 22 :name "some-entity"}

;; (conj-entity db  2     {:age 22 :name "some-entity"})
;; (drop-entity db 2) => {:entities {} :components {}}
;; (add-entry db  2    :age 22)  
;; (drop-entry db 2    :age 22) 
;; )

;;These are the primary operations associated with entities...
;;Basically, having a managed 2dimensional map for us.
;;__Convenience operations on the entity store__
(defn gete     [store nm k]
  (when-let [e (get-entry store nm k)]
    e))
;;this is a little hackish, we need to remove the mapentry storage, for now it'll work.
(defn assoce   [store nm k v] (add-entry  store nm k v ))  ;(->component k v)))
(defn dissoce  [store nm k]   (drop-entry store nm  k))
(defn mergee   [store nm m]
  (reduce-kv (fn [acc c v]
               (add-entry acc nm c v)) store m))
;;==note== we can probably sidestep most of this if we just overload how the store acts like and
;;assoc map.  If we did that, the underlying clojure stuff would work out of the box.
;;stand-inds for nested updates and association.
(defmacro updatee [store nm k f & args]
  `(if-let [entry# (get-entry ~store ~nm ~k)]
     (assoce ~store ~nm ~k (~f entry# ~@args))
     (throw (Exception. (str [:no-entry ~nm ~k])))))

(defmacro update-entity [store nm f & args]
  `(if-let [e# (get-entity ~store ~nm)]
     (add-entity ~store (~f e# ~@args))))

(defmacro update-ine
  [store [nm dom & path :as xs] f & args]
  (if (seq path)
    `(if-let [entry# (get-entry ~store ~nm ~dom)]
       (let [res# (update-in  entry# ~(vec path) ~f ~@args)]
         (assoce ~store ~nm ~dom res#))
       (throw (Exception. (str [:invalid-entry ~xs]))))
    `(updatee ~store ~nm ~dom ~f)))
 
(defmacro assoc-ine
  [store [nm dom & path] v]
  (if (seq path)  
    `(if-let [entry# (or (get-entry ~store ~nm ~dom) {})]
       (let [res# (assoc-in  entry# ~(vec path) ~v)]
         (assoce ~store ~nm ~dom res#)))
    `(assoce ~store ~nm ~dom ~v)))

(defmacro get-ine
  [store [nm dom & path] & v]
  (if (seq path)  
    `(when-let [entry# (get-entry ~store ~nm ~dom)]
       (get-in entry# ~(vec path) ~@v))
    `(gete ~store ~nm ~dom)))

(def entity-at get-entity)

;protocol-derived functionality 

(defn reduce-entries
  "Mechanism for updating the entity store.  
   Fold function f::store -> [id component data] -> store 
   over a sequence of records, with store as its initial value."
  [store f recs] 
  (reduce (fn [store rec] (f store rec)) store recs))

(defn drop-entries
  "Fold a sequence of [id component data] using drop-record to return 
   an updated entitystore."
  [store drops]
  (reduce-entries store (fn [s [id c & more]] (drop-entry s id c)) drops))

(defn add-entries 
  "Fold a sequence of [id component data] using add-entry to return 
   an updated entitystore."
  [store adds]
  (reduce-entries store (fn [s [id c data]] (add-entry s id c data)) adds))

(defn add-parameter
  "Add a simple name and value to a dedicated parameters table.  We treat this 
   like any other component, except we provide explicit functionality to access
   it....for semantic purposes.  Parameters usually have a special meaning."
  [store name val]
  (add-entry store name :parameters val))
  
(defn drop-parameter
  "Add a simple name and value to a dedicated parameters table.  We treat this 
   like any other component, except we provide explicit functionality to access
   it....for semantic purposes.  Parameters usually have a special meaning."
  [store name]
  (drop-entry store name :parameters))

(defn add-altered-entity 
  "Associate component data with id.  Records are {:component data} or 
  [[component data]] form.  ."
  [db ^clojure.lang.IPersistentMap ent]
  (let [id (entity-name ent)]
    (reduce (fn alteration [acc k]
              (if-let [v (.valAt ent k)] ;entry exists.
                (assoce acc id k v)
                (dissoce acc id k) ;component has been dissoced
                ))
            db (altered-keys ent))))

(def en (atom nil))
(def store (atom nil))
;;What about records?
;;we can add support for maps here...
(defn add-entity 
  "Associate component data with id.  Records are {:component data} or 
  [[component data]] form.  Alternately, add a pre-built entity record."
  ([db id records]
   (if (map? records)
     (reduce-kv (fn [acc dom dat]
                 (add-entry acc id dom dat)) db records)
     (reduce (fn [acc domdat]
               (add-entry acc id (first domdat) (second domdat)))
             db records)))
  ([db ^clojure.lang.IPersistentMap ent]
   (if-let [altered (altered-keys ent)]
     (let [id (entity-name ent)
           _  (reset! en ent)
            ]
       (reduce (fn alteration [acc k]
                 (do 
                   (reset! store acc)
                   (try 
                     (if-let [^clojure.lang.MapEntry e (.entryAt ent k)] ;entry exists.
                       (assoce acc id k (.val e)) ;alteration added or updated.
                       (dissoce acc id k) ;component has been dissoced
                       )
                     (catch Exception e (throw (Exception. (str [:domain k :ent ent :err e])))))))
               db altered))
      db)))

(defn add-entities
  "Register multiple entity records at once..." 
  [db entities] (reduce add-entity db entities))

(defn drop-entity
  "drop component data associated with id, and id from entities in db." 
  [db id]
  (reduce (fn [acc dom] (drop-entry acc id dom)) db (domains-of db id)))

(defn drop-entities 
  [db xs]
  (reduce (fn [acc id] (drop-entity acc id)) db xs))

(defn entity-count
  "Return the count of unique entity ids in the entitystore."
  [db] (count (entities db)))

(defn entity-data
  "Query the entitystore for the components associated with entity id."
  [db id] (components-of db id))

(defn entities-in
  "Returns a sequence of entity ids that are members of a domain.
   Specifically, each entity has component data in the domain."
  [db d]
  (keys (get-domain db d)))

(defn get-entities 
  "Reify multiple entity records from ids from entitystore."
  [db ids]
  (map #(get-entity db %) ids))

(defn entity-seq
  "Return a lazy sequence of reified entity records for each entity in the 
   entitystore."
  [db] 
  (for [id (keys (entities db))]
    (get-entity db id)))

(defn entity-union
  "Returns the logical union of entities across one or more domains, 
   retuning a set of entity ids, in which each entity is a member of 
   one or more domains."
  [db domains]
  (->> domains 
    (map #((comp set entities-in) db %)) 
    (reduce clojure.set/union)))

;;this is putting us on the critical path, and it's slow as-is.
;;Coercing to sets and computing intersection is apparently slower
;;than we'd like.  Since we use this A LOT, we want other ways to
;;determine intersection...One way is to hash-and-count...
;; (defn entity-intersection
;;   "Returns the logical intersection of entities across one or more domains, 
;;    retuning a set of entity ids, in which each entity is a member of 
;;    all domains."
;;   [db domains]
;;   (->> domains 
;;     (map #((comp set entities-in) db %)) 
;;     (reduce clojure.set/intersection)))


;;we could have this return a reducer instead of building the
;;transient collection, eliminates the need for a transient.
;;the only problem we have is..
;;we get a reducible collection of
;; {k xs}

;;we want to count collections...
;;so [k | k <- (k,v), (k,v) <- kvps, where (count v) = n]
;;can we fold this? or do we kv reduce it...
;;frequencies creates a transient map too.

(defn entity-intersection
  "Returns the logical intersection of entities across one or more domains, 
   returning a set of entity ids, in which each entity is a member of 
   all domains."
  [db domains]
  (let [n (count domains) ;;number we have to have to have intersection...
        ]
    (->> domains 
         (r/mapcat #(entities-in db %)) ;concated rediucible seq of all the keys in each component, entitiy ids.         
         (frequencies) ;probably slow...
         (reduce-kv (fn [acc k qty] ;this is actually faster than r/filtering...
                      (if (== qty n)
                        (conj! acc k)
                        acc))
                    (transient [])
                    )         
         (persistent!))))

(defn key->symbol [k]
  (symbol (subs (str k) 1)))

(defn kvps->binds [m]
  (loop [acc nil
         ks (keys m)]
    (if (seq ks) 
      (recur (apply conj acc (let [k (first ks)]
                               `(~(get m k) ~(key->symbol k))))
             (rest ks))
      acc)))

(defn entity-binds [e]
  (kvps->binds (merge (entity-components e) {:name (entity-name e)})))
                 
(defmacro with-entity
 "Macro to allow binding of specific components"
 [ent & body]
    `(let [~@(entity-binds (eval ent))]
      ~@body))      

(defn default
  "If x is nil, returns y.  Used for implementing default values."
  [& [x y & rest]]
  (if x x y))

;I'd like to have a high-level abstraction for querying entities...
;If we treat the entity store as a database, our components are tables.
;Rows/records are the entities in the component's table.


;where should be a function on the entity....not the component.
;if select does its job, the resulting set will be a sequence of reified 
;entities 
  
;When we select entities from an entity store, our building blocks are 
;components.  Components are simple 2-column tables of [entityID data].
;Any predicates used for filtering must be phrased in terms of component
;and entity relationships. 

;Example query -> find the entity nearest to a coordinate, whose name is 
;NOT "Bob".
;(defn get-nearest-visible [id store]
;  (let [fromcoords (->> (get-entity store id) :components :coords)]
;    (->> store
;      (select-entities :from [named visible coordinates] ;intersection.
;                       :where #(and visible
;                                    (not= named "Bob")
;                                    (in-range? coordinates fromcoords))
;                       :order-by (fn [e1 e2] 
;                                   (euclidean coordinates fromcoords))))))

;in peter's work, and in a real db, from tells us which tables to join.
;where applies a predicate to each row in the joined table, indicating if
;the record should be excluded. 
;order-by indicates how to order of the final resulting recordset.

;the entitystore is a simplified datastore.  We can use relational semantics 
;for selection, projection, etc, since we have primary keys in every table. 
;The simplification is that each table is really just a 2-column recordset, 
;where the first field is always the entity's id, or the primary key. 
;The second field can be any kind of data (likely nested, possibly atomic).
;The simplified records are represented as a key-value pair. 
;So the entity store is a simple key-value store. 


;;if we have a flyweight entity, it'd be
;;the id, a pointer to the ces, and any cached components. we currently have.
;;upon reading a component, we update the cache.
;;note: another option is that we develop a cursor type specifically for the entitystore.
;;this provides field-level access to the entity.
;; (deftype entity-cursor [host id components cache]
;;   (

;Porting the SQL-like language in Peter Seibel's excellent Practical Common Lisp
(defn select-entities
  "Acts like a SQL select, in which components are analogous to single-column
   tables of data.  Filters the results by where predicate, optionally sorts 
   query if order-by is supplied. Values are automatically distinct due to the 
   nature of the entity store (e.g. component records are unique relative to the 
   entity).  Returns a (sub)set of the component data."  
  [store & {:keys [from join-by where order-by] :or {from (domain-keys store)
                                                join-by :intersection
                                                where nil
                                                order-by nil}}]
  (let [joinfunc (cond (= join-by :intersection) entity-intersection 
                       (= join-by :union)  entity-union
                       :else join-by)]                       
  (reduce (fn [acc f] (if (nil? f) acc (f acc))) 
      (get-entities store (joinfunc store (if (coll? from) from [from]))) ;returns ids
      [(when where 
         (fn [es] (filter where es))) 
       (when order-by
         (fn [es] (sort-by order-by es)))])))

(defn entity-reducer
  "Intermediate function to build traversable sequences of entity records.  Can be coerced 
   to a seq, a reducer, or a KVReducible.  If no components are supplied, defaults to 
   returning all of the entity's components. get-ids may be a sequence of entity ids, or 
   more typically, a function of ces->components->[id] "
  ([get-ids ces] (entity-reducer get-ids ces nil))
  ([get-ids ces components]
   (let [entity->record (if (seq components)
                          (fn [id] (reduce (fn [acc c]
                                             (assoc acc c  (get-entry ces id c)))
                                           {:id id} components))
                          (fn [id] (reduce (fn [acc c]
                                             (assoc acc c (get-entry ces id c)))
                                           {:id id} (domains-of ces id))))
         get-ids (cond (fn? get-ids) get-ids
                       (or (identical? get-ids :*)
                           (identical? get-ids :all)) (fn [ces _] (keys (entities ces)))
                       (seq get-ids) (fn [ces components] (vec get-ids))
                       :else (throw (Exception.
                                     (str ("get-ids must be a seq of entity ids, a function, or :*/:all ;"
                                           get-ids)))))]
     (reify
       clojure.core.protocols/IKVReduce
       (kv-reduce [amap f init]
         (->>  (get-ids ces components) ;produces a reducible/foldable vector.
               (reduce (fn [acc id]
                         (f acc id (entity->record id))) init)))
       clojure.core.protocols/CollReduce
       (coll-reduce [this f1]
         (clojure.core.protocols/coll-reduce this f1 (f1)))
       (coll-reduce [this f1 init]
         (->>  (get-ids ces components) ;produces a reducible/foldable vector.
               (clojure.core.reducers/map entity->record )
               (reduce f1 init)))
       clojure.lang.Seqable
       (seq [this] (map entity->record (get-ids ces components)))
       clojure.lang.Counted
       (count [this] (count (get-ids ces components)))))))

(defn all-entities
  "Select entities that have all components"
  [ces components]
  (entity-reducer entity-union ces components))

(defn only-entities
  "Select entities that have only the specified components"
  [ces components]
  (entity-reducer entity-intersection ces components)) 

(defn select-store [store & {:keys [from join-by where order-by] 
                             :or {from (domain-keys store)
                                  join-by :intersection
                                  where nil
                                  order-by nil}}]
  (->> (select-entities  store 
         :from from :join-by join-by :where where :order-by order-by)
       (add-entities emptystore)))

;;build queries on this...
(defmacro with-components [e cs & body]
  (let [cs (mapv (fn [c] (if (keyword? c) (symbol (subs (str c) 1)) c)) cs)]
    `(let [{:keys [~@cs] :as ~'*components*}  ~e]
           ;~@(reduce concat (for [c cs]
           ;                   `(~c (when ~c (val ~c)))))]
       ~@body)))

(defmacro defcomponent
  "Macro to define a new component (for use in specifying entity templates and 
   general convenience).  Defined components get a namespace-local constructor 
   prefixed with ->  Allows definition of complex constructors for the generic 
   ->component record type.  Args are evaluated in the context of body when 
   a defined component is evaluated, which allows for parameterizing of 
   components.

   Usage:  
  (defcomponent basicstats [{:keys [health agility strength] :as stats}] stats)"
  ([name args body]                       
    `(defn ~name ~args 
       (->component ~(keyword name) ~body)))
  ([name docstring args body]
    `(defn ~name ~docstring ~args 
       (->component ~(keyword name) ~body))))

;It'd also be nice to easily define functions that dispatch on components...
(defn binding->component [expr] 
   (if (keyword? (first expr)) 
     `(~'keyval->component ~(first expr) ~(second expr))
      expr))

;; (defmacro emit-entity-builder [args cs]
;;   `(fn [~'id ~@(distinct (remove #{'id} args))]    
;;      (~'spork.entitysystem.store/build-entity ~'id 
;;        [~@(map binding->component 
;;                (partition 2 (filter
;;                              (complement nil?)
;;                              cs)))])))

(defmacro emit-entity-builder [args cs]
  `(fn [~'id ~@(distinct (remove #{'id} args))]    
     (~'spork.entitysystem.store/build-entity ~'id 
       [~@(map binding->component 
               (partition 2 
                             
                             cs))])))

(defn spec-merger [args specs]
  `(fn ~args
     (merge-entities (map (fn ~'[s]  
                            (if (fn? ~'s) (~'s ~'id) ~'s)) ~specs) :id ~'id)))

(defn flatten-args [args]
  (loop [acc []
         xs args]
    (if (empty? xs) 
      acc
      (let [x (first xs)]      
        (cond (= x '&)    (recur acc (rest xs))        
              (map? x)    (recur (into acc (get x :keys)) (rest xs))
              (vector? x)
                  (recur (into acc (flatten-args x)) (rest xs))
              :else  (recur (conj acc x) (rest xs)))))))

;specs are either [symbol]
;or [(entity-builder args)]
;where symbol is a function of one arg, ID, 
;entity-builder is a function of more than one arg, to be threaded an ID...
;
(defmacro emit-complex-entity-builder [args specs cs]
  (let [newargs (into ['id] (distinct (remove #{'id} args)))
        flatargs (flatten-args newargs)]
    `(let [specbuilder#  (eval (spec-merger (quote ~flatargs) (quote ~specs)))]             
       (fn ~newargs
         (conj-components (specbuilder# ~@flatargs)  
            (list ~@(map binding->component 
                         (partition 2      cs)
                                    )))))))

;macro to define functions for building stock templates for entities
;allows us to define namespaced functions to build default entities easily.
;I could probably borrow from CLOS here....but I'm not there yet...it'd be 
;nice to have specs belong to pseudo classes and inheritance, but it's 
;really not "that" necessary....all they're doing is defining a set of 
;components for us.  It might behoove us to push this off into a pure data 
;representation as well...Actually, the constructor built by defspec actually
;"looks" like a record anyway...
(defmacro entity-spec
  "Helper macro for composing anonymous entity-building functions.  We can 
   inline these, or formally define them as with defspec.
   
   Formal components declared by defcomponent must be declared ahead of time,
   but they may be parameterized in the body using arguments supplied by args.
   Component parameterization is not mutually recursive, thus there is currently 
   no shared lexical environment between components.
     
   Inlined or ad-hoc components  can be evaluated as bindings of the form 
   [:component-name component-data] , where component-name is a keyword.
   
   Usage, with defined components and one inline component called playertag:

   (entity-spec [id h] 
      [basicstats {:health h :agility 30 :strength 30}
       offense 10
       visage (str The remnant of a lost age, standing alone against evil)
       coords {:x 0 :y 0}
       :playertag :player1]
       )   
   Yields a lambda, (fn [id] ...) which will create entities with the
   specified components.  If no arguments are supplied, a single id arg 
   will be inserted."
  ([args specs components]
    (if (empty? specs)
      `(~'spork.entitysystem.store/emit-entity-builder ~args ~components)
      `(~'spork.entitysystem.store/emit-complex-entity-builder ~args ~specs ~components)))
  ([args components]
      `(~'spork.entitysystem.store/emit-entity-builder ~args ~components)))

(defn entity-doc
  "Function for auto-documenting entity constructors."
  [{:keys [args specs components] :as m} ]
  (str "A function for generating" 
       (when (:name m) (str " " (:name m) " ")) "entities. "
       "From " args " to an entity with components " 
       (into [] (map #(keyword (first %))
            (partition 2 components)))
       (if (empty? specs) nil (str "inheriting from " specs))))

(defn valid-spec?
  "Determines if map m has the minimal keys necessary for an entity 
   specification."
  [m]
  (and (map? m) 
       (contains? m :args)
       (or (contains? m :specs) 
           (contains? m :components))))

(defn split-by [pred coll]
  (reduce (fn [[ex leftover] x]
            (if (pred x)
              [(conj ex x) leftover]
              [ex (conj leftover x)])) [[] []] coll))

;;We should accept this only: 
;;[name doc args {:keys [specs components]}]
;;[name args {:keys [specs components]}]
;;Invariants: 
;;Last arg must define a mixture.
;;Mixtures are maps.
;;First arg is name

(defn entitydec 
  "Parses entity declarations.  Helper for defentity.
   Processes a list of args, converting into a corresponding entity 
   specification.  Allows flexibility for having optional args.  Supports 
   things like docstrings and such.  Args are of the form 
   [args? docstring? specs? components], where ? indicates
   optional arguments."
  ([name doc args specs components]
     (entitydec name doc args {:specs specs :components components}))
  ([name doc args mix]
     (assert (symbol? name))
     (assert (string? doc))
     (assert (vector? args))
     (assert (map? mix))
     (let [{:keys [specs components]} mix
           m {:name name :args args :specs specs :components components}]
       (assoc m :doc (entity-doc m))))
  ([name args {:keys [specs components] :as mix}]
     (entitydec name "" args mix)))


;;Might be nice to formalize the information or an entity spec.
;(deftype entityspec [name doc args specs components]

(defmacro defentity
  "Allows composition of a set of components into an entity template.  Creates 
   a function in the current namespace, prefixed with 'build-', taking at least
   one argument - id -  that allows for declaration of entities based on the 
   specification.  id will always be the first argument, but callers may 
   declare more arguments that will become part of lexical environment of 
   the entity building function.  This allows parameterization of entity 
   specifications, where component expressions can be further parameterized if 
   desired. 
   
   Formal components declared by defcomponent must be declared ahead of time,
   but they may be parameterized in the body using arguments supplied by args.
   Component parameterization is not mutually recursive, thus there is currently 
   no shared lexical environment between components.
     
   Inlined or ad-hoc components  can be evaluated as bindings of the form 
   [:component-name component-data] , where component-name is a keyword.
   
   Usage, with defined components and one inline component called playertag:

   (defentity player [id] 
     {:components
       [basicstats {:health 30 :agility 30 :strength 30}
        offense 10
        visage (str The remnant of a lost age, standing alone against evil)
        coords {:x 0 :y 0}
        :playertag :player1]}
       )   
   Yields a function (build-player id) which will create player entities.
   If no arguments are supplied, a single id arg will be inserted.
             
   Alternately, new specs can be derived from existing specs.  If a vector of 
   specs is supplied prior to the components, then a spec-merging function is 
   created to initialize each entity.  The merging function will merge each 
   spec into a new entity (possibly causing side-effects), before adding the 
   components below.  Components with identical domains will retain the last 
   value in the final spec, which is more or less how inheritance typically 
   works.  If the spec is an anonymous spec, as defined by entity-spec, then it 
   will work as well.  This should allow variable means to compose entities, 
   as well as overriding pieces of entity construction.

   An example of another entity leveraging the player spec:  
   (defspec computer-player [aitype name]       
     [player] 
     [:playertag :computer
      :ai aitype])
   This yields a function, (computer-player id aitype name) that 
   produces parameterized computer players."   
  [name & doc+args+mix]
    (let [decargs    (into [name] (sort-by #(cond (string? %) 0  
                                                  (vector? %) 1 
                                                  (map? %) 2) doc+args+mix))         
          specmap    (apply entitydec decargs)]
     (if (valid-spec? specmap)
       (let [{:keys [doc args specs components]} specmap
             m (merge (meta name) {:doc doc :arglists (list 'quote (list args))})]
         `(def ~(with-meta name m)  
            (entity-spec ~args ~specs ~components)))
       (throw (Exception. "Entity spec is invalid!")))))

(comment ;testing
  (def cs [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z])
  (def es (for [n (range 50)]
            (into {:name n} (map (fn [c] [c (str  c n)]) cs))))            
                
  (def the-store
    (-> emptystore
        (add-entities es)))

  (let [^clojure.lang.IPersistentMap store {:es {0 #{:a :b :c}}  :cs {:a {0 true} :b {0 true} :c {0 true}}}]
    (time (dotimes [i 1000000] (update store :cs
                                       (fn [m]
                                         (update m :a
                                                 (fn [m] (assoc m 0 false))))))))

  (let [^clojure.lang.IPersistentMap store {:es {0 #{:a :b :c}}  :cs {:a {0 true} :b {0 true} :c {0 true}}}]
    (time (dotimes [i 1000000] (update store :cs
                                       (fn [m]
                                         (update m :a
                                                 (fn [m] (assoc m 0 false))))))))
  
)
