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
  (component-domain [x] "Returns the logical domain of the component")
  (component-data   [x] "Returns the values associated with the component."))

(extend-protocol IComponent
  clojure.lang.PersistentArrayMap
  (component-domain [x] (:domain x))
  (component-data [x]   (:data x)))

(extend-protocol IComponent
  clojure.lang.MapEntry
  (component-domain [x] (.key x))
  (component-data [x]   (.val x)))

(defn ->component   [domain data]  (clojure.lang.MapEntry. domain data))
(defn as-component  [domain data]  (->component  domain  data))

;;Entity Definition
;;=================

(defprotocol IEntity 
  "A protocol for defining fundamental operations on entities."
  (entity-name       [e] "Get the unique name of entity e")
  (conj-component    [e c] "Conjoin a component onto the entity")
  (disj-component    [e c] "Disjoin a component from the entity")
  (get-component     [e domain] "Return a component from the entity that matches the domain")
  (entity-components [e] "Get the unique components that define entity e"))

;We define a useful container for entities.  We use this as an initial and 
;intermediate form for querying and computation, but the entity is "really" 
;stored across entries in the entity store's component tables.  Still, 
;it's useful to have a logical association of an entity to its components, 
;particularly when creating entities, or when inspecting them.  We use this 
;to get a reified form of the entity as needed.
(defrecord entity [name components]
  IEntity 
  (entity-name [e] name)
  (conj-component [e c] 
    (entity. name (assoc components (component-domain c) c)))
  (disj-component [e c] 
    (entity. name (dissoc components (component-domain c) c)))
  (get-component [e domain] (get components domain))
  (entity-components [e] components))

;;What we really want here is a flyweight entity container...

;;We can define entity-reductions which allow the dsl to extend for reduce...
;;(entity-merge ent {component val*}) => update the entries in the db via assoc
;;and friends.  Could further optimize via diffing and other stuff.

(extend-protocol IEntity 
  nil 
  (entity-name [n] nil)
  (entity-components [n] nil)
  clojure.lang.PersistentArrayMap
  (entity-name [e] (.valAt e :name))
  (conj-component [e c] 
    (.assoc e :components 
       (assoc (.valAt e :components) (component-domain c) (component-data c))))
  (disj-component [e c]     (.without e (component-domain c)))
  (get-component [e domain] (get (.valAt e :components) domain))
  (entity-components [e] (.valAt e :components))
  clojure.lang.PersistentHashMap
  (entity-name [e] (.valAt e :name))
  (conj-component [e c] 
    (.assoc e :components 
       (assoc (.valAt e :components) (component-domain c) (component-data c))))
  (disj-component [e c]     (.without e (component-domain c)))
  (get-component [e domain] (get (.valAt e :components) domain))
  (entity-components [e] (.valAt e :components)))

(def empty-entity (->entity nil {}))

(defn conj-components
  "Conjoins each component in cs to the components in ent ."
  [ent cs]
  (reduce (fn [e c] (conj-component e c)) ent cs))

(defn build-entity
  "Assembles an entity record from one or more component records."
  [name components]
  (conj-components (->entity name {})  components))

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
  (let [entname (get m :name)]
    (->entity entname 
              (reduce #(assoc %1 (:domain %2) %2) {}
                      (map keyval->component (dissoc m :name))))))
                  
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
     (->entity id
               (reduce-kv (fn [acc k v]
                            (assoc acc k v))
                          (entity-components e1)
                          (entity-components e2))))
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

;EntityStore is the default implementation of our protocol, and it uses maps 
;to maintain records of component data, keyed by entity ID, as well as a map of
;entities to the set of components they are associated with.  
(defrecord EntityStore [entity-map domain-map]
  IEntityStore
  (add-entry [db id domain data]
    (EntityStore. (assoc entity-map id (conj (get entity-map id #{}) domain)) 
                  (assoc-in domain-map [domain id] data)))
  (drop-entry [db id domain]
    (let [cnext (update-in domain-map [domain] dissoc id)
          enext (let [parts (disj (get entity-map id) domain)]
                      (if (zero? (count parts))
                        (dissoc entity-map id)
                        (assoc entity-map id parts)))]
      (EntityStore. enext cnext)))
  (get-entry     [db id domain] (get (get domain-map domain) id))
  (entities [db] entity-map)
  (domains [db]   domain-map)
  (domains-of     [db id]  (get entity-map id))
  (components-of  [db id]  (reduce (fn [acc dom] (assoc acc dom (get-in domain-map [dom id]))) 
                                   {} (get entity-map id)))
  (get-entity [db id] (when-let [comps (.components-of db id)]
                        (entity. id comps)))
  (conj-entity     [db id components] 
      (if (map? components) 
        (reduce-kv (fn [acc dom dat]
                     (.add-entry acc id dom dat))
                   db components)
        (reduce (fn [acc domdat] (.add-entry acc id (first domdat) (second domdat)))
                db components))))

(def emptystore (->EntityStore {} {}))

(defn domain-keys [db] (keys (domains db)))
(defn get-domain [db d] (get (domains db) d))
(defn disj-entity [db id xs] 
  (reduce (fn [acc dom] (drop-entry acc id dom)) db xs))

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

;;__Convenience operations on the entity store__
(defn gete     [store nm k]
  (when-let [e (get-entry store nm k)]
    (val e)))
;;this is a little hackish, we need to remove the mapentry storage, for now it'll work.
(defn assoce   [store nm k v] (add-entry  store nm k (->component k v)))
(defn dissoce  [store nm k]   (drop-entry store nm  k))
(defn mergee   [store nm m]
  (reduce-kv (fn [acc c v]
               (assoce acc nm c v)) store m))
;;==note== we can probably sidestep most of this if we just overload how the store acts like and
;;assoc map.  If we did that, the underlying clojure stuff would work out of the box.
;;stand-inds for nested updates and association.
(defmacro updatee [store nm k f & args]
  `(if-let [entry# (get-entry ~store ~nm ~k)]
     (assoce ~store ~nm ~k (~f (val entry#) ~@args))
     (throw (Exception. (str [:no-entry ~nm ~k])))))

(defmacro update-ine
  [store [nm dom & path] f & args]
  (if (seq path)
    `(if-let [entry# (get-entry ~store ~nm ~dom)]
       `(let [res# (update-in (val entry#) ~(vec path) ~f ~@args)]
          (assoce ~store ~nm ~dom res#)))
    `(updatee ~nm ~dom ~f)))
 
(defmacro assoc-ine
  [store [nm dom & path] v]
  (if (seq path)  
    `(if-let [entry# (get-entry ~store ~nm ~dom)]
       (let [res# (assoc-in (val entry#) ~(vec path) ~v)]
         (assoce ~store ~nm ~dom res#)))
    `(assoce ~store ~nm ~dom ~v)))

(defmacro get-ine
  [store [nm dom & path] v]
  (if (seq path)  
    `(when-let [entry# (get-entry ~store ~nm ~dom)]
       (get-in (val entry#) ~(vec path) ~v))
    `(gete ~store ~nm ~dom)))

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

(defn add-entity 
  "Associate component data with id.  Records are {:component data} or 
  [[component data]] form.  Alternately, add a pre-built entity record."
  ([db id records] (reduce (fn [acc domdat]
                             (add-entry acc id (first domdat) (second domdat)))
                           db records))
  ([db ent] (conj-entity db (entity-name ent) (entity-components ent))))

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

(defn entity-intersection
  "Returns the logical intersection of entities across one or more domains, 
   retuning a set of entity ids, in which each entity is a member of 
   all domains."
  [db domains]
  (let [n (count domains) ;;number we have to have to have intersection...
        ]
    (->> domains 
         (r/mapcat #(entities-in db %))         
         (frequencies) ;;probably slow...
         (reduce-kv (fn [acc k qty]
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

;; (deftype entity-cursor [host id components]
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
    `(let [{:keys [~@cs] :as ~'*components*} (spork.entitysystem.store/entity-components ~e)
           ~@(reduce concat (for [c cs]
                              `(~c (when ~c (val ~c)))))]
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

(defmacro emit-entity-builder [args cs]
  `(fn [~'id ~@(distinct (remove #{'id} args))]    
     (~'spork.entitysystem.store/build-entity ~'id 
       [~@(map binding->component 
               (partition 2 (filter (complement nil?) cs)))])))

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
                         (partition 2 (filter (complement nil?) cs)))))))))

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

;;This is where a flyweight entity would come in handy.
(defn all-entities [ces components]
  (let [entity->record (fn [e] (reduce (fn [acc c]
                                         (assoc acc c (val (get-entry ces e c))))
                                       {:id e} components))]
    (reify
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]
        (clojure.core.protocols/coll-reduce this f1 (f1)))
      (coll-reduce [this f1 init]
        (->>  (entity-union ces components)
              (clojure.core.reducers/map entity->record )
              (reduce f1 init)))
      clojure.lang.Seqable
      (seq [this] (map entity->record (entity-union ces components))))))

(defn only-entities [ces components]
  (let [entity->record (fn [e] (reduce (fn [acc c]
                                         (assoc acc c (val (get-entry ces e c))))
                                       {:id e} components))]
    (reify
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]
        (clojure.core.protocols/coll-reduce this f1 (f1)))
      (coll-reduce [this f1 init]
        (->>  (entity-intersection ces components)
              (clojure.core.reducers/map entity->record)
              (reduce f1 init)))
      clojure.lang.Seqable
      (seq [this] (map entity->record (entity-intersection ces components))))))

;;testing
(comment
  (defn simple [nd & {:keys [transform background]}]
    (let  [cnv (doto (picc/->canvas)
                     (.setPreferredSize (java.awt.Dimension. 600 600)))
           layer (.getLayer cnv)
           _     (when transform (.setTransform layer transform))
           _     (when background (if (picc/node? background)
                                    (picc/add-child! layer nd)
                                    (.setPaint (.getCamera cnv) background)))]
      (picc/add-child! layer nd)
      (picc/show! cnv)))
)



;;Different IEntityStore implementations.  Displaced

(comment

;helper macro -> we can probably use a more general binding here...
(defmacro with-store
  "Evaluate body in the context of bound vars, where entities and components 
   are assoc'd values in the entitystore"
  [store body]
  `(let [~'entities (:entities ~store) 
         ~'components (:components ~store)]
     ~body))



;We can also implement a destructive store....we just keep the state in an 
;atom, and wrap our operations behind it....
(defn ->MutableStore
  "Defines an implementation of IEntitystore whose add/drop operations are 
   destructive.  Maintains an atom of state, to persist the effects of 
   operations.  Reads are non-destructive.  Still conforms to the functional 
   API of the IEntityStore protocol, in that add/drop still return an 
   entitystore, in this case, the same store that was passed in and mutated. 
   We can easily extend this to provide a fluent interface for database-backed 
   stores, which is pretty useful."
  [ents comps] 
  (let [state (atom (->EntityStore ents comps))]
    (reify IEntityStore 
        (has-entity? [db id] (with-store @state (contains? entities id)))
        (get-keys [db] (with-store @state (keys entities)))
        (get-parts [db id] (with-store @state (get entities id #{})))
        (add-entry [db id component data]
          (do 
	          (swap! state (fn [s]
	            (with-store s
	              (EntityStore. 
	                (assoc entities id (conj (get entities id #{}) component)) 
	                (assoc components component
	                       (merge (get components component {}) {id data})))))))
             db)
        (drop-entry [db id component]
           (do
	           (swap! state	 (fn [s]
	             (with-store s
		             (let [cnext (dissoc components component id)
		                   enext (let [parts (disj (get-parts db id) component)]
		                           (if (zero? (count parts))
		                             (dissoc entities id)
		                             (assoc entities id parts)))]
		                   (EntityStore. enext cnext))))))
             db)
        (get-entry [db id component] 
          (with-store @state (get (get components component) id)))
        (get-entities [db] (with-store @state entities)) 
        (get-component [db component] 
          (with-store @state (get components component)))
        (get-components [db] (:components @state))
        (get-domains [db] (keys (:components @state))))))

;While we're at it...let's allow regular maps to recognize our entitystore 
;functions...
(defn make-mapstore [entities components] 
  {:entities entities :components components})

(defn make-mutable 
  "Create an empty mutable entity store, or derive one from an existing 
   store."
  ([] (->MutableStore {} {}))
  ([store] (->MutableStore (:entities store) (:components store))))

(extend-type clojure.lang.PersistentArrayMap 
  IEntityStore
	  (has-entity? [db id] (with-store db (contains? entities id)))
	  (get-keys [db] (with-store db (keys entities)))
	  (get-parts [db id] (with-store db (get entities id #{})))
	  (add-entry [db id component data]
	    (with-store db 
         (make-mapstore  
             (assoc entities id (conj (get entities id #{}) component)) 
             (assoc components component
                (merge (get components component {}) {id data})))))
	  (drop-entry [db id component]
	    (with-store db
		    (let [cnext (dissoc components component id)
		          enext (let [parts (disj (get-parts db id) component)]
		                      (if (zero? (count parts))
		                        (dissoc entities id)
		                        (assoc entities id parts)))]
		      (make-mapstore enext cnext))))
	  (get-entry [db id component] (with-store db 
	                                  (get (get components component) id)))
	  (get-entities [db] (with-store db entities)) 
	  (get-component [db component] (with-store db (get components component)))
	  (get-components [db] (with-store db components))
	  (get-domains [db] (with-store db (keys components))))

)


;;Working on easily defining interface implementations for entity stores.
;; ;;Makes it easy to wrap up our entity store definitions....Maybe useful.  
;; (defmacro wrap-entity-store [es update-expr]
;;   `(IEntityStore
;;     (~'add-entry  [db# id# domain# data#]   (~update-expr db# (~'add-entry  es id# domain# data#)))
;;     (~'drop-entry [db# id# domain#]         (~update-expr db# (~'drop-entry es id# domain#)))
;;     (~'get-entry  [db# id# domain#]         (~'get-entry es id# domain#))
;;     (~'entities   [db#]                     (~'entities es))
;;     (~'domains    [db#]                     (~'domain#s es))
;;     (~'domains-of     [db# id#]             (~'domain#s-of es id#))
;;     (~'components-of  [db# id#]             (~'components-of es id#))
;;     (~'get-entity [db# id#]                 (~'get-entity es id#))
;;     (~'conj-entity     [db# id# components] (~update-expr db# (~'conj-entity es id# components)))))

;; ;ideally, i'd like to just say this... `(gamestate. ~expr monsternum)
;; (defrecord gamestate [store monsternum]
;;   ~@(wrapped-entity-store store (fn [expr] `(gamestate. ~expr monsternum))))
