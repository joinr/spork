;;A namespace for defining and working with ephemeral data
;;stored in components in entity stores, per
;;spork.entitysystem.store/IEntityStore.
(ns spork.entitysystem.ephemeral
  (:require
   [spork.util.general :as general]
   [spork.entitysystem.store :as store]))

;;TODO:
;;Perhaps look into mutable cells here instead of
;;atoms.  We can probably get some speed benefits
;;if we use arrays or concurrent maps at some point..

;;Ephemeral Data
;;==============
;;Typically used for storing append-only daily logging
;;information.  Ephemeral data lets us selectively
;;use atoms in components and then persistent them
;;as needed.  This provides a balance between the
;;outward purity of persistent entitystore implementations
;;with a granular ability to define - temporarily
;;- mutable components.

(defn get-ephemeral
  "Given a valid IEntityStore, retreives a pair of 
   [estore' some-atom], where estore' is estore if 
   the component is already an atom (e.g., it exists.)
   and some-atom is the component associated with entity id, 
   else a new atom is created, via the default value supplied, 
   and assoce onto the component for entity id, resulting in 
   'estore."
  ([estore id component default]
   (if-let [state (store/gete estore id component)]
     (if (general/atom? state)
       [estore state] 
       (let [new-state (atom state)]
         [(store/assoce estore id component new-state) new-state]))
     (let [atm (atom default)
           estore (store/assoce estore id component atm)]
       [estore atm])))
  ([estore id component] (get-ephemeral estore id component (transient []))))

(defn conj-ephemeral
  "Returns the result of conjoining a v onto an assumably 
   ephemeral, transient datastucture, associated with entity id, 
   under component, in the IEntityStore estore."
  [estore id component v]
  (let [[estore state] (get-ephemeral estore id component)]
    (swap! state conj! v)
    estore))

(defn reset-ephemeral
  "Returns the result of resetting the atom associated with entity id, 
   under component, in the IEntityStore estore, to v."
  [estore id component v]
  (let [[estore state] (get-ephemeral estore id component)]
    (reset! state v)
    estore))

(defn swap-ephemeral
  "Returns the result of swapping the atom associated with entity id, 
   under component, in the IEntityStore estore, by the function f."
  [estore id component f]
   (let [[estore state] (get-ephemeral estore id component)]
     (swap! state f)
     estore))

(defn some-ephemeral
  "Returns truthy if the ephemeral component exists, and 
   if the count of the ephemeral is positive."
  [estore id component]
  (when-let [atm (store/gete estore id component)]    
    (when (pos? (count @atm)) atm)))


;;Ephemeral Counter
;;=================
;;Typically used for storing append-only daily logging
;;information.

;;Associate an entity with a unique numerical value.
;;Provides an efficient way to get "global" numerical
;;counters while maintaining the ability to persistent
;;at the end of the day.
(defn swap-counter
  "Swaps the atom associated with component :counter, for entity id, 
   in IEntityStore estore, with the function f"
  [estore id f]
   (let [[estore state] (get-ephemeral estore id :counter 0)]
     (swap! state f)
     estore))

(defn inc-counter
  "Increments the atom associated with component :counter, for entity id, 
   in IEntityStore estore."
  ([estore id]
   (let [[estore storage] (get-ephemeral estore id :counter 0)]         
     (swap-counter estore id #(unchecked-inc %))))
  ([estore id n] 
   (swap-counter estore  id #(unchecked-add % n))))

(defn get-counter
  "Returns the :counter component associated with entity id, 
   in IEntityStore estore, else 0. "
  [estore id]
  (or (store/gete estore id :counter) 0))

(defn persist-counters
  "For all entities with a :counter component in IEntityStore estore,
   where the counters are atoms, the components are replaced with 
   the dereferenced value of the atom, effectively turning them 
   persisent."
  [estore]
  (store/filter-map-component estore :counter
      (fn [_ x] (general/atom? x)) ;filter
      deref))                              

(defn get-count
  "Returns the current value for the :counter component 
   of entity id, in IEntityStore estore, if the entity has
   a :counter.  Otherwise, returns 0.  Works with ephemeral 
   atoms and normal numerical components."
  [estore id]
  (if-let [cnt (get-counter estore id)]
    (if (number? cnt) cnt
        (deref cnt))
    0))
