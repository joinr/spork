;;This namespace should be effectively OBE.
(ns spork.sim.updates
  (:require [spork.sim [data :as sim]]
            [spork.sim.pure [network :as simnet]]
            [spork.util [general :as gen]]
            [spork.data.mutable :as mut]))
(defrecord update-packet [update-time requested-by update-type request-time])

(defprotocol IUpdates    
  (get-updates [u update-type  t]   
    "Return a list of all requested updates from the updatestore, where 
     utype is a key for updates, and t is a time index.  Updates are represented
     as update-packets.")
  (last-update [u entity-name]   "Returns the last time the entity was updated, if ever.")
  (request-update [u update-time requested-by update-type trequest]   
    "Schedule an update for requestor, of type request, at")
  (record-update  [u  ent t]   
    "Returns an update store that reflects the  sucessful updating of an entity x, 
     with an updated last known update time for the entity.."))

(declare ->mupdatestore)

;;Note -> assoc is killing us here.
;;Updates are called pretty frequently, so we need to be able to bulk 
;;load them.  One option is to have a transient version of the 
;;update store.  I like this idea.  We can have lazy transients and 
;;convert the structure.

;;The map-based version is great and all, but we probably want 
;;a transient version to handle tons of updates.
;;Also, there are other possible representations.  We may look at 
;;different ways to organize updates in memory....for now 
;;the performance with the transient version is sufficient.  More 
;;profiling is needed to justify moving away from it.


;All the update manager does is listen for update request traffic.
;It records the time of the update request, who made the request, and the future
;update. It's really just a nice way to decouple and centralize the updating 
;information in the sim. This represents a shift from just requesting a time to 
;update the entire system, to a more localized method of updating individual 
;pieces of the system as needed. It also provides a big hook for our observers..
;..and for debugging purposes. The simulation cannot progress without updates, 
;else every bit of state remains static. When we go to sample the state, we see
;no changes without updates.
(defrecord updatestore [name         ;the name of the manager  
                        updates      ;a map of all scheduled updates, by time.
                        lastupdate]  ;a map of the last update for
                                     ;each entity.
  
 IUpdates       
 (get-updates [u update-type  t] (gen/get2 updates update-type t {}))
 (last-update [u entity-name]    (get lastupdate entity-name))
 (request-update [u update-time requested-by update-type trequest]   
   (let [pending-updates (gen/get2 updates update-type update-time {})] 
     (updatestore. name 
                   (->> (update-packet. update-time requested-by update-type trequest)
                        (assoc pending-updates requested-by)
                        (gen/deep-assoc updates [update-type update-time]))
                   lastupdate)))                                              
 (record-update  [u  ent t]   
   (if-let [tprev (get lastupdate ent t)]
     (let [tnext (if (> t tprev) t tprev)]  
       (updatestore. name updates (assoc lastupdate ent tnext)))
     (updatestore. name updates   (assoc lastupdate ent t))))
 clojure.lang.IEditableCollection 
 (asTransient [coll] (->mupdatestore name (gen/transient2 updates)
                                          (transient lastupdate))))

;;Transient Version for fast/batch updates.
;;Note: we can replace this with hashmaps if we want....
;;That may be the preferable way, allow some dynamic var that holds
;;onto the mutation strategy.
(mut/defmutable mupdatestore [name         ;the name of the manager  
                              updates      ;a map of all scheduled updates, by time.
                              lastupdate]  ;a map of the last update for
                                           ;each entity.
  
 IUpdates       
 (get-updates [u update-type  t] (gen/get2 updates update-type t (transient {})))
 (last-update [u entity-name]    (get lastupdate entity-name))
 (request-update [u update-time requested-by update-type trequest]   
   (let [pending-updates (gen/get2 updates update-type update-time (transient {}))] 
     (do (set! updates 
               (->> (update-packet. update-time requested-by update-type trequest)
                    (assoc! pending-updates requested-by)
                    (gen/assoc2! updates update-type update-time))) 
         u)))
 (record-update  [u  ent t]   
   (if-let [tprev (get lastupdate ent t)]
     (let [tnext (if (> t tprev) t tprev)]  
       (do (set! lastupdate (assoc! lastupdate ent tnext)) u))
     (do (set! lastupdate   (assoc! lastupdate ent t)))))
 (conj       [coll e] (throw (Exception. "unsupported op")))
  (persistent [coll]
     (let [realized (persistent! updates)]
       (updatestore. name
                     (reduce-kv (fn [acc k m]
                                  (assoc acc k
                                         (gen/persistent2! m))) {} realized)
                     (persistent! lastupdate)))))

;;#Operations on stores and packets...
(defn elapsed 
  "Computes the time elapsed since the last update for this packet."
  ([^update-packet upacket tnow last-update]
    (if (= last-update 0)
      (.request-time upacket)
      (- tnow last-update)))
  ([update-packet tnow] (elapsed update-packet tnow 0)))

(def empty-updatestore (->updatestore :UpdateStore {} {}))

;Most managers will need a trigger function... 
;We need to find a way to establish "event trigger" behavior for these guys...
;Actually, we can take the existing "trigger" behavior, and just apply it like
;normal...Rather than having a side-effect cause the triggering, we'll have our
;higher order state-transition function route events to interested parties, and 
;fold over the result of their triggering.  As such, "trigger" really defines 
;a routing for specific events.
(defn trigger [store msgid {:keys [entity-to t]}]
  (record-update store entity-to  t))  
 
(defn record-update-handler
  "A handler that assumes an update-store is present in the event context,
   inside the context's state."
  [ctx edata name]
  (gen/deep-update ctx [:updater] 
       record-update (get edata :entity-to) (get edata :t)))

;;#Possibly lift this guy out...
(defn update-request-handler
  "A handler that assumes an update-store is present in the event context,
   inside the context's state.  Passes requests for update to the update-store."
  [ctx edata name]
  (let [{:keys [update-time requested-by update-type trequest]}  edata]
    (gen/deep-update ctx [:updater] 
               request-update update-time requested-by update-type trequest)))

;NOTE: this is too specific, the update mechanism is actually quite general.
;The notion of different "kinds" or channels for updates is externally defined.
;TODO -> may need to add a route for :policy-update
(def routes {:supply-update record-update-handler 
             :demand-update record-update-handler
             :update-request update-request-handler})

(defn add-routes
  "Registers default event-handlers for update events."
  [ctx store] 
  (->> ctx (simnet/register-routes {(:name store) routes})))

;;Update Modification
;;===================
(defn entity-updates
  "Relatively quick hack.  Not intended for mutable updater. Allows us to 
   query the updater for entity-specific updates by id and update-type.
   We typically use sets for filtering on entity."
  ([updater ids update-type?]
  ;;drop the last update.
  ;;drop any events the entities might be involved in.
   (let [res  (atom (transient []))
         updates  (:updates updater)]
     (doseq [[update-type xs] updates
             [t name-evts]    xs
             :when (update-type? update-type)]
       (reduce (fn [acc id]
                 (if-let [update (name-evts id)]
                   (do (swap! acc conj! update)
                       acc)))
               res (case ids :* (keys name-evts) ids)))
     (persistent! @res)))
  ([updater ids]
   (case ids
     :* (entity-updates updater)
     (entity-updates updater ids identity)))
  ([updater]
   (for [[u xs] (:updates updater)
         [t name-evts] xs
         [name u] name-evts]
     u)))

(defn drop-update
  ([updater update]
   (drop-update updater (:update-type  update)
                        (:update-time update)
                        (:requested-by update)))                               
  ([updater update-type update-time requested-by]
   (let [us (get-in updater [:updates update-type update-time])
         us (dissoc us requested-by)]
     (if (zero? (count us))
       (update-in updater [:updates update-type] #(dissoc % update-time))
       (assoc-in updater [:updates update-type update-time] us)))))

(defn drop-entity-updates 
  "Wipes all updates associated with id from the schedule, including 
   last-update."
  [updater id]
  (let [ids (if (seq id) (set id) #{id})]
    (-> (reduce drop-update updater (entity-updates updater ids))
        (update :last-update #(dissoc % id)))))

(defn drop-update-type    
  "Wipes an entire class of updates from the schedule."
  [updater utype]
  (reduce  drop-update updater (entity-updates updater :* #{utype})))
 
;;need to elevate this....
;(defn listen-updates
;  "Register the update store as an event listener with the update source.
;   Probably re-think this guy."  
;  [store source] 
;  (add-listener source (:name ustore) ustore :supply-update)
;  (add-listener source (:name ustore) ustore :demand-update))

;;deprecated

;; (defn request-update
;;   "Schedule an update for requestor, of type request, at"
;;   [^updatestore store update-time requested-by update-type trequest]
;;   (let [updates (.updates store)
;;         pending-updates (get-in updates [update-type update-time] {})] 
;;     (->> (update-packet. update-time requested-by update-type trequest)
;;          (assoc pending-updates requested-by)
;;          (gen/deep-assoc updates [update-type update-time])
;;          (.assoc store :updates))))

;; (defn record-update
;;   "Returns an update store that reflects the  sucessful updating of an entity x, 
;;    with an updated last known update time for the entity.."
;;   [^updatestore store ent t]
;;   (let [lastupdate (.lastupdate store)]
;;     (if (contains? lastupdate ent)
;;       (let [tprev (get lastupdate ent t)
;;             tnext (if (> t tprev) t tprev)]  
;;         (->> tnext
;;              (assoc lastupdate ent)
;;              (.assoc store :lastupdate))))))
