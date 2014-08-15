(ns spork.sim.updates
  (:require [spork.sim [data :as sim]]
            [spork.sim.pure [network :as simnet]]
            [spork.util [general :as gen]]))

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
                        lastupdate]) ;a map of the last update for each entity.

(defn ->update-packet
  "Creates an update packet that contains the time of the future update,
   the entity that requested the update, the type of the update, and the 
   time of the request."
  [update-time requested-by update-type request-time]
  {:update-time update-time 
   :requested-by requested-by
   :update-type update-type 
   :request-time request-time})

(defn elapsed 
  "Computes the time elapsed since the last update for this packet."
  ([update-packet tnow last-update]
    (if (= last-update 0)
      (:request-time update-packet)
      (- tnow last-update)))
  ([update-packet tnow] (elapsed update-packet tnow 0)))

(def empty-updatestore (->updatestore :UpdateStore {} {}))
(defn get-updates
  "Return a list of all requested updates from the updatestore, where 
   utype is a key for updates, and t is a time index.  Updates are represented
   as update-packets."
  [store update-type  t]
  (get-in store [:updates update-type t] {}))

(defn last-update
  "Returns the last time the entity was updated, if ever."
  [store entity-name]
  (get (:last-update store) entity-name))

(defn request-update
  "Schedule an update for requestor, of type request, at"
  [store update-time requested-by update-type trequest]
  (let [pending-updates (get-in store [:updates update-type update-time] {})] 
    (gen/deep-assoc store [:updates update-type update-time]
      (assoc pending-updates requested-by
       (->update-packet update-time requested-by update-type trequest)))))

(defn record-update
  "Returns an update store that reflects the  sucessful updating of an entity x, 
   with an updated last know update time for the entity.."
  [store ent t]
  (let [lastupdate (:lastupdate store)]
    (if (contains? lastupdate ent)
      (let [tprev (get lastupdate ent t)
            tnext (if (> t tprev) t tprev)]  
          (gen/deep-assoc store [:lastupdate ent] tnext)))))

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

;;need to elevate this....
;(defn listen-updates
;  "Register the update store as an event listener with the update source.
;   Probably re-think this guy."  
;  [store source] 
;  (add-listener source (:name ustore) ustore :supply-update)
;  (add-listener source (:name ustore) ustore :demand-update))


