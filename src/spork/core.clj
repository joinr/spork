;;Currently empty.  I may include a simple script to build and install all the 
;;dependent libraries, or tie in some simple user interface stuff.  Who knows.
(ns spork.core)
;;##Todo
;;Deprecate spork.util.gui
;;Unify cljgui.
;;Isolate externally-dependent modules, spork.incanter, spork.util.excel, 
;;spork.cljgraph, and strip out or modularize apache pio, incanter, 
;;jung dependencies.  

;;Possibly resolve the search parameters from spork.data.search with the 
;;stuff in spork.opt.  I think there's some overlap there and opportunity for
;;re-use.

(defn read-request  [id] [:read id])
(defn write-request [id state] [:write id state])
(defn msg-type [xs] (first  xs))
(defn msg-id [xs]   (second xs))
(defn msg-data [xs] (nth    xs 2)) 

(defn acquire? [msg] (= (msg-type msg) :acquire))
(defn update-state [state n]	(assoc-in state [:state] n))

(defn handle-message [env msg]
  (do 
    (println msg)
    (case (first msg)
      :init  (msg-data msg)
      :read  (if (not (contains? env :locked)) 
                 (assoc env :response (:state env))
                 (let [pending (promise)]
                   (-> env 
                     (update-in [:requests] conj [:read pending])
                     (assoc :response pending))))
      :acquire (if (not (contains? env :locked))
                 (-> env 
                   (assoc :locked  (msg-id msg))
                   (assoc :response :lock-acquired))
                 (let [pending (promise)]
                   (-> env 
                     (update-in [:requests] conj [:acquire pending 
                                                  (msg-id msg)])
                     (assoc :response pending))))
      :release  (if (not= (:locked env) (msg-id msg))
                    (assoc env :response (Exception. (str "Wrong ID")))
                    
                    (loop [acc (dissoc env :locked)
                           rs  (:requests env)]
                      (if-let [r (first rs)]
                        (if (acquire? r) 
                          (do (send *agent* handle-message r)
                            (assoc acc :requests (rest rs)))
                          (let [pending (second r)]
                            (do (deliver pending (:state env))
                              (recur acc (rest rs)))))
                        (merge acc {:requests []
                                    :response :released}))))
      :write (if (= (:locked env) (msg-id msg))               
               (-> env 
                 (update-state (msg-data msg))
                 (assoc  :response :wrote))))))				 

(def the-lock 
  (let [agt (agent {})]
    (send agt handle-message [:init nil {:state 0 :requests []}])))
                              
(defmacro awaiting-response [agt msg]
  `(let [res#   (send-off ~agt ~'handle-message ~msg)]
     (do  (await ~agt)
          (force (:response (deref res#))))))
(defn promise? [x]
  (= (type x) clojure.core$promise$reify__6322))

(defn read-state [agt id]
   (awaiting-response agt [:read id]))
(defn get-lock [agt id]
   (awaiting-response agt [:acquire id]))
(defn write-state [agt id data]
  (awaiting-response agt [:write id data]))
(defn release-lock [agt id]
  (awaiting-response agt  [:release id]))               

(defn report! [agt ]  
  (let [t (System/currentTimeMillis)
        res (read-state agt t)]
   [t :state (if (promise? res) (deref res) res)]))

(defn reploop [env]
  (let [res (read-state the-lock 0)]
    (do ;(when (> (:count env) 0) (send *agent* reploop))
      ;(Thread/sleep 100
        (-> env 
        (update-in [:count] dec)
        (update-in [:reads] conj res)))))

(defn reporter [n] (agent {:count n :reads []}))    

(defn simulate []
    (send reporter  reploop)
               

  
