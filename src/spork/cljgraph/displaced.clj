(ns spork.cljgraph.displaced)

(comment ;spork.cljgraph.search 
;;What were these? 

;(defn- compound-filter [filters state targetnode nextnode w]
;  (if-let [f (first filters)]
;    (if (f state targetnode nextnode w)
;      (recur (rest filters) state targetnode nextnode w)
;      false))
;  true)
;

;
;(defn- positive-more? [state targetnode nextnode w]
;  (if (< 0 w) ;negative edgeweight
;     (do (throw (Exception. (format "negative weight: %d detected near %s" 
;                                     w nextnode))) false)
;                   
;    true))
;
;(defn- acyclic-more? [state targetnode nextnode w]
;  (if (contains? (:shortest state) nextnode) ;found a cycle.
;    (do (throw (Exception. (format "negative weight: %d detected near %s" 
;                                   w nextnode))) false)
;    true))    
;
;
;(defn- DAG-more? (partial compound-filter [default-more? acyclic-more?]))
;(defn- djikstra-more? (partial compound-filter [default-more? positive-more?]))
;
;(defmulti halting-fn (fn [state & others] (:purpose state)))
;
;(defmethod halting-fn nil [state & [targetnode nextnode w]]
;  (default-more? state targetnode nextnode w))
;(defmethod halting-fn :djikstra [state & [targetnode nextnode w]] 
;  (djikstra-more? state targetnode nextnode w))  
;(defmethod halting-fn :bellman-ford [state & [targetnode nextnode w]]
;  (DAG-more? state targetnode nextnode w))
)