;;Module for defining generic graph search data.  Uses the generic fringe 
;;structures from spork.data.fringe, to provide a generic search state.
;;We'll typically use this with priority queues, for djikstra's algorithm and 
;;the bellman-ford algorithm, but depth-first, breadth-first, and random 
;;searching are also supported.
(ns spork.data.searchstate
  (:require [spork.protocols [core :as generic]]
            [spork.data      [priorityq :as pq]
                             [fringe :as fr]
                             [mutable :as m]]))

;;These allows us to use complex keys in our spt.
(defn branch?  [v] (and (coll? v) (contains? (meta v) :branch)))
(defn ->branch [& xs]  (with-meta (into [] xs) {:branch true}))
  
;;__TODO__ Use transient operations for updating the search state.
;; (defn update-search [state shortest distance fringe]
;;   (-> state
;;       (assoc :shortest shortest)
;;       (assoc :distance distance)
;;       (assoc :fringe fringe)))

(definline update-search [state shortest distance fringe]
  `(-> ~state
       (assoc :shortest ~shortest)
       (assoc :distance ~distance)
       (assoc :fringe ~fringe)))


(definline estimating-conj [estimator fringe sink w target]
  `(generic/conj-fringe ~fringe ~sink (+ ~w (~estimator ~sink ~target))))

;; (defn- estimating-conj [estimator fringe sink w target]
;;   (generic/conj-fringe fringe sink (+ w (estimator sink target))))


;;Experimental performance enhancements...

(definline conj-fringe* [state sink w] 
  `(if-let [e# (:estimator ~state)]
    (estimating-conj e#   (:fringe ~state) ~sink ~w (:target ~state))
    (generic/conj-fringe (:fringe ~state) ~sink ~w)))


;; (defn- conj-fringe*    [state sink w] 
;;   (if-let [e (:estimator state)]
;;     (estimating-conj e   (:fringe state) sink w (:target state))
;;     (generic/conj-fringe (:fringe state) sink w)))

;;When we discover a new path via from source to sink, we add sink to the 
;;shortest path tree, register the distance, and add sink to the fringe.
(definline new-path*   
  [source sink w state] 
  `(update-search ~state 
                  (assoc (:shortest ~state) ~sink ~source)
                  (assoc (:distance ~state) ~sink ~w)
                  (conj-fringe* ~state ~sink ~w)))

;; (defn- new-path*   
;;   "When we discover a new path via from source to sink, we add sink to the 
;;    shortest path tree, register the distance, and add sink to the fringe."
;;   [source sink w {:keys [shortest distance fringe] :as state}]
;;     (update-search state (assoc shortest sink source)
;;                          (assoc distance sink w)
;;                          (conj-fringe* state sink w)))


(definline shorter-path* 
  [source sink wnew wpast state]
  `(update-search ~state (assoc (:shortest ~state) ~sink ~source) ;new spt
                  (assoc (:distance ~state) ~sink ~wnew)  ;shorter distance
                  (conj-fringe* ~state ~sink ~wnew)))

;; (defn- shorter-path*
;;   "When a shorter path is found to a node already on the fringe, we update the 
;;    SPT, distance, and add the sink back to the fringe based on the new path."   
;;   [source sink wnew wpast {:keys [shortest distance fringe] :as state}]
;;     (update-search state (assoc shortest sink source) ;new spt
;;                          (assoc distance sink wnew)  ;shorter distance
;;                          (conj-fringe* state sink wnew)))

(definline equal-path* 
  [source sink state]
  `(let [shortest# (:shortest ~state)
         current#  (get shortest# ~sink)
         newspt#   (assoc shortest# ~sink 
                     (-> (if (branch? current#) current# (->branch current#))
                         (conj ~source)))]                 
     (update-search ~state newspt# (:distance ~state) (:fringe ~state))))

;; (defn- equal-path* 
;;   "When we discover equivalent paths, we conj them onto the shortest path tree.
;;    Note, if a better path is found, the other paths will be eliminated."
;;   [source sink {:keys [shortest distance fringe] :as state}]
;;     (let [current (get shortest sink)
;; 		      context (if (branch? current) current (->branch current))
;; 		      newspt  (assoc shortest sink (conj context source))]                 
;; 		     (update-search state newspt distance fringe)))
  
;A general container for any abstract graph search.
;Might shift to a simple map here....not sure yet.
(defrecord searchstate 
  [startnode targetnode shortest distance fringe estimator visited]
  generic/IGraphSearch
	  (new-path     [state source sink w] (new-path* source sink w state))           
	  (shorter-path [state source sink wnew wpast]
	    (shorter-path* source sink wnew wpast state))
	  (equal-path   [state source sink] (equal-path* source sink state))
    (best-known-distance   [state nd] (get distance nd))
    (conj-visited [state source] 
      (assoc state :visited (conj visited source)))
  generic/IFringe 
	  (conj-fringe [state n w] (assoc state :fringe 
                                   (generic/conj-fringe fringe n w)))
	  (next-fringe [state]  (generic/next-fringe fringe))
	  (pop-fringe  [state]  (assoc state :fringe (generic/pop-fringe fringe))))                 
                             
;; (m/defmutable msearchstate 
;;   [startnode targetnode shortest distance fringe estimator visited]
;;   generic/IGraphSearch
;;   (new-path     [state source sink w]
;;     (do (set! shortest (assoc shortest sink source))
;;         (set! distance (assoc distance sink w))
;;         (set! fringe   
;;               (if-let [e estimator]
;;                 (estimating-conj e   fringe sink w targetnode)
;;                 (generic/conj-fringe fringe sink w))) 
;;         state))
;;   (shorter-path [state source sink wnew wpast]
;;      (do (set! shortest (assoc shortest sink source)) ;new spt
;;          (set! distance (assoc distance sink wnew))   ;shorter distance
;;          (set! fringe   
;;                (if-let [e estimator]
;;                  (estimating-conj e   fringe sink wnew targetnode)
;;                  (generic/conj-fringe fringe sink wnew))) 
;;          state))  
;;   (equal-path   [state source sink] 
;;      (let [current  (get shortest sink)]
;;        (do (set! shortest (assoc shortest sink 
;;                                   (-> (if (branch? current) current (->branch current))
;;                                       (conj source)))) 
;;            state)))
;;   (best-known-distance   [state nd] (get distance nd))
;;   (conj-visited [state source] 
;;       (do (set! visited (conj visited source))
;;           state))
;;   generic/IFringe 
;;   (conj-fringe [state n w] 
;;                (do (set! fringe (generic/conj-fringe fringe n w)) 
;;                    state))
;;   (next-fringe [state]  (generic/next-fringe fringe))
;;   (pop-fringe  [state]  (do (set! fringe (generic/pop-fringe fringe)) state)))

(m/defmutable msearchstate 
  [startnode targetnode  
   ^spork.data.mutable.mutmap shortest 
   ^spork.data.mutable.mutmap distance 
   fringe estimator visited]
  generic/IGraphSearch
  (new-path     [state source sink w]
    (do (set! shortest (assoc! shortest sink source))
        (set! distance (assoc! distance sink w))
        (set! fringe   
              (if-let [e estimator]
                (estimating-conj e   fringe sink w targetnode)
                (generic/conj-fringe fringe sink w))) 
        state))
  (shorter-path [state source sink wnew wpast]
     (do (set! shortest (assoc! shortest sink source)) ;new spt
         (set! distance (assoc! distance sink wnew))   ;shorter distance
         (set! fringe   
               (if-let [e estimator]
                 (estimating-conj e   fringe sink wnew targetnode)
                 (generic/conj-fringe fringe sink wnew))) 
         state))  
  (equal-path   [state source sink] 
     (let [current  (get shortest sink)]
       (do (set! shortest (assoc! shortest sink 
                                  (-> (if (branch? current) current (->branch current))
                                      (conj source)))) 
           state)))
  (best-known-distance   [state nd] (get distance nd))
  (conj-visited [state source] 
      (do (set! visited (conj visited source))
          state))
  generic/IFringe 
  (conj-fringe [state n w] 
               (do (set! fringe (generic/conj-fringe fringe n w)) 
                   state))
  (next-fringe [state]  (generic/next-fringe fringe))
  (pop-fringe  [state]  (do (set! fringe (generic/pop-fringe fringe)) state)))    

                                
(def empty-search (searchstate. nil nil {} {} nil nil []))

(defn init-search
  "Populates an empty search state with an initial set of parameters.  Allows
   searches to be customized by varying the start, target, and the type of 
   fringe used to prosecute the search."
  [startnode & {:keys [targetnode fringe] 
                :or   {targetnode ::nullnode fringe fr/depth-fringe}}]
    (assert (and (not (nil? fringe)) (generic/fringe? fringe))
            (str "Invalid fringe: " fringe))
      (merge empty-search {:startnode  startnode 
                           :targetnode targetnode
                           :distance {startnode 0}
                           :shortest {startnode startnode}
                           :fringe   fringe}))

(defn minit-search
  "Populates an empty search state with an initial set of parameters.  Allows
   searches to be customized by varying the start, target, and the type of 
   fringe used to prosecute the search."
  [startnode & {:keys [targetnode fringe] 
                :or   {targetnode ::nullnode fringe fr/depth-fringe}}]
    (assert (and (not (nil? fringe)) (generic/fringe? fringe))
            (str "Invalid fringe: " fringe))
    (msearchstate.  startnode 
                    targetnode
                    (m/->mutmap [startnode startnode])
                    (m/->mutmap [startnode 0])
                    fringe
                    nil
                    []))
        
(defn backtrack
  "Given a shortest-path-tree, a start-node, and an initial, or tail, path, 
   backtrack throught the shortest path tree to yield a pair of a path, and 
   all the branching subpaths encountered along the way."
  [preds startnode tail-path]
  (when (seq tail-path)
    (loop [path          tail-path
           pending-paths []]
      (let [node (first path)] 
        (if (= node startnode) [path pending-paths]
            (let [prior  (get preds node)
                  prior-node   (if (branch? prior) (first prior) prior)
                  branch-paths (when (branch? prior) 
                                 (for [nd (rest prior)]
                                   (cons nd path)))]
              (recur (cons prior-node path) 
                     (into pending-paths branch-paths))))))))

(defn paths 
  "Given a shortest-path-tree that encodes the predecessors of nodes, yield a 
   sequence of all the paths from start node to end node."
  [preds startnode endnode]
  (->> (iterate (fn [[current-path pending-paths]]
                  (when (not (empty? pending-paths))
                    (let [[next-path new-subpaths] 
                              (backtrack preds startnode 
                                         (first pending-paths))]
                      [next-path (into (rest pending-paths) new-subpaths)])))
                (backtrack preds startnode (list endnode)))
         (take-while identity)
         (map first)))                 

(defn path? 
  ([state target] (generic/best-known-distance state target))
  ([state] (path? state (:targetnode state))))

(defn get-paths 
  ([state target] 
    (when (path? state target)
      (paths (:shortest state) (:startnode state) target)))
  ([state] (get-paths state (:targetnode state))))


;;A mutable empty depth-first search...
(def mempty-DFS (memoize (fn [startnode] (minit-search startnode :fringe fr/depth-fringe))))
;;An empty breadth-first search.
(def mempty-BFS (memoize (fn [startnode] (minit-search startnode :fringe fr/breadth-fringe))))
;;An empty priority-first search.  Note: THis uses a mutable priority
;;queue 
(let [init-fringe (memoize (fn [startnode] (minit-search startnode)))]
  (defn empty-PFS [startnode] (assoc! (init-fringe startnode) :fringe (fr/make-pq))))



;;An empty depth-first search.
(def empty-DFS (memoize (fn [startnode] (init-search startnode :fringe fr/depth-fringe))))
;;An empty breadth-first search.
(def empty-BFS (memoize (fn [startnode] (init-search startnode :fringe fr/breadth-fringe))))
;;An empty priority-first search.  Note: THis uses a mutable priority
;;queue 
(let [init-fringe (memoize (fn [startnode] (init-search startnode)))]
  (defn empty-PFS [startnode] (assoc (init-fringe startnode) :fringe (fr/make-pq))))

;;An empty random-first search.
(def empty-RFS (memoize (fn [startnode] (init-search startnode :fringe fr/random-fringe))))

;;testing
(comment
;; e e e
;; d d b
;; c a a 
;; b
;; a
(def the-spt
  {:a :a, :b :a, :c :b, :d [:c :a], :e [:d :b]})
(def the-state 
  (-> (init-search :startnode :a :targetnode :e)
      (assoc :shortest the-spt)))

;;=>(get-paths the-state)
;;((:a :b :c :d :e) (:a :b :e) (:a :d :e))         
)
