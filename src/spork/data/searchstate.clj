;;Module for defining generic graph search data.  Uses the generic fringe 
;;structures from spork.data.fringe, to provide a generic search state.
;;We'll typically use this with priority queues, for djikstra's algorithm and 
;;the bellman-ford algorithm, but depth-first, breadth-first, and random 
;;searching are also supported.
(ns spork.data.searchstate
  (:require [spork.protocols [core :as generic]]
            [spork.data      [priorityq :as pq]
                             [fringe :as fr]]))

;;__TODO__ Use transient operations for updating the search state.
(defn update-search [state shortest distance fringe]
  (merge state {:shortest shortest 
                :distance distance 
                :fringe fringe}))

(defn- estimating-conj [estimator fringe sink w target]
  (generic/conj-fringe fringe sink (+ w (estimator sink target) w)))

(defn- conj-fringe* [state sink w] 
  (if-let [e (:estimator state)]
    (estimating-conj e   (:fringe state) sink w (:target state))
    (generic/conj-fringe (:fringe state) sink w)))

(defn- new-path*   
  "When we discover a new path via from source to sink, we add sink to the 
   shortest path tree, register the distance, and add sink to the fringe.
   If an estimator is provided, we apply the estimator to the "
  [source sink w {:keys [shortest distance fringe] :as state}]
    (update-search state (assoc shortest sink source)
                         (assoc distance sink w)
                         (conj-fringe* state sink w)))
(defn- shorter-path*
  "When a shorter path is found to a node already on the fringe, we update the 
   SPT, distance, and add the sink back to the fringe based on the new path."   
  [source sink wnew wpast {:keys [shortest distance fringe] :as state}]
    (update-search state (assoc shortest sink source) ;new spt
                   (assoc distance sink wnew)  ;shorter distance
                   (conj-fringe* state sink wnew)))

(defn- equal-path* 
  "When we discover equivalent paths, we conj them onto the shortest path tree.
   Note, if a better path is found, the other paths will be eliminated."
  [source sink {:keys [shortest distance fringe] :as state}]
    (let [current (get shortest sink)
		      context (if (vector? current) current [current])
		      newspt (assoc shortest sink (conj context source))]                 
		     (update-search state newspt distance fringe)))
  
;A general container for any abstract graph search.
;Might shift to a simple map here....not sure yet.
(defrecord searchstate [startnode targetnode shortest distance fringe estimator]
  generic/IGraphSearch
	  (new-path     [state source sink w] (new-path* source sink w state))           
	  (shorter-path [state source sink wnew wpast]
	    (shorter-path* source sink wnew wpast state))
	  (equal-path   [state source sink] (equal-path* source sink state))
    (best-known-distance   [state nd] (get distance nd))
  generic/IFringe 
	  (conj-fringe [state n w] (assoc state :fringe 
                                   (generic/conj-fringe fringe n w)))
	  (next-fringe [state]  (generic/next-fringe fringe))
	  (pop-fringe  [state]  (assoc state :fringe (generic/pop-fringe fringe))))                 
                                
(def empty-search (searchstate. nil nil {} {} nil nil))

(defn init-search 
  [& {:keys [startnode targetnode fringe] 
      :or   {startnode nil targetnode nil fringe fr/depth-fringe}}]
    (assert (and (not (nil? fringe)) (generic/fringe? fringe))
            (str "Invalid fringe: " fringe))
      (merge empty-search {:startnode startnode 
                           :targetnode (generic/maybe targetnode ::nullnode)
                           :distance {startnode 0}
                           :shortest {startnode startnode}
                           :fringe fringe}))

;revisit these definitions....
(def empty-DFS (init-search :fringe fr/depth-fringe))
(def empty-BFS (init-search :fringe fr/breadth-fringe))   
(def empty-PFS (init-search :fringe fr/priority-fringe))
(def empty-RFS (init-search :fringe fr/random-fringe))


