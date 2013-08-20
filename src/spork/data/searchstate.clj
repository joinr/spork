;;Module for defining generic graph search data.  Uses the generic fringe 
;;structures from spork.data.fringe, to provide a generic search state.
;;We'll typically use this with priority queues, for djikstra's algorithm and 
;;the bellman-ford algorithm, but depth-first, breadth-first, and random 
;;searching are also supported.
(ns spork.data.searchstate
  (:require [spork.protocols [core :as generic]]
            [spork.data      [priorityq :as pq]
                             [fringe :as fr]]))

;;__TODO__ Use transient operations for 

(defn- maybe 
  ([coll elseval] (if-let [v (first coll)] v elseval))
  ([coll] (maybe coll nil)))

(defn update-search [state shortest distance fringe]
  (merge state {:shortest shortest 
                :distance distance 
                :fringe fringe}))

(defn- new-path*   
  "When we discover a new path via from source to sink, we add sink to the 
   shortest path tree, register the distance, and add source to the fringe."
  [source sink w {:keys [shortest distance fringe] :as state}]
    (update-search state (assoc shortest sink source)
                         (assoc distance sink w) 
                         (generic/conj-fringe fringe sink w)))

(defn- shorter-path*
  "When a shorter path is found to a node already on the fringe, we update the 
   SPT, distance, and re-weight the fringe based on the new path."   
  [source sink wnew wpast {:keys [shortest distance fringe] :as state}]
    (update-search state (assoc shortest sink source) ;new spt
                   (assoc distance sink wnew)  ;shorter distance
                   (generic/re-weigh fringe sink wpast wnew)))

(defn- equal-path* 
  "When we discover equivalent paths, we conj them onto the shortest path tree.
   Note, if a better path is found, the other paths will be eliminated."
  [source sink {:keys [shortest distance fringe] :as state}]
    (let [current (get shortest sink)
		      context (if (vector? current) current [current])
		      newspt (assoc shortest sink (conj context source))]                 
		     (update-search state newspt distance fringe)))

(defn- relax*
  "Given a shortest path map, a distance map, a source node, sink node, 
   and weight(source,sink) = w, update the search state.  

   The implication of a relaxation on sink, relative to source, is that 
   source no longer exists in the fringe (it's permanently labeled).  
   So a relaxation can mean one of three things: 
   1: sink is a newly discovered-node (as a consequence of visiting source);
   2: sink was visited earlier (from a different source), but this visit exposes
      a shorter path to sink, so it should be elevated in consideration in 
      the search fringe.
   3: sink is a node of equal length to the currently shortest-known path from 
      an unnamed startnode.  We want to record this equivalence, which means 
      that we may ultimately end up with multiple shortest* paths."
     
  [source sink w {:keys [shortest distance fringe] :as state}]
    (let [relaxed (+ (get distance source) w)]
      (if-let [known (distance sink)]
	      (cond 
	        (< relaxed known) (generic/shorter-path state source sink relaxed known )            
	        (= relaxed known) (generic/equal-path state source sink )                         
	        :else state)            
       ;if sink doesn't exist in distance, sink is new...
       (new-path* source sink relaxed state))))
  
;A general container for any abstract graph search.
;Might shift to a simple map here....not sure yet.
(defrecord searchstate [startnode targetnode shortest distance fringe]
  generic/IGraphSearch
	  (new-path [state source sink w] (new-path* source sink w state))           
	  (shorter-path [state source sink wnew wpast]
	    (shorter-path* source sink wnew wpast state))
	  (equal-path [state source sink] (equal-path* source sink state))            
  generic/IFringe 
	  (conj-fringe [state n w] (assoc state :fringe (generic/conj-fringe fringe n w)))
	  (next-fringe [state] (generic/next-fringe fringe))
	  (pop-fringe [state] (assoc state :fringe (generic/pop-fringe fringe)))
	  (re-weigh [state n wold wnew] (assoc state :fringe 
                                        (generic/re-weigh fringe n wold wnew)))
	  (re-label [state n w newlabel] (assoc state :fringe 
                                         (generic/re-label fringe n w newlabel))))                 
                                
(def empty-search (searchstate. nil nil {} {} nil))
(def fringe-types {::depth   fr/stack-fringe
                   ::breadth fr/q-fringe
                   ::priority fr/priority-fringe
                   ::random  fr/random-fringe})

(defn init-search 
  ([& {:keys [startnode targetnode fringe] 
       :or   {startnode nil targetnode nil fringe ::depth}}]
    (let [fringe (if (keyword? fringe) (get fringe-types fringe))]
      (assert (and (not (nil? fringe)) (generic/fringe? fringe))
               (str "Invalid fringe: " fringe))
      (merge empty-search {:startnode startnode 
                           :targetnode (maybe targetnode ::nullnode)
                           :distance {startnode 0}
                           :shortest {startnode startnode}
                           :fringe fringe}))))

;revisit these definitions....
(def empty-DFS (init-search))
(def empty-BFS (init-search :fringe ::breadth))   
(def empty-PFS (init-search :fringe ::priority))
(def empty-RFS (init-search :fringe ::random))


