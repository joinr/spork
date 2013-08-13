;Module for defining generic graph search data.
(ns spork.cljgraph.data.search
  (:require [cljgraph [graph :as gr]]
            [cljgraph.data [fringe :as fr]]
            [cljgraph.data [priorityq :as pq]]))

(defn- maybe 
  ([coll elseval] (if-let [v (first coll)] v elseval))
  ([coll] (maybe coll nil)))

(defprotocol IGraphsearchstate
  (new-path [state source sink w])
  (shorter-path [state source sink wnew wpast])
  (equal-path  [state source sink]))

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
                         (fr/conj-fringe fringe sink w)))

(defn- shorter-path*
  "When a shorter path is found to a node already on the fringe, we update the 
   SPT, distance, and re-weight the fringe based on the new path."   
  [source sink wnew wpast {:keys [shortest distance fringe] :as state}]
    (update-search state (assoc shortest sink source) ;new spt
                   (assoc distance sink wnew)  ;shorter distance
                   (fr/re-weigh fringe sink wpast wnew)))

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
	        (< relaxed known) (shorter-path state source sink relaxed known )            
	        (= relaxed known) (equal-path state source sink )                         
	        :else state)            
       ;if sink doesn't exist in distance, sink is new...
       (new-path* source sink relaxed state))))
  
;A general container for any abstract graph search.
;Might shift to a simple map here....not sure yet.
(defrecord searchstate [startnode targetnode shortest distance fringe]
  IGraphsearchstate
	  (new-path [state source sink w] (new-path* source sink w state))           
	  (shorter-path [state source sink wnew wpast]
	    (shorter-path* source sink wnew wpast state))
	  (equal-path [state source sink] (equal-path* source sink state))            
  fr/IFringe 
	  (conj-fringe [state n w] (assoc state :fringe (fr/conj-fringe fringe n w)))
	  (next-fringe [state] (fr/next-fringe fringe))
	  (pop-fringe [state] (assoc state :fringe (fr/pop-fringe fringe)))
	  (re-weigh [state n wold wnew] (assoc state :fringe 
                                        (fr/re-weigh fringe n wold wnew)))
	  (re-label [state n w newlabel] (assoc state :fringe 
                                         (fr/re-label fringe n w newlabel))))                 
                                
(def empty-search (searchstate. nil nil {} {} nil))

(defn init-search 
  ([startnode & targetnode]
    (merge empty-search {:startnode startnode 
                         :targetnode (maybe targetnode ::nullnode)
                         :distance {startnode 0}
                         :shortest {startnode startnode}})))

(defn swap-fringe [fr state] (assoc state :fringe fr))

;revisit these definitions....
(def empty-DFS (comp (partial swap-fringe (fr/stack-fringe)) init-search))
(def empty-BFS (comp (partial swap-fringe (fr/q-fringe)) init-search))   
(def empty-PFS (comp (partial swap-fringe pq/minq) init-search))

(defn- possible?
  "Are start and target possibly transitively related in graph g?" 
  [g startnode targetnode]
  (if (and (gr/get-sinks g startnode) (gr/get-sources g targetnode)) true 
    false))
   
(defn- get-weight
  "Return the distance from source node to sink node."
  [g source sink] 
  (gr/arc-weight (gr/get-arc g source sink)))

(defn- unit-weight [g source sink] 1)

(defn- get-candidates
  "Return a list of adjacent nodes.  Can optionally inclusively filter 
   candidates using filterf."
  ([g nd] (gr/get-sinks g nd))
  ([g nd filterf] (filter filterf (gr/get-sinks g nd))))

(defn- default-neighborf 
  "Return a list of valid fringe nodes, adjacent to node nd in graph g, 
   relative to searchstate.  Ignores the state.  This will allow multiple 
   visits.  Useful for some algorithms."
  [g nd state] (gr/get-sinks g nd))

(defn- visit-once
  "Screen nodes that have already been visited.  If we have visited any nodes 
   at least once, they will show up in the shortest path tree."
  [g nd {:keys [shortest] :as state}] 
  (filter #(not (contains? shortest %)) (gr/get-sinks g nd))) 

(defn- default-halt?  [state targetnode nextnode w]
  (or (= targetnode nextnode) (fr/empty-fringe? (:fringe state))))

(defn- unexplored
  "Given a search state, with a shortest path tree, and a graph g, 
   determine which nodes have not been explored in g."
  [g state]
  (clojure.set/difference (set (gr/get-nodelabels g)) 
                          (set (keys (:spt state)))))

;the normal mode for graph walking/searching.  
(def default-walk {:halt? default-halt?
                   :weightf get-weight
                   :neighborf default-neighborf})
(def limited-walk (assoc default-walk :neighborf visit-once))
(def unit-walk (assoc limited-walk :weightf unit-weight))

(defn graph-walk
  "Generic fn to walk a graph.  The type of walk can vary by changing the 
   fringe of the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode state  & {:keys [halt? weightf neighborf] 
                                    :or {halt? default-halt?
                                         weightf get-weight
                                         neighborf default-neighborf} }]
    (let [walker 
          (fn walker [g targetnode {:keys [fringe] :as searchstate}]
					  (if-let [candidate (fr/next-fringe fringe)]
					    (let [w (:weight candidate) ;perceived weight from start
					          nd (:node candidate)]
						    (if-not (halt? searchstate targetnode nd w) 
							    (let [sinkweights (for [sink (neighborf g nd searchstate)] 
                                      [sink (weightf g nd sink)])
                       relaxation (fn [state [sink w]] (relax* nd sink w state))
                       nextstate (reduce relaxation 
                                 (fr/pop-fringe searchstate) 
                                 sinkweights)]
						       (recur g targetnode nextstate))
					       searchstate))
              searchstate))]
        (walker g targetnode (fr/conj-fringe state startnode 0))))


(defn graph-walk-seq
  "Generic fn to walk a graph.  The type of walk can vary by changing the 
   fringe of the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a sequence of 
   searchstates of the walk, which contains the shortest path trees, distances, 
   etc. for multiple kinds of walks, depending on the searchstate's fringe 
   structure."
  [g startnode targetnode state & {:keys [halt? weightf neighborf] 
                                   :or {halt? default-halt?
                                        weightf get-weight
                                        neighborf default-neighborf} }]
    (let [walker 
          (fn walker [g targetnode {:keys [fringe] :as searchstate}]
					  (if-let [candidate (fr/next-fringe fringe)]
					    (let [w (:weight candidate) ;perceived weight from start
					          nd (:node candidate)]
						    (if-not (halt? searchstate targetnode nd w)
                  (lazy-seq
                    (let [sinkweights (for [sink (neighborf g nd searchstate)] 
                                        [sink (weightf g nd sink)])
                          relaxation (fn [state [sink w]] 
                                       (relax* nd sink w state))
                          nextstate (reduce relaxation 
                                            (fr/pop-fringe searchstate) 
                                            sinkweights)]
                      (concat (list searchstate) 
                              (walker g targetnode nextstate))))
					       (list searchstate)))
              (list searchstate)))]
        (walker g targetnode (fr/conj-fringe state startnode 0))))
     
(defn mapargs [f m]
  (apply f (flatten (seq m))))

;I need to abstract out these walks better...there are some obviously factorable 
;parameters here, particularly (walkf g startnode (maybe endnode ::nullnode)...
(defn- depth-walker [g walkf] 
  (fn [startnode & endnode]
	  (walkf g startnode (maybe endnode ::nullnode) 
                     (empty-DFS startnode)
                     :neighborf visit-once
                     :weightf unit-weight)))
  
(defn depth-walk
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  [g] (depth-walker g graph-walk))

(defn depth-walk-seq
  "Returns a function that lazily explores all of graph g in depth-first 
   topological order from startnode.  This is not a search.  Any paths returned 
   will be relative to unit-weight."
  [g] (depth-walker g graph-walk-seq))
  
(defn- breadth-walker 
  [g walkf]
   (fn [startnode & endnode]
	  (walkf g startnode 
          (maybe endnode ::nullnode) 
          (empty-BFS startnode) 
          :weightf unit-weight 
          :neighborf visit-once)))  
  
(defn breadth-walk
  "Returns a function that explores all of graph g in a breadth-first 
   topological order from startnode.  This is not a search, any paths returned 
   will be relative to unit-weight."
  [g] (breadth-walker g graph-walk))

(defn breadth-walk-seq
  "Returns a function that lazily explores all of graph g in a breadth-first 
   topological order from startnode.  This is not a search, any paths returned 
   will be relative to unit-weight."
  [g] (breadth-walker g graph-walk-seq))
  
(defn- priority-walker 
  [g walkf] 
  (fn [startnode & endnode]
	  (walkf g startnode (maybe endnode ::nullnode) 
          (empty-PFS startnode))))

(defn priority-walk
  "Returns a function that explores all of graph g in a priority-first 
  topological order from a startnode. Weights returned will be in terms of the 
  edge weights in the graph."
  [g] (priority-walker g graph-walk))
   
(defn priority-walk-seq
  "Returns a function that lazily explores all of graph g in a priority-first 
  topological order from a startnode. Weights returned will be in terms of the 
  edge weights in the graph."
  [g] (priority-walker g graph-walk-seq))

(defn graph-forest 
  "Given a graph g, and an arbitrary startnode, return the sequence of all 
   depth-first shortest path trees.  This should return a sequence of 
   components within the graph, a series of depth-walks from a monotonically 
   decreasing set of startnodes."
  [g startnode]
  (let [walk (depth-walk g)
        take-step (fn take-step [g found state]
                    (if-let [remaining 
                             (clojure.set/difference found 
                                                     (unexplored g state))]
                      (lazy-seq 
                        (let [nextstart (first remaining)
                              nextstate (walk nextstart)
                              remaining (disj remaining nextstart)]
                          (concat 
                            (list state) (take-step g remaining nextstate))))
                       state))]
    (take-step g #{} (walk startnode))))

;(defprotocol IGraphsearch
;  (search ([g startnode endnode])
;          ([g startnode]))
;  (search-more? [state nextnode w])
;  (relax [state source sink w]))

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
