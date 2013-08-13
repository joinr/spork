(ns spork.cljgraph.algorithms.bellmanford
  "An implementation of the Bellman-Ford algorithm for directed
   acyclic graphs.  Bellman-Ford works for directed graphs with 
   negative edgeweights, with the restriction that there are no 
   negative cycles."
   (:use [cljgraph.graph]))

;;in bellman-ford, we have similar stuff from djikstra....
;;we have searchstats...
;;  shortest
;;  distance
;;  is there a fringe? 
(defn- positive-graph [] 
  (-> (make-graph)
    (add-arcs (map #(apply make-arc %) 
                      [[1 3 1] [1 2 1] [3 2 1] [3 4 5] [2 4 1] [2 5 8]]))))

(defn- negate-arcs [arcs]
  (let [label-pairs (for [[label arc] arcs] 
                      (assoc arc :weight (* -1 (:weight arc))))] 
   (reduce #(assoc %1 (get-label %2) %2) arcs label-pairs)))
           
(defn negate-graph [g] (assoc g :arcs (negate-arcs (:arcs g))))

;;Bellman-ford works similarly to Djikstra, in that there is a fringe, 
;;a relaxation step, a value or distance function, etc.

;Given a directed acyclic graph, with no negative cycles, Bellman-Ford is 
;guaranteed to build the shortest path[s] from a start node to a source node 
;in O(VE) time, probably less if not all nodes have to be examined.

;In a way Bellman-Ford is very-much like Djikstra....
;  It has a fringe of nodes.
;  It allows for revision of the shortest path tree.
;  It allows a node 

;  Bellman-Ford differs in a couple of important respects:
;    It does not use priority to determine which nodes to inspect.
;    It does not assume a node has 

;Search algorithm implementation:
;I am intentionally verbose in the decomposition of functionality and in 
;comments.  I'll try to bake it into a more elegant form in the near future.

(defn- possible?
  "Are start and target possibly transitively related in graph g?" 
  [g startnode targetnode]
  (if (and (get-sinks g startnode) (get-sources g targetnode)) true 
    false))
   
(defn- get-weight
  "Return the distance from source node to sink node."
  [g source sink] 
  (:weight (get-arc g (make-arc source sink))))

;a container for our search stats related to djikstra.  Might make this 
;generic....
(defrecord djikstats [shortest distance fringe]) 
(def empty-search (djikstats. {} {} pq/minq))

(defn- new-path   
  "When we discover a new path via from source to sink, we add sink to the 
   shortest path tree, register the distance, and add source to the fringe."
  [source sink w shortest distance fringe]
    (djikstats. (assoc shortest sink source)
                (assoc distance sink w) 
                (fr/conj-fringe fringe sink w)))
(defn- shorter-path
  "When a shorter path is found to a node already on the fringe, we update the 
   SPT, distance, and re-weight the fringe based on the new path."   
  ([source sink wnew wpast shortest distance fringe]
    (djikstats. (assoc shortest sink source) ;new spt
                (assoc distance sink wnew)  ;shorter distance
                (fr/re-weigh fringe sink wpast wnew))))

(defn- equal-path [source sink shortest distance fringe]
  "When we discover equivalent paths, we conj them onto the shortest path tree.
   Note, if a better path is found, the other paths will be eliminated."
  (let [current (get shortest sink)
        context (if (vector? current) current [current])
        newspt (assoc shortest sink (conj context source))]                 
       (djikstats. newspt distance fringe)))                                                  

(defn- search-more? "Predicate to allow early termination of djikstra" 
  [targetnode nextnode w]
  (if (< w  0) (throw 
                (Exception. 
                  (format "negative weight: %d detected near %s" w nextnode)))
     (not=  targetnode nextnode)))

(defn- relax
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
     
  ([source sink w shortest distance fringe]
    (let [relaxed (+ (distance source) w)]
      (if-let [known (distance sink)]
	      (cond 
	        (< relaxed known) ;found a shorter path to sink
            (shorter-path source sink relaxed known shortest distance fringe)
	        (= relaxed known) ;record an equivalent path to sink
            (equal-path source sink shortest distance fringe)              
	        :else 
            (djikstats. shortest distance fringe))
       ;if sink doesn't exist in distance, sink is new...
       (new-path source sink relaxed shortest distance fringe))))
  ;accept map (usually a djiksats record) in lieu of explicit args.
  ([source sink w {:keys [shortest distance fringe]}] 
    (relax source sink w shortest distance fringe)))

(defn- dj-search
  "Using Djikstra's algorithm, derive the shortest path tree (spt) from source 
   to target.  If target is found, terminate early.  Only explores the nodes 
   required during the search to reach target." 
  [g targetnode {:keys [fringe] :as searchstate}]
	  (if-let [candidate (fr/next-fringe fringe)]
	    (let [w (:weight candidate) ;perceived weight from start to nextsource
	          nextnode (:node candidate)]
		    (if (search-more? targetnode nextnode w) 
			    (let [sinkweights (for [sink (get-sinks g nextnode)] 
	                            [sink (get-weight g nextnode sink)])
	              poppedstate (assoc searchstate :fringe (fr/pop-fringe fringe))              
	              relaxation (fn [state [sink w]] (relax nextnode sink w state))
	              nextstate (reduce relaxation poppedstate sinkweights)]
		       (recur g targetnode nextstate))
	       searchstate))
	     searchstate))

(defn- lazy-dj-search
  "Using Djikstra's algorithm, derive the shortest path tree (spt) from source 
   to target.  If target is found, terminate early.  Only explores the nodes 
   required during the search to reach target.  Returns lazy sequence of search
   state." 
  [g targetnode {:keys [fringe] :as searchstate}]
	  (if-let [candidate (fr/next-fringe fringe)]
	    (let [w (:weight candidate) ;perceived weight from start to nextsource
	          nextnode (:node candidate)]
		    (if (search-more? targetnode nextnode w)
	        (lazy-seq
				    (let [sinkweights (for [sink (get-sinks g nextnode)] 
		                            [sink (get-weight g nextnode sink)])
		              poppedstate (assoc searchstate :fringe (fr/pop-fringe fringe))              
		              relaxation (fn [state [sink w]] (relax nextnode sink w state))
		              nextstate (reduce relaxation poppedstate sinkweights)]
			        (concat (list searchstate) 
                   (lazy-dj-search g targetnode nextstate))))
	       searchstate))
	     searchstate))

(defn djikstra [g startnode targetnode]
  (if (possible? g startnode targetnode)
    (dj-search g targetnode 
       (djikstats. {} {startnode 0} (fr/conj-fringe pq/minq startnode 0)))
    nil))

(defn djikstra-seq [g startnode targetnode]
  (if (possible? g startnode targetnode)
    (djikstra-seq g targetnode 
       (djikstats. {} {startnode 0} (fr/conj-fringe pq/minq startnode 0)))
    nil))


;apply djikstra's algorithm to find the shortest path.  I'm going to slip in 
;bellman-ford as a backup, in case there are negative edgeweights....so that 
;the caller has a single-stop path algorithm to use for most graphs.


