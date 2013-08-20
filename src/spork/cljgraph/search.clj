;;Ported from cljgraph.  Was intended to provide generic walks.  I think 
;;we already do that in spork.util.topographic, but they don't incorporate the 
;;end nodes.  They're meant to be simpler walks.  Maybe unify?
(ns spork.cljgraph.search
  (:require [spork.protocols [core :as generic]]
            [spork.data [searchstate :as searchstate]]
            [spork.util [topographic :as top]]))

(defn- possible?
  "Are start and target possibly transitively related in graph g?" 
  [g startnode targetnode]
  (if (and (top/sinks g startnode) (top/sources g targetnode)) true 
    false))
   
(defn- default-neighborf 
  "Return a list of valid fringe nodes, adjacent to node nd in graph g, 
   relative to searchstate.  Ignores the state.  This will allow multiple 
   visits.  Useful for some algorithms."
  [g nd state] (top/sinks g nd))

;;Note: We could probably refactor these guys.  The only thing that's different
;;about them is the function generating neighbors.

(defn- visit-once
  "Screen nodes that have already been visited.  If we have visited any nodes 
   at least once, they will show up in the shortest path tree."
  [g nd {:keys [shortest] :as state}] 
  (filter #(not (contains? shortest %)) (top/sinks g nd))) 

(defn- visit-ordered-once
  "Screen nodes that have already been visited.  If we have visited any nodes 
   at least once, they will show up in the shortest path tree.  Additionally, 
   this will visit the nodes in the order the incident arcs were appended to the 
   graph, if the underlying the graph supports it."
  [g nd {:keys [shortest] :as state}] 
  (filter #(not (contains? shortest %)) (rseq (top/sinks g nd))))

(defn- visit-neighbors-once
  "Treats the graph as if it's undirected.  Screen nodes that have already been 
   visited.  If we have visited any nodes at least once, they will show up in 
   the shortest path tree."
  [g nd {:keys [shortest] :as state}] 
  (filter #(not (contains? shortest %)) (top/neighbors g nd)))

(defn- default-halt?  [state targetnode nextnode w]
  (or (= targetnode nextnode) (generic/empty-fringe? (:fringe state))))

;;_POSSIBLE OPTIMIZATION__ Maintain the open set explicitly in the search state
;;vs explicitly computing __unexplored__ .

(defn- unexplored
  "Given a search state, with a shortest path tree, and a graph g, 
   determine which nodes have not been explored in g."
  [g state]
  (clojure.set/difference (set (top/get-node-labels g)) 
                          (set (keys (:spt state)))))

;the normal mode for graph walking/searching.  
(def default-walk {:halt? default-halt?
                   :weightf top/arc-weight
                   :neighborf default-neighborf})

(def limited-walk (assoc default-walk :neighborf visit-once))
(def unit-walk    (assoc limited-walk :weightf (fn [g source sink] 1)))

(defn traverse
  "Generic fn to walk a graph.  The type of walk can vary by changing the 
   fringe of the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode state  & {:keys [halt? weightf neighborf] 
                                    :or {halt?     default-halt?
                                         weightf   top/arc-weight
                                         neighborf default-neighborf} }]
    (let [relaxation (fn [nd state [sink w]] (generic/relax nd sink w state))
          step (fn step [g targetnode {:keys [fringe] :as searchstate}]
                   (if (empty? fringe) searchstate 
                     (let [candidate (generic/next-fringe fringe)
                           w  (:weight candidate) ;perceived weight from start
                           nd (:node candidate)]
                       (if-not (halt? searchstate targetnode nd w) 
                         (let [sinkweights (for [sink (neighborf g nd searchstate)] 
                                                [sink (weightf g nd sink)])
                               nextstate  (reduce (partial relaxation nd) 
                                                  (generic/pop-fringe searchstate) 
                                                  sinkweights)]
                           (recur g targetnode nextstate))
                         searchstate))))]
        (step g targetnode (generic/conj-fringe state startnode 0))))    

(defn depth-traversal
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  [g startnode & endnode ] 
  (traverse g startnode (maybe endnode ::nullnode) 
              (searchstate/empty-DFS startnode)
              :neighborf visit-once
              :weightf unit-weight))
 
(defn breadth-traversal
  "Returns a function that explores all of graph g in a breadth-first 
   topological order from startnode.  This is not a search, any paths returned 
   will be relative to unit-weight."
  [g startnode & endnode] 	  
  (traverse g startnode 
              (maybe endnode ::nullnode) 
              (searchstate/empty-BFS startnode) 
              :weightf unit-weight 
              :neighborf visit-once))
 
(defn ordered-traversal
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  [g startnode & endnode] 
  (traverse g startnode (maybe endnode ::nullnode) 
              (searchstate/empty-DFS startnode)
              :neighborf visit-ordered-once
              :weightf unit-weight))

(defn random-traversal
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  [g startnode & endnode] 
  (traverse g startnode (maybe endnode ::nullnode) 
              (searchstate/empty-RFS startnode)
              :neighborf visit-once
              :weightf unit-weight))

(defn undirected-traversal
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  [g startnode & endnode] 
  (traverse g startnode (maybe endnode ::nullnode) 
              (searchstate/empty-RFS startnode)
              :neighborf visit-neighbors-once
              :weightf unit-weight))

(defn priority-traversal
  "Returns a function that explores all of graph g in a priority-first 
  topological order from a startnode. Weights returned will be in terms of the 
  edge weights in the graph."
  [g startnode & endnode] 	  
  (traverse g startnode 
              (maybe endnode ::nullnode) 
              (searchstate/empty-PFS startnode)))

;;explicit searches, merely enforces a walk called with an actual destination
;;node.

(defn depth-first-search
  "Starting from startnode, explores g using a depth-first strategy, looking for
   endnode.  Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  Note: depth first search is 
   not guaranteed to find the actual shortest path, thus the shortest path tree
   may be invalid."
  [g startnode endnode]
  (depth-traversal g startnode endnode))

(defn bread-first-search
  "Starting from startnode, explores g using a breadth-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree 
   or precedence tree, the shortest distance tree.  Note: breadth first search 
   is not guaranteed to find the actual shortest path, thus the shortest path 
   tree may be invalid."
  [g startnode endnode]
  (breadth-traversal g startnode endnode))

(defn priority-first-search
  "Starting from startnode, explores g using a priority-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  The is equivalent to djikstra's
   algorithm.  Note: Requires that arc weights are non-negative.  For negative 
   arc weights, use Bellman-Ford, or condition the graph."
  [g startnode endnode]
  (priority-traversal g startnode endnode))

(defn djikstra
  "Starting from startnode, explores g using djikstra's algorithm, looking for
   endnode.  Gradually relaxes the shortest path tree as new nodes are found.  
   If a relaxation provides a shorter path, the new path is recorded.  Returns a 
   search state, which contains the shortest path tree or precedence tree, the 
   shortest distance tree.  Note: Requires that arc weights are non-negative.  
   For negative arc weights, use Bellman-Ford, or condition the graph."
  [g startnode endnode] 
  (priority-traversal g startnode endnode))

;;This is identical to get components, or decompose.
(defn graph-forest 
  "Given a graph g, and an arbitrary startnode, return the sequence of all 
   depth-first shortest path trees.  This should return a sequence of 
   components within the graph, a series of depth-walks from a monotonically 
   decreasing set of startnodes."
  [g startnode]
  (let [walk (partial depth-traversal g)
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