;;Generic graph search libraries, with default implementations for depth,
;;breadth, priority, random searches and traversals (walks).
(ns spork.cljgraph.search
  (:require [spork.protocols [core :as generic]]
            [spork.data      [searchstate :as searchstate]]))

;;minor duplication here, due to some copying around.
(defn arc-weight [tg from to]
  (assert (generic/-has-arc? tg from to) (str "Arc does not exist " [from to]))
  (generic/-arc-weight tg from to))

(defn get-node-labels  [tg] (keys (generic/-get-nodes tg)))

;;This is a slight hack.  We have a neighbors function defined in cljgraph.core,
;;but we need it for one tiny function here.  I could pull in cljgraph.core as 
;;a dependency...but that would create a cyclical dependency.  For now, I just 
;;duplicate the function using the protocol functions, to avoid a cyclical 
;;dependency. We'll keep the function hidden and slightly alter the name to 
;;prevent collisions with the "real" neighbors function in cljgraph.core
(defn- neighbors* [g k] 
  (vec (distinct (mapcat #(% g k) [generic/-get-sources generic/-get-sinks]))))

(defn- possible?
  "Are start and target possibly transitively related in graph g?" 
  [g startnode targetnode]
  (and (generic/-get-sinks g startnode) (generic/-get-sources g targetnode)))

   
(defn- default-neighborf 
  "Return a list of valid fringe nodes, adjacent to node nd in graph g, 
   relative to searchstate.  Ignores the state.  This will allow multiple 
   visits.  Useful for some algorithms."
  [g nd state] (generic/-get-sinks g nd))

(defn unit-weight [g source sink] 1)
;;Note: We could probably refactor these guys.  The only thing that's different
;;about them is the function generating neighbors.

(defn- visit-once
  "Screen nodes that have already been visited.  If we have visited any nodes 
   at least once, they will show up in the shortest path tree."
  [g nd {:keys [shortest] :as state}] 
  (filter #(not (contains? shortest %)) (generic/-get-sinks g nd))) 

(defn- visit-ordered-once
  "Screen nodes that have already been visited.  If we have visited any nodes 
   at least once, they will show up in the shortest path tree.  Additionally, 
   this will visit the nodes in the order the incident arcs were appended to the 
   graph, if the underlying the graph supports it."
  [g nd {:keys [shortest] :as state}] 
  (filter #(not (contains? shortest %)) (reverse (generic/-get-sinks g nd))))

(defn- visit-neighbors-once
  "Treats the graph as if it's undirected.  Screen nodes that have already been 
   visited.  If we have visited any nodes at least once, they will show up in 
   the shortest path tree."
  [g nd {:keys [shortest] :as state}] 
  (filter #(not (contains? shortest %)) (neighbors* g nd)))

;;Removed empty-fringe? from check, since we already cover it in the
;;traversal loop.
(defn- default-halt?  [state nextnode]
  (= (:targetnode state) nextnode))

;;_POSSIBLE OPTIMIZATION__ Maintain the open set explicitly in the search state
;;vs explicitly computing __unexplored__ .

(defn- unexplored
  "Given a search state, with a shortest path tree, and a graph g, 
   determine which nodes have not been explored in g."
  [g state]
  (clojure.set/difference (set (get-node-labels g)) 
                          (set (keys (:spt state)))))

(def search-defaults {:endnode  ::nullnode
                      :halt?     default-halt? 
                      :weightf   arc-weight 
                      :neighborf default-neighborf})

;;Default for simple graph walks/explorations
(def walk-defaults 
  (merge search-defaults {:weightf unit-weight :neighborf visit-once}))

(def limited-walk (assoc walk-defaults :neighborf visit-once))
(def unit-walk    (assoc limited-walk  :weightf  unit-weight))

;;Weight and Neighborhood Filters
;;===============================

;;It's useful to define ways to represent the same graph, via simple 
;;transformations, so that we can use arbitrary weight functions and 
;;neighborhood functions to extend or simplify traversal.

;;We'll formalize the concept of graph transforms by defining 
;;the two fundamental transforms: distance (weighting) and
;;connectivity (neighbors).  

;;__TODO__ Think about relocating these from meta to the search
;;state....might be a better place..
(defn get-weightf   [g]   
  (or (get (meta g) :weightf   arc-weight)
      (throw (Exception. "No weight function defined!"))))

(defn get-neighborf [g]   
  (or (get (meta g) :neighborf (:neighborf search-defaults))
      (throw (Exception. "No neighborhood function defined!"))))

;;Allows us to add a hook for node filtering during search.
(defn get-nodefilter [g]  (get (meta g) :nodefilter nil))

;;Unrolling a reduce from traverse.
(definline redloop [f init coll]
  `(loop [acc# ~init 
          xs#  ~coll]
     (if (empty? xs#) acc#
         (recur (~f acc# (first xs#)) (rest xs#)))))

;;__TODO__ Reformat to use nested entries, or build your own primitive type. 
(defn traverse
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode startstate & {:keys [halt? weightf neighborf] 
                                         :or  {halt?     default-halt?
                                               weightf   (get-weightf g)
                                               neighborf (get-neighborf g)}}]
    (let [get-weight    (partial weightf   g)
          get-neighbors (if-let [nodefilter (get-nodefilter g)]
                           (fn [nd s] (nodefilter (neighborf g nd s)))
                           (partial neighborf g))
          relax-by      (fn [source s sink]
                          (generic/relax s get-weight source sink))]
      (loop [state   (-> (assoc startstate :targetnode targetnode)
                         (generic/conj-fringe startnode 0))]
        (if (generic/empty-fringe? state) state 
            (let [nd        (generic/next-fringe state) ;next node to visit
                  visited   (generic/visit-node state nd)] ;record visit.
              (if (halt? state nd) visited                     
                  (recur (reduce (partial relax-by nd) 
                                 visited
                                 (get-neighbors nd state))))))))) 

(defn drop-empty-options [m]
  (reduce (fn [m k] (if (nil? (get m k)) (dissoc m k) m)) m (keys m)))

(defmacro defwalk
  "Macro for helping us define various kinds of walks, built on top of 
   traverse.  Caller can supply a map of default options and a function that 
   consumes a startnode and produces a search state, to produce different kinds 
   of searchs.  Returns a function that acts as a wrapper for traverse, 
   shuttling the supplied defaults to traverse, while allowing callers to
   provide key arguments for neighborf, weightf, and halt?."
  [name docstring state-ctor default-opts]
  `(defn ~name ~docstring [~'g ~'startnode 
                & {:keys [~'endnode ~'halt? ~'neighborf ~'weightf] 
                   :as user-opts#}]
     (let [clean-opts# (drop-empty-options user-opts#) 
           {:keys [~'endnode ~'halt? ~'neighborf ~'weightf]}
           (merge ~default-opts clean-opts#)]
       (traverse ~'g ~'startnode ~'endnode (~state-ctor ~'startnode)
                 :halt? ~'halt?                  
                 :neighborf ~'neighborf 
                 :weightf   ~'weightf))))        

(defwalk depth-walk 
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight." 
  searchstate/empty-DFS walk-defaults)
  
(defwalk breadth-walk
  "Returns a function that explores all of graph g in a breadth-first 
   topological order from startnode.  This is not a search, any paths returned 
   will be relative to unit-weight."
  searchstate/empty-BFS walk-defaults)  
 
(defwalk ordered-walk
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  searchstate/empty-DFS
  (merge walk-defaults {:neighborf visit-ordered-once :weightf unit-weight}))

(defwalk random-walk
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  searchstate/empty-RFS walk-defaults)

(defwalk undirected-walk
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  searchstate/empty-RFS
  (merge walk-defaults {:neighborf visit-neighbors-once :weightf unit-weight}))

(defwalk priority-walk
  "Returns a function that explores all of graph g in a priority-first 
  topological order from a startnode. Weights returned will be in terms of the 
  edge weights in the graph."
 searchstate/empty-PFS search-defaults)

;;explicit searches, merely enforces a walk called with an actual destination
;;node.

(defn dfs
  "Starting from startnode, explores g using a depth-first strategy, looking for
   endnode.  Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  Note: depth first search is 
   not guaranteed to find the actual shortest path, thus the shortest path tree
   may be invalid."
  [g startnode endnode]
  (depth-walk g startnode :endnode endnode))

(defn bfs
  "Starting from startnode, explores g using a breadth-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree 
   or precedence tree, the shortest distance tree.  Note: breadth first search 
   is not guaranteed to find the actual shortest path, thus the shortest path 
   tree may be invalid."
  [g startnode endnode]
  (breadth-walk g startnode :endnode endnode))

(defn pfs
  "Starting from startnode, explores g using a priority-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  The is equivalent to dijkstra's
   algorithm.  Note: Requires that arc weights are non-negative.  For negative 
   arc weights, use Bellman-Ford, or condition the graph."
  [g startnode endnode]
  (priority-walk g startnode :endnode endnode))

(defn rfs
  "Starting from startnode, explores g using random choice, looking for endnode. 
   Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree."
  [g startnode endnode]
  (random-walk g startnode :endnode endnode))

;;__TODO__ Consolidate these guys into a unified SSP function that defaults to 
;;dijkstra's algorithm, but allows user to supply a heuristic function, and 
;;automatically switches to A*.

(defn dijkstra
  "Starting from startnode, explores g using dijkstra's algorithm, looking for
   endnode.  Gradually relaxes the shortest path tree as new nodes are found.  
   If a relaxation provides a shorter path, the new path is recorded.  Returns a 
   search state, which contains the shortest path tree or precedence tree, the 
   shortest distance tree.  Note: Requires that arc weights are non-negative.  
   For negative arc weights, use Bellman-Ford, or condition the graph."
  [g startnode endnode & {:keys [weightf neighborf] 
                          :or   {weightf   (get-weightf g) 
                                 neighborf (get-neighborf g)}}]
  (priority-walk g startnode :endnode endnode
   :weightf  (memoize (fn [g source sink]
                        (let [w (weightf g source sink)]
                          (if (>= w 0)  w ;positive only
                            (throw 
                              (Exception. 
                                (str "Negative Arc Weight Detected in Dijkstra: " 
                                     [source sink w])))))))
   :neighborf neighborf))                                            

;;__TODO__ Check implementation of a-star, I think this is generally correct.
(defn a*
  "Given a heuristic function, searches graph g for the shortest path from 
   start node to end node.  Operates similarly to dijkstra or 
   priority-first-search, but uses a custom weight function that applies the 
   hueristic to the weight.  heuristic should be a function that takes a 
   a source node and target node, and returns an estimated weight to be 
   added to the actual weight.  Note, the estimated value must be non-negative."
  [g heuristic-func startnode endnode]
  (traverse g startnode endnode 
    (assoc (searchstate/empty-PFS startnode) :estimator heuristic-func)))

;;__TODO__ Check implementation of Bellman-Ford.  Looks okay, but not tested.

(defn negative-cycles?
  "Predicate for determining if the spt in the search state resulted in negative
   cycles."
  [g final-search-state get-weight]
  (let [distance  (:distance final-search-state)
        ;we violate the triangle inequality if we can improve any distance.
        improvement? (fn [arc]
                       (let [[u v] arc]
                         (when (< (+ (get distance u) (get-weight u v)) 
                                  (get distance v))    u)))
        nodes (keys (generic/-get-nodes g))]
    (filter improvement? (for [u nodes
                             v (generic/-get-sinks g u)]  [u v]))))

(defn bellman-ford
  "The Bellman-Ford algorithm can be represented as a generic search similar
   to the relaxation steps from dijkstra's algorithm.  The difference is that
   we allow negative edge weights, and non-negative cycles.  The search uses 
   a queue for the fringe, rather than a priority queue.  Other than that, 
   the search steps are almost identical."
  [g startnode endnode & {:keys [weightf neighborf] 
                          :or   {weightf   (get-weightf g) 
                                 neighborf (get-neighborf g)}}]
  (let [startstate    (assoc (searchstate/empty-BFS startnode) 
                             :targetnode endnode)
        bound         (dec (count (generic/-get-nodes g))) ;v - 1 
        get-weight    (partial weightf g)
        get-neighbors (partial neighborf g)
        relaxation    (fn [source s sink] 
                        (generic/relax s get-weight source sink))
        validate      (fn [s] (if-let [res (first 
                                             (negative-cycles? g s get-weight))]
                                (assoc s :negative-cycles res)
                                  s))]
    (loop [state (generic/conj-fringe startstate startnode 0)
           idx   0]
        (if (or  (= idx bound) (generic/empty-fringe? state))
          (validate state) 
          (let [nd        (generic/next-fringe state)]  ;next node to visit   
            (recur (reduce (partial relaxation nd) 
                           (generic/visit-node state nd) 
                           (get-neighbors nd state))
                   (unchecked-inc idx)))))))





;;with-dropped-edges 
;;with-dropped-nodes 
;;with-added-edges 
;;with-added-nodes 
