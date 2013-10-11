(ns spork.cljgraph.core
  (:require [spork.protocols.core :refer :all]
            [spork.cljgraph [search :as search]]
            [spork.data [digraph :as dig] [searchstate :as sstate]] 
            [spork.util     [topographic :as top]]))
 
(def empty-graph dig/empty-digraph)

;;Graph-Backed Operations
;;=======================
(defn u-arcbound [nodecount] 
  (/ (* nodecount (dec nodecount)) 2))

(defn d-arcbound [nodecount]
  (* nodecount (dec nodecount)))

;;General Graph Operations
;;========================

;;Protocol derived functionality for operations on Topographs
;;Some of these are mere wrappers around the protocol functions.  When possible,
;;the lower-level protocol functions are to be eschewed for the API presented
;;herein.

;;Node operations (where nodes act as primary keys in a graph database)
;;=====================================================================

(defn nodes
  "Returns a persistent map - of node keys to node data - used by topograph."
  [g] (-get-nodes g))

(defn has-node? [g nd] (-has-node? g nd))
(defn has-arc?  [g source sink] (-has-arc? g source sink))

(defn conj-node 
  ([g k v] (-conj-node g k v))
  ([g k]   (conj-node g k (count (-get-nodes g)))))


;;This could be a bit destructive.  It allows changing existing nodes.
(defn set-node
  "Allows the data mapped to an existing node to be replaced."
  [g k v]
  (assert (has-node? g k) (str "Cannot set a non-existent node: " k))
  (-set-nodes g (assoc (-get-nodes g) k v)))

(defn disj-node
  "Removes node k, and any arcs incident to node k are also removed."
  [g k] (-disj-node g k))

(defn drop-nodes
  "Disjoins all nodes in coll from the g, including incident arcs."
  [g coll] (reduce disj-node g coll))

(defn ensure-nodes [g ks] 
  (reduce (fn [acc k] (if (has-node? acc k) acc (conj-node acc k))) g ks))

(defn get-node
  "Fetches the data associated with k in the topograph's node map."
  [g k] (get (-get-nodes g) k)) 

(defn get-node-labels
  "Fetches the keys, implied to be node labels, associated with the nodes in 
   topograph g."
  [g] (keys (nodes g)))

;;Arc Operations
;;==============

(defn conj-arc
  "Adds an arc from source to sink, with an optional weight w.  If neither node
   exists, nodes are created implicity.  This allows easy inline construction of 
   graphs."  
  ([g source sink w] (-conj-arc (ensure-nodes g [source sink]) source sink w))
  ([g source sink] (conj-arc g source sink 1)))

(defn disj-arc
  "Drops the arc from source to sink, if one exists in the topograph."
  [g source sink] (-disj-arc g source sink))

(defn add-arcs
  "Adds a sequence of arcs, xs, where xs are [from to & [w]]."
  [g xs]  
  (reduce (fn [acc [from to & [w]]] 
            (conj-arc acc from to w)) g xs))
(defn drop-arcs
  "Drops a sequence of arcs, of the form [from to], from the topograph."
  [g xs] (reduce #(disj-arc %1 (first %2) (second %2)) g xs))

(defn arc-weight
  "The weight of an arc [from to] in g."
  [g from to]
  (assert (has-arc? g from to) (str "Arc does not exist " [from to]))
  (nth (-get-arc g from to) 2))

;;Neighborhood operations
;;=======================
(defn sinks   "Nodes with arcs from k"  [g k]  (-get-sinks g k))
(defn sources "Nodes with arcs to   k"  [g k]  (-get-sources g k))
(defn neighbors "Nodes with arcs to or from k" [g k]  
  (vec (distinct (mapcat #(% g k) [sources sinks]))))

(defn- get-degree [g nd f]
  (if-let [itms (f g nd)]
    (if (contains? itms  nd)
      (inc (count itms))
      (count itms))
    0))

(defn get-indegree
  "What is the in-degree of the node?  For directed graphs, how many incident 
   arcs does this node serve as the sink for?  Self-loops count twice."
  [g nd]
  (get-degree g nd sources))
      
(defn get-outdegree
  "What is the in-degree of the node?  For directed graphs, how many incident 
   arcs does this node serve as the sink for?  Self-loops count twice."
  [g nd]
  (get-degree g nd sinks))

(def missing-node? (comp not has-node?))

(defn terminal-node?
  "Is node n devoid of outbound arcs?"
  [g n] (not (seq (sinks g n))))

(defn source-node?
  "Is node n devoid of inbound arcs?"
  [g n] (not (seq (sources g n))))

(defn island?
  "Does node n have any neighbors?"
  [g n] (and (terminal-node? g n) (source-node? g n)))

;;Borrowed idea from Data.Graph.Inductive 

(defn node-context
  "Provides a lens, or a focus, of all the graph data for a particular node."
  [g k] 
  (when (has-node? g k) {:node k 
                          :data (get (nodes g) k)
                          :sources (sources g k)
                          :sinks   (sinks g k)}))
(defn graph-seq
  "Views the topograph as a sequence of node contexts."
  [g] 
  (sort-by :data (map (partial node-context g) (keys (nodes g)))))

;;Relabeling Nodes
(defn arcs-from
  "Returns a vector of arcs sourced by nd, of the form [nd to weight]."
  [g nd]  
  (vec (map (partial -get-arc g nd) (sinks g nd))))

(defn arcs-to
  "Returns a vector of arcs terminated by nd, of the form [from nd weight]."
  [g nd]  
  (vec (map #(-get-arc g % nd) (sources g nd))))
    
(defn relabel-node
  "Allows effecient relabeling of a node key.  Automatically updates related 
   arc data."
  [g old-label new-label]
  (let [{:keys [node data sources sinks]} (node-context g old-label)]
    (-> (disj-node g old-label)
        (conj-node new-label data)
        (add-arcs (map (fn [[_ to w]] [new-label to w]) 
                       (arcs-from g old-label)))
        (add-arcs (map (fn [[from _ w]] [from new-label w]) 
                         (arcs-to g old-label)))))) 

;;A simple lifting function for developing neighborhood functions for searches 
;;and walks that are agnostic to the searchstate.
(defn neighbor-by [f] (fn [g nd _] (f g nd)))

;;Simple Graph Walks
;;===========================

;;Walks and searches are based on the notion of a generic traversal, which 
;;uses a generic search state and retains detailed information.  As a result,
;;walks and searches return the detailed search state, including the shortest
;;path tree, the distance map, the order of visitation, the remaining fringe, 
;;and more.   

;;We build more mundane queries, like the depth-first ordering of nodes, out 
;;of the walks, simply yielding the order of visitation.

(defn depth-walk
  "A wrapper around the more thorough traversals defined in
   spork.cljgraph.search  .  Performs a depth traversal of the graph, starting 
   at startnode.  Used to define other higher order graph queries."
  [g startnode & {:keys [neighborf]}]
  (search/depth-walk g startnode :neighborf neighborf))
 
(defn breadth-walk
  "A wrapper around the more thorough traversals defined in
   spork.cljgraph.search  .  Performs a breadth traversal of the graph, starting 
   at startnode.  Used to define other higher order graph queries."
  [g startnode & {:keys [neighborf]}]
  (search/breadth-walk g startnode :neighborf neighborf))

(defn random-walk
  "A wrapper around the more thorough traversals defined in
   spork.cljgraph.search  .  Performs a random traversal of the graph, starting 
   at startnode.  Used to define other higher order graph queries."
  [g startnode & {:keys [neighborf]}]
  (search/random-walk g startnode :neighborf neighborf))

(defn ordered-walk
  "A wrapper around the more thorough traversals defined in
   spork.cljgraph.search  .  Performs an ordered traversal of the graph, where 
   the neighbors are visited in the order they were appended to the graph.
   Starts walking from  startnode.  Used to define other higher order graph 
   queries."
  [g startnode & {:keys [neighborf]}]
  (search/ordered-walk g startnode :neighborf neighborf))

(defn undirected-walk
  "Performs a depth-first traversal of the graph, treating the directed graph 
   as an undirected graph.  Starts walking from  startnode."
  [g startnode]
  (search/depth-walk g startnode :neighborf (neighbor-by neighbors)))

;;Node Orderings
;;==============
;;Node Orderings yield a vector of the nodes visited during traversal, including
;;the startnode. 

(defn depth-nodes
  "Returns the nodes visited in a depth traversal of the graph, starting 
   at startnode."  
  [g startnode & {:keys [neighborf]}] 
  (:visited (depth-walk g startnode :neighborf neighborf)))

(defn breadth-nodes
  "Returns the nodes visited in a breadth traversal of the graph, starting 
   at startnode."  
  [g startnode & {:keys [neighborf]}] 
  (:visited (breadth-walk g startnode :neighborf neighborf)))

(defn random-nodes
  "Returns the nodes visited in a random traversal of the graph, starting 
   at startnode."  
  [g startnode & {:keys [neighborf]}] 
  (:visited (random-walk g startnode :neighborf neighborf)))

(defn ordered-nodes
  "Returns the nodes visited in an ordered traversal of the graph, starting 
   at startnode."  
  [g startnode & {:keys [neighborf]}] 
  (:visited (ordered-walk g startnode :neighborf neighborf)))

(defn undirected-nodes
  "Returns the nodes visited in a depth-first traversal of the graph, starting 
   at startnode.  Treats graph g as undirected."  
  [g startnode] 
  (:visited (undirected-walk g startnode)))

;;Simple Connectivity Queries
;;===========================

;;I may need to rethink these guys, and prefere lazy seqs, if we ever apply 
;;this library to large graphs or trees.  succs and preds will walk the whole 
;;thing, which may be untenable.

(defn succs
  "Returns the a set of all the nodes reachable starting from node k."
  [g k] 
  (disj (set (depth-nodes g k :neighborf (neighbor-by sinks)) k)))

(defn preds
  "Returns the set of all the nodes that can reach node k."
  [g k] 
  (disj (set (depth-nodes g k :neighborf (neighbor-by sources))) k))

(defn component
  "Returns the set of nodes that can reach or can be reached through node k."  
  [g k] (set (depth-nodes g k :neighborf (neighbor-by neighbors))))

(defn components
  "Finds all components in the topograh, returning a mapping of component size 
   to vectors of node sets in each component."
  [g]
  (loop [xs (set (keys (nodes g)))
         acc []]
    (if (empty? xs) 
      (group-by count acc)
      (let [c (component g (first xs))]
        (recur (clojure.set/difference xs (set c))
               (conj acc c))))))

;;__Rewrite islands, you can do it more efficiently that using components.__
(defn islands
  "Returns a sequence of nodes that are unreachable."
  [g] 
  (filter (partial island? g)  (get-node-labels g)))

(defn get-roots
  "Fetch the roots for graph g, where roots are nodes that have no inbound or 
   source arcs.  Caller may provide a set of candidate node labels with a second
   arg, as xs."
  ([g xs] (filter #(source-node? g %) xs))
  ([g] (get-roots g (get-node-labels g))))
        
(defn drop-roots
  "Return the result of dropping the current set of root nodes from graph g. 
   Root nodes are nodes that have no inbound or source arcs.  Dropping the node
   will act akin to disj-node, and will eliminate incident arcs, possibly 
   creating new root nodes in the resulting graph."
  [g]
  (reduce #(disj-node %1 %2) g (get-roots g)))

;;Graph Reduction and Decomposition
;;=================================

(defn subgraph 
  "Given a starting point, root-key, and an optional node filter, derives a 
   reduced, or filtered graph based on the nodes that pass the filter.  Defaults
   to a depth-walk as a node filter, so only nodes reachable from root-key are 
   retained."
  ([g node-filter-func root-key]
    (let [retained-keys (set (node-filter-func g root-key))
          valid?   (partial contains? retained-keys)
          arcs-to-clone (fn [k] 
                          (distinct 
                            (concat 
                              (filter #(valid? (second %)) (arcs-from  g k)) 
                              (filter #(valid? (first %))  (arcs-to    g k)))))
          old-keys      (set (keys (nodes g)))]
      (if (< (count retained-keys) (- (count old-keys) (count retained-keys)))
          (reduce (fn [acc x] (-> (conj-node acc x (get-node g x))
                                  (add-arcs (arcs-to-clone x))))
                  empty-graph retained-keys)
          (reduce (fn [acc x] (disj-node acc x)) g 
                  (clojure.set/difference old-keys retained-keys)))))
  ([g root-key] (subgraph g depth-walk root-key)))

(defn decompose
  "Maps the topograph into one or more subgraphs, one for each component."
  [g]
  (for [[size node-sets] (components g)
        xs               node-sets]
    (subgraph g (fn [& args] xs) (first xs))))

;;Topological Sorting
;;=================================

(defn topsort
  "Topologically sort the graph, returning a sequence of sets of topological 
   roots.  Multiple valid topological orderings may be returned from the
   traversing cartesian products of the result sets."
  [g]
	(loop [graph g
         roots (set (get-roots g)) ;;current roots in g 
         acc []]
     (if (seq roots)
       (let [xs  (mapcat (partial sinks graph)  roots) ;only possible new roots.
             next-graph  (drop-nodes graph roots)]
         (recur next-graph (set (get-roots next-graph xs))  (conj acc roots)))
       (if (= (reduce + (map count acc)) 
              (count (top/nodes g)))
           acc
           nil))))

(defn topsort-nodes
  "Return an arbitrary valid topological ordering for graph g."
  [g]
  (persistent! 
    (reduce (fn [acc xs] (reduce conj! acc xs)) (transient []) (topsort g)))) 

;;Searches
;;========

;;Explicit searches, merely enforces a walk called with an actual destination
;;node.  Return the common searchstate data that the walks utilize.

(defn depth-first-search
  "Starting from startnode, explores g using a depth-first strategy, looking for
   endnode.  Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  Note: depth first search is 
   not guaranteed to find the actual shortest path, thus the shortest path tree
   may be invalid."
  [g startnode endnode]
  (search/dfs g startnode endnode))

(defn breadth-first-search
  "Starting from startnode, explores g using a breadth-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree 
   or precedence tree, the shortest distance tree.  Note: breadth first search 
   is not guaranteed to find the actual shortest path, thus the shortest path 
   tree may be invalid."
  [g startnode endnode]
  (search/bfs g startnode endnode))

(defn priority-first-search
  "Starting from startnode, explores g using a priority-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  The is equivalent to dijkstra's
   algorithm.  Note: Requires that arc weights are non-negative.  For negative 
   arc weights, use Bellman-Ford, or condition the graph."
  [g startnode endnode]
  (search/pfs g startnode endnode))

(defn random-search 
  "Starting from startnode, explores g using random choices, looking 
   for endnode. Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  The is equivalent to dijkstra's
   algorithm.  Note: Requires that arc weights are non-negative.  For negative 
   arc weights, use Bellman-Ford, or condition the graph."
  [g startnode endnode]
  (search/rfs g startnode endnode))

;;Single Source Shortest Paths
;;============================
(defn dijkstra
  "Starting from startnode, explores g using dijkstra's algorithm, looking for
   endnode.  Gradually relaxes the shortest path tree as new nodes are found.  
   If a relaxation provides a shorter path, the new path is recorded.  Returns a 
   search state, which contains the shortest path tree or precedence tree, the 
   shortest distance tree.  Note: Requires that arc weights are non-negative.  
   For negative arc weights, use Bellman-Ford, or condition the graph."
  [g startnode endnode] 
  (search/priority-walk g startnode :endnode endnode))

;;__TODO__ Check implementation of a-star, I think this is generally correct.
(defn a-star
  "Given a heuristic function, searches graph g for the shortest path from 
   start node to end node.  Operates similarly to dijkstra or 
   priority-first-search, but uses a custom weight function that applies the 
   hueristic to the weight.  heuristic should be a function that takes a 
   a source node and target node, and returns an estimated weight to be 
   added to the actual weight.  Note, the estimated value must be non-negative."
  [g heuristic-func startnode endnode]
  (search/a* g heuristic-func startnode endnode))

(defn bellman-ford
  "The Bellman-Ford algorithm can be represented as a generic search similar
   to the relaxation steps from dijkstra's algorithm.  The difference is that
   we allow negative edge weights, and non-negative cycles.  The search uses 
   a queue for the fringe, rather than a priority queue.  Other than that, 
   the search steps are almost identical."
  [g startnode endnode & {:keys [weightf neighborf]
                          :or   {weightf   arc-weight
                                 neighborf (neighbor-by sinks)}}]
  (search/bellman-ford g startnode endnode :weightf weightf 
                                           :neighborf neighborf))

(defn bellman-ford-DAG 
  "If we know that graph g is a Directed Acyclic Graph before hand, we can 
   topologically sort the graph, find the dependent nodes between startnode
   and endnode, and bellman-ford just those nodes.  This is faster, as it can 
   eliminate many vertices, and only performs one pass through the vertices to 
   compute the shortest path tree."
  [g startnode endnode & {:keys [weightf neighborf]
                          :or   {weightf   arc-weight
                                 neighborf (neighbor-by sinks)}}]
  (if-let [ordered-nodes (->> (topsort-nodes g)
                              (drop-while #(not= % startnode))
                              (take-while #(not= % endnode)))]
      (let [node-filter (hash-set ordered-nodes)
            filtered-neighbors (fn [g nd state] 
                                 (filter node-filter (neighborf g nd state)))]
        (bellman-ford g startnode endnode :weightf weightf 
                                          :neighborf filtered-neighbors))
      ;startnode does not precede endnode in the topological order.
      nil))

;;Paths
;;=====
;;We can recover paths from walks, or searches, or any process that yields a 
;;searchstate.  More general operations are found in __spork.data.searchstate__.
(defn path?
  "Given a search state and a target node, returns the path weight or nil if 
   no path exists.  There may be multiple valid paths, this merely indicates one
   exists.  If no target is provided, the intended end node is pulled from the 
   search state."
  ([state target] (sstate/path? state target))
  ([state] (path? state (:targetnode state))))

(defn get-paths 
  "Given a search state and a target node, returns a lazy sequence of paths 
   discovered during the search.  If no target is provided, the intended end 
   node is pulled from the search state."
  ([state target] (sstate/paths (:shortest state) (:startnode state) target))
  ([state] (get-paths state (:targetnode state))))
(defn get-weighted-paths 
  "Given a search state and a target node, returns a lazy sequence of 
   [path path-weight] pairs discovered during the search.  If no target is 
   provided, the intended end node is pulled from the search state."
  ([state target] (sstate/paths (:shortest state) (:startnode state) target))
  ([state] (get-weighted-paths state (:targetnode state))))

;;Testing/Examples
(comment 


  ;a tree of elements:
  ;              a
  ;            b c d 
  ;           e f    g
  ;          h   i     j
  ;         k l  m n o  p q
  ;                        r s t u v w x y z a1 a2 a3 
 (defn tree-arcs [from xs] (map #(vector from %) xs))
 (def the-tree (-> empty-graph
                 (add-arcs (tree-arcs :a [:b :c :d]))
                 (add-arcs (conj (tree-arcs :b [:e :f]) [:d :g]))
                 (add-arcs [[:e :h] [:f :i] [:g :j]])
                 (add-arcs [[:h :k] [:h :l] [:i :m] [:i :n] [:i :o]
                            [:j :p] [:j :q]])
                 (add-arcs (tree-arcs :q  
                             [:r :s :t :u :v :w :x :y :z :a1 :a2 :a3]))))
 
 (assert (= (depth-nodes the-tree :a)
             [:a :d :g :j :q :a3 :a2 :a1 :z :y :x :w :v :u :t :s :r :p :c :b 
              :f :i :o :n :m :e :h :l :k]))
 
 (assert (= (ordered-nodes the-tree :a)
            [:a :b :e :h :k :l :f :i :m :n :o :c :d :g :j :p :q :r :s :t :u :v 
             :w :x :y :z :a1 :a2 :a3]))
 
 (assert (= (breadth-nodes the-tree :a)
            [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v 
             :w :x :y :z :a1 :a2 :a3]))
 
 (assert (= (undirected-nodes the-tree :a) 
           [:a :d :g :j :q :a3 :a2 :a1 :z :y :x :w :v :u :t :s :r :p :c :b :f 
            :i :o :n :m :e :h :l :k]))
 (assert (= (topsort-nodes the-tree)
            [:a :c :b :d :f :g :e :j :i :h :l :k :m :n :o :q 
             :p :a3 :r :z :y :x :v :w :t :u :a2 :a1 :s]))
 
 ;a directed graph...
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f/^
 
 (def the-graph 
   (-> empty-graph
       (add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:f :c] [:b :d]]))) 
 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 
 (def the-graph2 
   (-> empty-graph
       (add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])))
 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 ;    g 
 ;    h
 
 (def the-graph3
   (-> empty-graph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])
     (conj-node :g)
     (conj-node :h)))

 ;a directed graph with 2 components
 ;    a - b - c - d - e
 (def the-list 
   (-> empty-graph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e]])))
 (def dlist (->double-list :a :e the-list))

 (def the-root-tree (-> empty-graph (conj-node :root)))
 (def the-other-tree (-> empty-graph (add-arcs (tree-arcs :a [:b :c]))))
 
)

