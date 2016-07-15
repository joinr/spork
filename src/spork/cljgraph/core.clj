;;A suite of operations on persistent graph structures.  Operations 
;;include creation and manipulation of directed graphs, topological
;;queries, generic walks, searches, and single-source shortest path
;;algorithms.  
(ns spork.cljgraph.core
  (:require [spork.protocols [core :as generic]]
            [spork.cljgraph  [search :as search]]
            [spork.data      [digraph :as dig] [searchstate :as sstate] orderedmap]))

;;Utils
;;=====

(defn- single? [x]
  (cond (instance? clojure.lang.Counted x)
        (== (count x) 1)
        (seq? x)
        (nil? (next x))))
;;Macrology
;;=========
;;Helpful infrastructure, maybe move this guy over to
;;spork.utils.metaprogramming

;;I wrote these to address a current weakness in clojure's idiomatic 
;;varargs implementation:  any RestFn objects that are created - via 
;;partial or having a varargs function declaration, injects a serious 
;;performance penalty in practice.  Clojure defaults to array-seq'ing 
;;the args every time, which injects a ton of bottlenecks.  So, 
;;my way around it is to provide some forms for defining functions 
;;that look similar to the variadic forms.  Users supply a single
;;value for the optional args, else a lower-arity function will 
;;be invoked and use defaults at no cost.  In practice, this is 
;;much more efficient.  Anything you can do to avoid RestFn 
;;generation is really important for efficiency.
(defmacro defn-curried-options [name doc args opt-map & body]
  (let [opts (gensym "opt-map")
        args+opts (conj args opt-map)
        args+nil  (conj args nil)]
    `(defn ~name ~doc 
       ([~@args] (~name ~@args+nil))
       ([~@args+opts] ~@body))))

;;interprets a function declaration's argument vector as if the 
;;last argument is implicitly curried.
(defmacro implicitly-curried-options 
  [name doc args body]
  `(defn-curried-options ~name ~doc ~(vec (butlast args)) ~(last args) ~body))

;;Traverses an expression, replacing defns with curried macros.
(defmacro with-implicitly-curried-options [& exprs]
  `(do ~@(map (fn [expr] 
            (if (not= (first expr) 'defn) expr
                (cons 'implicitly-curried-options (rest expr)))) exprs)))

;;Graph API
;;=========

(def empty-graph dig/empty-digraph)
(def empty-ordered-graph dig/empty-ordered-digraph)


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
  [g] (generic/-get-nodes g))

(definline has-node? [g nd] `(generic/-has-node? ~g ~nd))
(definline has-arc?  [g source sink] `(generic/-has-arc? ~g ~source ~sink))

;;This could be a bit destructive.  It allows changing existing nodes.
(defn set-node
  "Allows the data mapped to an existing node to be replaced."
  [g k v]
  (assert (has-node? g k) (str "Cannot set a non-existent node: " k))
  (generic/-set-nodes g (assoc (generic/-get-nodes g) k v)))

;;Fixed to prevent losing neighborhoods.  When nodes are conjoined, if they 
;;already exist, the neighborhood is maintained, only the value is conjoined.
(defn conj-node 
  ([g k v]   (if (generic/-has-node? g k) (set-node g k v) (generic/-conj-node g k v)))
  ([g k]     (conj-node g k (count (generic/-get-nodes g)))))

(defn disj-node
  "Removes node k, and any arcs incident to node k are also removed."
  [g k] (generic/-disj-node g k))

(defn drop-nodes
  "Disjoins all nodes in coll from the g, including incident arcs."
  [g coll] (reduce disj-node g coll))

(defn ensure-nodes [g ks] 
  (reduce (fn [acc k] (if (has-node? acc k) acc (conj-node acc k))) g ks))

(defn get-node
  "Fetches the data associated with k in the topograph's node map."
  [g k] (get (generic/-get-nodes g) k)) 

(defn get-node-labels
  "Fetches the keys, implied to be node labels, associated with the nodes in 
   topograph g."
  [g] (keys (nodes g)))

(defn get-graph-data 
  "Returns any non-graph data associated with the graph g."
  [g] (generic/-get-graph-data g))

(defn set-graph-data 
  "Associates some data d with the graph."
  [g d] (generic/-set-graph-data g d))

;;Arc Operations
;;==============

(defn conj-arc
  "Adds an arc from source to sink, with an optional weight w.  If neither node
   exists, nodes are created implicity.  This allows easy inline construction of 
   graphs."  
  ([g source sink w] (generic/-conj-arc (ensure-nodes g [source sink]) source sink w))
  ([g source sink] (conj-arc g source sink 1)))

(defn disj-arc
  "Drops the arc from source to sink, if one exists in the topograph."
  [g source sink] (generic/-disj-arc g source sink))

(defn add-arcs
  "Adds a sequence of arcs, xs, where xs are [from to & [w]]."
  [g xs]  
  (reduce (fn [acc [from to & [w]]] 
            (conj-arc acc from to w)) g xs))
(defn drop-arcs
  "Drops a sequence of arcs, of the form [from to], from the topograph."
  [g xs] (reduce #(disj-arc %1 (first %2) (second %2)) g xs))

;"The weight of an arc [from to] in g."
(definline arc-weight
  [g from to]
  `(generic/-arc-weight ~g ~from ~to))

(defn arc-weight-safe
  "The weight of an arc [from to] in g."
  [g from to]
  (assert (has-arc? g from to) (str "Arc does not exist " [from to]))
  (generic/-arc-weight g from to))

;;Graph Construction
;;==================
(defn arcs->graph [xs]
  (-> empty-graph (add-arcs xs)))

;;Neighborhood operations
;;=======================

;"Nodes with arcs from k"
(definline sinks       [g k]  `(generic/-get-sinks ~g ~k))
;"Nodes with arcs to   k"
(definline sources     [g k]  `(generic/-get-sources ~g ~k))

(defn neighbors "Nodes with arcs to or from k" [g k]  
  (vec (distinct (mapcat #(% g k) [sources sinks]))))

;"A  map of node->weight for every node with arcs from k"
(definline sink-map     [g k]  `(generic/-sink-map ~g ~k))
;"A map of node->weight for every node with arcs to k"
(definline source-map   [g k]  `(generic/-source-map ~g ~k))

(defn get-indegree
  "What is the in-degree of the node?  For directed graphs, how many incident 
   arcs does this node serve as the sink for?  Self-loops count twice."
  [g nd]
  (count (sources g nd)))
      
(defn get-outdegree
  "What is the in-degree of the node?  For directed graphs, how many incident 
   arcs does this node serve as the sink for?  Self-loops count twice."
  [g nd]
  (count (sinks  g nd)))

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
  (mapv #(generic/-get-arc g nd %) (sinks g nd)))

(defn arcs-to
  "Returns a vector of arcs terminated by nd, of the form [from nd weight]."
  [g nd]  
  (mapv #(generic/-get-arc g % nd) (sources g nd)))

(defn arc-seq 
  "Return a sequence of directed arcs the consitute the graph."
  [g] (mapcat #(arcs-from g %) (get-node-labels g)))

(defn copy-arcs  
  "Copies arcs from one graph to another."
  [from to]  (add-arcs to (arc-seq from)))

(defn relabel-node
  "Allows efficient relabeling of a node key.  Automatically updates related 
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

(with-implicitly-curried-options
  (defn depth-walk
    "A wrapper around the more thorough traversals defined in
     spork.cljgraph.search  .  Performs a depth traversal of the graph, starting 
     at startnode.  Used to define other higher order graph queries."
    [g startnode opts]
    (search/depth-walk g startnode ::undefined opts))
  
  (defn breadth-walk
    "A wrapper around the more thorough traversals defined in
     spork.cljgraph.search  .  Performs a breadth traversal of the graph, starting 
     at startnode.  Used to define other higher order graph queries."
    [g startnode opts]
    (search/breadth-walk g startnode ::undefined opts))

  (defn ordered-walk 
    "A wrapper around the more thorough traversals defined in
     spork.cljgraph.search  .  Performs an ordered traversal of the graph, starting 
     at startnode.  Used to define other higher order graph queries."
    [g startnode opts]    
    (search/ordered-walk g startnode ::undefined opts))
    

  (defn random-walk
    "A wrapper around the more thorough traversals defined in
     spork.cljgraph.search  .  Performs a random traversal of the graph, starting 
     at startnode.  Used to define other higher order graph queries."
    [g startnode opts]
    (search/random-walk g startnode ::undefined opts)))

(defn undirected-walk
  "Performs a depth-first traversal of the graph, treating the directed graph 
   as an undirected graph.  Starts walking from  startnode."
  [g startnode]
  (search/depth-walk g startnode ::undefined  {:neighborf (neighbor-by neighbors)}))

;;Node Orderings
;;==============
;;Node Orderings yield a vector of the nodes visited during traversal, including
;;the startnode. 

(with-implicitly-curried-options
  (defn depth-nodes
    "Returns the nodes visited in a depth traversal of the graph, starting 
     at startnode."  
    [g startnode opts] 
    (generic/visited-nodes (depth-walk g startnode opts)))

  (defn breadth-nodes
    "Returns the nodes visited in a breadth traversal of the graph, starting 
     at startnode."  
    [g startnode opts] 
    (generic/visited-nodes (breadth-walk g startnode  opts)))

  (defn ordered-nodes
    "Returns the nodes visited in an ordered traversal of the graph, starting 
     at startnode.  Implementation-wise, this is the fastest means of traversal, 
     since it uses a simple list to collect the nodes ala stack fashion."  
    [g startnode opts] 
    (generic/visited-nodes (ordered-walk g startnode  opts)))

  (defn random-nodes
    "Returns the nodes visited in a random traversal of the graph, starting 
     at startnode."  
    [g startnode opts] 
    (generic/visited-nodes (random-walk g startnode  opts))))

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
  (disj (set (depth-nodes g k {:neighborf (neighbor-by sinks)})) k))

(defn preds
  "Returns the set of all the nodes that can reach node k."
  [g k] 
  (disj (set (depth-nodes g k {:neighborf (neighbor-by sources)})) k))

(defn component
  "Returns the set of nodes that can reach or can be reached through node k."  
  [g k] (set (depth-nodes g k {:neighborf (neighbor-by neighbors)})))

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


;;We could unify this for our components function too....hmmm
(declare strong-connect) 
(defn strongly-connected-components 
  "Computes the strongly connected components of graph g.  These correspond to cycles in digraphs.  While there is 
   guaranteed to be a cycle indicated by the component "
  ([g nodes containerf]
     (let [s        (atom '())
           indices  (atom  {})
           lowlinks (atom  {})
           active   (atom nil) 
           scc      (atom  {})
           idx      (atom 0)]
       (doseq [v nodes]
         (when (not (get @indices v))
           (strong-connect g idx s v indices lowlinks scc active containerf)))
       @scc))
  ([g] (strongly-connected-components g (get-node-labels g) hash-set)))

;;Note: Optimization point: we're seemingly spending a lot of time
;;conjing onto the list...This gets called a lot from a client
;;lib, and conj inside the atom seems to be a weak point.   Alternately,
;;replace the atom with an arraylist.
;aux function for Tarjan's strongly connected components algo.
(defn- strong-connect [g idx s v indices links sccs active containerf]
  (do  (swap! indices assoc  v @idx)
       (swap! links   assoc  v @idx)
       (swap! s conj         v)
       (swap! idx unchecked-inc)
       (swap! active (fn [m] (assoc m v 1))))
       (reduce (fn [acc w]
                 (cond  (not (contains? @indices w)) 
                        (do (strong-connect g idx s w indices links sccs active containerf)
                            (swap! links (fn [m] (assoc m v (min (get m v) (get m w))))))
                        (contains? @active w)
                        (swap! links (fn [m] (assoc m v (min (get m v) (get @indices w)))))))                                          
               links (sinks g v))
       (if (== (get @links v) (get @indices v))
         (if (not (next @s))            
           (do (swap! s pop)
               (swap! active dissoc v))
           (do (swap! sccs 
                      (fn [m]
                        (let [cnt (inc (count m))]
                          (->> @s
                               (reduce (fn [acc w]
                                         (do (swap! s pop)
                                             (swap! active dissoc w)
                                             (if (not= v w)
                                               (conj acc w)
                                               (reduced (conj acc w)))))
                                       (containerf)
                                        ;spork.data.orderedmap/empty-ordered-map
                                       )
                               (assoc m cnt)))))))))


(defn cyclical-components 
  "Returns non-singleton components, i.e. sequences of strongly connected nodes which are guaranteed to 
   contain one or more cycles."
  ([g nodes] (filter (complement single?) (vals (strongly-connected-components g nodes list))))
  ([g]       (filter (complement single?) (vals (strongly-connected-components g (get-node-labels g) list)))))

(defn directed-cycles 
  "Note: This was intended to be a computation of simple cycles in a graph.  It uses Tarjan's SCC algorithm, and 
   currently only returns the non-singleton SCCs of the graph - of which there is guarantee to be one or more 
   cycles in the SCC.  Computes the directed cycles of digraph g."
  ([g nodes] (throw (Exception. "Not Implemented: directed-cycles")))
  ([g]       (throw (Exception. "Not Implemented: directed-cycles"))))

;;__Rewrite islands, you can do it more efficiently that using components.__
(defn islands
  "Returns a sequence of nodes that are unreachable."
  [g] 
  (filter #(island? g %)  (get-node-labels g)))

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
          valid?   #(contains? retained-keys %)
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
  ([g root-key] (subgraph g depth-nodes root-key)))

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
              (count (nodes g)))
           acc
           nil))))

(defn topsort-nodes
  "Return an arbitrary valid topological ordering for graph g."
  [g]
  (persistent! 
    (reduce (fn [acc xs] (reduce conj! acc xs)) (transient []) (topsort g)))) 

(defn find-improving-arcs
  "Predicate for determining if any of the directed arcs in the node set have further improvement.  If 
   their distance can be improved, then we have a negative cycle somewhere."
  [g distance nodes weightf]
  (let [;we violate the triangle inequality if we can improve any distance.
        improvement? (fn [x]
                       (let [u (nth x 0)
                             v (nth x 1)]
                         (let [wold (get distance v)
                               wnew (+ (get distance u) (weightf g u v))]
                           (when (< wnew wold)
                             [u v wold :now wnew]))))]
    (->> (for [u nodes
               v (generic/-get-sinks g u)]
           (improvement? [u v]))
         (filter identity))))

;;Searches
;;========

;;Defines a search function with two declarations:  Assumes the last arg in the 
;;init-args declaration is a default argument map for spork.cljgraph.search/traverse, and 
;;sets it up so that those are piped in.  Caller can override any options traverse uses, 
;;providing keys for :weight :neighborf :halt?


(with-implicitly-curried-options                                             
  (defn depth-first-search
    "Starting from startnode, explores g using a depth-first strategy, looking for
     endnode.  Returns a search state, which contains the shortest path tree or 
     precedence tree, the shortest distance tree.  Note: depth first search is 
     not guaranteed to find the actual shortest path, thus the shortest path tree
     may be invalid."
    [g startnode endnode opts] (search/depth-walk g startnode endnode opts))

  (defn breadth-first-search
    "Starting from startnode, explores g using a breadth-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree 
   or precedence tree, the shortest distance tree.  Note: breadth first search 
   is not guaranteed to find the actual shortest path, thus the shortest path 
   tree may be invalid."
    [g startnode endnode opts] (search/breadth-walk g startnode endnode opts))

  (defn priority-first-search
    "Starting from startnode, explores g using a priority-first strategy, looking 
   for endnode. Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree.  The is equivalent to dijkstra's
   algorithm.  Note: Requires that arc weights are non-negative.  For negative 
   arc weights, use Bellman-Ford, or condition the graph."
    [g startnode endnode opts] (search/priority-walk g startnode endnode opts))

  (defn random-search
    "Starting from startnode, explores g using random choice, looking for endnode. 
   Returns a search state, which contains the shortest path tree or 
   precedence tree, the shortest distance tree."
    [g startnode endnode opts] (search/random-walk g startnode endnode opts)))


;;Single Source Shortest Paths
;;============================
(with-implicitly-curried-options 
  (defn dijkstra
    "Starting from startnode, explores g using Dijkstra's algorithm, looking for
   endnode.  Gradually relaxes the shortest path tree as new nodes are found.  
   If a relaxation provides a shorter path, the new path is recorded.  Returns a 
   search state, which contains the shortest path tree or precedence tree, the 
   shortest distance tree.  Note: Requires that arc weights are non-negative.  
   For negative arc weights, use Bellman-Ford, or condition the graph."
    [g startnode endnode  {:keys [weightf neighborf multipath] 
                           :or   {weightf   (search/get-weightf g) 
                                  neighborf (search/get-neighborf g)}}]
    (search/priority-walk g startnode endnode
                          {:weightf  (fn [g source sink]
                                       (let [w (weightf g source sink)]
                                         (if (>= w 0)  w ;positive only
                                             (throw 
                                              (Exception. 
                                               (str "Negative Arc Weight Detected in Dijkstra: " 
                                                    [source sink w]))))))
                           :neighborf neighborf
                           :multipath multipath}))                                            

  ;;__TODO__ Check implementation of a-star, I think this is generally correct.
  (defn a-star
    "Given a heuristic function, searches graph g for the shortest path from 
   start node to end node.  Operates similarly to dijkstra or 
   priority-first-search, but uses a custom weight function that applies the 
   hueristic to the weight.  heuristic should be a function that takes a 
   a source node and target node, and returns an estimated weight to be 
   added to the actual weight.  Note, the estimated value must be non-negative."
    [g heuristic-func startnode endnode {:keys [weightf neighborf multipath] 
                                         :or   {weightf   (search/get-weightf g) 
                                                neighborf (search/get-neighborf g)}}]
    (let [graph-weight weightf
          weightf (fn [g from to] (+ (graph-weight g from to) (heuristic-func from to)))]
      (search/traverse g startnode endnode 
                       (sstate/mempty-PFS startnode) {:weightf weightf 
                                                      :multipath multipath})))


  ;;__TODO__ Check implementation of Bellman-Ford.  Looks okay, but not tested.
  (defn bellman-ford
    "The Bellman-Ford algorithm can be represented as a generic search similar
   to the relaxation steps from dijkstra's algorithm.  The difference is that
   we allow negative edge weights, and non-negative cycles.  The search uses 
   a queue for the fringe, rather than a priority queue.  Other than that, 
   the search steps are almost identical."
    [g startnode endnode {:keys [weightf neighborf multipath] 
                          :or   {weightf   (search/get-weightf g) 
                                 neighborf (search/get-neighborf g)}}]
    (let [startstate    (-> (sstate/mempty-bellman startnode)
                            (generic/set-multipath multipath)
                            (generic/set-start startnode)
                            (generic/set-target endnode))
          bound            (count (generic/-get-nodes g)) 
          validate (fn [s] (if-let [res (first (find-improving-arcs g 
                                                                    (:distance s)
                                                                    (:visited s) weightf))]
                             (throw (Exception. (str "Possible negative cycles in Bellman Ford!" res)))
                             s))]
      (loop [state (generic/conj-fringe startstate startnode 0)
             idx   0]
        (if (or  (== idx bound)  ;v - 1  iterations
                 (generic/empty-fringe? state)) 
          (validate state)
          (let [source     (generic/next-fringe state)]  ;next node to visit   
            (recur (generic/loop-reduce 
                    (fn [acc sink] 
                      (generic/relax acc (weightf g source sink) source sink))
                    (generic/visit-node state source) 
                    (neighborf g source state))
                   (unchecked-inc idx)))))))


  ;;__TODO__ Convert filter
  (defn bellman-ford-DAG 
    "If we know that graph g is a Directed Acyclic Graph before hand, we can 
   topologically sort the graph, find the dependent nodes between startnode
   and endnode, and bellman-ford just those nodes.  This is faster, as it can 
   eliminate many vertices, and only performs one pass through the vertices to 
   compute the shortest path tree."
    [g startnode endnode  {:keys [weightf neighborf multipath]
                           :or   {weightf   arc-weight
                                  neighborf (neighbor-by sinks)}}]
    (if-let [ordered-nodes (->> (topsort-nodes g)
                                (drop-while #(not= % startnode))
                                (take-while #(not= % endnode)))]
      (let [node-filter (into #{endnode} ordered-nodes)
            filtered-neighbors (fn [g nd state] 
                                 (reduce (fn [acc x] (if (node-filter x) (cons x acc) acc)) '()
                                         (neighborf g nd state)))]
        (bellman-ford g startnode endnode {:weightf weightf 
                                           :neighborf filtered-neighbors
                                           :multipath multipath}))
                                        ;startnode does not precede endnode in the topological order.
      nil))
  )


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
  ([state target] (sstate/get-paths state target))
  ([state] (get-paths state (:targetnode state))))

(defn first-path [state] (sstate/first-path state))

(defn get-weighted-paths 
  "Given a search state and a target node, returns a lazy sequence of 
   [path path-weight] pairs discovered during the search.  If no target is 
   provided, the intended end node is pulled from the search state."
  ([state target] 
     (let [distance (get state :distance)]       
       (->> (get-paths state target)
            (map (fn [p] [(get distance target) p]))))) 
  ([state] (get-weighted-paths state (:targetnode state))))


;;Graph Transformations
;;=====================

(defn transform-graph 
  "Caller provides a map of options that are merged with the graph's metadata,
   which subsequent graph libraries will pick up on."
  [g opts]  (with-meta g (merge (meta g) opts)))

(defn with-weights   
  "Returns a graph with an alternate default weight function in its meta.  
   This weight function will be the default used by graph searches."
  [weightf g]     (transform-graph g {:weightf weightf}))

(defn with-neighbors 
  "Returns a graph with an alternate default neighbor function in its meta.  
   This neighbor function will be the default used by graph searches."
  [neighborf g] (transform-graph g {:neighborf neighborf}))

(defn with-nodefilter 
  "Uses nodefilter, a predicate that dispatches based on a node label, 
   to filter the results of neighbor queries during graph searches."
  [pred g]
  (transform-graph g {:nodefilter #(filter pred %)}))


(defmacro with-graph-transform 
  "User provides a map of {weightf f, neighborf f} to be merged 
   with the graph's meta with the intent of temporarily altering
   searches and walks.  Searches will check the graph's meta for 
   these and use them if provided.  Graph-binding, of the form 
   [symbol the-graph], will bind the input graph, the-graph, 
   with its altered meta, to symbol.  The resulting body is then 
   evaluated with the modified graph information."
  [graph-binding weight+neighbors & body]
  (let [[sym the-graph] (first (partition 2 graph-binding))]
    `(let [graph# (transform-graph ~the-graph  ~weight+neighbors)
           ~sym graph#]
       ~@body)))

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

;;a cyclical graph with a strongly connected component:
;    a -> b -> c -> d -> e
;         ^
;         |              /
;         \--------------

(def cycle-graph 
     (-> empty-graph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e] [:e :b]])))

(def big-cycle-graph 
     (-> empty-graph
         (add-arcs 
          (partition 2 1 [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :a]))))
           
;;a cyclical graph with a strongly connected component:
;    a -> b -> c -> d -> e
;         ^
;         |              /
;         \--------------
;;   f -> g -> h 
;;   ^ --------|
(def cycles-graph 
     (-> empty-graph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e] [:e :b]
                [:f :g] [:g :h] [:h :f]])))


 ;a directed graph with 2 components
 ;    a - b - c - d - e
 (def the-list 
   (-> empty-graph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e]])))
 (def dlist (->double-list :a :e the-list))

 (def the-root-tree (-> empty-graph (conj-node :root)))
 (def the-other-tree (-> empty-graph (add-arcs (tree-arcs :a [:b :c]))))
 
)
