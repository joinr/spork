;;Generic graph search libraries, with default implementations for depth,
;;breadth, priority, random searches and traversals (walks).
(ns spork.cljgraph.search
  (:require [clojure.core [reducers :as r]]
            [spork.protocols [core :as generic]]
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

(definline visited? [state nd] `(generic/best-known-distance ~state ~nd))

(defn- visit-once
  "Screen nodes that have already been visited.  If we have visited any nodes 
   at least once, they will show up in the shortest path tree."
  [g nd state] 
  (into '() (r/filter #(not (visited? state %)) (generic/-get-sinks g nd))))

(defn- visit-ordered-once
  "Screen nodes that have already been visited.  If we have visited any nodes 
   at least once, they will show up in the shortest path tree.  Additionally, 
   this will visit the nodes in the order the incident arcs were appended to the 
   graph, if the underlying the graph supports it."
  [g nd state] 
  (filterv  #(not (visited? state %)) (reverse (generic/-get-sinks g nd))))

(defn- visit-neighbors-once
  "Treats the graph as if it's undirected.  Screen nodes that have already been 
   visited.  If we have visited any nodes at least once, they will show up in 
   the shortest path tree."
  [g nd state] 
  (into '() (r/filter #(not (visited? state %)) (neighbors* g nd))))

;;Removed empty-fringe? from check, since we already cover it in the
;;traversal loop.
(definline default-halt?  [state nextnode]
  `(identical? (generic/get-target ~state) ~nextnode))

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
(definline get-weightf   [g]   
  `(or (get (meta ~g) :weightf)
       (get ~'spork.cljgraph.search/search-defaults :weightf)
       (throw (Exception. "No weight function defined!"))))

(definline get-neighborf [g]   
  `(or (get (meta ~g) :neighborf) 
       (get ~'spork.cljgraph.search/search-defaults :neighborf)
       (throw (Exception. "No neighborhood function defined!"))))

;;Allows us to add a hook for node filtering during search.
(definline get-nodefilter [g]  
  `(get (meta ~g) :nodefilter))


;;This should be an altogether faster version of traverse.  Most of
;;the speed improvements are achieved from agressive inlining of
;;functions for relaxation, and better implementations of conj-fringe.
(defn traverse
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode startstate  {:keys [halt? weightf neighborf] 
                                        :or  {halt?     default-halt?
                                              weightf   (get-weightf g)
                                              neighborf (get-neighborf g)}}]
    (let [get-neighbors (if-let [nodefilter (get-nodefilter g)]
                          (fn [nd s] (nodefilter (neighborf g nd s)))
                          (fn [source state] (neighborf g source state)))]
      (loop [state   (-> (generic/set-target startstate targetnode)
                         (generic/conj-fringe startnode 0))]
        (if-let [source    (generic/next-fringe state)] ;next node to visit
          (let  [visited   (generic/visit-node state source)] ;record visit.
            (if (halt? visited source) visited                     
                (recur (generic/loop-reduce (fn [acc sink] (generic/relax acc (weightf g source sink) source sink))
                                            visited
                                            (get-neighbors source state)))))
          state))))

;;Needs to be implemented....should validate the options map to 
;;ensure we have defaults for everything.
(defn validate-walk-options [option-map]  true)

(defmacro defwalk
  "Macro for helping us define various kinds of walks, built on top of 
   traverse.  Caller can supply a map of default options and a function that 
   consumes a startnode and produces a search state, to produce different kinds 
   of searchs.  Returns a function that acts as a wrapper for traverse, 
   shuttling the supplied defaults to traverse, while allowing callers to
   provide key arguments for neighborf, weightf, and halt?."
  [name docstring state-ctor default-opts]
  `(let [defaults#  ~default-opts
         ~'_ (validate-walk-options defaults#)]
     (defn ~name ~docstring 
       ([~'g ~'startnode]
            (traverse ~'g ~'startnode ::undefined (~state-ctor ~'startnode) defaults#))
       ([~'g ~'startnode ~'endnode]
            (traverse ~'g ~'startnode ~'endnode (~state-ctor ~'startnode) defaults#))
       ([~'g ~'startnode ~'endnode ~'user-opts] 
          (let [clean-opts# 
                (if (or (not (identical? ~'user-opts defaults#)) (pos? (count ~'user-opts)))
                  (reduce-kv (fn [m# k# v#] (assoc m# k# v#)) defaults# ~'user-opts)
                  defaults#)]
            (traverse ~'g ~'startnode ~'endnode (~state-ctor ~'startnode) clean-opts#))))))
     
(defwalk depth-walk 
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight." 
  searchstate/mempty-DFS walk-defaults)
  
(defwalk breadth-walk
  "Returns a function that explores all of graph g in a breadth-first 
   topological order from startnode.  This is not a search, any paths returned 
   will be relative to unit-weight."
  searchstate/mempty-BFS walk-defaults)  
 
(defwalk ordered-walk
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  searchstate/mempty-DFS
  (merge walk-defaults {:neighborf visit-ordered-once :weightf unit-weight}))

(defwalk random-walk
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  searchstate/mempty-RFS walk-defaults)

(defwalk undirected-walk
  "Returns a function that explores all of graph g in depth-first topological 
   order from startnode.  This is not a search.  Any paths returned will be 
   relative to unit-weight."
  searchstate/mempty-RFS
  (merge walk-defaults {:neighborf visit-neighbors-once :weightf unit-weight}))

(defwalk priority-walk
  "Returns a function that explores all of graph g in a priority-first 
  topological order from a startnode. Weights returned will be in terms of the 
  edge weights in the graph."
 searchstate/mempty-PFS search-defaults)
