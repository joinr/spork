;;A namespace for network flow algorithms.
(ns spork.cljgraph.network
  (:require [spork.protocols [core :as generic]]
            [spork.cljgraph  [search :as search]]
            [spork.data      [searchstate :as searchstate]
                             [digraph :as graph]]))
  
;;a network wraps a digraph with some metadata.
;;since we use a dual adjacency list (map) representation, we can compute 
;;residual networks implicitly, and use augmenting path algorithms effectively.


;;we'll start with simple augmenting path algorithms.

;;ford fulkerson is pretty easy..
;;this is identical to the typical graph-search ideas of relaxation and 
;;neighborhoods.  The only difference is that there are new criteria for 
;;neighborhoods.  

;;When we push nodes onto the fringe, we maintian some information about the 
;;node, specifically whether the node is to be visited as a forward node or a 
;;backward node.  This information is for after we compute an augmenting path.
;;The basic algorithm is simply : compute an augmenting path to increase max
;;flow.  Augment the current flows using the augmenting path (which may contain
;;backwards edges, edges in which flow is reduced).  The minimum augmenting 
;;flow or capacity along the path is pushed, updating flows along the path.
;;Keep looking for augmenting paths until no more exist.  That's a max flow. 

;;To generalize to mincost, max-flow, we can choose to use a priority fringe 
;;and incorporate cost into our choice of edges to visit.  The cost or distance
;;functions are identical to distance in generic graph search, with the caveat
;;that, for reverse edges, where we are considering reducing flow, we reduce 
;;cost, and effectively shorten distance.  So, for forward edges, the search 
;;is identical to dijkstra/PFS relaxation, but for backward edges, the search 
;;considers the reduced cost of decreasing flow along the vertex as part of the
;;relaxation.

(defn ->info [from to flow capacity])

;;We can add capacities to our stock search-state...
(defrecord netflow [g flow capacity state])
(defn edge-info    [net from to] (get info [from to]))

;;(defn ->max-flow [g capacities]
;;  (->netflow g (searchstate/

