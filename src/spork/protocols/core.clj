;;A set of common protocols implemented and used throughout spork.
;;Only protocols that are fairly generic, and see usage through multiple 
;;components will show up here.  Other components may have more localized, 
;;or specific protocols unique to their domain.
(ns spork.protocols.core)

;;Common function used throughout Spork.
(defn ^clojure.lang.MapEntry entry [k v] (clojure.lang.MapEntry. k v))


;;Definition for associative containers that can traverse their keys in the 
;;first-in-first-out order, while retaining a constant, or near-constant 
;;lookup time.
(defprotocol IOrderedMap
  (get-ordering [m] "Returns a pair of m's ordering maps [idx->key key->idx]")
  (set-ordering [m idx->key key->idx] 
     "Sets m's key ordering using maps idx->key key->idx"))

(defn swap-order
  "For ordered map om, assumes "
  [om k1 k2]
  (assert (and (contains? om k1) (contains? om k2)) 
          (str "All keys must exist in map: " [k1 k2])) 
  (let [[idx->key key->idx] (get-ordering om) 
        idx1 (get key->idx k1)
        idx2 (get key->idx k2)]    
      (set-ordering om 
          (-> (assoc idx->key idx2 k1) (assoc idx1 k2))
          (-> (assoc key->idx k1 idx2) (assoc k2 idx1)))))

(defn relabel-key
  "For ordered map om, replaces the existing key, k1, with a new key k2, 
   which maps to both the value and the ordering of k1.  Useful for graph
   relabeling."
  [om k1 k2]
  (-> (assoc om k2 (get om k1))
      (swap-order k1 k2)
      (dissoc k1)))

;;Abstract fringes to support generic search operations.
(defprotocol IFringe 
  (conj-fringe [fringe n w] "Add node n with weight w to the fringe.")
  (next-fringe [fringe] "Get the next node on the fringe")
  (pop-fringe [fringe] "Remove the next node from the fringe")
  (re-weigh [fringe n wold wnew]     
    "Return the result of updating n's weight.  Old weight wold must be 
     provided, since it is a part of n's key." )
  (re-label [fringe n w newlabel]
    "Return the result of updating n's label. Weight w must be 
     provided, since it is a part of n's key."))

(defn conj-fringe-all
  "Add many [node weight] pairs onto the fringe."
  [fringe nws]
  (reduce (partial apply conj-fringe) fringe nws))  
   
(defn fringe-stream
  "Return a sequence of popped {node weight} maps from 
   priorityq q."
  [fringe]
  (if-let [kv (next-fringe fringe)]
    (lazy-seq (cons kv (fringe-stream (pop-fringe fringe))))))
(defn empty-fringe? [fringe] (empty? (fringe-stream fringe)))
(defn fringe? [x] (satisfies? IFringe x))

;;Abstract protocol for operating on shortest path searches.
(defprotocol IGraphSearch
  (new-path     [state source sink w])
  (shorter-path [state source sink wnew wpast])
  (equal-path   [state source sink]))

;;A protocol for defining various topologies using a graph-based API.  Most
;;of the types used in this library build of a topology encoded by a persistent
;;map.  As a result, we define a slew of operations that are useable by general
;;graphs, as well as the specialized views of graphs.
(defprotocol ITopograph
  (-get-nodes [tg] "Returns a mapping of node keys to node data.")
  (-set-nodes [tg m] "Replaces the existing node map with m.")
  (-conj-node [tg k v] "Adds a node labeled k, mapped to v, to the node map.")
  (-disj-node [tg k] "Removes the node labeled k from the set of nodes.")
  (-has-node? [tg k] "Determines if node k exists in the node map.")
  (-conj-arc  [tg source sink w] 
    "Associates an arc from source to sink of weight w.")
  (-disj-arc  [tg source sink] "Dissociates the arc from source to sink.")
  (-has-arc?  [tg source sink] "Determines is an arc from source to sink exists.")
  (-get-arc   [tg source sink] "Fetches the weight of the arc from source to sink.")
  (-get-sources [tg k] "Returns a sequence of nodes that have arcs to node k.")
  (-get-sinks [tg k] "Returns a sequence of nodes that k has an arc to."))

(defn topograph? [x] (satisfies? ITopograph x))

;;A protocol for allowing us to coerce values into a graph representation.    
;;If we need the underlying topological information, it would be nice to have 
;;access. This protocol allows that.
(defprotocol IGraphable (-get-graph [g]))

(defn graphable? [x] (satisfies? IGraphable x))

;;We often times have great need for tree-like structures, which are GREAT in 
;;functional programming, as long as you don't need bi-directionality or 
;;the ability to traverse from an arbitrary node to a parent.  The IIndexedTree
;;protocol is designed to allow us to use an indexed topography to simulate 
;;tree-like operations, using an ITopograph as a backing structure.
(defprotocol IIndexedTree
  (tree-root [t] "Return the root index of a tree")
  (append [t other-tree] "Append the nodes of an external tree the root.")
  (prune  [t k] "Remove all nodes reachable from k, including k."))

;;We provide a protocol for operating on persistent doubly-linked lists. 
;;Doubly-linked lists represent a very simple graph with bi-directional arcs 
;;between nodes.  Still, we can use a graph-based topology to supply the 
;;information for the list, or other implementations.
(defprotocol ILinked 
  (attach [l parent child] "Attach child to parent")
  (detach [l parent child] "Remove child from parent"))
