;;A work-in-progress-library for persistent indexed data structures that 
;;retain some notion of topology.  Things like bi-directional trees, and 
;;doubly-linked lists, and basically any imperative structure using pointers 
;;should be feasible to represent by the graph-backed data structures here.

;;This library arose after realizing that, in fact, you do need 
;;navigable structures.  In particular, for things like scene-graphs and other 
;;logical hierarchies, changes to the structure typically happen at the leaves,
;;and must propogate data upward.  In this case, zippers don't quite work - 
;;although they could if you built one for each leaf.  Imperative structures 
;;typically handle this problem implicitly via pointers, or parent/child 
;;references.  That way, when a child needs to propogate information up, or a 
;;parent need to propogate down, you just follow the references. 

;;In the library, I implemented the imperative pointer-backed topology 
;;explicitly via a persistent data structure built from canonical clojure maps
;;and stuff.  The explicit topology is then encoded in a general graph, over 
;;which we place multiple functional facades, including doubly-linked lists, 
;;and bi-directional trees.  This provides a familiar, persistent alternative to 
;;imperative structures.
;;I need to rework the name of the library, as it is no longer a topographic
;;list.  
(ns spork.util.topographic
  (:require [clojure [zip :as zip]]))

(defn assoc-exists [m k v]  (if (empty? v) (dissoc m k) (assoc  m k v )))

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

;;A protocol for allowing us to coerce values into a graph representation.    
;;If we need the underlying topological information, it would be nice to have 
;;access. This protocol allows that.
(defprotocol IGraphable (-get-graph [g]))

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
;  (prepend [l x] [l  v] "Conjoins child to parent, after ")
;  ())

;;Graph-Backed Operations
;;=======================

;;A generic search fringe protocol.  A lot of queries will be simple graph walks
;;so by supporting the canonical walks we can extend the querying abilities to 
;;all of the types based on a graph topology.
(defprotocol IFringe 
  (conj-fringe [fr x])
  (next-fringe [fr])
  (pop-fringe  [fr]))

;;Delayed for now.  Need for djikstra and other search algorithms.
(comment 
(deftype priority-queue [basemap priority-func]
  IFringe 
  (conj-fringe [fr x] (assoc basemap (entry (priority-func x) x) x))
  (next-fringe [fr]   (first basemap))
  (pop-fringe  [fr]   (dissoc basemap (key (first basemap)))))

(defn ->min-priority-queue [f] 
  (->priority-queue (sorted-map-by key) f))
(defn ->max-priority-queue [f] 
  (->priority-queue (sorted-map-by key) (fn [l r] (* -1 (f l r)))))
)

;;Implementations of basic stack (depth first) and queue (breadth first) 
;;fringes.
(extend-protocol IFringe 
  nil 
  (conj-fringe [fr x] (conj '() x))
  (next-fringe [fr]   nil)
  (pop-fringe  [fr]   nil)  
  clojure.lang.PersistentQueue
  (conj-fringe [fr x] (conj fr x))
  (next-fringe [fr]   (first fr))
  (pop-fringe  [fr]   (pop fr))  
  clojure.lang.PersistentList
  (conj-fringe [fr x] (conj fr x))
  (next-fringe [fr]   (first fr))
  (pop-fringe  [fr]   (next fr))
  clojure.lang.Cons 
  (conj-fringe [fr x] (conj  fr x))
  (next-fringe [fr]   (first fr))
  (pop-fringe  [fr]   (next fr)))


(def emptyq clojure.lang.PersistentQueue/EMPTY)
(defn ^clojure.lang.MapEntry entry [k v] (clojure.lang.MapEntry. k v))

;;Ordered Map (Move to a separate library)
;;===========

;;This is an independent implementation of an ordered map type.  While the graph
;;backed topology is nice, one downside of using persistent maps is that 
;;traversal of their contents is unordered.  We'd like to maintain the nice 
;;properties of hash-maps, like fast lookup and insertion, but with a little 
;;extra meta data that allows us to traverse values according to an ordering.
;;Hence, the ordered map.  This allows us to perform walks in a deterministic 
;;fashion, which is important for tree structures.  We only use ordered maps for
;;the parent->child (sink) and child->parent (source) data.  

(defprotocol IOrderedMap
  (get-ordering [m])
  (set-ordering [m idx->key key->idx]))

(defn- swap-order
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

(declare empty-ordered-map)

;;The ordered map acts, for all intents and purposes, like a normal persistent
;;map.  The main difference is that it maintains a sequence of keys to visit 
;;that is based on the order keys were associated with the map.  I have seen 
;;another implementation of this, which uses a vector for the ordering.  Here, 
;;I use a sorted-map, due to the need for ordered-maps to be able to change 
;;orderings effeciently.  A vector would be more efficient, but at the moment, 
;;the sorted map is doing the job.
(deftype ordered-map [n basemap idx->key key->idx _meta]
  Object
  (toString [this] (str (.seq this)))
  IOrderedMap
  (get-ordering [m] [idx->key key->idx])
  (set-ordering [m idx->k k->idx] 
    (ordered-map. n basemap idx->k k->idx _meta))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (get basemap k))
  (valAt [this k not-found] (get basemap k not-found))  
  clojure.lang.IPersistentMap
  (count [this] (count basemap))
  (assoc [this k v]     ;;revisit        
   (let [new-order (inc n)]
     (if (contains? basemap k) 
       (ordered-map. n (assoc basemap k v) idx->key key->idx _meta)
       (ordered-map. new-order 
                     (assoc basemap k v)
                     (assoc idx->key n k)
                     (assoc key->idx k n)
                     _meta))))
  (empty [this] (ordered-map. 0 {} (sorted-map) {} {}))  
  ;cons defines conj behavior
  (cons [this e]   (.assoc this (first e) (second e)))
  (equiv [this o]  (.equiv basemap o))  
  (hashCode [this] (.hashCode basemap))
  (equals [this o] (or (identical? this o) (.equals basemap o)))
  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (contains? basemap k))
  (entryAt [this k]
    (let [v (.valAt this k this)]
      (when-not (identical? v this) ;might need to yank this guy.
        (clojure.lang.MapEntry. k v))))
  (seq [this] (if (empty? basemap) (seq {})
                (map (fn [k] (clojure.lang.MapEntry. k (get basemap k))) 
                     (vals idx->key))))  
  ;without implements (dissoc pm k) behavior
  (without [this k] 
    (if (not (contains? basemap k)) this
        (ordered-map. n
                      (dissoc basemap k) 
                      (dissoc idx->key (get key->idx k))
                      (dissoc key->idx k)
                      _meta)))
    
  clojure.lang.Indexed
  (nth [this i] (if (and (>= i 0) 
                         (< i (count basemap))) 
                  (let [k (get idx->key i)]
                    (entry k (get basemap k)))
                  (throw (Exception. (str "Index out of range " i )))))
  (nth [this i not-found] 
    (if (and (< i (count basemap)) (>= i 0))
        (get basemap (get idx->key i))        
         not-found))  
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (ordered-map. n basemap idx->key key->idx m))
      
  clojure.lang.Reversible
  (rseq [this]
    (seq (map (fn [k] (clojure.lang.MapEntry. k (get basemap k))) 
              (reverse (vals idx->key)))))

  java.io.Serializable ;Serialization comes for free with the other things implemented
  clojure.lang.MapEquivalence
  
  java.util.Map ;Makes this compatible with java's map
  (size [this] (count basemap))
  (isEmpty [this] (zero? (count basemap)))
  (containsValue [this v] (some #{v} (vals (basemap this)) v))
  (get [this k] (.valAt this k))
  (put [this k v] (throw (UnsupportedOperationException.)))
  (remove [this k] (throw (UnsupportedOperationException.)))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (set (keys basemap))) ;;modify
  (values [this] (map val (.seq this)))
  (entrySet [this] (set (.seq this))))

(def empty-ordered-map (->ordered-map 0 {} (sorted-map) {} {}))

;;The topograph Data Structure, and Implementation of ITopograph
;;==============================================================

;;The topograph is implemented as a directed graph.  Instead of edge lists, we 
;;maintain two edge maps for each node: 
;;sources : {nd #{sink nodes}}
;;sinks   : {nd #{source nodes}}
;;These are analagous to neighborhoods in my cljgraph library.
(defrecord topograph [nodes sources sinks]
  ITopograph
  (-get-nodes [tg] nodes)
  (-set-nodes [tg m] (topograph. m sources sinks))
  (-conj-node [tg k v] 
    (topograph. (assoc nodes k v) 
                (assoc sources k empty-ordered-map)  
                (assoc sinks k empty-ordered-map)))
  (-disj-node [tg k]
    (assert (contains? nodes k) (str "Node " k " does not exist!")) 
    (let [new-sources (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sources k)  (-get-sinks tg k))
          new-sinks   (reduce #(update-in %1 [%2] dissoc k)
                              (dissoc sinks k)  (-get-sources tg k))]
      (topograph. (dissoc nodes k) new-sources new-sinks)))
  (-has-node? [tg k]  (contains? nodes k))
  (-conj-arc  [tg source sink w] 
    (topograph. nodes 
      (update-in sources [sink]   assoc source w)
      (update-in sinks   [source] assoc sink   w)))
  (-disj-arc  [tg source sink]   
    (topograph. nodes 
      (assoc sources sink  (or (dissoc (get sources sink) source) 
                                empty-ordered-map)) 
      (assoc sinks   source (or (dissoc (get sinks source) sink) 
                                empty-ordered-map))))
  (-has-arc?  [tg source sink] (contains? (get sources sink) source))
  (-get-arc   [tg source sink] [source sink (get-in sinks [source sink])])
  (-get-sources [tg k] (vec (keys (get sources k))))
  (-get-sinks [tg k] (vec (keys (get sinks k)))))

(def empty-topograph (->topograph {} {} {}))

;;General Graph Operations
;;========================

;;Protocol derived functionality for operations on Topographs
;;Some of these are mere wrappers around the protocol functions.  When possible,
;;the lower-level protocol functions are to be eschewed for the API presented
;;herein.

(defn topograph? [x] (satisfies? ITopograph x))

;;Node operations (where nodes act as primary keys in a graph database)
;;=====================================================================

(defn nodes
  "Returns a persistent map - of node keys to node data - used by topograph."
  [tg] (-get-nodes tg))
(defn has-node? [tg nd] (-has-node? tg nd))
(defn has-arc?  [tg source sink] (-has-arc? tg source sink))

(defn conj-node 
  ([tg k v] (-conj-node tg k v))
  ([tg k]   (conj-node tg k (count (-get-nodes tg)))))

;;This could be a bit destructive.  It allows changing existing nodes.
(defn set-node
  "Allows the data mapped to an existing node to be replaced."
  [tg k v]
  (assert (has-node? tg k) (str "Cannot set a non-existent node: " k))
  (-set-nodes tg (assoc (-get-nodes tg) k v)))

(defn disj-node
  "Removes node k, and any arcs incident to node k are also removed."
  [tg k] (-disj-node tg k))

(defn ensure-nodes [tg ks] 
  (reduce (fn [acc k] (if (has-node? acc k) acc (conj-node acc k))) tg ks))

(defn get-node
  "Fetches the data associated with k in the topograph's node map."
  [tg k] (get (-get-nodes tg) k)) 

;;Arc Operations
;;==============

(defn conj-arc
  "Adds an arc from source to sink, with an optional weight w.  If neither node
   exists, nodes are created implicity.  This allows easy inline construction of 
   graphs."  
  ([tg source sink w] (-conj-arc (ensure-nodes tg [source sink]) source sink w))
  ([tg source sink] (conj-arc tg source sink 1)))

(defn disj-arc
  "Drops the arc from source to sink, if one exists in the topograph."
  [tg source sink] (-disj-arc tg source sink))

(defn add-arcs
  "Adds a sequence of arcs, xs, where xs are [from to & [w]]."
  [tg xs]  
  (reduce (fn [acc [from to & [w]]] 
            (conj-arc acc from to w)) tg xs))
(defn drop-arcs
  "Drops a sequence of arcs, of the form [from to], from the topograph."
  [tg xs] (reduce #(disj-arc %1 (first %2) (second %2)) tg xs))

;;Neighborhood operations
;;=======================
(defn sinks   [tg k] (-get-sinks tg k))
(defn sources [tg k] (-get-sources tg k))
(defn neighbors [tg k]  (vec (distinct (mapcat #(% tg k) [sources sinks]))))

;;Borrowed idea from Data.Graph.Inductive 

(defn node-context
  "Provides a lens, or a focus, of all the graph data for a particular node."
  [tg k] 
  (when (has-node? tg k) {:node k 
                          :data (get (nodes tg) k)
                          :sources (sources tg k)
                          :sinks   (sinks tg k)}))


(defn graph-seq
  "Views the topograph as a sequence of node contexts."
  [tg] 
  (sort-by :data (map (partial node-context tg) (keys (nodes tg)))))

;;Relabeling Nodes
(defn arcs-from
  "Returns a vector of arcs sourced by nd, of the form [nd to weight]."
  [tg nd]  
  (vec (map (partial -get-arc tg nd) (sinks tg nd))))

(defn arcs-to
  "Returns a vector of arcs terminated by nd, of the form [from nd weight]."
  [tg nd]  
  (vec (map #(-get-arc tg % nd) (sources tg nd))))
    
(defn relabel-node
  "Allows effecient relabeling of a node key.  Automatically updates related 
   arc data."
  [tg old-label new-label]
  (let [{:keys [node data sources sinks]} (node-context tg old-label)]
    (-> (disj-node tg old-label)
        (conj-node new-label data)
        (add-arcs (map (fn [[_ to w]] [new-label to w]) 
                       (arcs-from tg old-label)))
        (add-arcs (map (fn [[from _ w]] [from new-label w]) 
                         (arcs-to tg old-label)))))) 
 
;;Rudimental Graph Traversals - note search is not in here at the moment.
;;=======================================================================

(defn walk
  "Initiates a walk across the topograph, starting at node k, using an optional
   neighborhood function and an option fringe structure.  fringe must 
   implement the IFringe protocol.  Defaults to a depth walk.  Returns a vector
   of nodes visited."
  [tg k & {:keys [neighbor-func fringe]
                    :or   {neighbor-func (partial sinks tg)
                           fringe nil}}]
  (loop [fr        (conj-fringe fringe k 0)
         visited  #{}
         acc       []]
    (cond (empty? fr) acc
          (visited (first (next-fringe fr)) (recur (pop-fringe fr) visited acc) 
          :else    
           (let [e     (next-fringe fr)
                 nd    (first e) 
                 w     (second e)
                 vnext (conj visited nd)
                 xs    (filter  (complement vnext) (neighbor-func nd))]
             (recur (reduce conj-fringe (pop-fringe fr) xs)
                    vnext
                    (conj acc nd))))))

(defn depth-walk
  "Walks the topograph in a depth-first fashion.  Note that depth-first is not 
   an in-order traversal."
  [tg k]   (walk tg k))
(defn breadth-walk
  "Walks the topograph in a breadth-first fashion."
  [tg k]   (walk tg k :fringe emptyq))
(defn ordered-walk
  "Walks the topograph in a depth-first fashion, but preserves the ordering of 
   the nodes so that it corresponds to the order in which arcs were assoc'd.  
   This is most useful for tree traversals where the order of children matters."
  [tg k]   
  (walk tg k :neighbor-func #(rseq (sinks tg %))))

(defn undirected-walk
  "Views the topograph as an undirected graph, visiting all neighbors of a node,
   including sources and sinks, in a depth-first fashion.  Useful for 
   connectedness queries and finding connected components."
  [tg k] (walk tg k :neighbor-func (partial neighbors tg)))
                                   

;;Pending.
;(defn priority-walk [tg k f])

;;I may need to rethink these guys, and prefere lazy seqs, if we ever apply 
;;this library to large graphs or trees.  succs and preds will walk the whole 
;;thing, which may be untenable.

(defn succs
  "Returns the a set of all the nodes reachable starting from node k."
  [tg k] (disj (set (walk tg k :neighbor-func (partial sinks tg))) k))

(defn preds
  "Returns the set of all the nodes that can reach node k."
  [tg k] 
  (disj (set (walk tg k :neighbor-func (partial sources tg))) k))

(defn component
  "Returns the set of nodes that can reach or can be reached through node k."  
  [tg k] (set (walk tg k :neighbor-func (partial neighbors tg))))

(defn components
  "Finds all components in the topograh, returning a mapping of component size 
   to vectors of node sets in each component."
  [tg]
  (loop [xs (set (keys (nodes tg)))
         acc []]
    (if (empty? xs) 
      (group-by count acc)
      (let [c (component tg (first xs))]
        (recur (clojure.set/difference xs (set c))
               (conj acc c))))))

(defn subgraph 
  "Given a starting point, root-key, and an optional node filter, derives a 
   reduced, or filtered graph based on the nodes that pass the filter.  Defaults
   to a depth-walk as a node filter, so only nodes reachable from root-key are 
   retained."
  ([tg node-filter-func root-key]
    (let [retained-keys (set (node-filter-func tg root-key))
          valid?   (partial contains? retained-keys)
          arcs-to-clone (fn [k] 
                          (distinct 
                            (concat 
                              (filter #(valid? (second %)) (arcs-from  tg k)) 
                              (filter #(valid? (first %))  (arcs-to    tg k)))))
          old-keys      (set (keys (nodes tg)))]
      (if (< (count retained-keys) (- (count old-keys) (count retained-keys)))
          (reduce (fn [acc x] (-> (conj-node acc x (get-node tg x))
                                  (add-arcs (arcs-to-clone x))))
                  empty-topograph retained-keys)
          (reduce (fn [acc x] (disj-node acc x)) tg 
                  (clojure.set/difference old-keys retained-keys)))))
  ([tg root-key] (subgraph tg depth-walk root-key)))

(defn decompose
  "Maps the topograph into one or more subgraphs, one for each component."
  [tg]
  (for [[size node-sets] (components tg)
        xs               node-sets]
    (subgraph tg (fn [& args] xs) (first xs))))

;;__Rewrite islands, you can do it more efficiently that using components.__

(defn islands
  "Returns a sequence of nodes that are unreachable."
  [tg] (get (components tg) 1))


;;Tree-like Operations
;;====================

;;Directional walks - either source->sink, or sink->source
(defn ascend  [tg k]  (walk tg k :neighbor-func (partial sources tg)))
(defn descend [tg k]  (ordered-walk tg k))

(defn parent-nodes
  "Returns the parent(s) of k."  
  [tg k] (sources  tg k))

(defn child-nodes [tg k]   (sinks tg k))
(defn sibling-nodes
  "Returns a sequence of the children of the parent of node k."
  [tg k] 
  (disj (apply clojure.set/union 
               (map (comp set (partial child-nodes tg)) 
                    (parent-nodes tg k)))   k))

(defn has-children? [tg k] (not= 0 (count (child-nodes tg k))))
 
(defn subtree
  "Returns the subset of topograph, where root-key is the root 
   of the subtree."
  ([tg node-filter-func root-key] (subgraph node-filter-func root-key))
  ([tg root-key] (subgraph tg root-key)))     
 
(defn merge-tree
  "Merges two topographs together akin to clojure.core/merge.  Where nodes are
   identically-keyed, the data from other-tg replaces the original data in tg.
   If there are similar arcs, the weights associated with other-tg's arcs take
   precedence.  The choice of nodes to merge is based on a depth walk of 
   other-tg."
  [tg root-key other-tg other-root-key]
  (->> (depth-walk other-tg other-root-key)
       (reduce (fn [acc nd] 
                 (let [new-sinks     (arcs-from other-tg nd)
                       new-sources   (arcs-to other-tg nd)
                       new-data      (get-node other-tg nd)
                       new-tg        (conj-node acc nd new-data)
                       prior-arcs    (when (has-node? tg nd)
                                       (concat (arcs-from tg nd) 
                                               (arcs-to tg nd)))]
                   (->> (concat prior-arcs new-sinks new-sources)
                        (distinct)
                        (add-arcs new-tg)))) tg)))

(defn append-tree
  "Appends two topographs together, where the second topograph is rooted at 
   root-key.  The choice of nodes to append is based on a depth walk of 
   other-tg.  Where nodes are identically-keyed, other-tg is re-labeled with 
   new node names prior to appending.  The end result is an indexed tree that 
   has the relations and data from other-tg as a child of root-key in tg."
  [tg root-key other-tg other-root-key]
  (let [nodes-to-copy  (depth-walk other-tg other-root-key)
        existing-nodes (-get-nodes tg)
        changed-nodes  (->> (filter #(contains? existing-nodes %) nodes-to-copy)
                            (map (fn [k] [k (keyword (gensym k))]))
                            (into {}))
        new-root-key   (get changed-nodes other-root-key other-root-key)
        conditioned-tg (reduce (fn [acc [k k-new]] (relabel-node acc k k-new)) 
                               other-tg changed-nodes)]
    (-> (merge-tree tg root-key conditioned-tg new-root-key)
        (add-arcs [[root-key new-root-key nil]]))))

(defn prune-tree
  "Removes all nodes in tg reachable from drop-root-key."
  [tg drop-root-key]        
  (reduce #(disj-node %1 %2) tg (depth-walk tg drop-root-key)))  

;list-like ops 

;in progress...
(defmacro with-graph [g body]
  `(let [~'*graph* ~g]
     ~body))

;;An entirely different way to go about this enterprise...
;;This is much simpler....but more limited...create doubly-linked vectors.
;;Or a doubly-linked tries.  Use vectors to encode parent/child relations.
(comment 
;;our connectivity may look like this:
;;          a   
;;         b c  
;;             d

(def vec-tree 
  [{:parent nil :data :a :children [1 2]}
   {:parent 0   :data :b :children [3]}
   {:parent 0   :data :c :children []}
   {:parent 1   :data :d :children []}])
  
(defrecord bi-node [parent data children])
;;implement tree and other stuff in vec-tree.
(defrecord vec-tree [nodes])

(def root-node (->bi-node nil nil []))
(defn empty-vec-tree (->vec-tree [root-node]))
(defn get-root  [t] (first t))
(defn idx->node [t] (nth (:nodes t) idx))
(defn get-children [t idx] (:children (idx->node idx t)))
(defn get-parent   [t idx] (:parent   (idx->node idx t)))

)
;;Graph-Backed Indexed Data Structures
;;====================================

;;double lists are effectively doubly-linked lists, and represented as very 
;;simple graphs. double lists should act as a typical persistent list, i.e. 
;;supporting operations to conj, cons, etc., with the caveat that we maintain a
;;topology of the parent-child relations...
(deftype double-list [h t pointers]
  IGraphable 
  (-get-graph [l] pointers)
  clojure.lang.IObj  
  (meta [this]   (meta pointers))  
  (withMeta [this m]    
    (double-list. (with-meta pointers m) h t))
  Object 
  (toString [l] 
    (str "(" (clojure.string/join " "
                (map str (map (partial get-node pointers)
                              (ordered-walk pointers h)))) ")")))

;;The indexed-tree is a data structure the provides tree-like access to a 
;;topology, based on a pred-determined root-node for the tree.
(deftype indexed-tree [root-node topology]
  IGraphable 
  (-get-graph [t] topology)
  clojure.lang.IObj  
  (meta [this]   (meta topology))  
  (withMeta [this m]    
    (indexed-tree. root-node (with-meta topology m)))
  IIndexedTree 
  (tree-root [t] root-node)
  (append [t other] 
    (indexed-tree. root-node (append-tree t root-node other (tree-root other))))
  (prune [t k] (prune-tree t k))
  )
  
;;Testing/Examples
(comment 
(def the-map (into empty-ordered-map [[:a 1] [:b 2] [:c 3]]))

  ;a tree of elements:
  ;              a
  ;            b c d 
  ;           e f    g
  ;          h   i     j
  ;         k l  m n o  p q
  ;                        r s t u v w x y z a1 a2 a3 
 (defn tree-arcs [from xs] (map #(vector from %) xs))
 (def the-tree (-> empty-topograph
                 (add-arcs (tree-arcs :a [:b :c :d]))
                 (add-arcs (conj (tree-arcs :b [:e :f]) [:d :g]))
                 (add-arcs [[:e :h] [:f :i] [:g :j]])
                 (add-arcs [[:h :k] [:h :l] [:i :m] [:i :n] [:i :o]
                            [:j :p] [:j :q]])
                 (add-arcs (tree-arcs :q  
                             [:r :s :t :u :v :w :x :y :z :a1 :a2 :a3]))))
 
 ;          h   
 ;         k l  
 (def h-tree (subtree the-tree :h))

 ;          i     
 ;         m n o
 (def i-tree (subtree the-tree :i))

 ;          h     i   
 ;         k l  m n o  
 (def h-i-tree (merge-tree h-tree :h i-tree  :i))
 
 ;          h   
 ;         k l
 ;        i     
 ;      m n o 
(def h-i-at-k-tree (append-tree h-tree :k i-tree  :i))

 ;a directed graph...
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f/^
 
 (def the-graph 
   (-> empty-topograph
       (add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:f :c] [:b :d]]))) 
 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 
 (def the-graph2 
   (-> empty-topograph
       (add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])))
 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 ;    g 
 ;    h
 
 (def the-graph3
   (-> empty-topograph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])
     (conj-node :g)
     (conj-node :h)))

 ;a directed graph with 2 components
 ;    a - b - c - d - e
 (def the-list 
   (-> empty-topograph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e]])))
 (def dlist (->double-list :a :e the-list))

 (def the-root-tree (-> empty-topograph (conj-node :root)))
 (def the-other-tree (-> empty-topograph (add-arcs (tree-arcs :a [:b :c]))))
 ;(def the-merged-tree 
 

;=> (sibling-nodes the-tree :b)
;#{:c :d}
;=> (sibling-nodes the-tree :c)
;#{:b :d}
;=> (child-nodes the-tree :i)
;[:m :n :o]

;;since we have bidirectionality....
;;we can traverse from the parents down, or the children up, or any 
;;arbitrary point in between.

;=> (ascend the-tree :a)
;[:a]
;=> (ascend the-tree :s)
;[:s :q :j :g :d :a]
;=> (ascend the-tree :k)
;[:k :h :e :b :a]

;=> (descend the-tree :f)
;[:f :i :m :n :o]

;=> (ordered-walk the-tree :a)
;[:a :b :e :h :k :l :f :i :m :n :o :c :d :g :j :p :q :r :s :t :u :v :w :x :y :z 
; :a1 :a2 :a3]
;=> (depth-walk the-tree :a)
;[:a :d :g :j :q :a3 :a2 :a1 :z :y :x :w :v :u :t :s :r :p :c :b :f :i :o :n :m 
; :e :h :l :k]
;=> (breadth-walk the-tree :a)
;[:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z 
; :a1 :a2 :a3]

;=> (pprint (graph-seq the-tree))
;({:node :a, :data 0, :sources nil, :sinks [:b :c :d]}
; {:node :b, :data 1, :sources [:a], :sinks [:e :f]}
; {:node :c, :data 2, :sources [:a], :sinks nil}
; {:node :d, :data 3, :sources [:a], :sinks [:g]}
; {:node :g, :data 4, :sources [:d], :sinks [:j]}
; {:node :e, :data 5, :sources [:b], :sinks [:h]}
; {:node :f, :data 6, :sources [:b], :sinks [:i]}
; {:node :h, :data 7, :sources [:e], :sinks [:k :l]}
; {:node :i, :data 8, :sources [:f], :sinks [:m :n :o]}
; {:node :j, :data 9, :sources [:g], :sinks [:p :q]}
; {:node :k, :data 10, :sources [:h], :sinks nil}
; {:node :l, :data 11, :sources [:h], :sinks nil}
; {:node :m, :data 12, :sources [:i], :sinks nil}
; {:node :n, :data 13, :sources [:i], :sinks nil}
; {:node :o, :data 14, :sources [:i], :sinks nil}
; {:node :p, :data 15, :sources [:j], :sinks nil}
; {:node :q, :data 16, :sources [:j], 
;  :sinks [:r :s :t :u :v :w :x :y :z :a1 :a2 :a3]}
; {:node :r, :data 17, :sources [:q], :sinks nil}
; {:node :s, :data 18, :sources [:q], :sinks nil}
; {:node :t, :data 19, :sources [:q], :sinks nil}
; {:node :u, :data 20, :sources [:q], :sinks nil}
; {:node :v, :data 21, :sources [:q], :sinks nil}
; {:node :w, :data 22, :sources [:q], :sinks nil}
; {:node :x, :data 23, :sources [:q], :sinks nil}
; {:node :y, :data 24, :sources [:q], :sinks nil}
; {:node :z, :data 25, :sources [:q], :sinks nil}
; {:node :a1, :data 26, :sources [:q], :sinks nil}
; {:node :a2, :data 27, :sources [:q], :sinks nil}
; {:node :a3, :data 28, :sources [:q], :sinks nil})

)
