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
  (:require [clojure [zip :as zip]]
            [spork.data [orderedmap :refer [empty-ordered-map]]
                        [fringe :as fr]]
            [spork.protocols.core :refer :all]))

(defn assoc-exists [m k v]  (if (empty? v) (dissoc m k) (assoc  m k v )))

;;Graph-Backed Operations
;;=======================
(defn u-arcbound [nodecount] 
  (/ (* nodecount (dec nodecount)) 2))

(defn d-arcbound [nodecount]
  (* nodecount (dec nodecount)))

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
  (-get-sinks [tg k]   (vec (keys (get sinks k)))))

(def empty-topograph (->topograph {} {} {}))

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

(defn drop-nodes [g coll] (reduce disj-node g coll))

(defn ensure-nodes [tg ks] 
  (reduce (fn [acc k] (if (has-node? acc k) acc (conj-node acc k))) tg ks))

(defn get-node
  "Fetches the data associated with k in the topograph's node map."
  [tg k] (get (-get-nodes tg) k)) 

(defn get-node-labels
  "Fetches the keys, implied to be node labels, associated with the nodes in 
   topograph tg."
  [tg] (keys (nodes tg)))

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

(defn arc-weight [tg from to]
  (assert (has-arc? tg from to) (str "Arc does not exist " [from to]))
  (nth (-get-arc tg from to) 2))

;;Neighborhood operations
;;=======================
(defn sinks   [tg k]    (-get-sinks tg k))
(defn sources [tg k]    (-get-sources tg k))
(defn neighbors [tg k]  (vec (distinct (mapcat #(% tg k) [sources sinks]))))

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
 
;;We might be able to factor this out with the graph searches.
;;We simply walk the graph searching for a node that doesn't exist.
;;These are ALL eager walks, in that they return a vector of the walk.
;;That being said, we can significantly speed up the walks if we use transient 
;;vectors.  I'll work that in later as an optimization.
;;We may want lazy walks (and maybe not!) for larger (possibly infinite) graphs.
;;Rudimental Graph Traversals - note search is handled elsewhere.
;;===============================================================

(defn walk
  "Initiates a walk across the topograph, starting at node k, using an optional
   neighborhood function and an option fringe structure.  fringe must 
   implement the IFringe protocol.  Defaults to a depth walk.  Returns a vector
   of nodes visited."
  [tg k & {:keys [neighbor-func fringe]
                    :or   {neighbor-func (partial sinks tg)
                           fringe fr/depth-fringe}}]
  (let [get-weight (partial arc-weight tg)]
	  (loop [fr        (conj-fringe fringe k 0)
	         visited  #{}
	         acc       []]
	    (cond (empty? fr) acc
	          (visited (first (next-fringe fr))) (recur (pop-fringe fr) visited acc) 
	          :else    
	           (let [e     (next-fringe fr)
	                 nd    (first e) 
	                 w     (second e)
	                 vnext (conj visited nd)
	                 xs    (filter  (complement vnext) (neighbor-func nd))]
	             (recur (reduce (fn [acc x] (conj-fringe acc x (get-weight nd x)))  
                              (pop-fringe fr) xs)
	                    vnext
	                    (conj acc nd)))))))

(defn depth-walk
  "Walks the topograph in a depth-first fashion.  Note that depth-first is not 
   an in-order traversal."
  [tg k]   (walk tg k))

(defn breadth-walk
  "Walks the topograph in a breadth-first fashion."
  [tg k]   (walk tg k :fringe fr/breadth-fringe))

(defn ordered-walk
  "Walks the topograph in a depth-first fashion, but preserves the ordering of 
   the nodes so that it corresponds to the order in which arcs were assoc'd.  
   This is most useful for tree traversals where the order of children matters."
  [tg k]   
  (walk tg k :neighbor-func #(rseq (sinks tg %))))

(defn random-walk
  "Walks the topograph in randomly.  Returns a vector of the random walks.  Note
   this will return a different walk each time it is invoked."
  [tg k] (walk tg k :fringe fr/random-fringe))                                  

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
 
 (assert (= (depth-walk the-tree :a)
             [:a :d :g :j :q :a3 :a2 :a1 :z :y :x :w :v :u :t :s :r :p :c :b 
              :f :i :o :n :m :e :h :l :k]))
 (assert (= (ordered-walk the-tree :a)
            [:a :b :e :h :k :l :f :i :m :n :o :c :d :g :j :p :q :r :s :t :u :v 
             :w :x :y :z :a1 :a2 :a3]))
 (assert (= (breadth-walk the-tree :a)
            [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v 
             :w :x :y :z :a1 :a2 :a3]))
 (assert (= (undirected-walk the-tree :a) 
           [:a :d :g :j :q :a3 :a2 :a1 :z :y :x :w :v :u :t :s :r :p :c :b :f 
            :i :o :n :m :e :h :l :k]))
 
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
