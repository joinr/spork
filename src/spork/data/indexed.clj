;;A useful collection of generic, indexed data structures.  Useful for 
;;emulating pointer-based structures from imperative land, using persistent 
;;data structures and clojure's idiomatic APIs.  Sometimes you just can't solve 
;;a problem with a zipper...
(ns spork.data.indexed
  (:require [spork.protocols.core :refer :all]
            [spork.util [topographic :as top]
                        [topotree    :as toptree]]))

;;Graph-Backed Indexed Data Structures
;;====================================

;;The indexed-tree is a data structure the provides tree-like access to a 
;;topology, based on a pre-determined root-node for the tree.
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
    (indexed-tree. root-node 
       (toptree/append-tree t root-node other (tree-root other))))
  (prune [t k] (toptree/prune-tree t k)))

;;TODO -> implement this using vectors.  Also, get it working like an indexed
;;list, kind of like a persistent vector.

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
                (map str (map (partial top/get-node pointers)
                              (top/ordered-walk pointers h)))) ")")))

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




