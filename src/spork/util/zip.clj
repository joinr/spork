;;Additonal functions for clojure.zip.  Implemented as a wrapper around 
;;clojure.zip, aliasing all the names there.
;;functions will be available via zip/func-name.  Should be a drop-in 
;;replacement for clojure.zip
(ns spork.util.zip
  (:refer-clojure :exclude (replace remove next))
  (:require [clojure [zip :as zip]]))

;aliases the rest...
(def seq-zip zip/seq-zip) 
(def vector-zip zip/vector-zip) 
(def xml-zip zip/xml-zip) 
(def children zip/children) 
(def branch? zip/branch?) 
(def path zip/path) 
(def lefts zip/lefts) 
(def rights zip/rights) 
(def down zip/down) 
(def up zip/up) 
(def right zip/right) 
(def rightmost zip/rightmost) 
(def left zip/left) 
(def leftmost zip/leftmost) 
(def insert-left zip/insert-left) 
(def insert-right zip/insert-right) 
(def replace zip/replace) 
(def insert-child zip/insert-child) 
(def append-child zip/append-child) 
(def prev zip/prev) 
(def end? zip/end?)
(def next zip/next)
(def edit zip/edit)
(def make-node zip/make-node)
(def root zip/root)
(def remove zip/remove)
(def zipper zip/zipper)
(def node zip/node)

;;A protocol for datastructures that support zippers!
;;Eliminates some boilerplate for use, and allows a consistent interface 
;;for higher-order operations on zippers.
(defprotocol IZippable
  (-branch?       [x])
  (-get-children  [x])
  (-append-child  [x]))
;(defn -append-children [x xs] (reduce (-append-child x) x xs))
(defn zipper?
  "Determines if x is a zipper, created by clojure.zip/zipper by examining 
   its meta data."
  [x] (contains? (meta x) :zip/make-node))
(defn as-zipper
  "For zippable, which satisfies IZippable, builds a clojure.zip 
   zipper for it.  If applied to something that is already a zipper, returns
   the zipper."
  [zippable] 
  (if (zipper? zippable) zippable 
    (let [singleton (-append-child zippable)]
      (clojure.zip/zipper (-branch? zippable) 
                          (-get-children zippable)
                          (fn [n cs] (reduce singleton n cs))
                          zippable))))
;;Generic operations on zippables.
(defn map-zipper
  "Map function f to each node of the zipper.  Preserves the structure of the 
   zipper.  Note: f should return a node, and should not mess with the children
   or strange things may happen."  
  [f zippable]
  (loop [z (as-zipper zippable)]
    (if (clojure.zip/end? z) (clojure.zip/root z)
      (recur (clojure.zip/next (clojure.zip/edit z f))))))

(defn filter-zipper
  "Filter the zipper, according to a f, which returns a - likely
   - smaller zipper structure.  For each node, if f is false, the node 
  (and any children) will be pruned from the zipper."  
  [f zippable]
  (loop [z (as-zipper zippable)]
    (if (clojure.zip/end? z) (clojure.zip/root z)
      (if (f (clojure.zip/node z)) 
        (recur (clojure.zip/next z))
        (recur (clojure.zip/next (clojure.zip/remove z)))))))

(defn reduce-zipper
  "Identical to reduce, but operates over an abstract node sequence defined 
   by walking a zipper."
  [f init zippable]
  (loop [z (as-zipper zippable)
         acc init]
     (if (clojure.zip/end? z) acc
       (recur (clojure.zip/next z) (f acc (clojure.zip/node z))))))
(comment 
(defn zip-seq [zippable]
  (map zip/node (take-while #(not (zip/end? %)) 
                            (iterate zip/next (as-zipper zippable)))))
)



