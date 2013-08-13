(ns spork.cljgraph.algorithms.algs
  "This is the beginning of a 'hopefully' robust graph algorithms library. 
   I intend to bring in more generic functionality, eventually implementing 
   most of the useful bits in Bob Sedgewick's opus on graphs, albeit with a 
   functional API, in Clojure.  This will form the basis for an additional 
   cljgraph.algorithms.networks.  I might migrate this namespace into 
   algorithms.paths, but for now it's fine.  I am still feeling out the 
   utility of protocols and 'good' function names, so you might run into 
   some funky names.  The API is still subject to later change, in an effort to 
   enhance readability.
   
   A few quick notes that highlight some 'GOTCHAS' in clojure: 

     *If you :use or (use) another namespace, clojure will quietly override your 
      function names in the current namespace, or something like that.  I had 
      some protocol functions defined in cljgraph.generics, specifically IFringe
      and I found out that due to some sloppy :uses, I was not using the 
      protocol functions, but an add-nodes function from the cljgraph.graph.  
      Ugh! Require, with :as, is a superior option, as it keeps everything 
      namespace-relative.

     *Protocols dispatch on the type of their first arguement.  When building 
      a functional API, we need access to constructor functions because we end 
      up (at the bare minimum), constructing an empty item and adding things to 
      it, or creating new structures on the fly using more sophisticated 
      arguments.  If you 'want' to create a datastructure that participates in 
      a protocol with using extend-protocol - like a record - then you can 
      implement the protocol, making sure to use the constructor relative to 
      your data structure, inside the protocol implementation.  That seems to 
      work....although I still seem to get a bit of a performance hit."
  
  (:use [cljgraph.graph])
  (:require [cljgraph.data.search :as search]
            [cljgraph.data.fringe :as fr]
            [cljgraph.data.priorityq :as pq]
            [cljgraph.algorithms [djikstra :as djik]]))


(defn get-path
  "Recover all paths from startnode to targetnode, given shortest path tree 
   spt."
  [startnode targetnode spt]
  (if (and (map? spt) (contains? spt targetnode))   
	  (let [singleton (fn [itm] (if (vector? itm) (first itm) itm))]
		  (loop [currnode targetnode  
		         path nil
             branch nil]	         
	     (if (= currnode startnode) 
		      (cons currnode path) 
		      (recur (singleton (get spt currnode))  (cons currnode path) nil))))
   nil))


(defn bfs
  "Execute a breadth-first search on graph g, from startnode to stopnode."
  [g startnode stopnode]
  ((search/breadth-walk g) startnode stopnode))
(defn dfs 
  "Execute a depth-first search on graph g, from startnode to stopnode."
  [g startnode stopnode] 
  ((search/depth-walk g) startnode stopnode))

(defn pfs 
  "Exceute a priority-first search on graph g, from startnode to stopnode.
   This is identical to Djikstra's algorithm."
  [g startnode stopnode]
  ((search/priority-walk g) startnode stopnode))

;probably going to stick this in a higher level abstraction....along with 
;generic search state....I need to figure out how to enable the user to get 
;multiple paths....simply.
(defn single-shortest
  "Derive a single-source shortest path from startnode to target node, 
   if one exists.  Returns a map containing path, length, and the final 
   search state."
  [g startnode targetnode & {:keys [searchf] :or {searchf djik/djikstra}}]
  (if-let [result (searchf g startnode targetnode)]
    {:path (get-path startnode targetnode (:shortest result))
     :length ((:distance result) targetnode)
     :stats result}
    nil))
   


