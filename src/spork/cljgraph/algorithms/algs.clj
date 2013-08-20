(ns spork.cljgraph.algorithms.algs
  "This is the beginning of a 'hopefully' robust graph algorithms library. 
   I intend to bring in more generic functionality, eventually implementing 
   most of the useful bits from Bob Sedgewick's opus on graphs, albeit with a 
   functional API, in Clojure.  This will form the basis for an additional 
   cljgraph.algorithms.networks.  I might migrate this namespace into 
   algorithms.paths, but for now it's fine.  I am still feeling out the 
   utility of protocols and 'good' function names, so you might run into 
   some funky names.  The API is still subject to later change, in an effort to 
   enhance readability."
  (:require [spork.protocols.core :refer :all]
            [spork.data [searchstate :as search]
                        [fringe :as fr]
                        [priorityq :as pq]]
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
   


