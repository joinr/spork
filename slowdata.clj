(ns testgraph
  (:require [spork.cljgraph [flow :as flow] 
                            [core :as graph]]
            [spork.protocols [core :as generic]])
  (:import [java.util ArrayList PriorityQueue ArrayDeque]))

;;This is a chunk of data that exposes some flaws in the graph algos.
;;Good test data for netflow stuff.
;;Seems like the priority-first-search is slow...
(def g 
#spork.data.digraph.digraph{:nodes
                            {:filled 7
                             "5530LJ00" 6
                             RCAD-BIG 5
                             RCAD 4
                             RC 3
                             AC 2
                             Supply 1
                             Total 0}
                            :sources
                            {:filled {"55530LJ00" 0}
                             "55530LJ00" {AC 0, RCAD 1}
                             RCAD-BIG {RC 0}
                             RCAD {RC 0}
                             RC {Supply 0}
                             AC {Supply 0}
                             Supply {Total 0}
                             Total {}}
                            :sinks 
                            {:filled {}
                             "55530LJ00" {:filled 0}
                             RCAD-BIG {}
                             RCAD {"55530LJ00" 1}
                             RC {RCAD 0 RCAD-BIG 0}
                             AC {"55530LJ00" 0}
                             Supply {AC 0 RC 0}
                             Total {Supply 0}}
                            :flow-info 
                            {["55530LJ00" :filled]
                             {:from "55530LJ00" :to :filled :capacity 31.0 :flow 0}
                             [RCAD "55530LJ00"]
                             {:from RCAD
                              :to "55530LJ00"
                              :capacity 9223372036854775807
                              :flow 0}
                             [AC "5530LJ00"]
                             {:from AC :to "55530LJ00" :capacity 9223372036854775807 :flow 0}
                             [RC RCAD-BIG] {:from RC :to RCAD-BIG :capacity 25000 :flow 0}
                             [RC RCAD]     {:from RC :to RCAD :capacity 25000 :flow 0}
                             [Supply RC]   {:from Supply :to RC :capacity 530000 :flow 0}
                             [Supply AC]   {:from Supply :to AC :capacity 450000 :flow 0}
                             [Total Supply] {:from Total :to Supply :capacity 980000 :flow 0}}})
   

;;We can get a significant performance boost (x 5) by eliminating the call
;;to vec in the implementation of -get-sinks and -get-sources for
;;spork.data.digraph.digraph - just return the resulting list.
;;[done]

;;Also, I use ordered maps for the digraph.
;;These are about 2x as slow for seq instancing as normal maps...
;;Another optimization is to eliminate ordered maps and just use {} 
;;That lets us use array maps where possible (dictated by the
;;runtime).

;;Note -> did a quick test without the ordered-map backing the graph, 
;;and runtime savings were slight for walks.  Since walks are a worst
;;case scenario, I assume the ordered map in not currently a
;;significant hotspot.

;;We only really need ordered traversal for tree operations...
;;Thus, I don't think the ordered graph should be the default.
;;There's overhead incurred every time there...

;;Another likely performance killer is the priorityq implementation
;;for the search fringes. 

;;For our pq, I built an alter value function that allows us to
;;reweight the node on the priority queue.  This is potentially a
;;performance bottleneck, since I find a persistent queue associated
;;with the priority, then traverse that queue to filter out the old
;;node.  This could take o(n) time worst case, where n is the length
;;of the queue...For PFS or Dijkstra, we really don't use the PQ in
;;isolation; i.e. we have a distance map that we're maintaining.  So,
;;we can avoid removing items from the queue as a consequence of
;;re-weighing; we'll automatically ignore them if they show up on the
;;fringe again with a higher distance.  We can basically just add them
;;to the fringe and not worry about removing AND updating.

;;Fringes, in general, will be transient in nature: They only exist to
;;facilitate the search.  Since we want searches to be as fast as
;;possible, we probably want to use mutable structures for the
;;fringes. That should be an assload faster. 


;;ArraySeq is the biggest bottleneck at the moment, even for the
;;comparatively simple depth-first-search.


;;Looking at using fast, mutable structures for search fringes.
(defn ^ArrayList make-array-list [] (ArrayList.))
(defn ^ArrayList array-list [xs] 
  (reduce (fn [^ArrayList acc x] (doto acc (.add x))) (make-array-list) xs))

(defn ^ArrayDeque make-queue [] (ArrayDeque.))
(defn ^ArrayDeque queue [xs] 
  (reduce (fn [^ArrayDeque acc x] (doto acc (.add x))) (make-queue) xs))

(defn entry-comparer [l r] 
  (let [pl (generic/entry-priority l)
        pr (generic/entry-priority r)]
    (cond (< pl pr) -1 
          (> pl pr) 1
          :else 0)))   

(defn ^ArrayList  add-list   [^ArrayList l obj]  (doto l  (.add obj)))
(defn ^ArrayDeque add-q      [^ArrayDeque q obj]  (doto q (.add obj)))


(defn ^PriorityQueue make-pq [] (PriorityQueue. 11 entry-comparer))
(defn ^PriorityQueue pq [xs] 
  (reduce (fn [^PriorityQueue acc x]   
            (doto acc (.add x))) (make-pq) xs))
            
(defn ^PriorityQueue add-pq  [^PriorityQueue q obj]  (doto q (.add obj)))
(defn ^PriorityQueue pop-pq  [^PriorityQueue q    ]  (do (.poll q) q))


(extend-protocol generic/IFringe 
  java.util.ArrayList
  (conj-fringe [fringe n w] (add-list fringe n))
  (next-fringe [fringe]     (when (> (count fringe) 0) (nth fringe (dec (count fringe)))))
  (pop-fringe  [fringe]     (doto fringe (.remove (dec (count fringe)))))
  java.util.ArrayDeque
  (conj-fringe [fringe n w] (add-q fringe n))
  (next-fringe [fringe]     (.peek fringe))
  (pop-fringe  [fringe]     (do (.pop fringe) fringe))
  java.util.PriorityQueue
  (conj-fringe [fringe n w] (add-pq fringe (generic/entry w n)))
  (next-fringe [fringe]     (when-let [e (.peek ^PriorityQueue fringe)] (val e)))
  (pop-fringe [fringe]      (pop-pq fringe)))


(defn empty-PFS [startnode] (spork.data.searchstate/init-search startnode :fringe (make-pq)))
(defn empty-DFS [startnode] (spork.data.searchstate/init-search startnode :fringe (make-array-list)))
(defn empty-BFS [startnode] (spork.data.searchstate/init-search startnode :fringe (make-queue)))
;;testing

(comment 
(def the-nodes (map generic/entry (repeatedly (fn [] (rand))) (range 10000)))
(defmacro qtime  [expr] `(time (do ~expr nil)))
(defn round-trip [fr] 
  (loop [acc (generic/conj-fringe-all fr the-nodes)]
    (when (generic/next-fringe acc)
      (recur (generic/pop-fringe acc)))))


;Searches...  
(println "mutable searches:")
(qtime (dotimes [i 1000] (search/traverse g 'Total :filled (empty-PFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :filled (empty-DFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :filled (empty-BFS 'Total))))
(println "persistent  searches:")
(qtime (dotimes [i 1000] (search/traverse g 'Total :filled (searchstate/empty-PFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :filled (searchstate/empty-DFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :filled (searchstate/empty-BFS 'Total))))

;Walks
(println "mutable walks:")
(qtime (dotimes [i 1000] (search/traverse g 'Total :blah (empty-PFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :blah (empty-DFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :blah (empty-BFS 'Total))))
(println "persistent  walks:")
(qtime (dotimes [i 1000] (search/traverse g 'Total :blah (searchstate/empty-PFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :blah (searchstate/empty-DFS 'Total))))
(qtime (dotimes [i 1000] (search/traverse g 'Total :blah (searchstate/empty-BFS 'Total))))

;;Testing to ensure our path finding is the same with the new stuff...
(defn same-res? [s1 s2] 
  (= (graph/get-weighted-paths s1) 
     (graph/get-weighted-paths s2)))

(assert (same-res? (search/traverse g 'Total :filled (empty-PFS 'Total))
                   (graph/priority-first-search g 'Total :filled)))
(assert (same-res?  (search/traverse g 'Total :filled (empty-DFS 'Total))
                    (search/traverse g 'Total :filled (searchstate/empty-DFS 'Total))))
(assert (same-res?  (search/traverse g 'Total :filled (empty-BFS 'Total))
                    (search/traverse g 'Total :filled (searchstate/empty-BFS 'Total))))


(assert (same-res?  (search/traverse g 'Total :blah (empty-PFS 'Total))
                    (graph/priority-first-search g 'Total :blah)))
(assert (same-res?  (search/traverse g 'Total :blah (empty-DFS 'Total))
                    (search/traverse g 'Total :blah (searchstate/empty-DFS 'Total))))
(assert (same-res?  (search/traverse g 'Total :blah (empty-BFS 'Total))
                    (search/traverse g 'Total :blah (searchstate/empty-BFS 'Total))))
)
