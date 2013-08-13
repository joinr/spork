(ns spork.cljgraph.test
  (:require [cljgraph.graph :as g]
            [cljgraph.algorithms.algs :as algs]))

;tests
;this little graph possesses some nice characteristics:
;A has 2 shortest paths to B
;A has 1 shortest path to C and D.
;There is a negative arc from D to E.  If a caller requests a path that forces 
;  Djikstra to engage this arc in the search, it should throw an exception.
;  An alternative is to apply a quick check to all the arcs...but this is 
;  costly.  Still debating how best to approach this.
(def negative-graph 
  (let [arcs [["A" "B" 5] 
              ["A" "C" 2] 
              ["C" "B" 3] 
              ["B" "D" 10]
              ["C" "D" 1] 
              ["D" "E" -200]]]
    (-> (g/make-graph) (g/add-arcs arcs))))

(def positive-graph 
  (let [arcs [["A" "B" 5] 
              ["A" "C" 2] 
              ["C" "B" 3] 
              ["B" "D" 10]
              ["C" "D" 1] 
              ["D" "E" 200]]]
    (-> (g/make-graph) (g/add-arcs arcs))))


;currently ignores the second path, but registers it in the final search state.
(pprint (algs/single-shortest positive-graph "A" "B")) 
;;Without E
;{:path ("A" "B"),
; :length 5,
; :stats
; {:shortest {"D" "C", "C" "A", "B" ["A" "C"]},
;  :distance {"D" 3, "C" 2, "B" 5, "A" 0},
;  :fringe {5 ["B"]}}}

;; With E negated...
;#<Exception java.lang.Exception: negative weight: -197 detected near E>

(pprint (algs/single-shortest negative-graph "A" "D"))
;{:path ("A" "C" "D"),
; :length 3,
; :stats
; {:shortest {"D" "C", "C" "A", "B" ["A" "C"]},
;  :distance {"D" 3, "C" 2, "B" 5, "A" 0},
;  :fringe {3 ["D"], 5 ["B"]}}}

(pprint (algs/single-shortest negative-graph "A" "D" :searchf algs/dfs))
(pprint (algs/single-shortest negative-graph "A" "D" :searchf algs/bfs))
(pprint (algs/single-shortest negative-graph "A" "D" :searchf algs/pfs))
