(ns testgraph
  (:require [spork.cljgraph [flow :as flow] 
                            [core :as graph]]))

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
;;and runtime savings were slight for walks.

;;We only really need ordered traversal for tree operations...
;;Thus, I don't think the ordered graph should be the default.
;;There's overhead incurred every time there...

;;Another likely performance killer is the priorityq implementation
;;for the search fringes. 

;;Fringes, in general, will be transient in nature: They only exist to
;;facilitate the search.  Since we want searches to be as fast as
;;possible, we probably want to use mutable structures for the
;;fringes. That should be an assload faster. 


;;ArraySeq is the biggest bottleneck at the moment, even for the
;;comparatively simple depth-first-search.
