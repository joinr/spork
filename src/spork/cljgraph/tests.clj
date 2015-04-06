;;A testing suite for all things graph related.
(ns spork.cljgraph.tests
  (:require [clojure.test :refer :all]
            [spork.cljgraph.core :refer :all]
            [spork.cljgraph.flow :refer :all]))

;;helper macro for some error catching I want to ensure.
(defmacro get-error [expr]
  `(try (do ~expr nil) 
      (catch Exception ~'e  ~'e)))

;a tree of elements:
  ;              a
  ;            b c d 
  ;           e f    g
  ;          h   i     j
  ;         k l  m n o  p q
  ;                        r s t u v w x y z a1 a2 a3 
(defn ->tree-arc [from to] [from to 1])
(defn tree-arcs  [from xs] (map #(->tree-arc from %) xs))



(def  the-tree (-> empty-ordered-graph
                  (add-arcs (tree-arcs :a [:b :c :d]))
                  (add-arcs (conj (tree-arcs :b [:e :f]) (->tree-arc :d :g)))
                  (add-arcs [(->tree-arc :e :h) 
                             (->tree-arc :f :i) 
                             (->tree-arc :g :j)])
                  (add-arcs [(->tree-arc :h :k) (->tree-arc :h :l) 
                             (->tree-arc :i :m) (->tree-arc :i :n) 
                             (->tree-arc :i :o) (->tree-arc :j :p) 
                             (->tree-arc :j :q)])
                  (add-arcs (tree-arcs :q  
                                       [:r :s :t :u :v :w :x :y :z :a1 :a2 :a3]))))

;;used for testing topsort
(def tree-depths  (:distance (depth-walk the-tree :a)))

(def unordered-tree 
  (-> empty-graph
      (add-arcs (tree-arcs :a [:b :c :d]))
      (add-arcs (conj (tree-arcs :b [:e :f]) (->tree-arc :d :g)))
      (add-arcs [(->tree-arc :e :h) 
                 (->tree-arc :f :i) 
                 (->tree-arc :g :j)])
      (add-arcs [(->tree-arc :h :k) (->tree-arc :h :l) 
                 (->tree-arc :i :m) (->tree-arc :i :n) 
                 (->tree-arc :i :o) (->tree-arc :j :p) 
                 (->tree-arc :j :q)])
      (add-arcs (tree-arcs :q  
                           [:r :s :t :u :v :w :x :y :z :a1 :a2 :a3]))))


(deftest graph-walks
  (is (= (ordered-nodes the-tree :a)
         [:a :b :e :h :k :l :f :i :m :n :o :c :d :g :j :p :q 
          :r :s :t :u :v :w :x :y :z :a1 :a2 :a3])
      "ordered first node ordering")  
  (is (= (breadth-nodes the-tree :a)
         [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v 
          :w :x :y :z :a1 :a2 :a3])
      "breadth-first node ordering")  
  (is (= (undirected-nodes the-tree :a) 
         [:a :d :g :j :q :a3 :a2 :a1 :z :y :x :w :v :u :t :s :r :p :c :b :f 
          :i :o :n :m :e :h :l :k])
      "undirected, depth-first node ordering")  
  (is (= (topsort the-tree)
         [#{:a} 
          #{:c :b :d} 
          #{:e :g :f} 
          #{:j :h :i} 
          #{:q :o :n :m :l :k :p} 
          #{:y :a2 :r :v :w :a3 :a1 :s :z :t :x :u}])
      "top-sort node clusters")
  (is (every? (fn [[l r]] (<= (get tree-depths l) (get tree-depths r))) (partition 2 1 (topsort-nodes the-tree)))
      "top-sort node ordering"))
 
(deftest tree-searching 
  (is (= (path? (depth-first-search the-tree :a :q)) 4)
      "Simple depth path in the-tree.  :q should be 4 hops from :a")
  (is (= (path? (breadth-first-search the-tree :a :q)) 4)
      "Simple breadth path in the-tree.  :q should be 4 hops from :a")
  (is (= (path? (priority-first-search the-tree :a :q)) 4)
      "Simple priority path in the-tree.  :q should be 4 hops from :a")
  (is (= (path? (random-search the-tree :a :q)) 4)
      "Simple random path in the-tree.  :q should be 4 hops from :a")
  )
  
 ;a directed graph...
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f/^
 
(def the-graph 
  (arcs->graph [[:a :b] [:b :c] [:c :d] [:e :f] [:f :c] [:b :d]]))
 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 
(def the-graph2 
  (-> empty-graph
      (add-arcs [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])))
 ;a directed graph with 2 components
 ;    a -> b -> d
 ;          \. ^
 ;           c/
 ;    e -> f
 ;    g 
 ;    h
 
(def the-graph3
  (-> (arcs->graph [[:a :b] [:b :c] [:c :d] [:e :f] [:b :d]])
      (conj-node :g)
      (conj-node :h)))

(deftest  connectivity-queries
  (is (= (components the-graph)  {6 [#{:e :c :b :d :f :a}]})
      "Should be one component in the-graph, containing all six nodes.")
  (is (= (components the-graph2) {2 [#{:e :f}], 4 [#{:c :b :d :a}]}) 
      "Should be two components in the-graph2, containing 2 and 4 nodes.")
  (is (= (components the-graph3) 
         {2 [#{:e :f}], 1 [#{:g} #{:h}], 4 [#{:c :b :d :a}]}) 
      "Should be 4 components in the-graph3, containing 4, 2, 1,1 nodes.")
  (is (empty? (islands the-graph)) "There should be no islands in the-graph.")
  (is (empty? (islands the-graph2)) "There should be no islands in the-graph2.")
  (is (= (count (islands the-graph3)) 2) 
      "There should be 2 islands in the-graph3."))     

 ;class-graph: a weighted directed graph with  components
 ;                  |-----4-->G--1-->F
 ;                  |                ^
 ;                  |                |
 ;    A--2-->B--2-->D--3-|           |
 ;    |             ^    |           |
 ;    ---3-->C--4---|    >E-----5----|
 ;           |            ^
 ;           |----1-------|
 ;
 ;    X--1-->Y--1-->Z
 ;    
 ;    M--1-->N

(def class-graph 
  (-> (arcs->graph
       [["A" "B" 2]     ;1st eq class
        ["A" "C" 3]     ;1st eq class
        ["B" "D" 2]     ;1st eq class
        ["C" "D" 4]     ;1st eq class
        ["C" "E" 1]     ;1st eq class
        ["D" "E" 3]     ;1st eq class
        ["D" "G" 4]     ;1st eq class
        ["E" "F" 5]     ;1st eq class
        ["G" "F" 1]     ;1st eq class
        ["Y" "Z" 1]     ;2nd eq class
        ["X" "Y" 1]     ;2nd eq class
        ["M" "N" 1]])   ;3rd eq class
      (conj-node "P")))  ;4th eq class, island

(deftest decomposition-test
  (is (= (sort-by count (map (comp sort nodes) (decompose class-graph)))
         '((["P" 12]) 
           (["M" 10] ["N" 11]) 
           (["X" 9] ["Y" 7] ["Z" 8]) 
           (["A" 0] ["B" 1] ["C" 2] ["D" 3] ["E" 4] ["F" 6] ["G" 5])))
      "class-graph should decompose into 4 smaller graphs."))

(deftest filter-test 
  (is (=  (get-paths (dijkstra class-graph "A" "F" {:multipath true}))
          '(("A" "C" "E" "F") ("A" "B" "D" "G" "F")))
      "class-graph should have 2 equivalent shortest paths")
  (is (=  (get-paths (dijkstra (with-nodefilter (complement #{"D"}) class-graph) "A" "F" {:multipath true}))
          '(("A" "C" "E" "F"))) 
      "class-graph with node D dropped should only have one shortest path"))

 ;sample-dag: a weighted directed acyclic graph with 
 ;            1 component
 ;
 ;Start
 ; |                    |-----4-->G--1-->F--0->Destination
 ; 0                    |                ^
 ; |                    |                |
 ; ---->A--2-->B--2-->D--3-|             |
 ;      |             ^    |             |
 ;      ---3-->C--4---|    >E-----5------|
 ;      |       |            ^           |
 ;      1       |----1-------|           0
 ;      |                                |
 ;      ->X--7-->Y---1------>ShortCut!---|
 ;        |
 ;        --2--->Z            

(def sample-dag
  (arcs->graph
   [[ "Start"  "A"  0]
    [ "A"  "B"  2]
    [ "A"  "C"  3]
    [ "B"  "D"  2]
    [ "C"  "D"  4]
    [ "C"  "E"  1]
    [ "D"  "E"  3]
    [ "D"  "G"  4]
    [ "E"  "F"  5]
    [ "G"  "F"  1]
    [ "A"  "X"  1]
    [ "X"  "Y"  7]
    [ "X"  "Z"  2]
    [ "Y"  "Shortcut!"   1]
    [ "Shortcut!"  "F"   0]
    [ "F"  "Destination" 0]]))

(deftest sssp-sample-dag
  (is (= (mapcat sort (topsort sample-dag))
         ["Start" "A" "B" "C" "X" "D" "Y" "Z" "E" "G" "Shortcut!" "F" "Destination"])
      "Topological ordering of sample-dag.")
  (is (= (get-weighted-paths 
          (depth-first-search sample-dag "Start" "Destination" {:multipath true}))
         '([6 ("Start" "A" "B" "D" "E" "F" "Destination")]))
      "Depth distance should be 6 for sample-dag")
  (is (= (get-weighted-paths 
          (breadth-first-search sample-dag "Start" "Destination" {:multipath true}))
         '([5 ("Start" "A" "C" "E" "F" "Destination")]))
      "Breadth distance should be 5 for sample-dag.")
  (is (= (get-weighted-paths 
          (dijkstra sample-dag "Start" "Destination" {:multipath true}))
         '([9 ("Start" "A" "C" "E" "F" "Destination")] 
           [9 ("Start" "A" "B" "D" "G" "F" "Destination")] 
           [9 ("Start" "A" "X" "Y" "Shortcut!" "F" "Destination")]))      
      "Dijkstra and priority-first-search should produce 3 9-weight paths for sample-dag.")
  (is (= (get-weighted-paths 
           (a-star sample-dag (fn [source target] 0) "Start" "Destination" {:multipath true}))
         (get-weighted-paths (dijkstra sample-dag "Start" "Destination" {:multipath true})))
      "a-star with a 0-weight heuristic is identical to dijkstra's algorithm")
  (is (= (get-weighted-paths (bellman-ford sample-dag "Start" "Destination" {:multipath true}))
         '([9 ("Start" "A" "C" "E" "F" "Destination")] 
           [9 ("Start" "A" "X" "Y" "Shortcut!" "F" "Destination")]
           [9 ("Start" "A" "B" "D" "G" "F" "Destination")]))
      "Bellman-Ford should produce 3 9-weight paths for sample-dag, albeit slower."))


;;positive-graph:
;;A----5---->B----10-->D---200--->E
;;|          ^         ^
;;|          |         |
;;|          3         |
;;---2-->C---|-----1---|

;;A has 2 shortest paths to B
;;A has 1 shortest path to C and D.
(def positive-graph 
  (arcs->graph [["A" "B" 5] 
                ["A" "C" 2] 
                ["C" "B" 3] 
                ["B" "D" 10]
                ["C" "D" 1] 
                ["D" "E" 200]]))

(deftest positive-graph-tests
  (is (= (get-weighted-paths (dijkstra positive-graph "A" "B" {:multipath true}))
         (get-weighted-paths (bellman-ford positive-graph "A" "B" {:multipath true}))
         '([5 ("A" "B")] [5 ("A" "C" "B")]))
         "Should be 2 shortest paths in positive-graph from A to B")
  (is (= (get-weighted-paths (dijkstra positive-graph "A" "C" {:multipath true}))
         (get-weighted-paths (bellman-ford positive-graph "A" "C" {:multipath true}))
         '([2 ("A" "C")]))
         "Should be 1 path in positive-graph from A to C")
  (is (= (get-weighted-paths (dijkstra positive-graph "A" "D" {:multipath true}))
         (get-weighted-paths (bellman-ford positive-graph "A" "D" {:multipath true}))
         '([3 ("A" "C" "D")]))
         "Should be 1 path in positive-graph from A to C")
  (is (= (get-weighted-paths (dijkstra positive-graph "A" "E" {:multipath true}))
         (get-weighted-paths (bellman-ford positive-graph "A" "E" {:multipath true}))
         '([203 ("A" "C" "D" "E")]))
         "Should be one path from A to E in positive-graph."))

;;A variation on positive-graph to test Dijkstra's non-negativity requirement.
;;There is a negative arc from D to E.  If a caller requests a path that forces 
;;Djikstra to engage this arc in the search, it should throw an exception.
;;An alternative is to apply a quick check to all the arcs...but this is 
;;costly.  Still debating how best to approach this.

;;negative-graph:
;;A----5---->B----10-->D--(-200)--->E
;;|          ^         ^
;;|          |         |
;;|          3         |
;;---2-->C---|-----1---|
(def negative-graph 
  (arcs->graph [["A" "B" 5] 
                ["A" "C" 2] 
                ["C" "B" 3] 
                ["B" "D" 10]
                ["C" "D" 1] 
                ["D" "E" -200]]))

(deftest negative-graph-tests 
  (is (= (str (get-error (dijkstra negative-graph "A" "E")))
         "java.lang.Exception: Negative Arc Weight Detected in Dijkstra: [\"D\" \"E\" -200]")
      "dijkstra should throw negative arc exception on negative-graph [D E] .")
  (is (= (get-weighted-paths (bellman-ford negative-graph "A" "E"))
         '([-197 ("A" "C" "D" "E")]))
      "bellman-ford should find a path on negative-graph."))

;;negative-cycle-graph:
;;------------------0-----------------|  
;;|                                   |
;;|                                   |
;;> A----5---->B----10-->D--(-200)--->E
;;  |          ^         ^
;;  |          |         |
;;  |          3         |
;;  ---2-->C---|-----1---|
(def negative-cycle-graph 
  (arcs->graph [["A" "B" 5] 
                ["A" "C" 2] 
                ["C" "B" 3] 
                ["B" "D" 10]
                ["C" "D" 1] 
                ["D" "E" -200]
                ["E" "A" 0]]))

(deftest negative-cycle-tests 
  (is (= (str (get-error (dijkstra negative-cycle-graph "A" "E")))
         "java.lang.Exception: Negative Arc Weight Detected in Dijkstra: [\"D\" \"E\" -200]")
      "dijkstra should throw negative arc exception on negative-graph [D E] .")
  (is (= (str (get-error (bellman-ford negative-cycle-graph "A" "E")))
         "java.lang.Exception: Possible negative cycles in Bellman Ford![\"A\" \"C\"]")
      "bellman-ford should find a negative cycle on negative-cycle-graph."))

;a directed graph with 2 components
;a - b - c - d - e
(def the-list 
  (-> empty-graph
      (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e]])))
 


;;a cyclical graph with a strongly connected component:
;    a -> b -> c -> d -> e
;         ^
;         |              /
;         \--------------

(def cycle-graph 
     (-> empty-graph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e] [:e :b]])))


(def big-cycle-graph 
     (-> empty-graph
         (add-arcs 
          (partition 2 1 [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :a]))))

;;a cyclical graph with a strongly connected component:
;    a -> b -> c -> d -> e
;         ^
;         |              /
;         \--------------
;;   f -> g -> h 
;;   ^ --------|
(def cycles-graph 
     (-> empty-graph
     (add-arcs [[:a :b] [:b :c] [:c :d] [:d :e] [:e :b]
                [:f :g] [:g :h] [:h :f]])))

;     _______________
;    |              |
;    \/             |
;    a -> b -> c -> d -> e
;         ^
;         |              /
;         \--------------

(def dicycle-graph 
  (-> empty-graph 
      (add-arcs [[:a :b]
                 [:b :c]
                 [:c :d]
                 [:d :e]
                 [:e :b]
                 [:d :a]])))

(def weird-graph 
  (-> empty-graph 
      (add-arcs [[:c->a 
                  :source-a 0] 
                 [:a :a->c 1.0] 
                 [:a :source-a 0] 
                 [:G :G->H 4.0] 
                 [:fillrule-b :b 0] 
                 [:c :c->a 1.0] 
                 [:c :source-c 0] 
                 [:fillrule-a :a 0] 
                 [:source-c :filled 0] 
                 [:source-a :filled 0] 
                 [:source-b :filled 0] 
                 [:b :source-b 0] 
                 [:a->c 
                  :source-c 0] 
                 [:unfilled :fillrule-c 0] 
                 [:unfilled :fillrule-b 0] 
                 [:unfilled :fillrule-a 0] 
                 [:fillrule-c :c 0]])))

(deftest cycle-finding 
  (is (= (directed-cycles cycle-graph) '((:e :b :c :d)))
      "Should have one directed cycle spanning e to d")
  (is (= (directed-cycles dicycle-graph) '((:e :b :c :d :a)))
      "should have one directed cycle spanning a to e.")
  (is (= (directed-cycles negative-cycle-graph)
         '(("E" "A" "C" "D" "B")))
      "Should have one cycle spanning e and b.")
  (is (= (directed-cycles big-cycle-graph)
         '((:q :r :s :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p)))
      "should have one cycle from q through a to p")
  (is (= (directed-cycles cycles-graph)
         '((:e :b :c :d) (:h :f :g)))
      "should have two directed cycles"))

;;Network Flow Testing
;;====================
(def net-data 
 [[:s   :chi  0 300]
  [:s   :dc   0 300]
  [:dc  :hou  4 280]
  [:dc  :bos  6 350]
  [:chi :bos  6 200]
  [:chi :hou  7 200]
  [:hou :t    0 300]
  [:bos :t    0 300]])

(def the-net 
  (-> empty-network 
    (conj-cap-arcs net-data)))



(comment
(defn augmented [n]
  (reduce (fn [g p] (augment-flow! g p)) 
          (transient-network the-net)
          (take n '((:s :dc :hou :t) (:s :dc :bos :t) (:s :chi :bos :t) (:s :chi :hou :t) (:s :chi :hou :dc :bos :t)))))
          
)

(def flow-results (mincost-flow the-net :s :t))
(def cost-net (:net flow-results))
(def actives (:active flow-results))

(deftest mincostflow-test 
  (is (= actives
         '([[:bos :t] 300] 
           [[:hou :t] 300]
           [[:chi :hou] 100]
           [[:chi :bos] 200]
           [[:dc :bos] 100]
           [[:dc :hou] 200]
           [[:s :dc] 300] 
           [[:s :chi] 300])))
  (is (= (einfos cost-net)          
         '(#spork.cljgraph.flow.einfo{:from :s, :to :chi, :capacity 0, :flow 300, :data nil} 
           #spork.cljgraph.flow.einfo{:from :s, :to :dc, :capacity 0, :flow 300, :data nil} 
           #spork.cljgraph.flow.einfo{:from :dc, :to :hou, :capacity 80, :flow 200, :data nil} 
           #spork.cljgraph.flow.einfo{:from :dc, :to :bos, :capacity 250, :flow 100, :data nil} 
           #spork.cljgraph.flow.einfo{:from :chi, :to :bos, :capacity 0, :flow 200, :data nil} 
           #spork.cljgraph.flow.einfo{:from :chi, :to :hou, :capacity 100, :flow 100, :data nil} 
           #spork.cljgraph.flow.einfo{:from :hou, :to :t, :capacity 0, :flow 300, :data nil} 
           #spork.cljgraph.flow.einfo{:from :bos, :to :t, :capacity 0, :flow 300, :data nil}))
      "Network should have expected flows." )
  (is (= (total-flow cost-net) 600)
      "There should be 600 units of flow")
  (is (= (total-cost cost-net (:active flow-results)) 3300)
      "Shipping costs 3300 units."))

(def mutable-flow-results (mincost-flow (transient-network the-net) :s :t))
(def mutable-cost-net (:net mutable-flow-results))
(def persisted-cost-net (persistent-network! mutable-cost-net))
(def active-mutable-flows (:active mutable-flow-results))

(deftest mutable-mincostflow-test 
  (is (= (into {} active-mutable-flows) (into {} (:active flow-results)))
      "Network should have same expected flows on edges.")
  (is (= persisted-cost-net cost-net)
      "Resultant networks should be identical.")
  (is (= (total-flow persisted-cost-net) 600)
      "There should be 600 units of flow")
  (is (= (total-cost persisted-cost-net (:active mutable-flow-results)) 3300)
      "Shipping costs 3300 units."))
  
(deftest augmentation-test 
  (is (= (:augmentations (augmentations the-net  :s :t)) 
         (:augmentations (augmentations (transient-network the-net) :s :t))
         '[[280 (:s :dc :hou :t)] [20 (:s :dc :bos :t)] [200 (:s :chi :bos :t)] [20 (:s :chi :hou :t)] [80 (:s :chi :hou :dc :bos :t)]])
      "Augmentations should be identical in both networks."))

(def scalar 7)

(def scaled-flow-results
  (with-scaled-flow scalar     
    (let [scaled-flow (flow-fn *flow-options*)]
      (compute-flow scaled-flow  the-net :s :t))))
(def scaled-flows (:active scaled-flow-results))

(deftest scaled-flow-test 
  (is (= scaled-flows
         '([[:bos :t] 294] [[:hou :t] 294] [[:chi :hou] 98] [[:chi :bos] 196] [[:dc :bos] 98] [[:dc :hou] 196] [[:s :dc] 294] [[:s :chi] 294]))
      "Scaled flows should be equal to reference values.")
  (is (every? zero? (for [[e flow] scaled-flows] (mod flow scalar)))
      "All flows should be divisible by the scalar, with no remainder"))

;;more tests to come.
