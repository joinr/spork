(ns spork.cljgraph.flowtests
  (:require [spork.cljgraph.flow :refer :all]
            [spork.cljgraph [arrayflow :as arr]]))

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

(def anet 
  (arr/net->array-net the-net))

(defn aflow-test [& {:keys [n] :or {n 100000}}]
  (time (dotimes [i n]
          (arr/array-mincost-flow anet 0 5))))

(defn aflow-test! [& {:keys [n] :or {n 100000}}]
  (let [bnet (arr/clone-network anet)]
    (time (dotimes [i n]
            (do (arr/array-mincost-flow! bnet 0 5)
                (arr/reset-flow! bnet))))))
(defn mf-test [& {:keys [n] :or {n 100000}}]
  (time (dotimes [i n]
          (mincost-flow the-net :s :t))))

(defn mf-test! [& {:keys [n] :or {n 100000}}]
  (time (dotimes [i n]
          (mincost-flow (transient-network the-net) :s :t))))

(defn scaled-flow-test [& {:keys [n scalar] :or {n 100000 scalar 3}}]
  (with-scaled-flow scalar     
    (let [scaled-flow (eval (flow-fn *flow-options*))]
      (time (dotimes [i n]
            (scaled-flow (transient-network the-net) :s :t))))))

(defn pflow-test [& {:keys [n] :or {n 100000}}]
  (time (doseq [i (pmap (fn [i] (mincost-flow the-net :s :t)) (range n))]
          nil)))

(defn pflow-test! [& {:keys [n] :or {n 100000}}]
  (time (doseq [i (pmap (fn [i] (mincost-flow (transient-network the-net) :s :t)) (range n))]
          nil)))

(comment 

;;Fastest so far...
(defn traverse2d
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode startstate]
  (loop [state   (-> (assoc! startstate :targetnode targetnode)
                     (generic/conj-fringe startnode 0))]
    (if-let [source    (generic/next-fringe state)] ;next node to visit
      (let  [visited   (generic/visit-node state source)] ;record visit.
        (if (= targetnode source) visited                     
            (recur (loop [acc visited
                          xs (flow-neighbors!!! g source)]
                     (if (empty? xs) acc                        
                           (recur (generic/relax acc (flow-weight2 g source (first xs)) source (first xs))
                                  (rest xs)))))))
      state)))

(defn traverse2e
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode startstate]
  (loop [state   (-> (assoc! startstate :targetnode targetnode)
                     (generic/conj-fringe startnode 0))]
    (if-let [source    (generic/next-fringe state)] ;next node to visit
      (let  [visited   (generic/visit-node state source)] ;record visit.
        (if (= targetnode source) visited                     
            (recur (let [^objects xs (to-array (flow-neighbors!!! g source))
                         n (alength xs)]
                     (loop [acc visited
                            idx 0]
                       (if (== idx n) acc                        
                           (recur (generic/relax acc (flow-weight2 g source (aget xs idx)) source (aget xs idx))
                                  (unchecked-inc idx))))))))
      state)))

(defn transient-traverse2e
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [net startnode targetnode startstate]
  (let [g (:g net)]
    (loop [state   (-> (assoc! startstate :targetnode targetnode)
                       (generic/conj-fringe startnode 0))]
      (if-let [source    (generic/next-fringe state)] ;next node to visit
        (let  [visited   (generic/visit-node state source)] ;record visit.
          (if (= targetnode source) visited                     
              (recur (let [^objects xs (to-array (transient-flow-neighbors!!! net source))
                           n (alength xs)]
                       (loop [acc visited
                              idx 0]
                         (if (== idx n) acc                        
                             (recur (generic/relax acc (flow-weight2 g source (aget xs idx)) source (aget xs idx))
                                    (unchecked-inc idx))))))))
        state))))

(defn transient-traverse2e-int
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [net startnode targetnode startstate]
  (let [g (:g net)]
    (loop [state   (-> (assoc! startstate :targetnode targetnode)
                       (generic/conj-fringe startnode 0))]
      (if-let [source    (generic/next-fringe state)] ;next node to visit
        (let  [visited   (generic/visit-node state source)] ;record visit.
          (if (= targetnode source) visited                     
              (recur (let [^objects xs (to-array (transient-flow-neighbors!!! net source))
                           n (alength xs)]
                       (loop [acc visited
                              idx 0]
                         (if (== idx n) acc                        
                             (recur (generic/int-relax acc (flow-weight2 g source (aget xs idx)) source (aget xs idx))
                                    (unchecked-inc idx))))))))
        state))))

(defn traverse2f
  "Generic fn to eagerly walk a graph.  The type of walk can vary by changing 
   the searchstate, the halting criteria, the weight-generating 
   function, or criteria for filtering candidates.  Returns a searchstate 
   of the walk, which contains the shortest path trees, distances, etc. for 
   multiple kinds of walks, depending on the searchstate's fringe structure."
  [g startnode targetnode startstate]
  (loop [state   (-> (assoc! startstate :targetnode targetnode)
                     (generic/conj-fringe startnode 0))]
    (if-let [source    (generic/next-fringe state)] ;next node to visit
      (let  [visited   (generic/visit-node state source)] ;record visit.
        (if (= targetnode source) visited                     
            (recur (let [^objects xs (to-array (flow-neighbors!!!!! g source))
                         n (alength xs)]
                     (loop [acc visited
                            idx 0]
                       (if (== idx n) acc                        
                           (recur (generic/relax acc (flow-weight2 g source (aget xs idx)) source (aget xs idx))
                                  (unchecked-inc idx))))))))
      state)))

(definline mincost-aug-path3d [g from to]
  `(traverse2d ~g ~from ~to (searchstate/empty-PFS3 ~from)))

;; (definline mincost-aug-pathm [g from to]
;;   `(first (graph/get-paths 
;;            (search/traverse2a ~g ~from ~to (searchstate/mempty-PFS ~from)
;;                             :weightf flow-weight :neighborf flow-neighbors))))

(definline mincost-aug-pathm [g from to]
  `(traverse2d ~g ~from ~to (searchstate/mempty-PFS ~from)))


(definline mincost-aug-stateme [g from to]
  `(traverse2e ~g ~from ~to (searchstate/mempty-PFS ~from)))

(definline mincost-aug-pathme [g from to]
  `(searchstate/first-path (traverse2e ~g ~from ~to (searchstate/mempty-PFS ~from))))

(definline mincost-aug-pathme!! [g from to]
  `(searchstate/first-path (transient-traverse2e ~g ~from ~to (searchstate/mempty-PFS ~from))))

(definline mincost-aug-pathme!!-int [g from to]
  `(searchstate/first-path (transient-traverse2e-int ~g ~from ~to (searchstate/mempty-PFS ~from))))

(definline mincost-aug-pathmf [g from to]
  `(traverse2f ~g ~from ~to (searchstate/mempty-PFS ~from)))

(definline mincost-aug-pathm2 [g from to]
  `(first (graph/get-paths 
           (search/traverse2a ~g ~from ~to (searchstate/mempty-PFS2 ~from)
                            :weightf flow-weight :neighborf flow-neighbors))))

(definline mincost-aug-pathm3 [g from to state]
  `(first (graph/get-paths 
           (search/traverse2a ~g ~from ~to ~state
                            :weightf flow-weight :neighborf flow-neighbors))))

(defn mincost-flow2
  ([graph from to]
    (loop [g graph]
      (if-let [p (mincost-aug-path3d g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow (assoc graph :flow-info flow-info) from to)))

(defn mincost-flowm
  ([graph from to]
    (loop [g graph]
      (if-let [p (mincost-aug-pathme g from to)]
        (recur (augment-flow g p))
        (let [active (active-flows g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flow (assoc graph :flow-info flow-info) from to)))

(defn mincost-flowm!
  ([graph from to]
    (loop [g (transient-network graph)]
      (if-let [p (mincost-aug-pathme g from to)]
        (recur (augment-flow! g p))
        (let [active (active-flows! g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flowm! (assoc graph :flow-info flow-info) from to)))

(defn mincost-flowm!!
  ([graph from to]
    (loop [g (transient-network!! graph)]
      (if-let [p (mincost-aug-pathme!! g from to)]
        (recur (augment-flow!! g p))
        (let [active (active-flows! g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flowm!! (assoc graph :flow-info flow-info) from to)))

(defn mincost-flowm!!-int
  ([graph from to]
    (loop [g (transient-network!! graph)]
      (if-let [p (mincost-aug-pathme!!-int g from to)]
        (recur (augment-flow!! g p))
        (let [active (active-flows! g)]
          {
           ;:cost (total-cost graph active)
           ;:flow (total-flow g active)
           :active active
           :net g}))))
  ([flow-info graph from to]
    (mincost-flowm!! (assoc graph :flow-info flow-info) from to)))

(defn mincost-flowm2
  [graph from to]
  (let [*the-state*
        (searchstate/mempty-PFS2 from)]
       (loop [g graph]
         (if-let [p (mincost-aug-pathm3 g from to *the-state*)]
           (recur (do (generic/clear! *the-state*) 
                      (augment-flow g p)))
           (let [active (active-flows g)]
             {
                                        ;:cost (total-cost graph active)
                                        ;:flow (total-flow g active)
              :active active
              :net g})))))
 

(def sample 
  '{["55530LJ00" :filled]
    {:from "55530LJ00", :to :filled, :capacity 31.0, :flow 0},
    [RCAD "55530LJ00"]
    {:from RCAD, :to "55530LJ00", :capacity 9223372036854775807, :flow 0},
    [AC "5530LJ00"]
    {:from AC, :to "55530LJ00", :capacity 9223372036854775807, :flow 0},
    [RC RCAD-BIG] {:from RC, :to RCAD-BIG, :capacity 25000, :flow 0},
    [RC RCAD] {:from RC, :to RCAD, :capacity 25000, :flow 0},
    [Supply RC] {:from Supply, :to RC, :capacity 530000, :flow 0},
    [Supply AC] {:from Supply, :to AC, :capacity 450000, :flow 0},
    [Total Supply] {:from Total, :to Supply, :capacity 980000, :flow 0}})

(def sample-net (->> (for [{:keys [from to capacity flow]} (vals sample)]
                      [from to 0 capacity])
                    (conj-cap-arcs empty-network)))

;;Graph Caching speeds things up a bit.
;;After this, however, I started blowing the heap somehow....
;;Actually, after recovering paths I started blowing the heap...
(def empty-network2 (assoc (spork.data.digraph/->cached-graph) :flow-info {}))
(def sample-net2 
  (->> (for [{:keys [from to capacity flow]} (vals sample)]
         [from to 0 capacity])
       (conj-cap-arcs empty-network2)))

(defn the-testd [x]
  (time (dotimes [i x]
          (mincost-aug-path3d the-net :s :t))))

(defn the-testm [x]
  (time (dotimes [i x]
          (mincost-aug-pathm the-net :s :t))))

(defn the-testme [x]
  (time (dotimes [i x]
          (mincost-aug-pathme the-net :s :t))))

(defn the-testmf [x]
  (time (dotimes [i x]
          (mincost-aug-pathmf the-net :s :t))))

(defn the-testme-cached [x]
  (time (dotimes [i x]
          (mincost-aug-pathme the-net2 :s :t))))

(defn the-testme-full [x]
  (time (dotimes [i x]
          (mincost-aug-pathme the-net :s :t))))

)
  
