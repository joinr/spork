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

(defn mf-test!! [& {:keys [n] :or {n 100000}}]
  (let [tnet (transient-network the-net)]
    (time (dotimes [i n]
            (do (mincost-flow tnet :s :t)
                (reset-flows :s tnet))))))

(defn scaled-flow-test [& {:keys [n scalar] :or {n 100000 scalar 3}}]
  (with-scaled-flow scalar     
    (let [scaled-flow (flow-fn *flow-options*)]
      (time (dotimes [i n]
            (compute-flow scaled-flow (transient-network the-net) :s :t))))))

(defn pflow-test [& {:keys [n] :or {n 100000}}]
  (time (doseq [i (pmap (fn [i] (mincost-flow the-net :s :t)) (range n))]
          nil)))

(defn pflow-test! [& {:keys [n] :or {n 100000}}]
  (time (doseq [i (pmap (fn [i] (mincost-flow (transient-network the-net) :s :t)) (range n))]
          nil)))
