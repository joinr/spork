;;some trivial utilities for spitting
;;graphs into simple formats that yed
;;can read...
(ns spork.cljgraph.io
  (:require [spork.cljgraph.core :as graph]
            [spork.util.io :as sio]
            [clojure.java.io :as io]))


(defn graph->tgf [g p]
  (let [nds (into {} (map-indexed vector (graph/get-node-labels g)))
        lbl->nd (reduce-kv (fn [acc k v] (assoc acc v k)) {} nds)
        arcs  (for [[from to cost] (graph/arc-seq g)]
                [(lbl->nd from) (lbl->nd to) (if (zero? cost) "" (str cost))])]
    (with-open [w (io/writer p)]
      (println (str "spitting graph to " p))
      (doseq [[idx lbl] (sort-by first (seq nds))]
        (sio/writeln! w (str idx " " lbl)))
      (sio/writeln! w  "#")
      (doseq [arc arcs]
        (sio/writeln! w (clojure.string/join " " arc )))
      (println "done!"))))
      
      
    
    
 
    

