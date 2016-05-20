(ns spork.util.inspection
  (:require [clojure [reflect :as reflect]]
            [clojure.inspector :as inspect]
            [spork.data [orderedmap :as om]]))

(def ^:dynamic *short-types* true)

(defn public? [meth] (contains? (:flags meth) :public)) 
(defn members [cls] (filter public? (:members (reflect/reflect cls))))
(def trunc-pattern #"\.[A-Za-z]*$")
(def short-name (memoize (fn [cls] (if-let [the-name (re-find trunc-pattern (str cls))]
                                     (symbol (subs the-name 1))
                                     cls))))
(defn method->sig [m]
  `((with-meta ~(:name m) {:tag ~(:return-type m)})   ~(:parameter-types m))) 

(defn close [sigs]
  (if (not (coll? sigs)) sigs
      (let [flip (fn [o] (if  o o '->))]
        (loop [acc []
               op  nil
               xs sigs]
          (if (empty? xs) acc
              (let [x (first xs)
                    res  (if (coll? x) (close x) (if *short-types* (short-name x) x))]
                (recur (-> (if op (conj acc op) acc) (conj res)) (flip op) (rest xs))))))))

(def as-type (symbol (str "::")))          
(defn func-sig [name sig] `(~name ~as-type  ~(close sig)))
           
(defn get-params [m]
  (let [p (get m :parameter-types)]
    (if (= p []) '[void] p)))
        
(defn method->funcsig [m]
  (func-sig (:name m)  (conj (get-params m) 
                             (get m :return-type (get m :name)))))
        
(defn flatten-bases [cls]
  (loop [visited #{cls}
         order [cls]
         bases (:bases (reflect/reflect cls))]
    (if (empty? bases) order
        (let [base (ns-resolve *ns* (first bases))]
          (if (visited base)
            (recur visited order (next bases))           
            (recur (conj visited  base) (conj order  base) 
                   (into bases (remove visited (:bases (reflect/reflect base))))))))))

(defn get-spec [cls]
  {:class cls :methods (map method->funcsig (members  cls))})

(defn replace-with
  "Derived from clojure.core/replace"
  [smap coll]
  (if (vector? coll)
    (reduce (fn [v i]
               (if-let [e (find smap (nth v i))]
                 (assoc v i (val e))
                 v))
             coll (range (count coll)))
    (map #(if-let [e (find smap %)] (val e) %) coll)))

(defn derive-spec [cls]
  (let [methods (atom #{})
        new-method? (fn [m] (when-not (@methods m)
                                (do (reset! methods (conj @methods m)) m)))                                
        add-spec! (fn [cls] 
                    (let [res (get-spec cls)]
                      (assoc res :methods (filter new-method? (:methods res)))))]                      
    (reduce (fn [acc [k xs]] (reduce conj (conj acc k) xs)) []  
            (for [{:keys [class methods]} 
                  (map get-spec (reverse (flatten-bases cls)))]
              [class methods]))))

(defn spec-map [cls]
  (-> (reduce (fn [[k xs m] x]
                          (if (coll? x) [k (conj xs x) m]
                              [x [] (assoc m k xs)])) [nil [] {}] (derive-spec cls))
      (last) 
      (dissoc nil)))

;;Useful functions for inspecting trees...
  
;;We could also be smarter about string comparers, to acccount for
;;numbers...
;;allows keywords and strings to be compared equally...
(defn generic-comp [l r]
  (cond (or (and (keyword? l) (string? r))
            (and (string? l) (keyword? r)))                   (compare (name l) (name r))
        (identical? (inspect/atom? l)  (inspect/atom? r))     (compare l r)
        :else
        (compare (str l) (str r))))

;;#Useful Vizualizations of the simulation context
;;TODO port this over to the new scenegraph api.
(defn entryvis [x]
  (cond (instance? java.util.Map$Entry x)
        (reify
          Object
          (toString [o] 
            (str (key x) " :: " (or (and (val x) (.getName (type (val x))))
                                    "nil"))
            )
          clojure.lang.Seqable
          (seq [o]
            (when (val x)
              (if (inspect/atom? (val x))
                (list (val x))
                (entryvis (val x))))))
        (instance? java.util.Map x)
         (let [entries (sort-by  key generic-comp (seq x))]
           (map entryvis entries))
        (inspect/atom? x) x
        :else (map entryvis (seq x))
        ))
  
;; (defn mapvis [^java.util.Map m]
;;   (reify
;;     Object
;;     (toString [o] (str "/"))
;;     clojure.lang.Seqable
;;     (seq [x] (map entryvis (seq m)))))
    
;; ;;creates a treemodel, but 
;; (defn map-model [data]
;;   (proxy [javax.swing.tree.TreeModel] []
;;     (getRoot [] (mapvis data))
;;     (addTreeModelListener [treeModelListener])
;;     (getChild [parent index]
;;       (let [res      (inspect/get-child parent index)]
;;         (case (inspect/collection-tag res)
;;           :entry (entryvis res)
;;           res)))
;;     (getChildCount [parent]
;;       (inspect/get-child-count parent))
;;     (isLeaf [node]
;;       (if (instance? clojure.lang.IDeref node)
;;         (inspect/is-leaf node)
;;         (inspect/is-leaf node)))
;;     (valueForPathChanged [path newValue])
;;     (getIndexOfChild [parent child] -1)
;;     (removeTreeModelListener [treeModelListener])))

;; (defn inspect-tree [data]
;;   (doto (javax.swing.JFrame. "Clojure Inspector")
;;     (.add (javax.swing.JScrollPane. (javax.swing.JTree. (entryvis data)
;;                                                         )))
;;     (.setSize 400 600)
;;     (.setVisible true)))

;;we'd like to make the clojure inspector a bit more useful...
;;specifically, if we're looking at nested maps, it'b nice to
;;use the map's keys as values instead of the entire string
;;reprsentation
(defn tree-view [obj] (inspect/inspect-tree (entryvis obj)))
