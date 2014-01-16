(ns spork.util.inspection
  (:require [clojure [reflect :as reflect]]
            [spork.data [orderedmap :as om]]))

(def ^:dynamic *short-types* true)

(defn members [cls] (:members (reflect/reflect cls)))
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
(defn func-sig [name sig]
  `(~name ~as-type  ~(close sig)))
           
(defn method->funcsig [m]
  (func-sig (:name m)  (conj (:parameter-types m) (:return-type m))))
        
(defn flatten-bases [cls]
  (loop [visited #{cls}
         order [cls]
         bases (:bases (reflect/reflect cls))]
    (if (empty? bases) order
        (let [base (ns-resolve *ns* (first bases))]
          (if (visited base)
            (recur visited order (next bases))           
            (recur (conj visited  base) (conj order  base) (into bases (remove visited (:bases (reflect/reflect base))))))))))

(defn get-spec [cls]
  {:class cls :methods (map method->funcsig (:members (reflect/reflect cls)))})

(defn replace-with
  "Derived from clojure.core/replace"
  [smap coll]
  (if (vector? coll)
    (reduce1 (fn [v i]
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

