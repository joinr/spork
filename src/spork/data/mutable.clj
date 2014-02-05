;;A set of idiomatic wrappers for mutable datastructures from
;;java.util.  These collections are treated like transients, allowing
;;for conj!. 
;; Work in progress.
(ns spork.data.mutable  
  (:require [spork.protocols [core :as generic]])
  (:import  [java.util ArrayList PriorityQueue ArrayDeque]))

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


(defn flatten-bindings [xs]
  (reduce (fn [acc [k v]] 
            (-> acc (conj k) (conj v))) [] xs))

(defn field-setter [fld hints v]
  (let [prims     (set '[long int boolean double char float object])       
        hint      (get hints fld)
        f         (symbol (str fld))]
       (cond (and hint (contains? prims hint))
             `(set! ~f (~hint ~v))
             hint 
             `(set! ~f ~(with-meta v {:tag hint}))
             :else
             `(set! ~f ~v))))

;;Defines a mutable container.
(defmacro defmutable [name fields & specs]
  (let [flds        (mapv (fn [sym] (vary-meta (symbol sym) merge {:unsynchronized-mutable true})) fields)
        fld-hints   (zipmap flds (map (comp :tag meta) flds))
        keyfields   (mapv keyword fields)
        field-symbs (map (fn [sym] (vary-meta sym dissoc :tag)) fields)
        the-value   (gensym "the-value")
        setters (flatten-bindings (map (fn [s] [(keyword s) (field-setter s fld-hints the-value)]) flds))
        getters (flatten-bindings (map (fn [s] [(keyword s) s]) field-symbs))
        fieldmap    (zipmap keyfields field-symbs)]
   `(deftype ~name ~flds 
      ~@specs
      clojure.lang.ITransientMap  
      (~'valAt [this# k#] 
        (case k# 
          ~@getters
          (throw (Error. (str "Invalid field: " k#)))))
      (~'valAt [this# k# not-found#] 
        (case k# 
          ~@getters
          (throw (Error. (str "Invalid field: " k#)))))
      (~'assoc [this# k# ~the-value]
        (do (case k#
              ~@setters
              (throw (Error. (str "Invalid field: " k#))))                                      
          this#))  
      (~'conj [this# e#]    
        (let [[k# v#] e#]      
          (.assoc this# k# v#)))  
      (~'without [this# k#]    
          (throw (Error. (str "Cannot dissoc from a mutable container: " k#))))      
      (~'persistent [this#]    
        ~fieldmap)
      clojure.lang.IDeref
      (~'deref [this#] ~fieldmap))))

;; (defmacro defarralias [name hint fields]
;;   (let [flds        (mapv (fn [sym] (vary-meta (symbol sym) merge {:tag hint})) fields)
;;         fld-hints   (zipmap flds (map (comp :tag meta) flds))
;;         hints       '{long longs int ints double doubles float floats char object}
;;         keyfields   (mapv keyword fields)
;;         field-symbs (map (fn [sym] (vary-meta sym dissoc :tag)) fields)
;;         the-value   (gensym "the-value")
;;         setters     (flatten-bindings (map-indexed (fn [i s] [(list (keyword s) i) (field-setter s fld-hints the-value)]) flds))
;;         getters     (flatten-bindings (map-indexed (fn [i s] [(list (keyword s) i) s]) field-symbs))
;;         arrsymb     (with-meta 'backing-array {:tag hint})
;;         ctor        (symbol (str "->" name))
;;         fieldmap    (zipmap keyfields field-symbs)]
;;    `(do
;;       (declare ~ctor)
;;       (deftype ~name [~arrsymb]
;;         ~@specs
;;         clojure.lang.ITransientMap  
;;         (~'valAt [this# k#] 
;;           (case k# 
;;             ~@getters
;;             (throw (Error. (str "Invalid field: " k#)))))
;;         (~'valAt [this# k# not-found#] 
;;           (case k# 
;;             ~@getters
;;             (throw (Error. (str "Invalid field: " k#)))))
;;         (~'assoc [this# k# ~the-value]
;;           (do (case k#
;;                 ~@setters
;;                 (throw (Error. (str "Invalid field: " k#))))                                      
;;               this#))  
;;         (~'conj [this# e#]    
;;           (let [[k# v#] e#]      
;;             (.assoc this# k# v#)))  
;;         (~'without [this# k#]    
;;           (throw (Error. (str "Cannot dissoc from a mutable container: " k#))))
;;         (~'persistent [this#] ~fieldmap)
;;         clojure.lang.IDeref
;;         (~'deref [this#] ~arrsymb))
;;       (defn ~(with-meta ctr {:tag name}) 
;;         [~@(map (fn [s] (with-meta  
      


  ;; ArrayDeque
  ;; (conj [o v] (add-q o v))
  ;; (persistent [o] (into [] (iterator-seq o)))
  ;; PriorityQueue
  ;; (conj [o v] (add-pq o v))
  ;; (persistent [o] (into [] (map first (iterator-seq o))))

