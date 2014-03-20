;;A set of idiomatic wrappers for mutable datastructures from
;;java.util.  These collections are treated like transients, allowing
;;for conj!. 
;; Work in progress.
(ns spork.data.mutable  
  (:require [spork.protocols [core :as generic]])
  (:import  [java.util ArrayList PriorityQueue ArrayDeque HashMap]))

;;Some notes on performance....
;;We can mimic field access by creating an interface that correponds
;;to the type.  The only difference is that the interface methods
;;cannot contain '-'.....so what's a good way around this? 

;;We have a mechanism to generate random interfaces for our 
;;types...specifically our mutable types...and from there 
;;we get native access speeds through the interface.

;;What we really want is a unified way to represent field access...
;;in clojure, this is typically .-the-field 
;;Well, we could have a macro called get-field....
;;that takes a type, sees if it's mutable, and exposes public fields.
;;Another option is to wrap the mutable in an object that has public
;;fields.  Then you get 2 allocations per object though...ugh...

;;There's always the good old with-slots form...
;;(with-slots [obj] [slot1 slot2 slot3] ...)
;;If a tag is provided, we can see if the type is a mutable.
;;If it is, we can replace the slot names with appropriate 
;;mutable alternatives at compile time.

(declare mutable? slot->field slot->accessor)

;;(defn slot->field [slot]   (symbol (str \. slot)))
;;given a form the-slot-at-blah, we need to turn it into camelcase
;;theSlotAtBlah
;; (defn slot->accessor [slot] 
;;   (let [xs (clojure.string/split (str slot) #"-")]
;;     (->> (map clojure.string/capitalize (rest xs))
;;          (into [(first xs)])
;;          (clojure.string/join)
;;          (symbol))))

;; (defn slot-binder [f] 
;;   (fn [slot] [slot (f slot)]))

;;This is a throwback to CommonLisp :)
;; (defmacro with-fields [obj slots & expr]
;;   (let [the-hint (:tag (meta obj))]
;;     `(let [~@(flatten (map (slot-binder (if (mutable? the-hint) 
;;                                           slot->slot-accessor slot->field))
;;                            slots))]
;;        ~@expr)))      

;; (let [hyphen #"-"]
;;   (defn invalid-field? [field]
;;     (re-find hyphen (str field))))

(defn ^ArrayList make-array-list [] (ArrayList.))
(defn ^ArrayList array-list [xs] 
  (reduce (fn [^ArrayList acc x] (doto acc (.add x))) (make-array-list) xs))

(defmacro hget [hint coll k]
  `(.get ~(with-meta coll {:tag hint})  ~k))
(defmacro hput [hint coll k v]
  `(doto ~(with-meta coll {:tag hint})  (.put ~k ~v)))
(defmacro hadd [hint coll v]
  `(doto ~(with-meta coll {:tag hint}) (.add ~v)))

(defprotocol IFastAccess
  (fast-get [coll k])
  (fast-put [coll k v])
  (fast-add [coll v]))

(extend-protocol IFastAccess 
  ArrayList 
  (fast-get [coll k]   (hget ArrayList coll k))
  (fast-put [coll k v] (hadd ArrayList coll [k v]))
  (fast-add [coll v]   (hadd ArrayList coll v))
  HashMap 
  (fast-get [coll k]   (hget HashMap coll k))
  (fast-put [coll k v] (hput HashMap coll k v))
  (fast-add [coll v]   (hput HashMap coll (first v) (second v))))

(definline get-arraylist [l n]
  `(.get ~(with-meta l {:tag 'java.util.ArrayList}) ~n))
(definline push-arraylist [l x]
  `(doto  ~(with-meta l {:tag 'java.util.ArrayList}) (.add ~x)))

(definline jassoc [m k v]
  `(doto ~(with-meta m {:tag 'java.util.HashMap})
     (.put ~k ~v)))

(defn ^ArrayDeque make-queue [] (ArrayDeque.))
(defn ^ArrayDeque queue [xs] 
  (reduce (fn [^ArrayDeque acc x] (doto acc (.add x))) (make-queue) xs))

(definline mutable-list? [coll]  `(= (type ~coll) java.util.ArrayList))

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

;;Tag protocol
(defprotocol IMutableContainer)


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
         spork.data.mutable.IMutableContainer
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

(deftype mutlist [^java.util.ArrayList m] 
  clojure.lang.ITransientVector  
  (assocN [this i val] (do (.add m i val) this))
  (pop    [this]       (do (.remove m (unchecked-dec (.size m))) this))
  (nth    [this k]     (.get m k))
  (nth    [this k not-found] (or (.get m k) not-found))
  (assoc  [this k v]   (do  (.add m k v) this))
  (conj    [this v]    (do (.add m v) this))
;  (without [this k]    (do (.remove m k) this))
  (persistent [this]   (into [] m))
  clojure.lang.IDeref
  (deref [this] m))

(deftype mutmap [^java.util.HashMap m] 
  clojure.lang.ITransientMap  
  (valAt [this k] (.get m k))
  (valAt [this k not-found] (or (.get m k) not-found))
  (assoc [this k v] (do  (.put m k v) this))
  (conj  [this e]    
    (let [[k v] e]      
      (do (.put m k v) this)))
  (without [this k]   (do (.remove m k) this))
  (persistent [this] (into {} (seq m)))
  clojure.lang.IDeref
  (deref [this] m))
(defn ^mutmap ->mutmap [& xs]  
  (reduce (fn [m [k v]] (assoc! m k v)) (mutmap. (HashMap.)) xs))

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

