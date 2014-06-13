;;A set of associative containers - alternatives to 
;;built-in HAMT-based stuff like persistent map.
;;These all have speed issues compared to persistent map.
;;Dammit.

(ns spork.data.associative
  (:require [clojure.core [reducers :as r]])
  (:import [clojure.lang MapEntry]))

(defn tag! [sym type]
  (vary-meta sym assoc :tag type))


(defmacro lil
  [entries k &  {:keys [get eq? entry-type get-bound map-type
                        entry-key entry-val] 
                 :or {get '.nth 
                      eq? 'identical? 
                      entry-type 'clojure.lang.MapEntry
                      get-bound '.count
                      entry-key '.key 
                      entry-val '.val}}]
  (let [entries (if map-type (tag! entries map-type) entries)
        entry   (tag! (gensym "entry") entry-type)]    
    `(let [bound# (~get-bound ~entries)]
       (loop [idx# 0]
         (if (== idx# bound#) nil
             (let [~entry (~get ~entries idx#)]
               (if  (~eq? (~entry-key ~entry) ~k) ~entry
                    (recur (unchecked-inc idx#)))))))))

(defmacro lif
  [entries k &  {:keys [get eq? entry-type get-bound map-type
                        entry-key entry-val] 
                 :or {get '.nth 
                      eq? 'identical?
                      entry-type 'clojure.lang.MapEntry
                      get-bound '.count
                      entry-key '.key 
                      entry-val '.val}}]
  (let [entries (if map-type (tag! entries map-type) entries)
        entry   (tag! (gensym "entry") entry-type)]    
    `(let [bound# (~get-bound ~entries)]
       (loop [idx# 0]
         (if (== idx# bound#) nil
             (let [~entry (~get ~entries idx#)]
               (if  (~eq? (~entry-key ~entry) ~k) idx#
                    (recur (unchecked-inc idx#)))))))))

(defmacro ld
  [entries k &  {:keys [eq? entry-type  map-type entry-key target-container] 
                 :or {eq? 'identical? 
                      entry-type 'clojure.lang.MapEntry
                      entry-key '.key}}]
  (let [entries (if map-type (tag! entries map-type) entries)
        entry   (tag! (gensym "entry") entry-type)]    
    `(into ~target-container 
           (r/filter (fn [~entry]
                       (if (~eq? (~entry-key ~entry) ~k)
                         false 
                         true))
                     ~entries))))

;;This is waaaay faster...
(defn linear-indexed-lookup 
  ([^clojure.lang.PersistentVector entries k eq?]
     (lil entries k :eq? eq? :map-type 'clojure.lang.PersistentVector))

  ([entries k] (linear-indexed-lookup entries k identical?)))

(defn linear-indexed-find 
  ([^clojure.lang.PersistentVector entries k eq?]
     (lif entries k :eq? eq? :map-type 'clojure.lang.PersistentVector))                   
  ([entries k] (linear-indexed-find entries k identical?)))



;;THis is o(n)....inefficient.
;;Another option here is to maintain a set of dropped slots...
(defn linear-indexed-drop
  ([^clojure.lang.PersistentVector entries k eq?]
     (ld entries k :eq? eq? 
                   :map-type 'clojure.lang.PersistentVector
                   :target-container []))                        
  ([entries k] (linear-indexed-drop entries k identical?)))

(defmacro ll
  [entries k &  {:keys [get eq? entry-type  map-type
                        entry-key entry-val] 
                 :or {get '.nth 
                      eq? '= 
                      map-type 'clojure.lang.ISeq
                      entry-type 'clojure.lang.MapEntry
                      entry-key '.key 
                      entry-val '.val}}]
  (let [entries (if map-type (tag! entries map-type) entries)
        entry   (tag! (gensym "entry") entry-type)]    
    `(loop [xs# ~entries]
       (if (empty? xs#) nil
           (let [~entry (.first xs#)]
             (if  (~eq?  (~entry-key ~entry) ~k) ~entry
                  (recur (.next xs#))))))))

(defmacro lf
  [entries k &  {:keys [get eq? entry-type  map-type
                        entry-key entry-val] 
                 :or {get '.nth 
                      eq? '= 
                      map-type 'clojure.lang.ISeq
                      entry-type 'clojure.lang.MapEntry
                      entry-key '.key 
                      entry-val '.val}}]
  (let [entries (if map-type (tag! entries map-type) entries)
        entry   (tag! (gensym "entry") entry-type)]    
    `(loop [xs# ~entries
            idx# 0]
       (if (empty? xs#) nil
           (let [~entry (.first xs#)]
             (if  (~eq?  (~entry-key ~entry) ~k) idx#
                  (recur (.next xs#) (unchecked-inc idx#))))))))


;;This is waaaay faster...
(defn linear-lookup 
  ([entries k eq?]  (ll entries k :eq? eq?))
  ([entries k] (linear-lookup entries k identical?)))


(defn linear-find 
  ([ entries k eq?]   (lf entries k :eq eq?))
  ([entries k] (linear-find entries k identical?)))

;;THis is o(n)....inefficient.
;;Another option here is to maintain a set of dropped slots...
(defn linear-drop
  ([^clojure.lang.ISeq entries k eq?]
     (ld entries k :eq? eq? :target-container '()))
  ([entries k] (linear-drop entries k identical?)))

              
;;One easy way to do this is to just use chunked Persistent Array Maps 
;;under the hood.  Another way to do it is to build traditional alists 
;;and amaps.
(deftype avec [^clojure.lang.PersistentVector entries]
  clojure.core.protocols.IKVReduce
  (kv-reduce [amap f init] (let [bound (.count entries)]
                             (loop [idx 0
                                    acc init]
                               (if (or (== idx bound) (reduced? acc)) acc
                                   (recur (unchecked-inc idx)
                                          (let [^MapEntry e (.nth entries idx)]
                                            (f acc (.key e) (.val e)))))))) 
  clojure.core.protocols.CollReduce
  (coll-reduce [coll f] (clojure.core.protocols/coll-reduce entries f))
  (coll-reduce [coll f val] (clojure.core.protocols/coll-reduce entries f val))
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (linear-indexed-lookup entries k))
  (valAt [this k not-found] (if-let [res (linear-indexed-lookup entries k)]
                              res 
                              not-found))
  clojure.lang.IPersistentMap
  (count [this] (.count entries))
  (assoc [this k v] (if (zero? (.count entries))
                      (avec. [(MapEntry. k v)])
                      (if-let [^long idx (linear-indexed-lookup entries k)]
                        (avec. (.assocN entries idx (MapEntry. k v)))
                        (avec. (conj entries (MapEntry. k v))))))
  (empty [this] (avec. []))
  ;cons defines conj behavior
  (cons [this e]   (.assoc this (first e) (second e)))
  (equiv [this o]  (.equiv entries o))  
  (hashCode [this] (.hashCode entries))
  (equals [this o] (or (identical? this o) (.equals entries o)))  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k]  (if (nil? (linear-indexed-find entries k)) false true))
  (entryAt [this k] (linear-indexed-lookup entries k))
  (seq [this] (.seq entries))
  ;without implements (dissoc pm k) behavior
  (without [this k] 
    (avec. (linear-indexed-drop entries k)))
  clojure.lang.Indexed
  (nth [this i] (.nth entries i))
  (nth [this i not-found]  (.nth entries i not-found))
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq entries)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] (meta entries))
  (withMeta [this m] (avec. (.withMeta entries m)))
      
  clojure.lang.Reversible
  (rseq [this] (.rseq entries))   

  java.io.Serializable ;Serialization comes for free with the other things implemented
  clojure.lang.MapEquivalence
  
  java.util.Map ;Makes this compatible with java's map
  (size [this] (.count entries))
  (isEmpty [this] (.isEmpty entries))
  (containsValue [this v] (reduce (fn [acc ^MapEntry kv]
                                    (if (= v (.val kv))
                                      (reduced true)
                                      acc)) nil entries))
  (get [this k] (.valAt this k))
  (put [this k v] (.assoc this k v))
  (remove [this k] (.without this k))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (into #{} (map key entries))) ;;modify
  (values [this] (map val entries))
  (entrySet [this] (set entries)))

(deftype alist [^clojure.lang.ISeq entries]
  clojure.core.protocols.IKVReduce
  (kv-reduce [amap f init] (reduce (fn [acc ^MapEntry e] (f acc (.key e) (.val e))) init entries ))
  clojure.core.protocols.CollReduce
  (coll-reduce [coll f] (clojure.core.protocols/coll-reduce entries f))
  (coll-reduce [coll f val] (clojure.core.protocols/coll-reduce entries f val))
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (linear-lookup entries k))
  (valAt [this k not-found] (if-let [res (linear-lookup entries k)]
                              res 
                              not-found))
  clojure.lang.IPersistentMap
  (count [this] (count entries)) ;cache this...
  (assoc [this k v] (if (empty? entries) 
                      (avec. [(MapEntry. k v)])
                      (if-let [idx (linear-lookup entries k)]
                        (avec. (.cons entries (MapEntry. k v)))
                        (avec. (.cons entries (MapEntry. k v))))))
  (empty [this] (alist. '()))
  ;cons defines conj behavior
  (cons [this e]   (.assoc this (first e) (second e)))
  (equiv [this o]  (.equiv entries o))  
  (hashCode [this] (.hashCode entries))
  (equals [this o] (or (identical? this o) (.equals entries o)))  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k]  (if (nil? (linear-find entries k)) false true))
  (entryAt [this k] (linear-lookup entries k))
  (seq [this]  entries)
  ;without implements (dissoc pm k) behavior
  (without [this k] 
    (avec. (linear-drop entries k)))
  clojure.lang.Indexed
  (nth [this i] (nth entries i))
  (nth [this i not-found]  (nth entries i not-found))
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq entries)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] (meta entries))
  (withMeta [this m] (alist. (with-meta entries m)))
      
  clojure.lang.Reversible
  (rseq [this] (reverse entries))   

  java.io.Serializable ;Serialization comes for free with the other things implemented
  clojure.lang.MapEquivalence
  
  java.util.Map ;Makes this compatible with java's map
  (size [this] (.count entries))
  (isEmpty [this] (empty? entries))
  (containsValue [this v] (reduce (fn [acc ^MapEntry kv]
                                    (if (= v (.val kv))
                                      (reduced true)
                                      acc)) nil entries))
  (get [this k] (.valAt this k))
  (put [this k v] (.assoc this k v))
  (remove [this k] (.without this k))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (into #{} (map key entries))) ;;modify
  (values [this] (map val entries))
  (entrySet [this] (set entries)))



 
;;This is waaaay faster...
(defn linear-indexed-lookup! 
  ([^java.util.ArrayList entries k eq?]
     (lil entries k :get .get 
                    :eq?  eq?
                    :map-type 'java.util.ArrayList
                    :get-bound .size))
  ([entries k] (linear-indexed-lookup! entries k identical?)))

(defn linear-indexed-find! 
  ([^java.util.ArrayList entries k eq?]
     (lif entries k :get .get 
                    :eq?  eq?
                    :map-type 'java.util.ArrayList
                    :get-bound .size))
  ([entries k] (linear-indexed-find! entries k identical?)))

;;THis is o(n)....inefficient.
;;Another option here is to maintain a set of dropped slots...
(defn linear-indexed-drop!
  ([^java.util.ArrayList entries k eq?]
     (if-let [idx (linear-indexed-find! entries k eq?)]
       (doto entries (.remove idx))))
  ([entries k] (linear-indexed-drop! entries k =)))

(deftype mutable-alist [^java.util.ArrayList entries _meta]
  clojure.core.protocols.IKVReduce
  (kv-reduce [amap f init] (let [bound (.size entries)]
                             (loop [idx 0
                                    acc init]
                               (if (or (== idx bound) (reduced? acc)) acc
                                   (recur (unchecked-inc idx)
                                          (let [^MapEntry e (.get entries idx)]
                                            (f acc (.key e) (.val e))))))))                                         
  clojure.core.protocols.CollReduce
  (coll-reduce [coll f] (clojure.core.protocols/coll-reduce entries f))
  (coll-reduce [coll f val] (clojure.core.protocols/coll-reduce entries f val))
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (lil entries k :get .get 
                    :map-type 'java.util.ArrayList
                    :get-bound .size))
  (valAt [this k not-found] (if-let [res (lil entries k :get .get 
                    :map-type 'java.util.ArrayList
                    :get-bound .size)]
                              res 
                              not-found))
  clojure.lang.IPersistentMap
  (count [this] (.size entries)) ;cache this...
  (assoc [this k v] (if (zero? (.size entries)) 
                      (mutable-alist. (doto (java.util.ArrayList. ) (.add (MapEntry. k v))) {})
                      (if-let [idx (linear-indexed-lookup! entries k)]
                        (mutable-alist. (doto entries (.set idx (MapEntry. k v))) _meta)
                        (mutable-alist. (doto entries (.add (MapEntry. k v))) _meta))))
  (empty [this] (mutable-alist. (java.util.ArrayList.) {}))
  ;cons defines conj behavior
  (cons [this e]   (.assoc this (first e) (second e)))
  (equiv [this o]  (.equals entries o))  
  (hashCode [this] (.hashCode entries))
  (equals [this o] (or (identical? this o) (.equals entries o)))  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k]  (if (nil? (linear-indexed-find! entries k)) false true))
  (entryAt [this k] (linear-indexed-lookup! entries k))
  (seq [this]  (seq entries))
  ;without implements (dissoc pm k) behavior
  (without [this k] 
    (mutable-alist. (linear-indexed-drop! entries k) _meta))
  clojure.lang.Indexed
  (nth [this i] (.get entries i))
  (nth [this i not-found]  (if-let [res (.get entries i)] res not-found))
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq entries)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (mutable-alist. entries m))
      
  clojure.lang.Reversible
  (rseq [this] (reverse entries))   

  java.io.Serializable ;Serialization comes for free with the other things implemented
  clojure.lang.MapEquivalence
  
  java.util.Map ;Makes this compatible with java's map
  (size [this] (.size entries))
  (isEmpty [this] (.isEmpty entries))
  (containsValue [this v] (reduce (fn [acc ^MapEntry kv]
                                    (if (= v (.val kv))
                                      (reduced true)
                                      acc)) nil entries))
  (get [this k] (.valAt this k))
  (put [this k v] (.assoc this k v))
  (remove [this k] (.without this k))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (into #{} (map key entries))) ;;modify
  (values [this] (map val entries))
  (entrySet [this] (set entries)))


(defn array-linear-lookup [^objects arr k]
  (lil arr k :get aget :map-type objects :get-bound alength))

(defn array-linear-lookup! [^objects arr k]
  (let [bound (alength arr)]
    (loop [idx 0]
      (if (== idx bound) nil
          (let [^MapEntry e (aget arr idx)]
            (if (identical? (.key e) k) e
                (recur (unchecked-inc idx))))))))

;; (deftype mutable-alist-simple [^objects entries ^long bound _meta]
;;   clojure.core.protocols.IKVReduce
;;   (kv-reduce [amap f init] 
;;     (loop [idx 0
;;            acc init]
;;       (if (or (== idx bound) (reduced? acc)) acc
;;           (recur (unchecked-inc idx)
;;                  (let [^MapEntry e (.get entries idx)]
;;                    (f acc (.key e) (.val e)))))))
;;   clojure.core.protocols.CollReduce
;;   (coll-reduce [coll f]     (clojure.core.protocols/coll-reduce entries f))
;;   (coll-reduce [coll f val] (clojure.core.protocols/coll-reduce entries f val))
;;   Object
;;   (toString [this] (str (.seq this)))
;;   clojure.lang.ILookup
;;   ; valAt gives (get pm key) and (get pm key not-found) behavior
;;   (valAt [this k] (lil entries k :get aget
;;                     :map-type objects
;;                     :get-bound (fn [x] bound)))
;;   (valAt [this k not-found] (if-let [res (lil entries k :get aget 
;;                                               :map-type objects
;;                                               :get-bound (fn [x] bound))]
;;                               res 
;;                               not-found))
;;   clojure.lang.IPersistentMap
;;   (count [this] (.size entries)) ;cache this...
;;   (assoc [this k v] (case bound
;;                       0 (let [os (object-array 100)] 
;;                           (mutable-alist. (aset os 0 (MapEntry. k v)) 1 {}))
;;                       100 (throw (Exception. "Exceeded the size of the map!"))
;;                       (if-let [idx (lil entries k :get aget
;;                                         :map-type objects
;;                                         :get-bound (fn [x] bound))]
;;                         (do (aset entries idx (MapEntry. k v))
;;                             this)
;;                         (do (aset entries n (MapEntry. k v))
;;                             (mutable-alist.  entries (unchecked-inc bound)  _meta)))))
;;   (empty [this] (mutable-alist. nil 0 {}))
;;   ;cons defines conj behavior
;;   (cons [this e]   (.assoc this (first e) (second e)))
;;   (equiv [this o]  (.equals entries o))  
;;   (hashCode [this] (.hashCode entries))
;;   (equals [this o] (or (identical? this o) (.equals entries o)))  
;;   ;containsKey implements (contains? pm k) behavior
;;   (containsKey [this k]  (if (nil? (lif entries k :get aget
;;                                         :map-type objects
;;                                         :get-bound (fn [x] bound))) false true))
;;   (entryAt [this k] (lil entries k :get aget
;;                          :map-type objects
;;                          :get-bound (fn [x] bound)))
;;   (seq [this]  (seq entries))
;;   ;without implements (dissoc pm k) behavior
;;   (without [this k] 
;;     (if-let [idx (lif entries k 
;;                       :get aget
;;                       :map-type objects
;;                       :get-bound (fn [x] bound))]
;;     (mutable-alist. (linear-indexed-drop! entries k) _meta))
;;   clojure.lang.Indexed
;;   (nth [this i] (aget entries i))
;;   (nth [this i not-found]  (if-let [res (aget entries i)] res not-found))
;;   Iterable
;;   (iterator [this] (clojure.lang.SeqIterator. (seq entries)))
      
;;   clojure.lang.IFn
;;   ;makes lex map usable as a function
;;   (invoke [this k] (.valAt this k))
;;   (invoke [this k not-found] (.valAt this k not-found))
  
;;   clojure.lang.IObj
;;   ;adds metadata support
;;   (meta [this] _meta)
;;   (withMeta [this m] (mutable-alist. entries bound m))
      
;;   clojure.lang.Reversible
;;   (rseq [this] (reverse entries))   

;;   java.io.Serializable ;Serialization comes for free with the other things implemented
;;   clojure.lang.MapEquivalence
  
;;   java.util.Map ;Makes this compatible with java's map
;;   (size [this]  bound)
;;   (isEmpty [this] (zero? bound))
;;   (containsValue [this v] (reduce (fn [acc ^MapEntry kv]
;;                                     (if (= v (.val kv))
;;                                       (reduced true)
;;                                       acc)) nil (seq entries)))
;;   (get [this k] (.valAt this k))
;;   (put [this k v] (.assoc this k v))
;;   (remove [this k] (.without this k))
;;   (putAll [this m] (throw (UnsupportedOperationException.)))
;;   (clear [this] (throw (UnsupportedOperationException.)))
;;   (keySet [this] (into #{} (map key entries))) ;;modify
;;   (values [this] (map val entries))
;;   (entrySet [this] (set entries)))

(defn make-avec [& xs]
  (into (->avec []) xs))

(defn make-alist [& xs]
  (into (->alist []) xs))

(defn mlist [& xs]
  (into (->mutable-alist (java.util.ArrayList.) {}) xs))



(def the-entries (map vector [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z] (iterate inc 0)))
(def the-entries-arr (object-array the-entries))
(def av (apply make-avec the-entries))
(def al (apply make-alist the-entries))
(def am (apply mlist the-entries))
(def m (into {} the-entries))
