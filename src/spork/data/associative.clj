;;A set of associative containers - alternatives to 
;;built-in HAMT-based stuff like persistent map.
;;These all have speed issues compared to persistent map.
;;Dammit.


;;Update Feb 2017 - EXPERIMENTAL / FAILED
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

;;lookup the item associated with the nested vectors in xs.
;; (definline row-col [row col xs]
;;   (let [xs (with-meta xs {:tag 'clojure.lang.PersistentVector})
;;         c  (with-meta (gensym "c") {:tag 'clojure.lang.PersistentVector})]
;;     `(let [~c (.nth ~xs ~col)]
;;        (.nth ~c ~row))))

(defn row-col [row col ^clojure.lang.IPersistentVector cols]
  (.nth ^clojure.lang.Indexed (.nth cols col) row))
(defn assoc-row-col [^clojure.lang.IPersistentVector cols row col v]
  (let [^clojure.lang.IPersistentVector c (.nth cols col)]
    (.assocN cols col (.assocN c row v))))

(defn index-of [itm xs]
  (reduce-kv (fn [acc idx x]
               (if (= x itm)
                 (reduced idx)
                 acc)) nil xs))
(defn drop-indices [idxs v]
  (reduce-kv (fn [acc idx x]
               (if (contains? idxs idx)
                 acc
                 (conj acc x)))
             [] v))

(defn sparse-seq
  ([idx rows length]
   (when (< idx length)
     (if-let [l (first rows)]
       (if (< idx (first l))
         (concat (repeat (- (first l) idx) nil)
                 (sparse-seq
                  (first l) rows length))
         (let [r (or (second rows) [length nil])
               [lidx lval] l
               [ridx rval] r
               len  (dec (- ridx lidx))]
           (cons lval (concat (repeat len nil)
                              (sparse-seq
                               ridx (rest rows) length)))))
       (if (== idx (dec length)) nil
           (repeat (- length idx) nil)))))
  ([rows length] (sparse-seq 0 rows length)))

;;__Sparse Hashing__
;;So...we'd like our sparse columns to be considered
;;structurally equal to normal vectors.  They just
;;take far fewer entries.
;;The hashing scheme clojure uses, with some tweaks, is this:
(defn simple-hash [xs]
  (reduce (fn [acc x]
            (unchecked-add-int
             (unchecked-multiply-int acc 31)
             (hash x)))
          1
          xs))
;;After which we pass it to another function for better hashing,
;;called mix-collection-hash.

;;Some ground rules: nil, like 0, hashes to 0.
;;So...if we have a vector of 10 nils, we get
;;(simple-hash (vec (repeat 10 nil))) => 31^10, although
;;with intentional integer overflow, we end up with
;;something negative....
;;-1796951359
;;(= (simple-hash (vec (repeat 10 nil))) (int-pow 31 10))
(defn int-pow [base exp]
  (cond (zero? exp) 1
        (neg? exp) (throw (Exception. "no negative powers at the moment"))
        :else
        (loop [i 1
               acc base]
          (if (== i exp) acc
              (recur (unchecked-inc i)
                     (unchecked-multiply-int acc base))))))
;;Since we're working with multiplication and addition, we
;;can use int-pow to compute the factor for our zeroes
;;in one step.

;;__Data Structures__
;;Simulates a vector with one or more rows.  Backed by a hashmap.
;;No equality checks at the moment, so we can't use it for hashkeys.
;;A sparse vector is no different than a vector, so we should be
;;able to simulate hash-codes easily enough.
;;We know the formula for hashing... multiply the hash of the value
;;by 31 and sum.
;;If every other value is nil, then the contribution is....
(deftype sparserows [^clojure.lang.IPersistentMap rows length]
  clojure.lang.ILookup
  ;;gives us (Get pm key) ... behavior
  (valAt [this k]
    (if-let [v (.valAt rows k)]
      v
      (cond (and (>= k 0) (< k length)) nil
            :else (throw (Exception.
                          (str "Index " k " out of bounds for sparserows"))))))
  (valAt [this k not-found]
    (if-let [v (.valAt rows k)]
      v
      (cond (and (>= k 0) (< k length)) nil
            :else not-found)))
  clojure.lang.IPersistentVector
  (count [this] length)
  (assocN [this k v]
    (if (>= k 0)
      (sparserows. (assoc rows k v) (max (unchecked-inc k) length))
      (throw (Exception. (str "cannot assoc a negative index in sparserows! " k)))))
  (empty [this] (sparserows. {} 0))
  ;;cons defines conj behavior
  (cons [this e] (.assocN this e length))
  (equiv [this o] (throw (Exception.
                          (str "Equiv not implemented for sparserows!"))))
  (hashCode [this] (* (hash rows) length))
  (equals [this o] (identical? this o))
  ;;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (and (pos? k) (< k length)))
  (entryAt [this k]
    (if-let [e (.entryAt rows k)] e
            (if (< k length) nil
                (throw (Exception. (str "Entry " k " out of bounds in sparserows!"))))))
  (seq [this] (sparse-seq 0 rows length))
  ;;without implements (dissoc pm k) behavior
  ;; (without [this k]
  ;;   (if (< k length)
  ;;     (sparserows. (dissoc rows k) (max (unchecked-inc k)
  ;;                                       (unchecked-dec length)))))
  clojure.lang.Indexed
  (nth [this i] (.valAt this i))
  (nth [this i not-found] (.valAt this i not-found))
  )


;;Simulates a persistent vector.  Only stores one value,
;;however will act like a persistent vector in that
;;calls will return nil for values within the range
;;defined by length.
;;Since we know the count, and the number of nils, we
;;can hash this as if it were a bunch of entries.
(deftype sparsecolumn [row val length]
  clojure.lang.ILookup
  (valAt [this k]
    (cond (== k row) val
          (and (pos? k) (< k length)) nil
          :else (throw (Exception.
                        (str "Index " k " out of bounds for sparserows")))))
  (valAt [this k not-found]
    (cond (== k row) val
          (and (pos? k) (< k length)) nil
          :else not-found))
  clojure.lang.IPersistentVector
  (count [this] length)
  (assocN [this k v]
    (cond (== k row) (sparsecolumn. row v length)
          (not (neg? k)) (sparserows. {row k} length) ;convert to sparserows
               :else (throw (Exception. (str "Cannot assoc a negative index in sparserows! " k)))))
  (empty [this] (sparsecolumn. 0 nil 0))
  ;cons defines conj behavior
  (cons [this e] (.assoc this e))
  (equiv [this o] (throw (Exception.
                          (str "Equiv not implemented for sparsecolumn!"))))
  (hashCode [this] (* (hash val) row length))
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (and (pos? k) (< k length)))
  (entryAt [this k]
    (cond (== k row) (clojure.lang.MapEntry. row val)
          (< k length) nil
          :else (throw (Exception. (str "Entry " k " out of bounds in sparsecolumn")))))
  (seq [this] (concat (take row (repeat nil))
                      (cons val (take (dec (- length row))
                                      (repeat nil)))))
  clojure.lang.Indexed
  (nth [this i] (if (== i row) val
                    (.valAt this i)))
  (nth [this i not-found] (if (== i row) val
                              (.valAt this i not-found)))
  )

(defn ->sparsecolumn
  "This simulates a vector with a single entry, surrounded by nil values.
   Length is known, so it's indexed.  Really, we only have a value at index row.
   This is specifically meant to support flyrecords, and 
   to allow assoc and dissoc behaviors on them rather than building maps."
  ([row val length]
   (sparsecolumn. row val length))
  ([rows length] (sparserows. rows length)))

;;Easiest thing to do for efficient assoc/dissoc behavior...
;;is to use the exdisting columns and fields (as we would a table).
;;Assoc => add a sparse column.
(defprotocol ICursor
  (set-cursor [obj n]))
(defn mapeq [l r]
  (reduce-kv (fn [acc k v]
               (if-let [other (get r k)]
                 (if (= other v)
                   acc
                   (reduced nil))
                 (reduced nil)))
             true l))

;;__Flyrecord__
;;a flyrecord can have hashequivalence, based in its fields.


;;We can have a read-only recordset....
;;A subtable, if you will, that shares structure with the parent
;;table.
;;There might be an argument to move out tables to this format in the
;;future.  For now, it's an optimization on the underlying naive
;;vector-based table implementation.

(deftype flyrecord [^:unsynchronized-mutable ^long n
                    ^clojure.lang.PersistentVector fields
                    ^clojure.lang.PersistentVector columns
                    ^:unsynchronized-mutable ^int _hasheq
                    ^:unsynchronized-mutable ^int _hash]
  ICursor
  (set-cursor [obj idx] (do (set! n (long idx))
                            (set! _hasheq (int -1))
                            (set! _hash   (int -1))
                            obj))
  clojure.core.protocols.IKVReduce
  (kv-reduce [amap f init]
    (reduce-kv (fn [acc col fld]
                    (f acc fld (row-col n col columns)))
               init fields))
  clojure.core.protocols.CollReduce
  (coll-reduce [coll f]
    (reduce-kv (fn [acc idx fld]
                 (f acc [fld (row-col n idx columns)]))
               [0 (.nth fields 0)]
               (subvec fields 1)))
  (coll-reduce [coll f val]
    (reduce-kv (fn [acc idx fld]
                 (f acc [fld (row-col n idx columns)]))
               val
               fields))
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (row-col n (index-of k fields)  columns))
  (valAt [this k not-found]
    (if-let [col (index-of k fields)]
      (row-col n col columns)
      not-found))
  clojure.lang.IHashEq
  (hasheq [this]
    (if (== _hasheq (int -1))
      (let [h (hash-unordered-coll (seq this))]
        (do (set! _hasheq (int h))
            h))
      _hasheq))
  clojure.lang.IPersistentMap
  (count [this] (.count fields))
  (assoc [this k v]
    (if-let [idx (index-of k fields)]
      (flyrecord. n fields (assoc-row-col columns n idx v) -1 -1)
      (let [new-column (->sparsecolumn n v (count (first columns)))]
        (flyrecord. n (conj fields k) (conj columns new-column) -1 -1))))
  (empty [this] (flyrecord. 0 [] [] -1 -1))
  ;cons defines conj behavior
  (cons [this e]   (.assoc this (first e) (second e)))
  (equiv [this o]
    (cond (identical? this o) true
          (instance? clojure.lang.IHashEq o) (== (hash this) (hash o))
          (instance? clojure.lang.IPersistentMap o) (and (== (count this) (count o))
                                                         (mapeq this o))
          (or (instance? clojure.lang.Sequential o)
              (instance? java.util.List o))  (clojure.lang.Util/equiv (seq this) (seq o))
              :else nil))  
  (hashCode [this]
    (if (== _hash (int -1))
      (let [h (hash-unordered-coll (seq this))]
        (do (set! _hash (int h))
            h))
      _hash))
  (equals [this o] (identical? this o))  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k]  (reduce (fn [acc k]
                                   (if (= k k) (reduced true)
                                       acc)) nil fields))
  (entryAt [this k]
    (reduce-kv
     (fn [acc idx fld]
       (if (= k fld)
         (let [^clojure.lang.PersistentVector col (.nth columns idx)]
           (reduced (clojure.lang.MapEntry. fld (.nth col n)))
           acc))) nil fields))
  (seq [this]
    (map-indexed (fn [idx fld]
                   (clojure.lang.MapEntry. fld (row-col n idx columns)))
                 fields))
  ;without implements (dissoc pm k) behavior
  (without [this k]
    (if-let [idx (index-of k fields)]
      (flyrecord. n (drop-indices #{idx} fields )
                  (drop-indices   #{idx} columns) -1 -1)))
  clojure.lang.Indexed
  (nth [this i] (row-col n i columns))
  (nth [this i not-found]  (if (<= i (.count fields))
                             (row-col n i columns)
                             not-found))
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (.seq this)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  ;; clojure.lang.IObj
  ;; ;adds metadata support
  ;; (meta [this] (meta entries))
  ;; (withMeta [this m] (avec. (.withMeta entries m)))
      
  clojure.lang.Reversible
  (rseq [this] (reverse (.seq this )))

  java.io.Serializable ;Serialization comes for free with the other things implemented
  ;clojure.lang.MapEquivalence
  )

;;this may be useful in other contexts....for our usecase it's
;;not.
(deftype assocmap [^java.util.HashMap m]
  clojure.lang.IPersistentMap
  (assoc       [obj k v] (do (.put m k v) obj))
  (assocEx     [obj k v] (do (.put m k v) obj))
  (without [obj k] (do (.remove m k) obj))
  java.lang.Iterable
  (iterator [this] (.iterator m))
  clojure.lang.Associative
  (entryAt     [obj k]   (when-let [v (.get m k)]
                           (clojure.lang.MapEntry. k v)))
  (containsKey [obj k]  (.containsKey m k))
  clojure.lang.IPersistentCollection
  (count [obj] (.size m))
  (cons [obj kv]
    (do  (.put m (.nth ^clojure.lang.Indexed kv 0)
               (.nth ^clojure.lang.Indexed  kv 1))
         obj))
  (empty [this] (assocmap. (java.util.HashMap.)))
  (equiv [this that] (clojure.lang.Util/equiv m  that))
  clojure.lang.Seqable
  (seq [this] (seq m))
  clojure.lang.ILookup
  (valAt [obj k] (.get m k))
  (valAt [obj k notfound] (if-let [res (.get m k)] res notfound))
  java.util.Map ;Makes this compatible with java's map
  (size [this] (.size m))
  (isEmpty [this] (.isEmpty m))
  (containsValue [this v] (.containsValue m v))
  (get [this k] (.get m k))
  (put [this k v] (do (.put m k v) this))
  (remove [this k] (do (.remove m k ) this))
  (putAll [this other] (do (.putAll m other) this))
  (clear [this] (do (.clear m) this))
  (keySet [this] (.keySet m)) ;;modify
  (values [this] (.values m))
  (entrySet [this] (.entrySet m)))
(defn ->assocmap [] (assocmap. (java.util.HashMap.)))
