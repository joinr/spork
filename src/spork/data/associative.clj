;;A set of associative containers - alternatives to 
;;built-in HAMT-based stuff like persistent map.
(ns spork.data.associative
  (:require [clojure.core [reducers :as r]])
  (:import [clojure.lang MapEntry]))


(defn naive-lookup [entries k]
  (r/fold (fn ([] nil)
              ([^MapEntry l ^MapEntry r]
                 (do (println [:comparing l r :to k])
                     (cond (= (.key l) k) l
                           (= (.key r) k) r
                           :else nil))))
          (fn [acc ^MapEntry res]
            (if  (= (.key res) k) (reduced res)
                 acc))
          entries))

(defn linear-lookup [entries k]
  (reduce (fn [acc ^MapEntry res]
            (if  (= (.key res) k) (reduced res)
                 acc))
          nil
          entries))

;;This is waaaay faster...
(defn linear-indexed-lookup 
  ([^clojure.lang.PersistentVector entries k eq?]
     (let [bound (.count entries)]
       (loop [idx 0]
         (if (== idx bound) nil
             (let [^MapEntry itm (.nth entries idx)]
               (if  (eq? (.key itm) k) itm
                    (recur (unchecked-inc idx))))))))
  ([entries k] (linear-indexed-lookup entries k =)))

(defn linear-indexed-find 
  ([^clojure.lang.PersistentVector entries k eq?]
     (let [bound (.count entries)]
       (loop [idx 0]
         (if (== idx bound) nil
             (let [^MapEntry itm (.nth entries idx)]
               (if  (eq? (.key itm) k) idx
                    (recur (unchecked-inc idx))))))))
  ([entries k] (linear-indexed-find entries k =)))

;;THis is o(n)....inefficient.
;;Another option here is to maintain a set of dropped slots...
(defn linear-indexed-drop
  ([^clojure.lang.PersistentVector entries k eq?]
     (into [] (r/filter (fn [^MapEntry itm]
                          (if (eq? (.key itm) k)
                            false 
                            true))
                        entries)))                         
  ([entries k] (linear-indexed-drop entries k =)))


;;This is waaaay faster...
(defn linear-lookup 
  ([entries k eq?]
     (loop [^clojure.lang.ISeq xs entries]
       (if (empty? xs) nil
           (let [^MapEntry itm (.first xs)]
             (if  (eq? (.key itm) k) itm
                  (recur (.rest xs)))))))
  ([entries k] (linear-lookup entries k =)))

(defn linear-find 
  ([ entries k eq?]
     (loop [^clojure.lang.ISeq xs entries
            idx  0]
         (if (empty? xs) nil
             (let [^MapEntry itm (.first entries)]
               (if  (eq? (.key itm) k) idx 
                    (recur (.next xs) (unchecked-inc idx)))))))
  ([entries k] (linear-find entries k =)))

;;THis is o(n)....inefficient.
;;Another option here is to maintain a set of dropped slots...
(defn linear-drop
  ([^clojure.lang.ISeq entries k eq?]
     (into '() (r/filter (fn [^MapEntry itm]
                           (if (eq? (.key itm) k)
                             false 
                             true))
                         entries)))                         
  ([entries k] (linear-drop entries k =)))
               
;;One easy way to do this is to just use chunked Persistent Array Maps 
;;under the hood.  Another way to do it is to build traditional alists 
;;and amaps.
(deftype avec [^clojure.lang.PersistentVector entries]
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
  (assoc [this k v] (if (.isEmpty entries) 
                      (avec. [(MapEntry. k v)])
                      (if-let [idx (linear-indexed-lookup entries k)]
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
                        (avec. (.assocN entries idx (MapEntry. k v)))
                        (avec. (conj entries (MapEntry. k v))))))
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
  (withMeta [this m] (alist. (.withMeta entries m)))
      
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

;;This is waaaay faster...
(defn linear-indexed-lookup! 
  ([^java.util.ArrayList entries k eq?]
     (let [bound (.size entries)]
       (loop [idx 0]
         (if (== idx bound) nil
             (let [^MapEntry itm (.get entries idx)]
               (if  (eq? (.key itm) k) itm
                    (recur (unchecked-inc idx))))))))
  ([entries k] (linear-indexed-lookup! entries k =)))

(defn linear-indexed-find! 
  ([^java.util.ArrayList entries k eq?]
     (let [bound (.size entries)]
       (loop [idx 0]
         (if (== idx bound) nil
             (let [^MapEntry itm (.get entries idx)]
               (if  (eq? (.key itm) k) idx
                    (recur (unchecked-inc idx))))))))
  ([entries k] (linear-indexed-find! entries k =)))

;;THis is o(n)....inefficient.
;;Another option here is to maintain a set of dropped slots...
(defn linear-indexed-drop!
  ([^java.util.ArrayList entries k eq?]
     (if-let [idx (linear-indexed-find! entries k eq?)]
       (doto entries (.remove idx))))
  ([entries k] (linear-indexed-drop! entries k =)))

(deftype mutable-alist [^java.util.ArrayList entries _meta]
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (linear-indexed-lookup! entries k))
  (valAt [this k not-found] (if-let [res (linear-indexed-lookup! entries k)]
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
  (equiv [this o]  (.equiv entries o))  
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
  (nth [this i not-found]  (.get entries i not-found))
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

(defn avec [& xs]
  (into (->avec []) xs))

(defn alist [& xs]
  (into (->alist []) xs))

(defn mlist [& xs]
  (into (->mutable-alist (java.util.ArrayList.) {}) xs))
