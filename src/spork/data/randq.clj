(ns spork.data.randq)

;;A random queue is just a facade for a persistent queue, backed a a sorted map,
;;that assigns random priorities to values conjoined to the queue. 
(deftype randomq [basemap _meta]
  Object
  (toString [this] (str (.seq this)))
  clojure.lang.ISeq
  (first [this] (first (vals basemap)))
  (next [this]
    (if (empty? basemap) nil
        (pop this)))
  (more [this]
    (if (empty? basemap)
      (.empty this)
      (pop this)))
  clojure.lang.IPersistentCollection
  (empty [this]  (randomq. (sorted-map) {}))
  (equiv [this that]
    (and (instance? randomq that)
         (identical? basemap (:basemap that))))
  clojure.lang.Seqable
  (seq [this]  ; returns a LazySeq
    (vals basemap))
  clojure.lang.Counted
  (count [this] (count basemap))
  clojure.lang.IPersistentVector
  (cons [this a]
    ; called by conj
    (loop [k (rand)
           i (long 0)]
      (cond (> i 10) 
               (throw (Exception. 
                        "Trying to conj onto random queue, generated an 
                         improbable number of identical keys.  
                         Check the implementation in spork.data.randomq"))
            (not (contains? basemap k)) 
               (randomq. (assoc basemap k a) _meta)
             :else (recur (rand) (unchecked-inc i)))))
  (length [this]  (.count this))
  (assocN [this index value]
    (if (>= index (count basemap)) (throw (Exception. "Index Out of Range"))
      (let [k (nth (keys basemap))]
        (randomq. (assoc basemap k value) _meta))))
  clojure.lang.IPersistentStack
  (pop  [this]  (randomq. (dissoc basemap (first (keys basemap))) _meta))
  (peek [this] (first    (vals basemap)))
  clojure.lang.Indexed
  (nth [this i] (if (and (>= i 0) 
                         (< i (count basemap))) 
                    (val (nth basemap i))
                    (throw (Exception. (str "Index out of range " i )))))
  (nth [this i not-found] 
    (if (and (< i (count basemap)) (>= i 0))
        (val (nth basemap i))       
         not-found))  
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))      
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (randomq. basemap m))      
  clojure.lang.Reversible
  (rseq [this]  (reverse (vals basemap)))
  java.io.Serializable ;Serialization comes for free with the other stuff.
  )

(def emptyrq (randomq. (sorted-map) {}))

