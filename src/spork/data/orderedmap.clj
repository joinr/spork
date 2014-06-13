;;An idiomatic implementation of ordered maps for clojure.  Fairly portable.
;;Ordered maps guarantee that, as keys are associated to the map, the order 
;;of assocation is preserved.  There's a bit of a space cost, in that we have 
;;to maintain a separate table of ordering, but it's reasonable for the 
;;data sets currently used.  In the future, there may be some space effeciencies
;;to be gained.

;;Optimization note: basemap in the typedef warns on reflection.  Might type 
;;hint that, but should profile to see if it matters.

(ns spork.data.orderedmap
  (:require [spork.protocols [core :as generic]]
            [spork.util [eager :as eager]]))

;;This is an independent implementation of an ordered map type.  While the graph
;;backed topology is nice, one downside of using persistent maps is that 
;;traversal of their contents is unordered.  We'd like to maintain the nice 
;;properties of hash-maps, like fast lookup and insertion, but with a little 
;;extra meta data that allows us to traverse values according to an ordering.
;;Hence, the ordered map.  This allows us to perform walks in a deterministic 
;;fashion, which is important for tree structures.  We only use ordered maps for
;;the parent->child (sink) and child->parent (source) data.  


(declare empty-ordered-map)

;;The ordered map acts, for all intents and purposes, like a normal persistent
;;map.  The main difference is that it maintains a sequence of keys to visit 
;;that is based on the order keys were associated with the map.  I have seen 
;;another implementation of this, which uses a vector for the ordering.  Here, 
;;I use a sorted-map, due to the need for ordered-maps to be able to change 
;;orderings effeciently.  A vector would be more efficient, but at the moment, 
;;the sorted map is doing the job.
(deftype ordered-map [^long n ^clojure.lang.IPersistentMap basemap 
                              ^clojure.lang.IPersistentMap idx->key 
                              ^clojure.lang.IPersistentMap key->idx _meta]
  Object
  (toString [this] (str (.seq this)))
  generic/IOrderedMap
  (get-ordering [m] [idx->key key->idx])
  (set-ordering [m idx->k k->idx] 
    (ordered-map. n basemap idx->k k->idx _meta))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (.valAt basemap k))
  (valAt [this k not-found] (.valAt basemap k not-found))  
  clojure.lang.IPersistentMap
  (count [this] (.count basemap))
  (assoc [this k v]     ;;revisit        
   (let [new-order (inc n)]
     (if (.containsKey basemap k) 
       (ordered-map. n (.assoc basemap k v) idx->key key->idx _meta)
       (ordered-map. new-order 
                     (.assoc basemap  k v)
                     (assoc idx->key n k)
                     (assoc key->idx k  n)
                     _meta))))
  (empty [this] (ordered-map. 0 {} (sorted-map) {} {}))  
  ;cons defines conj behavior
  (cons [this e]   (.assoc this (first e) (second e)))
  (equiv [this o]  (.equiv basemap o))  
  (hashCode [this] (.hashCode basemap))
  (equals [this o] (or (identical? this o) (.equals basemap o)))
  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (.containsKey basemap k))
  (entryAt [this k]
    (let [v (.valAt this k this)]
      (when-not (identical? v this) ;might need to yank this guy.
        (generic/entry k v))))
  (seq [this] (if (zero? (count basemap)) (seq {})
                (map (fn [k] (generic/entry k (.valAt basemap k))) 
                     (eager/vals! idx->key))))  
  ;without implements (dissoc pm k) behavior
  (without [this k] 
    (if (.valAt basemap k) 
        (ordered-map. n
                      (.without basemap k) 
                      (.without idx->key (.valAt key->idx k))
                      (.without key->idx k)
                      _meta)
        this))
    
  clojure.lang.Indexed
  (nth [this i] (if (and (>= i 0) 
                         (< i (.count basemap))) 
                  (let [k (.valAt idx->key i)]
                    (generic/entry k (.valAt basemap k)))
                  (throw (Exception. (str "Index out of range " i )))))
  (nth [this i not-found] 
    (if (and (< i (.count basemap)) (>= i 0))
        (.valAt basemap (.valAt idx->key i))        
         not-found))  
  Iterable
  (iterator [this] (clojure.lang.SeqIterator. (seq this)))
      
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))
  
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] _meta)
  (withMeta [this m] (ordered-map. n basemap idx->key key->idx m))
      
  clojure.lang.Reversible
  (rseq [this]
    (seq (map (fn [k] (clojure.lang.MapEntry. k (.valAt basemap k))) 
              (reverse (eager/vals! idx->key)))))

  java.io.Serializable ;Serialization comes for free with the other things implemented
  clojure.lang.MapEquivalence
  
  java.util.Map ;Makes this compatible with java's map
  (size [this] (.count basemap))
  (isEmpty [this] (zero? (.count basemap)))
  (containsValue [this v] (some #{v} (eager/vals! (basemap this)) v))
  (get [this k] (.valAt this k))
  (put [this k v] (throw (UnsupportedOperationException.)))
  (remove [this k] (throw (UnsupportedOperationException.)))
  (putAll [this m] (throw (UnsupportedOperationException.)))
  (clear [this] (throw (UnsupportedOperationException.)))
  (keySet [this] (set (eager/keys! basemap))) ;;modify
  (values [this] (map val (.seq this)))
  (entrySet [this] (set (.seq this))))

(def empty-ordered-map (->ordered-map 0 {} (sorted-map) {} {}))

(comment ;testing 
  (def the-map (into empty-ordered-map [[:a 1] [:b 2] [:c 3]]))
)         


;;another way to do this...is to use persistent array maps...
;;they seem to be pretty damn fast...
