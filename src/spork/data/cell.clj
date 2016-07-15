;;Implementation of a simple
;;mutable value container based on
;;a single array.
;;We use this for compatibility with
;;records.
(ns spork.data.cell) 

(defmacro ne []
  `(throw (Exception. str "Not Implemented!")))

;;no implementation of hash equality.
(deftype cell [^:unsynchronized-mutable contents
               ^:unsynchronized-mutable ^:boolean realized]
  java.io.Serializable 
  clojure.lang.IAtom
  (reset [o newval] (do (set! contents newval)
                        (set! realized true)
                        o))
  clojure.lang.IPending
  (isRealized [o] realized)
  java.util.Map
  (put    [this k v]  (ne))
  (putAll [this c] (ne))
  (clear  [this] (ne))
  (containsKey   [this o] (ne))
  (containsValue [this o] (ne))
  (entrySet [this]   #{(clojure.lang.MapEntry. :contents contents)
                       (clojure.lang.MapEntry. :realized realized)})
  (keySet   [this]   (ne))
  (get      [this k] (case k
                       :contents contents
                       :realized realized
                       (throw (Exception. "unknown field"))))
  ;(equals [this o] (.equals ^java.util.Map m o))
  (isEmpty [this] (ne))
  (remove [this o] (ne))
  (values [this] (ne))
  (size [this] 2)
  clojure.lang.IDeref
  (deref [obj]  (when realized contents))) 

;;hmmm...
(defmethod print-method spork.data.cell.cell [^cell x ^java.io.Writer w]
  (do (.write w "#spork.data.cell.cell") 
      (print-method {:contents (:contents x)
                     :realized (:realized x)} w)))
                                     
(defn ->cell
  ([]  (cell. nil false))
  ([v] (cell. v true)))
