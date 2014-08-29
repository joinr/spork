;;After looking at the source for clojure's persistent queue, 
;;I just realized they updated it to be backed by a persistent vector.
;;Without the benefit of being able to become a transient....
;;For my purposes, I'd like to have the queue transient-able...
;;So I'm re-implementing the persistentq, using a vector, 
;;with a transient implementation.  
;;*Work In Progress.
;;This is currently about 3x faster than the default persistent 
;;queue in clojure.lang.PersistentQueue  for traversal and removal.
;;May use them in lieu of.
(ns spork.data.persistentq)

(defmacro unsupported-op []
  `(throw (Error. "unsupported operation")))
(declare mutable-queue)

;; (deftype queue [^clojure.lang.PersistentQueue q]  
;;   Object
;;   (hashCode [coll] (.hashCode q))
;;   clojure.lang.Seqable
;;   (seq [coll] (.seq q))
;;   clojure.lang.ISeq
;;   (first [coll] (.first q))
;;   (next  [coll] (.next q))
;;   (more  [coll] (.more q))
;;   clojure.lang.IPersistentCollection 
;;   (count [itm] (.count q))
;;   (cons  [coll v]  (queue. (.cons q v )))
;;   (empty [coll]    (queue. clojure.lang.PersistentQueue/EMPTY))
;;   (equiv [this that] (.equiv q that))
;;   clojure.lang.IPersistentStack 
;;   (peek [coll] (.peek q))
;;   (pop  [coll] (queue. (.pop q)))
;;   clojure.lang.IPersistentList    
;;   clojure.lang.IHashEq
;;   (hasheq [coll] (.hasheq q))
;;   clojure.lang.IObj
;;   (meta [coll] (.meta q))
;;   (withMeta [coll m] (queue. (.withMeta q m)))
;;   java.util.Collection 
;;   (^objects toArray [coll] (.toArray q))
;;   (add     [coll o] (unsupported-op))
;;   (remove  [coll o] (unsupported-op))
;;   (addAll  [coll c] (unsupported-op))
;;   (clear   [coll] (unsupported-op))
;;   (removeAll [coll c] (unsupported-op))
;;   (retainAll [coll c]  (unsupported-op))
;;   (containsAll [coll c] (every? #(contains? q %) c))
;;   (^objects toArray  [coll ^objects a] (.toArray q a))
;;   (size     [coll] (.count q))
;;   (isEmpty  [coll] (.isEmpty q))
;;   (contains [coll o] (.contains q o))
;;   (iterator [coll] (.iterator q))
;;   clojure.lang.IEditableCollection
;;   (asTransient [coll] (mutable-queue q)))

;; (deftype mqueue [^clojure.lang.ISeq f ^java.util.ArrayList r ^int cnt]  
;;   Object
;;   (hashCode [coll] (.hashCode q))
;;   clojure.lang.Seqable
;;   (seq [coll] (.seq q))
;;   clojure.lang.ISeq
;;   (first [coll] (.first q))
;;   (next  [coll] (.next q))
;;   (more  [coll] (.more q))
;;   clojure.lang.IPersistentCollection 
;;   (count [itm] (.count q))
;;   (cons  [coll v]  (queue. (.cons q v )))
;;   (empty [coll]    (queue. clojure.lang.PersistentQueue/EMPTY))
;;   (equiv [this that] (.equiv q that))
;;   clojure.lang.IPersistentStack 
;;   (peek [coll] (.peek q))
;;   (pop  [coll] (queue. (.pop q)))
;;   clojure.lang.IPersistentList    
;;   clojure.lang.IHashEq
;;   (hasheq [coll] (.hasheq q))
;;   clojure.lang.IObj
;;   (meta [coll] (.meta q))
;;   (withMeta [coll m] (queue. (.withMeta q m)))
;;   java.util.Collection 
;;   (^objects toArray [coll] (.toArray q))
;;   (add     [coll o]   (unsupported-op))
;;   (remove  [coll o]   (unsupported-op))
;;   (addAll  [coll c]   (unsupported-op))
;;   (clear   [coll]     (unsupported-op))
;;   (removeAll [coll c] (unsupported-op))
;;   (retainAll [coll c] (unsupported-op))
;;   (containsAll [coll c] (every? #(contains? q %) c))
;;   (^objects toArray  [coll ^objects a] (.toArray q a))
;;   (size     [coll]   (.count q))
;;   (isEmpty  [coll]   (.isEmpty q))
;;   (contains [coll o] (.contains q o))
;;   (iterator [coll]   (.iterator q)))

;; (def ^:constant empty-queue (queue. clojure.lang.PersistentQueue/EMPTY))
  
;;why not just have a persistent vector with a pointer to front and
;;back? 

(declare emptyq)

(deftype pqueue [^int front ^clojure.lang.PersistentVector v]
  Object
  (hashCode [coll]  (.hashCode v))
  clojure.lang.Seqable
  (seq [coll]  (.seq ^clojure.lang.APersistentVector$SubVector (subvec v front)))
  clojure.lang.ISeq
  (first [coll] (.nth v front))
  (next  [coll] (if (== front (.count v)) emptyq 
                    (pqueue. (unchecked-inc front) v)))
  (more  [coll] (.next coll))
  clojure.lang.IPersistentCollection 
  (count [itm] (unchecked-subtract (.count v) front))
  (cons  [coll x]  (pqueue. front (.cons v x)))
  (empty [coll]    emptyq)
  (equiv [this that] (.equiv v that))
  clojure.lang.IPersistentStack 
  (peek [coll] (.nth v front))
  (pop  [coll] (let [idx (unchecked-inc front)] 
                 (if (== idx (.count v))
                     emptyq
                     (pqueue. idx v))))
  clojure.lang.IPersistentList    
  clojure.lang.IHashEq
  (hasheq [coll] (.hasheq v))
  clojure.lang.IObj
  (meta [coll] (.meta v))
  (withMeta [coll m] (pqueue. front (.withMeta v m)))
  java.util.Collection 
  (^objects toArray [coll] (.toArray ^clojure.lang.APersistentVector$SubVector (subvec v front)))
  (add     [coll o]     (unsupported-op))
  (remove  [coll o]     (unsupported-op))
  (addAll  [coll c]     (unsupported-op))
  (clear   [coll]       (unsupported-op))
  (removeAll [coll c]   (unsupported-op))
  (retainAll [coll c]   (unsupported-op))
  (containsAll [coll c] (unsupported-op))
  (^objects toArray  [coll ^objects a] (unsupported-op))
  (size     [coll] (unchecked-subtract (.count v) front))
  (isEmpty  [coll] (== front (.count v)))
  (contains [coll o] (unsupported-op))
  (iterator [coll] (.iterator ^clojure.lang.APersistentVector$SubVector (subvec v front))))
  ;; clojure.lang.IEditableCollection
  ;; (asTransient [coll] (mutable-queue coll)))
(def emptyq (pqueue. 0 []))

(declare memptyq)

(deftype mqueue [^java.util.ArrayDeque q _meta]
  Object
  (hashCode [coll]  (.hashCode q))
  clojure.lang.Seqable
  (seq [coll] (iterator-seq (.iterator q)))
  clojure.lang.ISeq
  (first [coll] (.peek q))
  (next  [coll] (do (.pop q) coll))
  (more  [coll] (.next coll))
  clojure.lang.IPersistentCollection 
  (count [itm] (.size q))
  (cons  [coll x]  (do (.add q x) coll))
  (empty [coll]    memptyq)
  (equiv [this that] (identical? q that))
  clojure.lang.IPersistentStack 
  (peek [coll] (.peek q))
  (pop  [coll] (do (.pop q)  coll))
  clojure.lang.IPersistentList    
  clojure.lang.IHashEq
  (hasheq [coll] (.hashCode q))
  clojure.lang.IObj
  (meta [coll] _meta)
  (withMeta [coll m] (mqueue. q m))
  java.util.Collection 
  (^objects toArray [coll]  (.toArray q))
  (add         [coll o] (unsupported-op))
  (remove      [coll o] (unsupported-op))
  (addAll      [coll c] (unsupported-op))
  (clear       [coll]   (unsupported-op))
  (removeAll   [coll c] (unsupported-op))
  (retainAll   [coll c] (unsupported-op))
  (containsAll [coll c] (unsupported-op))
  (^objects toArray  [coll ^objects a] (unsupported-op))
  (size     [coll]   (.size q))
  (isEmpty  [coll]   (.isEmpty q))
  (contains [coll o] (unsupported-op))
  (iterator [coll]   (.iterator q))
  clojure.lang.ITransientCollection
  (conj       [coll x] (do (.add q x) coll))
  (persistent [coll] (pqueue. 0 (with-meta (into [] q) _meta))))
  
(defn ^mqueue mutable-queue [^pqueue q] 
  (let [v (.v q)]
    (mqueue. (reduce (fn [^java.util.ArrayDeque acc x] 
                       (doto acc (.add x))) (java.util.ArrayDeque.) v)
             (.meta q))))
              
                                          
  
  
(defn memptyq! []   (mqueue. (java.util.ArrayDeque.) {}))


  
