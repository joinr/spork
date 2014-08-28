;;After looking at the source for clojure's persistent queue, 
;;I just realized they updated it to be backed by a persistent vector.
;;Without the benefit of being able to become a transient....
;;For my purposes, I'd like to have the queue transient-able...
;;So I'm re-implementing the persistentq, using a vector, 
;;with a transient implementation.
(ns spork.data.persistentq)

(defmacro unsupported-op []
  `(throw (Error. "unsupported operation")))

(deftype queue [^clojure.lang.PersistentQueue q]  
  Object
  (hashCode [coll] (.hashCode q))
  clojure.lang.Seqable
  (seq [coll] (.seq q))
  clojure.lang.ISeq
  (first [coll] (.first q))
  (next  [coll] (.next q))
  (more  [coll] (.more q))
  clojure.lang.IPersistentCollection 
  (count [itm] (.count q))
  (cons  [coll v]  (queue. (.cons q v )))
  (empty [coll]    (queue. clojure.lang.PersistentQueue/EMPTY))
  (equiv [this that] (.equiv q that))
  clojure.lang.IPersistentStack 
  (peek [coll] (.peek q))
  (pop  [coll] (queue. (.pop q)))
  clojure.lang.IPersistentList    
  clojure.lang.IHashEq
  (hasheq [coll] (.hasheq q))
  clojure.lang.IObj
  (meta [coll] (.meta q))
  (withMeta [coll m] (queue. (.withMeta q m)))
  java.util.Collection 
  (^objects toArray [coll] (.toArray q))
  (add     [coll o] (unsupported-op))
  (remove  [coll o] (unsupported-op))
  (addAll  [coll c] (unsupported-op))
  (clear   [coll] (unsupported-op))
  (removeAll [coll c] (unsupported-op))
  (retainAll [coll c]  (unsupported-op))
  (containsAll [coll c] (every? #(contains? q %) c))
  (^objects toArray  [coll ^objects a] (.toArray q a))
  (size     [coll] (.count q))
  (isEmpty  [coll] (.isEmpty q))
  (contains [coll o] (.contains q o))
  (iterator [coll] (.iterator q)))

(def ^:constant empty-queue (queue. clojure.lang.PersistentQueue/EMPTY))
  
  
  
  
  



  
