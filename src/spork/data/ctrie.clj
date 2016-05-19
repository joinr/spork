;;This wraps a nice interface around Michael Marczyk's
;;port of ctries from Scala.  We base a lot of
;;our operations on the ctrie implementation.
(ns spork.data.concurrent
  (:refer-clojure :exclude
     [into update update-in assoc-in assoc 
      dissoc dissoc-in conj disj contains?])
  (:require [ctries.clj :as ct]
            [spork.util.collections :refer :all]
            [spork.entitysystem.store :as store])
  (:import [ctries.clj Ctrie])
  )

;;ctries are concurrent HAMTs.
;;They implement the transient map interface,
;;and some other goodies (as well as java concurrent maps).

;;compared to persistent maps, they're about 3x as slow on lookup..
;;and 2x as slow on insert...

;;my bet was that the ctrie would give us the ability to a)
;;perform updates without the need to use nested-associng,
;;and b) allow for concurrent updates into the structure to
;;take advantage of multiple cores.
;;We certainly don't get concurrency in the current strategy (using
;;atomic CAS operations).
;; (defn mtest [n]
;;   (let [the-world (atom (into {} (for [[idx (range n)]]
;;                                    [idx {:name idx :data (rand)}])))]
;;     (dotimes [i 100000]
;;       (
  


(deftype CStore [^Ctrie entity-map
                 ^Ctrie domain-map]
  spork.entitysystem.store/IEntityStore
  (add-entry [db id domain data] ;;this gets called a lot....
    (let [^Ctrie
          domains (or (.valAt entity-map id)
                      (let [res (ct/concurrent-map)
                            _   (.assoc entity-map id res)]
                        res))
          ^Ctrie
          component (or (.valAt domain-map domain)
                        (let [res (ct/concurrent-map)
                              _  (.assoc domain-map domain res)]
                          res))]
      (do (when (not (.containsKey domains domain)) (.assoc domains domain :set))
          (assoc component id data)
          db)))
  (drop-entry [db id domain]
    (if-let [^Ctrie e (.valAt entity-map id)]
      (do (if (not (== 1 (.count e)))
            (.without e domain)
            (.without entity-map id))
          (let [^Ctrie comps (.valAt domain-map domain)]
            (if (not (== 1 (.count comps)))
              (.without comps id)
              (.without domain-map domain)))
          db)
      (throw (Exception. (str "entitystore does not contiain an entry for " id)))))
  (get-entry     [db id domain]
    (when-let [^Ctrie comps (.valAt domain-map domain)]
      (.valAt comps id)))
  (entities [db]  entity-map)
  (domains [db]   domain-map)
  (domains-of     [db id]  (keys (.valAt entity-map id)))
  (components-of  [db id]
    (store/lazy-join domain-map id)
    )  
  ;;We want to avoid large joins....hence, getting an entity reference that lazily loads and
  ;;caches values, so we only have to pay for what we load.
  (get-entity [db id]
    (when-let [comps (.components-of db id)]
      (spork.entitysystem.store.entity. id nil nil comps #{})))
  (conj-entity     [db id components])
  clojure.lang.IDeref
  (deref [this] {:entity-map  entity-map
                 :domain-map domain-map})
  )


(defn ->cstore [] (CStore. (ct/concurrent-map) (ct/concurrent-map)))

(def cs [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z])
(def es (for [n (range 50)]
          (into {:name n} (map (fn [c] [c (str  c n)]) cs))))

(def the-store
  (-> (->cstore)
      (store/add-entities es)))
