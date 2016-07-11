;;a namespace for containing differential
;;operations on entity stores.  Currently
;;these are fine-grained differences,
;;and only hold for persistent-backed
;;entitystores (i.e. mutable stores don't
;;may not have a meaningful diff in the current
;;implementation).
(ns spork.entitysystem.diff
  (:require [spork.entitysystem [store :as store]]
            [spork.util.diff :refer [diff-map]]
            [clojure.set]))



(defn entity-diff [lstore rstore]
  (let [{:keys [dropped changed added]} (diff-map (:domain-map lstore) (:domain-map rstore))]
    {:dropped dropped
     :changed   (for [[c [old new]]  changed] ;by component, find changes.  These are basically instructions.
                  [c (diff-map old new)])
     :added added}))



;;component add -> entity-add..
;;{:assoc-entity     [entity component value]
;; :dissoc-entity    [entity component]}
;;to update our entitystore.
(defn drops->patches [drops]
  (map (fn [d] {:drop-domain d}) drops))
(defn adds->patches [adds]
  (for  [[component es] adds
         [e v] es]
    {:add [e component v]}))

(defn changes->patches [changes]
  (apply concat
         (for  [[component diffs] changes]
           (when diffs
             (let [{:keys [dropped changed added]}  diffs]
               (apply concat
                      (map (fn one [e] {:drop [e component]}) dropped)
                      (map (fn two [[e v]] {:add  [e component v]}) added)
                      (map (fn tres [[e [old new]]] {:add [e component new]}) changed)))))))

(defn entity-diffs->patch [{:keys [dropped changed added]}]
   (apply concat (drops->patches   dropped)
          (adds->patches    added)
          (changes->patches changed)))

;;apply a patch to an existing store to get the
;;next store.
(defn patch->store [init patches]
  (reduce (fn [acc patch]
            (if-let [addition (:add patch)]
              (let [[e c v] addition]
                (store/assoce acc e c v))
              (store/drop-domain acc (:drop patch))))
          init patches))
