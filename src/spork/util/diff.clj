;;A little aux library for doing data diffs..
;;Different api and more limited in scope than
;;clojure.data.diff.  Primarily used for
;;diffing complex nested-maps as found in
;;spork.entitysystem entitystore.
(ns spork.util.diff)
;;can we perform an efficient entity-diff in the entity-store
;;library?, or do we have to go the long route?
;;Also...how long is the long route...

;;Also, if we diff this way, how does that play out when
;;we're using mutation?
;;Since it's a store, we really just care about which keys
;;changed in the component/entity mappings.
;;That's what we'll serialize.  Should cut down on our
;;problems significantly.  And, we can cache diffs in the
;;future to make this easier, maybe.
(defn diff-map [ls rs]
    (if (identical? ls rs)
      nil
      (let [lks (set (keys ls))
            rks (set (keys rs))
            ;;if anything is missing from either, we know
            ;;they are different.
            only-ls  (clojure.set/difference   lks rks)
            only-rs  (clojure.set/difference   rks lks)
            shared   (clojure.set/intersection lks rks) ;we only need to scan shared.
            changes  (reduce (fn [acc k]
                               (let [lv (get ls k)
                                     rv (get rs k)]
                                 (if (= lv rv) acc
                                     (conj acc [k [lv rv]]))))
                             []
                             shared)]        
          {:dropped only-ls
           :changed changes
           :added   (map (fn [c] [c (get rs c)]) only-rs)})))
