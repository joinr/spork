;;helper lib that allows us to specify
;;reload-all with exceptions.  mainly to
;;avoid problems with core.async reloads.
;;note: clj-reload is a better fit here.
(ns spork.util.require
  (:refer-clojure :exclude [require]))

(in-ns 'clojure.core)

(def ^:dynamic *preloaded* #{})

;;have load-all use user-bound exclusions to prevent
;;reloading preloaded libs (e.g. core.async)

(defn- load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. If require, records the load so any duplicate loads
  can be skipped."
  [lib need-ns require]
  (dosync
   (commute *loaded-libs* #(reduce1 conj %1 %2)
            (binding [*loaded-libs* (ref (apply sorted-set *preloaded*))] ;;inject our stuff.
              (println {:lib lib :preloaded *preloaded*})
              (if-not (*preloaded* lib)
                (load-one lib need-ns require)
                (println [:excluding lib :on-reload]))
              @*loaded-libs*))))

(in-ns 'spork.util.require)

;;(require 'blah :reload-all :exclude [blee])
(defn require
  "Identical to clojure.core/require, except we allow an extra flag in the form of
   :exclude [lib1 lib2 ...] to prevent reloading of lib1 ... libn.  This allows e.g.
  (require 'blah :reload-all :exclude [blee]) so that blee is presumed to be loaded.
  Designed to ensure libraries with load-once behavior like core.async are
  able to be depended on for reloads."
  [& args]
  (let [xs (->> args (drop-while #(not= % :exclude)))
        exclusions (some->> xs  second)
        cleaned (when exclusions
                  (filter #(and (not= % :exclude)
                                (not= % exclusions))
                          args))]
    (if exclusions
      (do (assert (vector? exclusions) "expected vector for :exclude key")
          (binding [clojure.core/*preloaded* (into #{} exclusions)]
            (apply clojure.core/require cleaned)))
      (apply clojure.core/require args))))
