;;A library for defining and managing systems of
;;communicating sequential processes.
;;This is a port from a prototype implementation.
;;The goal is to provide a more localized
;;way to have multiple systems, rather than
;;a single, localized system living in an atom.
;;The single system works nicely for the moment though.
;;We should probably lift this into a localized system
;;definition, so that we can apply local control to systems,
;;possibly with the same names.

;;Note Feb 2017 - There's a pending rewrite/update from
;;lessons learned using this system in production contexts.
;;It Works pretty well for the original use case, although
;;the global vars create some inflexibility.
;;THere also may be comparable libraries available that
;;serve the same purpose by now.
(ns spork.async.system
  (:require [clojure.core.async :as async :refer [alts! alts!!]]))

;;note:
;;we can put the system in an atom (it already is) and
;;wrap the protocol around that.
;;just need to add functions to allow us to specify which
;;system we're talking to.  that allows us to have multiple
;;local systems (i.e. side-effecting communications buses).

;;It's also easy to compose systems...

;;We can establish a kill-switch via the following:
;;Register the channels we have open
;;Register the threads we have running
;;Register the go-loops we have running

;;Alternately, we could synchronize based on an atom that indicates the locale.
;;We could also have a keep-alive to determine whether or not we should kill everything.

;;singleton system for now...
(defonce system (atom nil))
(defn kill-all! []
;;for hard reloads...i.e. reload-all, this keeps our protocols from getting
;;hosed.  Protocols don't play nice at the repl if we're restarting things often.
  (when @system
    (do (doseq [[nm proc] @system]
          (.stop! proc))
        (reset! system nil))))
(kill-all!)

(defprotocol IWorker
  (start! [obj])
  (stop!  [obj]))
(defprotocol IPauseable
  (pause!  [obj])
  (resume! [obj]))

(defn stopped? [w]
  (let [stat @w]
    (or (= stat :stopped) (vector? stat))))

(defn try-stop! [w]
    (if (stopped? w) w
        (stop! w)))

;;keywords are simple workers, they just map into the existing
;;values of active systems.
(extend-type clojure.lang.Keyword
  IWorker
  (start! [k] (if-let [w (get @system k)]
                (start! w)
                (throw (Exception. (str "no known worker " k)))))
  (stop! [k] (if-let [w (get @system k)]
                (try-stop! w)
                (throw (Exception. (str "no known worker " k))))))

;;another option..
;;let the expression dictate how to handle failure.  Since we don't
;;actively poison, we can just use a reference...

;;another idea is to interleave the poison on a pub/sub level...fyi..
;;This is an idea ripped from stackoverflow, thanks to danneu:
(defmacro go-pull [nm input-ch v expr] 
  `(let [cleanup#  (when-let [w# (get @spork.async.system/system ~nm)]
                     (do (spork.async.system/try-stop! w#) nil))
         poison# (clojure.core.async/chan)
         status# (atom :started)
         run# (fn ~'run! []
                 (clojure.core.async/go-loop []
                   (let [[~v ch#] (alts! [poison# ~input-ch])]
                     (cond (= ch# poison#) (do (println (str [~nm "stopping"]))
                                               (reset! status# :stopped))
                           (reduced? ~v)   (reset! status# [:stopped ~v])
                           :else (do
                                   ~expr 
                                   (recur))))))
         wnew# (reify ~'spork.async.system/IWorker
                 (~'stop! [obj#]
                   (case @status#
                     :started (do (clojure.core.async/put! poison# :stop)
                                  (reset! status# :stopped)
                                  obj#)
                     :stopped (throw (Exception. "trying to stop a stopped worker!"))))
                 (~'start! [obj#]
                   (case @status#
                     :started (throw (Exception. "Trying to start a running worker!"))
                     :stopped (do (reset! status# :started)
                                  (run#)
                                  obj#)))
                 ~'clojure.lang.IDeref
                 (~'deref [obj#] @status#))
          updated# (swap! spork.async.system/system assoc ~nm wnew#)]     
     (run#)
     wnew#
     ))

(defmacro go-push [nm output-ch expr] 
  `(let [cleanup#  (when-let [w# (get @spork.async.system/system ~nm)]
                     (do (spork.async.system/try-stop! w#) nil))
         poison# (clojure.core.async/chan)
         status# (atom :started)
         run# (fn ~'run! []
                 (clojure.core.async/go-loop []
                   (let [new-val# ~expr]
                     (if (or (nil? new-val#) (reduced? new-val#))
                       (reset! status# [:stopped new-val#])                     
                       (let [[v# ch#] (alts! [poison# [~output-ch new-val#]])]
                         (if (= ch# poison#) (do (println (str [~nm "stopping"]))
                                                   (reset! status# :stopped)) 
                               (recur)))))))
         wnew# (reify ~'spork.async.system/IWorker
                 (~'stop! [obj#]
                   (case @status#
                     :started (do (clojure.core.async/put! poison# :stop)
                                  (reset! status# :stopped)
                                  obj#)
                     :stopped (throw (Exception. "trying to stop a stopped worker!"))))
                 (~'start! [obj#]
                   (case @status#
                     :started (throw (Exception. "Trying to start a running worker!"))
                     :stopped (do (reset! status# :started) (run#) obj#)))
                 ~'clojure.lang.IDeref
                 (~'deref [obj#] @status#))
          updated# (swap! spork.async.system/system assoc ~nm wnew#)]     
     (run#)
     wnew#
     ))

(defmacro thread-pull [nm input-ch v expr] 
  `(let [cleanup#  (when-let [w# (get @spork.async.system/system ~nm)]
                     (do (spork.async.system/try-stop! w#) nil))
         poison# (clojure.core.async/chan)
         status# (atom :started)
         run# (fn ~'run! []
                (clojure.core.async/thread
                  (loop []
                    (let [[~v ch#] (alts!! [poison# ~input-ch])]
                      (cond (= ch# poison#) (do (println (str [~nm "stopping"]))
                                                (reset! status# :stopped))
                            (reduced? ~v)   (reset! status# [:stopped ~v])
                            :else (do
                                    ~expr 
                                   (recur)))))))
         wnew# (reify ~'spork.async.system/IWorker
                 (~'stop! [obj#]
                   (case @status#
                     :started (do (clojure.core.async/put! poison# :stop)
                                  (reset! status# :stopped)
                                  obj#)
                     :stopped (throw (Exception. "trying to stop a stopped worker!"))))
                 (~'start! [obj#]
                   (case @status#
                     :started (throw (Exception. "Trying to start a running worker!"))
                     :stopped (do (reset! status# :started) (run#) obj#)))
                 ~'clojure.lang.IDeref
                 (~'deref [obj#] @status#))
          updated# (swap! spork.async.system/system assoc ~nm wnew#)]     
     (run#)
     wnew#
     )) 

(defmacro thread-push [nm output-ch expr] 
  `(let [cleanup#  (when-let [w# (get @spork.async.system/system ~nm)]
                     (do (spork.async.system/try-stop! w#) nil))
         poison# (clojure.core.async/chan)
         status# (atom :started)
         run# (fn ~'run! []
                (clojure.core.async/thread
                  (loop []
                    (let [new-val# ~expr]
                      (if (or (nil? new-val#) (reduced? new-val#))
                        (reset! status# [:stopped new-val#])                     
                        (let [[v# ch#] (alts!! [poison# [~output-ch new-val#]])]
                          (if (= ch# poison#) (do (println (str [~nm "stopping"]))
                                                   (reset! status# :stopped)) 
                              (recur))))))))
         wnew# (reify ~'spork.async.system/IWorker
                 (~'stop! [obj#]
                   (case @status#
                     :started (do (clojure.core.async/put! poison# :stop)
                                  (reset! status# :stopped)
                                  obj#)
                     :stopped (throw (Exception. "trying to stop a stopped worker!"))))
                 (~'start! [obj#]
                   (case @status#
                     :started (throw (Exception. "Trying to start a running worker!"))
                     :stopped (do (reset! status# :started) (run#) obj#)))
                 ~'clojure.lang.IDeref
                 (~'deref [obj#] @status#))
          updated# (swap! spork.async.system/system assoc ~nm wnew#)]     
     (run#)
     wnew#
     ))

   
(defn stop-all! []
  (do (swap! system  (fn [s] (reduce-kv (fn [acc k v]
                                          (assoc acc k (try-stop! v))) s s)))
      :all-stopped))

(defn start-all! []
  (do (stop-all!)
      (swap! system  (fn [s] (reduce-kv (fn [acc k v]
                                          (assoc acc k (start! v))) s s)))
      :all-started))

(defn active
  "Return all active systems."
  []
  (filter (fn [[nm w]] (not= :stopped @w)) @system))
(defn inactive
  "Return all inactive systems."
  []
  (filter (fn [[nm w]] (= :stopped @w)) @system))
(defn prune!
  "Eliminate stopped systems from the active set of systems."
  []
  (swap! system (fn [s] (reduce-kv (fn [acc nm w]
                                     (if (stopped? w)
                                       (dissoc acc nm)
                                       acc)) s  s))))

;;note: we can create lifecycles fairly easily if we
;;formalize the names of the systems that are created.
;;We can have a greater system determine how to kill them via
;;propogating poison (calling their stop! functions).
