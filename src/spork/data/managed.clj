;;This is a namespace for defining managed
;;access to concurrent data structures.
;;Specifically, we want to utilize
;;clojure's concurrency semantics to
;;allow a view of the world in which
;;a (possibly persistent) state is
;;managed with an async-friendly
;;interface, which allows concurrent
;;reads and writes in-memory.
;;We should be able to have
;;a mutable state and a persisent
;;state, with journaling enabled.
;;THe primary interface will be
;;through an agent that manages
;;concurrent reads and writes
;;to the data structure.
;;We'll pass the structure around
;;normally, allowing asynchronous writes.

;;Update Feb 2017 - EXPERIMENTAL
(ns spork.data.managed
  (:require [clojure.core.async :as a]
            [clojure.core.reducers :as r]))

;;Write access will be serialized implicitly by
;;the agent.  Reads are derefed behind the scene
;;auotmagically.  The mutable version of this
;;will use a java concurrent map.  We should, by
;;default, provide journaling for all versions of the
;;map.  Basically, the agent will receive updates
;;i.e. assoc, dissoc, update, or simple functions,
;;queue the results and execute.  Ideally, we'll
;;have batch updates computed in bulk.

;;the model we'll use is this.
;;we'll maintain version control by retaining a list of all
;;previous maps (probably space inefficient).
;;We can embed the parent map in the meta of the current map.


;;thin wrapper around an agent.
;; (deftype async-map [clojure.lang.Agent executor]
;;   clojure.lang.Deref
;;   (deref [o] (.deref executor))
;;   clojure.lang.IPersistentMap 
  
;;   )

;;My strategy is to combine a map wrapped by an
;;agent with multiple goroutines to allow us
;;to update the "state" concurrently while
;;retaining previous versions of the state
;;according to some strategy.  Basically,
;;this ends up creating a very simple
;;in-memory database with the ability
;;roll-back to previous points
;;in "time".  This buys us a couple of
;;pretty cool capabilities:
;;primarily, we get unlimited undo.
;;we can scan backwords to find a previous
;;history, and revert to it (ala continuation).
;;Reversion will be side-effecting, in that it
;;rebases history.
;;For searching and simulation, this should
;;enable some pretty cool stuff.
;;Also, we get the ability to concurrently
;;"update" the state while retaining the
;;properties of both persistence and immutability.
;;Updates are handled by an agent that serializes
;;updates according to a queue.  Concurrent reads
;;cost nothing.  Note: we may look into nesting
;;managed maps so that we have managed submaps.
;;In a managed submap, operations that cause
;;the map to update are delegated to another agent
;;that handles the transaction in a serial manner.
;;What we end up with, is a network of agents
;;working cooperatively, and asynchronously, to
;;manage resources.  This is kind of an evolution
;;of the cells paradigm I was working on earlier.

;;I "think" it'll kill a couple of birds with one
;;stone, namely:
;;  we retain referentially transparent semantics
;;  on the outside.  A managed map looks like
;;  a map for all intents and purposes.
;;  The value of a managed map, at any point
;;  in time, is fully determined and immutable.
;;  We can examine previous versions of the
;;  managed map.
;;  We can revert to previous versions.

;;The initial version will be stone-age
;;simple.  Specifically, we'll maintain
;;a single persistent map, and allow
;;multiple writers via a serialized queue through
;;the agent interface.  I'm curious to see how
;;far we can get with this....
;;The only downside is we get non-deterministic
;;update order.  If we have a synchronizing point,
;;as in simulation, like the end-of-the-day,
;;particularly when writing unordered
;;structures like a map, the order of updates
;;should not matter.

;;transients should not be version controlled.
;;In fact, we may use transient/persistent!
;;as a means of checkpointing our map.
;;Every time we call persistent! we retain the
;;map.  Another way to do this is to just
;;maintain the history of updates and use
;;them to undo/redo.

(defrecord nd [previous])

;;Define all the operations on the map that
;;our managing agent will perform.
(defn passoc [^clojure.lang.Associative m k v]
  (.withMeta ^clojure.lang.IObj (.assoc m k v) (nd. m)))

(defn pdissoc [^clojure.lang.IPersistentMap m k]
  (.withMeta ^clojure.lang.IObj (.without m k) (nd. m)))

(defn pinto [^clojure.lang.IPersistentMap m xs]
  (.withMeta ^clojure.lang.IObj (into m xs) (nd. m)))

(defn undo   [^clojure.lang.IObj m]
  (.valAt ^clojure.lang.IPersistentMap (.meta m) :previous))

(defn undo-seq [m]
  (when-let [prev (undo m)]
      (lazy-seq
       (cons prev (undo-seq prev)))))

(defn rewind [pred m]
  (->> (undo-seq m)
       (drop-while (complement pred))
       (first)))

;;say we have an agent.
(defn ->async-map [init]  
  (agent {}))

(defn async-assoc
  [m k v]
  (send m passoc k v))

(defn async-dissoc
  [m k]
  (send m pdissoc k))

(defn async-get  [m k]  (get @m k))
(defn async-set  [m v]  (send m  (fn [_] v)))
(defn async-undo [m]    (send m undo))
(defn async-rewind! [pred m] (send m rewind pred))
(defn into! [m kvps]    (send m pinto kvps))

(comment ;testing
;;okay, so we have a managed resource via our
;;agent.  We're not looking at dozens of requests
;;coming in.
;;What can we do with this?
;;Well, we can have multiple readers write to the
;;managed map (ala the blackboard architecture).
;;Who decides if a reversion should happen?

;;for instance, if I have n units that want to update
;;state, they are queuing functions on the state to
;;happen.

;;computation of "what" to update
;;happens outside the managed thread.
(defn update-worker [am n]
  (if-let [work (async-get am n)]
    (async-assoc am n (+ (rand) work))
    am))

;;lets update a set of workers in parallel
;;and see what happens.
(defn update-all [am]
  (doall (pmap (fn [w]
                 (update-worker am w))
               (async-get am :workers)))
  (await am))

;;if computations are independent, we can map/reduce the
;;results, using into! to update the managed state.
(defn ->work-state [& {:keys [n] :or {n 100}}]
  (let [m  (into {} (map (fn [idx] [idx 0]) (range n)))]
    (assoc m :workers (vec (keys m)))))

;;so this more or less works.
;;we get the fundamental operations we'd like.
;;we can synchronize with await.
;;we can 
)
