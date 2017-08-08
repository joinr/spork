;;Defines utilities and workflows for performing higher-order analysis
;;of simulations.  Specifically, we define ways to process simulation
;;history and produce dynamic analysis.
(ns spork.sim.history
  (:require [spork.util [io :as io]
                        [serial    :as ser]]
            [clojure.core.reducers :as r]
            [spork.util.reducers]
            [spork.entitysystem
             [diff     :as diff]
             [store :as store]]
            [spork.sim [simcontext     :as sim]
                       [core :as core]]))

;;Utilities
;;=========
(defn- do-curried
  [name doc meta args body]
  (let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(defn try-seq
  "Tries to coerce x to a seq, returning nil if unable.
   Useful wrapper around seq, since some fundamental types
   like String and others cannot implement seq and are 
   handled via hardcoded dispatch."
  [x]
  (try (seq x)
       (catch Exception e nil)))

(defmacro  defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))
;;Note: there's a problem with the compile-time trick here...
;;in-ns, used in spork.util.reducers, actually produces
;;Huh...well, we'll have to cop this.
;;we're going to add in iterate, range, and friends
;;Reducers patch for Clojure courtesy of Alan Malloy, CLJ-992, Eclipse Public License
(defcurried r-iterate
  "A reducible collection of [seed, (f seed), (f (f seed)), ...]"
  {:added "1.5"}
  [f seed]
  (reify
    clojure.core.protocols/CollReduce
    (coll-reduce [this f1] (clojure.core.protocols/coll-reduce this f1 (f1)))
    (coll-reduce [this f1 init]
      (loop [ret (f1 init seed), seed seed]
        (if (reduced? ret)
          @ret
          (let [next (f seed)]
            (recur (f1 ret next) next)))))

    clojure.lang.Seqable
    (seq [this]
      (seq (clojure.core/iterate f seed)))))

(defn merge-meta [obj m]
  (with-meta obj (merge (get meta obj) m)))

(defn ->simreducer [stepf keep-simulating? init]
  (r/take-while identity (r-iterate (fn [ctx]
                                       (when  (keep-simulating? ctx)
                                          (let [init ctx
                                                t  (sim/get-time ctx)
                                                processed  (stepf t  ctx)
                                                nxt        (sim/advance-time processed)]
                                            (merge-meta nxt {:start-end {:t t
                                                                        :start init
                                                                        :end processed}}))))
                                    init)))

;;I think we want to convert this into a stream with the simulation
;;state.  So, instead of just [t ctx], we get [t ctx :begin|:end]
;;That way, other streams can filter on either begin/end or use both.

;;A wrapper for an abstract simulation.  Can produce a sequence of
;;simulation states; reducible.
(defn ->simulator [stepf keep-simulating? seed]
  (let [simred (->simreducer stepf keep-simulating? seed)]
    (reify
      clojure.lang.Seqable
      (seq [this]
        (take-while identity (iterate (fn [ctx]
                                        (when  (keep-simulating? ctx)
                                          (let [init       ctx
                                                t          (sim/get-time     ctx)
                                                processed  (stepf t          ctx)
                                                nxt        (sim/advance-time processed)]
                                            (merge-meta nxt {:start-end {:t t
                                                                        :start init
                                                                        :end processed}}))))
                                      seed)))
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]   (reduce f1 simred))
      (coll-reduce [_ f1 init] (reduce f1 init simred)))))

(defn ->history-stream [tfinal stepf  keep-simulating? init-ctx]
  (->> init-ctx
       (->simulator stepf  keep-simulating?)
       (map (fn [ctx] [(core/get-time ctx) ctx]))
       (take-while
        (fn [^clojure.lang.Indexed v]
          (<= (.nth v 0) tfinal)))
       ))

;;Now using transducers.
(defn ->history [tfinal stepf  keep-simulating? init-ctx]
  (into {} (comp (map (fn [ctx] [(core/get-time ctx) ctx]))
                 (take-while #(<= (first %) tfinal)))
        (->simulator stepf  keep-simulating? init-ctx)))

(defn ending [h t] (get (meta (get h t) :end  )))
(defn start  [h t] (get (meta (get h t) :start)))

;;most metrics should be collected at the end of the
;;day.  For debugging and verification purposes, we'd
;;like to have the history at the beginning of each day.
;;We technically provide access to both via the history stream.
;;we embed the previous day's sample in the meta.
(defn end-of-time-history [h]
  (->> h
       (map #(:start-end (meta (second %))))
       (filter identity)
       (map (fn [{:keys [t start end]}]
              [t end]))))

(defn expanded-history [h]
  (mapcat (fn [[t ctx]]
            (let [{:keys [start end]} (get (meta ctx) :start-end)]
              [[t start  :start]
               [t end :end]])) h))

;;API Definition
;;==============
(defn state-stream
  "Create a stream of simulation states, indexed by time.
   Optionally, set the maximum simulation time, define transformations
   on the project tables, like src filters, provide a custom step function,
   and choose to generate auditing information upon initializing the
   stream."
  [ctx & {:keys [tmax step-function keep-simulating?]
                  :or {tmax 0
                       step-function identity
                       keep-simulating? (fn [_] true)}}]
  (-> (->> ctx
           (->history-stream tmax step-function keep-simulating?)
           (end-of-time-history))
      (merge-meta (meta ctx))))

(defn state-stream? [x]
  (and (try-seq x)
       (core/context? (second (try-seq (first x))))))

;;maybe introduce another arity-fn for this...
;;we could also use dynamic vars......
;;That's an option: allow as-stream to fall-back to
;;a bound set of vars for as-context and context->stream...
;;That way, we can define host functions for our
;;specific simulations...
(def ^:dynamic *context->stream*
  "The default binding for coercing unknown types to a stream of 
   simulation contexts.  Errors by default, unless user provides 
   a compatible functoin to produce time-indexed sequences of 
   [t simcontext]."
  (fn [x]
    (throw (Exception. (str [(type x) :not-a-simcontext!])))))

(defn as-stream
  ([context->stream x]
   (if (state-stream? x)
     x
     (let [res (context->stream x)]
       (if (state-stream? res) res
           (throw (Exception.
                   (str [:expected :sequence-of  '[t simcontext]
                         :got (type res)])))))))
  ([x] (as-stream *context->stream*)))

;;Temporal API
;;============

(defn time-before-error
  "Given a sequence of frames, returns "
  [xs]
  (let [frm (atom nil)]
    (try (doseq [x xs]
           (reset! frm x))
         (catch Exception e
           (do (println [:error-occurs-in-next-frame])
               @frm)))))

(defn time-before
  "Given a sequence of frames, returns the closest context prior  
   to time t."
  [t xs]
  (->> xs
       (as-stream)
       (take-while (fn [[tf ctx]]
                     (< tf t)))
       (last)
       (second)))

(defn time-of
  "Given a sequence of frames, returns the context prior to 
   evaluating the step-function at t."
  [t xs]
  (->> xs
       (as-stream)
       (time-before t)
       (sim/advance-time)))
  
(defn frame-at
  "Fetch the simulation frame at or nearest (after) time t from a 
   sequence of [t ctx] frames xs."
  [t xs] (some (fn [[tc ctx]] (when (>= tc t) ctx)) (as-stream xs))) 

;;Entity Tracing and Debugging
;;============================
;;Migrated from quilsample.bridge

;;can we take a discrete entity history and create a
;;continuous entity history?  That's what the cycle-lerper
;;is supposed to be doing, but currently is failing a bit at.
(defn discrete-entity-history
  "Given a context and an entity id to follow, returns a 
   map of the discrete values of the entity's history 
   as a function of time.  Ensures that only inflections 
   where the entity history changes are captured. 
   Caller may supply an optional sample? function  
   to determine if frames should be dropped."
  [ctx id & {:keys [sample?] :or {sample?
                                  (fn [x] true)}}]
  (let [tfinal (when (and (coll? sample?)
                          (every? number? sample?))
                 (reduce max sample?))
        sample? (if tfinal (let [time?  (set sample?)]
                             (fn [ctx]
                               (time? (:t ctx))))
                    sample?)]
  (->>  (as-stream ctx)
        ;;(raw-frames) elided for now.
        (map (fn [[t ctx :as f]]
               (let [;ctx (:ctx f)
                     ;t   (:t   f)
                     e   (store/get-entity ctx id)]
                 (assoc e :t t))))
        (take-while (if tfinal (fn [f] (<= (:t f) tfinal))
                        (fn [x] true)))
        (filter #(and (sample? %)
                      (== (:last-update %) (:t %))))
        )))

(defn entity-trace
  "High level function for directing entity event and behavior 
   traces to *out*.  Allows us to walk through the entity's 
   behavior as it changes and see fine-grained event and 
   behavior messages about the entity, as well as its 
   discrete state changes."
  [ctx e & {:keys [debug? sample? trace]
            :or {debug? true sample? (fn [_] true) trace identity}}] 
  (let [eh (if debug?
             (core/debug-entity e
                (doall (discrete-entity-history ctx e :sample? sample?)))
             (doall (discrete-entity-history ctx e :sample? sample?)))]
    (println [:<<<<<<<<<<<<<<<<TRACE>>>>>>>>>>>>>])
    (doseq [x (map trace eh)]
      (println x))))


;;Delta Diffing and Compressed Simulation History Serialization
;;=============================================================

;;Another useful feature...
;;We'd like to optionally audit our project, when we create a stream and
;;initialize it.
;;We can do this by hooking into the table-xforms, since this allows us
;;to audit.

;;serializing all the snapshots is untenable...
;;can we compute diffs?
;;All we really care about, as we traverse forward,
;;is information regarding who changed...
;;So if any entity was touched or updated during the
;;t, the it'll show...
;;In theory, any last-updates to entities
;;will show up....so that limits our diffs
;;to the entities with last-update components..
;;From there, we can just compare them with their previous selves...

;;The goal here is to easily serialize our entity database...
;;Note...we have some options for how we do this...
;;We could do an initial state + diffs (similar to
;;git...) and save our stuff that way.  For now we
;;have a stream of state snapshots which have internal
;;references via persistent structures....so...
;;we should? be able to persist our stuff efficiently.
;;We're going to stream this rather than do it all in
;;memory...we can also add a diff buffer that can
;;be serialized at the end of the day...
;;So, anytime an entity is modified (via gete adde
;;assoce, etc.), the diff buffer (or dirty flag)
;;gets mutated in the db.  Then we compare dirty
;;entities with their previous versions to see
;;what the differences are...seems plausible...
;;the brute-force approach is to just use
;;hashing to compare...assuming we have hash
;;equality, we just hash-compare the stores, and
;;then the components in the stores, and then
;;the entities...
;;probably makes more sense to diff the components...
;;structural diffing is a pretty powerful way to
;;compute deltas...and laid back.  It "would" be
;;nice if we'd cached the values though.
(defn diff-stores [l r]
  (let [lcomps (-> l :state :store :domain-map)
        rcomps (-> r :state :stote :domain-map)]
    ;;many components will be the same..
    ;;man, we can actually save time if the hash hasn't been computed yet...
    (if (identical? lcomps rcomps) nil
        (reduce-kv (fn [acc lk lv]
                     (if-let [rv (get rcomps lk)]
                       (if (not (identical? lv rv))
                         (conj acc lk) acc) (conj acc lk))) [] lcomps))))

;;since components are maps...we can recursively diff to see which
;;entities changed.

;;If we constrain all access to go through assoce, etc,
;;then we can get away with diffing...
(defn diff-store [l r]
  (let [le (:store (sim/get-state l))
        re (:store (sim/get-state r))]
    (if (identical? (:domain-map le) (:domain-amp re))
      nil
      (diff/entity-diff le re))))

;;we might have a memory leak here if we're force the first and traversing the
;;rest of the history...
(defn patch-history [h]
  {:init    (first h)
   :patches (for [[[t1 l] [t2 r]] (partition 2 1 h)]
              [t2 (diff/entity-diffs->patch (diff-store l r))])})

(defn     write-history  [h path]  (ser/freeze-to (patch-history h)  path))
(defn     write-history! [h path]  (ser/freeze-to! (patch-history h) path))

(defmacro with-print [{:keys [level length]} & body]
  `(let [before-level# ~'*print-level*
         before-length# ~'*print-length*
         lvl#    ~level
         length# ~length]
    (do (set! ~'*print-level*  lvl#)
        (set! ~'*print-length* length#)
        ~@body
        (set! ~'*print-level*  before-level#)
        (set! ~'*print-length* before-length#))))

;;textual, printed version
;;if we use pprint, we get killed here.
(defn print-history [h path]
  (with-print {}
    (with-open [writer (clojure.java.io/writer path)]
      (binding [*out* writer]
        (let [{:keys [init patches]} (patch-history h)]
          (println "{:init")
          (pr init)
          (println " :patches")
          (doseq [[t patches] patches]
            (println "[" t)
            (pr patches)
            (println "]"))
          (println "}"))))))

(defn print-patches [h path]
  (with-print {}
    (with-open [writer (clojure.java.io/writer path)]
      (binding [*out* writer]
        (let [{:keys [init patches]} (patch-history h)]
          (println "{:patches ")
          (doseq [[t patches] patches]
            (println "[" t)
            (pr patches)
            (println "]"))
          (println "}"))))))

;;hmmm...can we actually slurp this up?  It's 28 mb...so maybe...
;;ahh...this poorly named...
(defn string->history [path]
  (println [:warning 'read-history "you're using read-string, vs. clojure.edn/read-string"])
  (read-string (slurp path)))

(defn read-history! [path]
  (let [{:keys [init patches]} (ser/thaw-from path)
        store  (atom (second init))]
    (into [init]
          (map (fn [[t patch]]
                 (let [prev @store
                       nxt  (diff/patch->store prev patch)
                       _    (reset! store nxt)]
                   [t nxt]))
               patches)
          )))

;;Gross Event and Behavioral Logging
;;==================================

;;spits a log of all the events passing through.
(defn spit-log
  ([h root nm]
   (println [:logging-to (str root nm)])
   (with-open [wrtr (clojure.java.io/writer (str root nm))]
     (binding [*out* wrtr]
       (core/debugging
        (doseq [hd h]
          )
        ))))
  ([h root] (spit-log h root "events.txt")))

;;spits a verbose log of all the events and
;;behavioral updates that are performed...
(defn spit-log!
  ([h root nm]
   (println [:logging-to (str root nm)])
   (with-open [wrtr (clojure.java.io/writer (str root nm))]
     (binding [*out* wrtr]
       (core/debugging!
        (doseq [hd h]
          )
        ))))
  ([h root] (spit-log! h root "events.txt")))
