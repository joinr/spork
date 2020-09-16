;;cellular is a library that evolved after patterning 
;;everything off of clojure's idiomatic (and inefficient!) 
;;functions for nested updates of maps.  While beautiful and 
;;conceptually simple, doing nested assocs and dissocs and 
;;update-in type stuff is really a bummer if you're trying to 
;;touch multiple "places" in a nested map in a large transaction.
;;The downside was that I had a large base of existing code that 
;;expected nested structures.  This was fine - for small transactions,
;;but for larger or more frequent transactions, it just didn't cut the
;;mustard.  Cellular operates off the notion of "cells" and provides 
;;a few convenience macros to help with things.  The big idea is that 
;;we can still have a nested structure with all of its goodies.  
;;We can still access it identically (if we want) to the way that 
;;we do with persistent maps and the like.  However, we maintain 
;;a set of "places", specifically cells, that are temporarily 
;;mutable containers.  A cell is really just a wrapper around 
;;the container that "was" stored at a "place" - as defined by 
;;a path of keys - to a location in the nested map. 
;;When we engage in a transaction, all we have to do is replace the 
;;places we want to mutate - temporarily - with cells.   Since cells 
;;implement functionality for all of clojure's structures, calling
;;code that expects a nested structure will just "work" like normal.
;;However, if we want to provide optimized implementations, we can 
;;take advantage of the the existence of the cells and, rather than 
;;traverseing the nested structure, we can operate directly on the 
;;values in the cell - exactly like we would in a mutable / imperative 
;;datastructure.  This alleviates a significant cost in hashing and 
;;garbage allocation, since we aren't going into and out of multiple 
;;structures multiple times.  As a clean up step, to facilitate
;;purity, we simply compute the resulting structure by replacing the 
;;associated cells with their current values in a finalization step.

;;Update 2017 - Needs further testing, and possibly revisiting some of
;;the lower-level implementation details.  The idea is pretty cool,
;;and there are definite benefits to avoiding associng and using
;;localized mutation (almost like haskells state-thread cells/refs).
;;We're not currently using this in production, marked as EXPERIMENTAL.
(ns spork.util.cellular
  (:require [spork.util [metaprogramming :as util]
                        [collections :refer :all]
             [general :as gen]
             [tags    :as tag]
             [table   :as tbl]])
   (:refer-clojure :exclude
     [into update update-in assoc-in assoc 
      dissoc dissoc-in conj disj contains?]))

;;Possible use of dynamic context for later....
;;Dunno if I want to do this yet..
;(def ^:dynamic *ctx* nil)
;; (defmacro defcontextual 
;;   "Define functions that work with or without a name context, ctx.
;;    Yields functions that an extra arity fun, taking ctx as a final argument.
;;    If no contextual function is provided, uses the var bound to marathon.sim.core/*ctx*
;;    Designed to eliminate boilerplate, and allow us to maintain contextual functions 
;;    that can use dynamic binding.  Note -> the main reason to use this, in lieu of."
;;   [name args & expr]
;;   (assert (vector? args) "arguments must be a vector")
;;   (let [rawargs (if (= (last args) 'ctx)
;;                     (subvec args 0 (dec (count args)))
;;                     args)]                                
;;   `(defn ~name ([~@rawargs] ~@expr)
;;                ([~@(conj rawargs 'ctx)] ~@expr))))


;;#still working on api for mutating transactions...
;;We can just cache our cells...
;;When we call inside, it means we want 
;;to operate inside the data structure via mutation 
;;if possible.  If we have a reference to the 
;;path we're after, then we use that, 
;;else fall back to deep-update.  
;; (within {:a {:b 2}} [:a]
;;         (assoc :c 3))
;; => (deep-update {:a {:b 2}} [:a]
;;                 assoc :c 3)
;; ;;{:a {:b 2 :c 3}}
;; (within {:a <cell>{:b 2}}
;;         (assoc :c 3))
;; =>
;; (if-let [ab (get *cells* :ab)]
;;   (do (assoc ab)
;;       input)
;;   (gen/deep-update {:a {:b 2}} [:a] assoc :c 3))

;; (def ^:dynamic *cells*)
;; (defn get-cell [path]
;;   (


;;#Cells
;;A cell acts as a container for maps, vectors, sets.
;;The difference between a cell and a raw atom is 
;;that the cell maintains a reference to what it contains, 
;;and applies operations directly to the item.  Any 
;;operation other than a read will result in 
;;an update to the state of the cell. 
;;Thus, cells support fast dereferencing, and 
;;implement protocols for operating on 
;;maps, vectors, etc. The difference is, 
;;their return value is the updated cell on 
;;operations like assoc and conj....

;;The intent is to use cells sparingly.
(defprotocol IAlterable 
  (set-altered [obj])
  (get-altered [obj]))

(defprotocol ICell 
  (swap-cell!  [c f]
               [c f x]
               [c f x y]
               [c f x y & args])
  (reset-cell! [c v]))
             
(deftype cell [^clojure.lang.Atom contents origin]
  IAlterable 
  (set-altered [obj] obj) ;(do (reset! altered true) obj))
  (get-altered [obj] (not (identical? origin @contents)))
  Object
  (toString [this] (str @contents))
  clojure.lang.ISeq
  (first [this] (first @contents))
  (next  [this] (next @contents))
  (more  [this]  (rest @contents))
  clojure.lang.IPersistentCollection
  (empty [this]  (cell. (atom nil) false))
  (equiv [this that] (= @contents that))
  ;;Note -> if we don't implement this, vector equality doesn't work both ways!
  java.util.Collection  
  (iterator [self]    (clojure.lang.SeqIterator. (seq  self)))  
  (size     [self]    (count self))  
  (^objects toArray  [self]    (.toArray (seq self)))
  clojure.lang.Seqable
  (seq [this]  (seq @contents))
  clojure.lang.Counted
  (count [this] (count @contents))
  clojure.lang.IPersistentVector
  (cons [this a] (do (swap! contents spork.util.collections/conj a) this))
  (length [this]  (count @contents))
  (assocN [this index value] (do (swap! contents assoc  index value) this))
  clojure.core.protocols/IKVReduce
  (kv-reduce [amap f init] (clojure.core.protocols/kv-reduce @contents f init))
  clojure.core.protocols/CollReduce 
  (coll-reduce [coll f]     (clojure.core.protocols/coll-reduce @contents f))
  (coll-reduce [coll f val] (clojure.core.protocols/coll-reduce @contents f val))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (get @contents  k))
  (valAt [this k not-found] (get @contents k not-found))  
  clojure.lang.IPersistentMap
  (assoc [this k v]    (do (swap! contents spork.util.collections/assoc k v) this))
  (equals [this o]     (or (identical? @contents o) (= @contents o)))  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (spork.util.collections/contains?  @contents k))
  (entryAt [this k]     (throw (Exception. "Unsupported op .entryAt on cells")))
  ;without implements (dissoc pm k) behavior
  (without [this k] (let [m @contents]
                      (do (reset! contents (spork.util.collections/dissoc m k))
                          this))) 
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (get @contents k))
  (invoke [this k not-found] (get @contents k not-found))
  clojure.lang.IPersistentStack
  (pop  [this] (do (swap! contents pop) this))
  (peek [this] (peek @contents))
  clojure.lang.Indexed
  (nth [this i] (nth @contents i))
  (nth [this i not-found] (nth @contents i not-found))
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] (meta @contents))
  (withMeta [this m] (do (swap! contents with-meta m) this))      
  clojure.lang.Reversible
  (rseq [this]  (rseq @contents))
  java.io.Serializable ;Serialization comes for free with the other
                       ;stuff.
  clojure.lang.IDeref
  (deref [this] @contents)
  clojure.lang.IRef
  (setValidator [this vf] (do (.setValidator contents vf) this))
  (getValidator [ths]     (.getValidator contents))
  (getWatches   [this]    (.getWatches contents)) 
  (addWatch     [this key callback] (do (.addWatch contents key callback) this)) 
  (removeWatch  [this key] (do (.removeWatch contents key) this))  
  ICell
  (swap-cell!  [c f]     (do  (swap! contents f)      c))
  (swap-cell!  [c f x]   (do  (swap! contents f x)  c))
  (swap-cell!  [c f x y] (do  (swap! contents f x y)  c))
  (swap-cell!  [c f x y & args] (do  (apply swap! contents f x y args)  c))
  (reset-cell! [c v] (do (reset! contents v) c))
  spork.util.tags.ITagStore
  (get-tags [store subject]  (tag/get-tags @contents subject))
  (get-subjects [store tag]  (tag/get-subjects @contents tag))
  (add-tag [store tag] (do (swap! contents tag/add-tag tag) store))
  (add-subject [store subject] (do (swap! contents tag/add-subject subject) store))
  (drop-tag [store tag]  (do (swap! contents tag/drop-tag tag) store))
  (drop-subject [store subject] (do (swap! contents tag/drop-subject subject) store))
  (tag-subject [store tag subject] (do (swap! contents tag/tag-subject tag subject) store))
  (untag-subject [store tag subject]  (do (swap! contents tag/untag-subject tag subject) store)))

(deftype mcell [^:unsynchronized-mutable contents origin]
  IAlterable 
  (set-altered [obj] obj);(do (set! alt true) obj)) 
  (get-altered [obj] (not (identical? contents origin)))
  Object
  (toString [this] (str contents))
  clojure.lang.ISeq
  (first [this] (first contents))
  (next  [this] (next contents))
  (more  [this]  (rest contents))
  clojure.lang.IPersistentCollection
  (empty [this]  (cell. nil false))
  (equiv [this that] (= contents that))
  ;;Note -> if we don't implement this, vector equality doesn't work both ways!
  java.util.Collection  
  (iterator [self]    (clojure.lang.SeqIterator. (seq  self)))  
  (size     [self]    (count self))  
  (^objects toArray  [self]    (.toArray (seq self)))
  clojure.lang.Seqable
  (seq [this]  (seq contents))
  clojure.lang.Counted
  (count [this] (count contents))
  clojure.lang.IPersistentVector
  (cons [this a] (do (set! contents (spork.util.collections/conj contents a)) this))
  (length [this]  (count contents))
  (assocN [this index value] (do (set! contents (assoc contents  index value)) this))
  clojure.core.protocols/IKVReduce
  (kv-reduce [amap f init] (clojure.core.protocols/kv-reduce contents f init))
  clojure.core.protocols/CollReduce 
  (coll-reduce [coll f]     (clojure.core.protocols/coll-reduce contents f))
  (coll-reduce [coll f val] (clojure.core.protocols/coll-reduce contents f val))
  clojure.lang.ILookup
  ; valAt gives (get pm key) and (get pm key not-found) behavior
  (valAt [this k] (get contents  k))
  (valAt [this k not-found] (get contents k not-found))  
  clojure.lang.IPersistentMap
  (assoc [this k v]    (do (set! contents (spork.util.collections/assoc contents k v)) this))
  (equals [this o] (or (identical? contents o) (= contents o)))  
  ;containsKey implements (contains? pm k) behavior
  (containsKey [this k] (spork.util.collections/contains? contents k))
  (entryAt [this k]     (throw (Exception. "Unsupported op .entryAt on mcell" )))
  ;without implements (dissoc pm k) behavior
  (without [this k]  (do (set! contents (spork.util.collections/dissoc contents k))
                         this)) 
  clojure.lang.IFn
  ;makes lex map usable as a function
  (invoke [this k] (get contents k))
  (invoke [this k not-found] (get contents k not-found))
  clojure.lang.IPersistentStack
  (pop  [this] (do (set! contents (pop contents)) this))
  (peek [this] (peek contents))
  clojure.lang.Indexed
  (nth [this i] (nth contents i))
  (nth [this i not-found] (nth contents i not-found))
  clojure.lang.IObj
  ;adds metadata support
  (meta [this] (meta contents))
  (withMeta [this m] (do (set! contents (with-meta contents m)) this))      
  clojure.lang.Reversible
  (rseq [this]  (rseq contents))
  java.io.Serializable ;Serialization comes for free with the other
                       ;stuff.
  clojure.lang.IDeref
  (deref [this] contents)
  ICell
  (swap-cell! [c f]     (do  (set! contents (f contents))      c))
  (swap-cell! [c f x]   (do  (set! contents (f contents x))  c))
  (swap-cell! [c f x y] (do  (set! contents (f contents x y))  c))
  (swap-cell! [c f x y & args] (do  (set! contents (apply f contents x y args))  c))
  (reset-cell! [c v] (do (set! contents v) c))
  spork.util.tags.ITagStore
  (get-tags [store subject]  (tag/get-tags contents subject))
  (get-subjects [store tag]  (tag/get-subjects contents tag))
  (add-tag [store tag] (do (set! contents (tag/add-tag contents tag)) store))
  (add-subject [store subject] (do (set! contents (tag/add-subject contents subject)) store))
  (drop-tag [store tag]  (do (set! contents (tag/drop-tag contents tag)) store))
  (drop-subject [store subject] (do (set! contents (tag/drop-subject contents subject)) store))
  (tag-subject [store tag subject] (do (set! contents (tag/tag-subject contents tag subject)) store))
  (untag-subject [store tag subject]  (do (set! contents (tag/untag-subject contents tag subject)) store)))

(defn inc! [^clojure.lang.Atom x] (swap! x inc))
(defn ->counter 
  ([n] (atom n))
  ([]  (atom 0)))


(defn altered? [c] (get-altered c))

;;Allows us to see cells transparently.
(defmethod print-method cell [s ^java.io.Writer  w] (.write w (str "<#cell: " @s ">")))
(defmethod print-method mcell [s ^java.io.Writer  w] (.write w (str "<#mcell: " @s ">")))
(defn ->cell
  "Wraps val in a cell, which allows transparent access to val though it's inside
   an atom.  Acts like a super reference.  Also keeps track of whether the 
   atom was ever altered.  Can check the status of the cell using get-altered"
  [val]  
  (if (= (type val) cell)
      val
      (let [contents (atom val)]
         (cell. contents val))))

(defn ->mcell
  "Wraps val in a cell, which allows transparent access to val though it's inside
   an atom.  Acts like a super reference.  Also keeps track of whether the 
   atom was ever altered.  Can check the status of the cell using get-altered"
  [val]  
  (if (= (type val) cell)
      val
      (mcell. val val)))

(defn try-transient [itm]
  (if (instance? clojure.lang.IEditableCollection itm)
    (transient itm)
    itm))
    
(defn transient-cells [xs]
  (doseq [x xs]
    (swap-cell! x try-transient)))
(defn persistent-cells! [xs]
  (doseq [x xs]
    (swap-cell! x persistent!)))

(defmacro with-transient-cells [symbs & expr] 
  `(let [~'_ (transient-cells ~symbs)
         res# ~@expr
         ~'_ (persistent-cells! ~symbs)]
     res#))



(comment ;examples using cells and not...
  (def nested-structure {:a {:b {:c 2}
                             :e {:f []}}})
  (defn simple-example [& {:keys [n] :or {n 10}}]
    (reduce (fn [acc n]
              (-> acc
                  (update-in [:a :b :c] inc)
                  (update-in [:a :e :f] conj n)))
            nested-structure (range n)))

  (defn simple-example! [& {:keys [n] :or {n 10}}]
    (let [abc (atom (get-in nested-structure [:a :b :c]))
          aef (transient (get-in nested-structure [:a :e :f]))]
      (do  (dotimes [i n]
             (swap! abc inc)
             (conj! aef n))
           (-> nested-structure 
               (assoc-in [:a :b :c] @abc)
               (assoc-in [:a :e :f] (persistent! aef))))))           
    
  (defn simple-cell-example [& {:keys [n mutable?] :or {n 10}}]
    (with-cells [{abc [:a :b :c]
                  aef [:a :e :f]
                  :as nested
                  :cellfn (if mutable? ->mcell ->cell)}     nested-structure]
      (do  (transient-cells [aef])
           (dotimes [i n] 
             (do (swap-cell! abc inc)
                 (conj aef i)))
           (persistent-cells! [aef])
           (update-nested!))))

  (defn simple-cell-example2 [& {:keys [n mutable?] :or {n 10}}]
    (with-cells [{abc [:a :b :c]
                  aef [:a :e :f]
                  :as nested
                  :cellfn (if mutable? ->mcell ->cell)}     nested-structure]
      (with-transient-cells [aef]
        (dotimes [i n] 
          (do (swap-cell! abc inc)
              (conj aef i))))
      (update-nested!))))


;;#Note -> early experiments lead me to believe we're missing the mark 
;;by not using transients or actually mutable structures inside of 
;;cells.  For now, we'll stick with the pure interface and see 
;;what the performance hits are.

(defn unpair [xs]
  (reduce (fn [acc [x y]] 
            (-> acc (conj x) (conj y))) [] xs))

(defn atom? [x] (= (type x) clojure.lang.Atom))

(defn transient? [x]
  (instance? clojure.lang.ITransientCollection x))

;;#Atomic access to avoid deep associating
;;we can have a variant of this guy, with-transients....
(defmacro with-atoms 
  "Macro that follows an idiom of extract-and-pack.
   We define paths into a nested associative structure, 
   and create a context in which the conceptual places 
   those paths point to are to be treated as mutable 
   containers - atoms.  Inside of expr, these 
   places take on the symbol names defined by the 
   path-map, a map of path-name to sequences of keys 
   inside the associative strucure symb.  From there, 
   we use the default clojure idioms for operating on atoms, 
   namely reset!, swap!, and (deref x) or @x to get at 
   values.  After we're done, much like transients, 
   we collect the current values of the named paths
   and push them into the associative structure, 
   returning a persisent structure as a result.  
   This is akin to automatically calling (persistent!)
   on a transient structure."
  [[symb path-map] & expr]
  (let [state   (symbol "*state*")
        updates (for [[s path] path-map] 
                  `(assoc-in ~path (deref ~s)))]
    `(let [~@(unpair (for [[s path] path-map]
                        [s `(let [res# (get-in ~symb ~path)]
                              (if (atom? res#) 
                                res# 
                                (atom res#)))]))
           ~state (reduce-kv (fn [acc# s# p#] 
                               (assoc-in acc# p# s#))
                          ~symb
                          ~path-map)]
       (do ~@expr
         (-> ~state 
             ~@updates)))))
     
(defmacro unpack-if [m pred path v]
  `(if ~pred
     (assoc-in ~m ~path (deref ~v))
     (deref ~v)))


(defn update-cells [m cell-paths]
  (reduce (fn [acc [cell path]]            
            (if (altered? cell)
                (spork.util.collections/assoc-in acc path @cell)
                acc))
          m cell-paths)) 

(defn create-cells 
  [m path-map]
  (reduce-kv 
   (fn [acc s p] 
     (spork.util.collections/assoc-in acc p s))
   m
   path-map))          
            
(defmacro with-cells
  ""
  [[path-map symb] & expr]
  (let [state    (get path-map :as '*state*)
        cell-fn  (get path-map :cellfn 'marathon.sim.core/->cell)
        path-map (->  path-map (dissoc :as) (dissoc :cellfn))
        update-state! (symbol (str "update-" state "!"))]
    `(let [~@(unpair (for [[s path] path-map]
                        [s `(let [res# (get-in ~symb ~path)]
                              (~cell-fn res#))]))
           ~state (reduce-kv (fn [acc# s# p#] 
                               (spork.util.collections/assoc-in acc# p# s#))
                             ~symb
                             ~path-map) ;;packs the path...
           ~update-state! (fn  ([]   (update-cells ~state ~path-map))
                               ([m#] (update-cells  m# ~path-map)))]
       ~@expr)))

(defmacro with-transients 
  "Macro that follows an idiom of extract-and-pack.
   We define paths into a nested associative structure, 
   and create a context in which the conceptual places 
   those paths point to are to be treated as mutable 
   containers - atoms.  Inside of expr, these 
   places take on the symbol names defined by the 
   path-map, a map of path-name to sequences of keys 
   inside the associative strucure symb.  From there, 
   we use the default clojure idioms for operating on transients, 
   namely assoc!, dissoc!, conj!, disj!, get, nth to access. 
   After we're done, we coerce the transients into persistent 
   values, and push them into the associative structure, 
   returning a persisent structure as a result.  
   This is akin to automatically calling (persistent!)
   on a transient structure."
  [[symb path-map] & expr]
  (let [state   (symbol "*state*")
        updates (for [[s path] path-map] 
                  `(assoc-in ~path (persistent! ~s)))]
    `(let [~@(unpair (for [[s path] path-map]
                        [s `(let [res# (get-in ~symb ~path)]
                              (if (transient? res#) 
                                res# 
                                (transient res#)))]))
           ~state (reduce-kv (fn [acc# s# p#] 
                            (assoc-in acc# p# s#))
                          ~symb
                          ~path-map)]
       (do ~@expr
           (-> ~state 
               ~@updates)))))

;;example usage
(comment ;testing
  (require '[clojure.core.reducers :as r])

  ;;much faster....more efficient.
  (def xs (r/range 1000000))
;  (def xs (vec (range 1000000)))

  (defn slow-test []
    (time (dotimes [i 1] 
            (core/with-cells [{dmap [:demandmap] 
                               :as state}         dstore] 
              (let [newmap  (reduce (fn [acc n] (assoc acc n n)) dmap xs)] (update-state!))))))

  (defn fast-test []    
    (time (dotimes [i 1] 
            (core/with-cells [{dmap [:demandmap] 
                               :as state}         dstore] 
              (let [newmap (core/reset-cell! dmap 
                              (persistent! 
                               (reduce (fn [acc n] (assoc! acc n n)) 
                                 (transient @dmap) xs)))] (update-state!))))))


  ;;This costs us a bit, because we're not slamming directly on the transient.
  (defn fast-test! []    
    (time (dotimes [i 1] 
            (core/with-cells [{dmap [:demandmap] 
                               :as state}         dstore] 
             (core/with-transient-cells [dmap]
               (reduce (fn [acc n] (assoc acc n n)) 
                       dmap  xs)) 
             (update-state!)))))

  (defn add-demand [dstore demandname d]
    (core/with-cells [{dmap          [:demandmap]
                       activations   [:activations]
                       deactivations [:deactivations]
                       :as state}                     dstore]
      (do (assoc dmap demandname d)
          (assoc activations   (:startday d) demandname)
          (assoc deactivations (+ (:startday d) (:duration d)) demandname)
          (update-state!))))

  (defn add-demand! [dmap activations deactivations  d]
    (let [n (get d :name)
          start (get d :startday)]
      (do (core/swap-cell!  dmap          assoc! n  d)
          (core/swap-cell!  activations   assoc! start n)
          (core/swap-cell!  deactivations assoc! (+ start (get d :duration)) n))))

  (defn add-demand!! [dmap activations deactivations  d]
    (let [n (get d :name)
          start (get d :startday)]
      (do (swap-cell!  dmap          spork.util.collections/assoc n  d)
          (swap-cell!  activations   spork.util.collections/assoc start n)
          (swap-cell!  deactivations spork.util.collections/assoc (+ start (get d :duration)) n))))

  (defn add-demand!!! [dmap activations deactivations  d]
    (let [n (get d :name)
          start (get d :startday)]
      (do (assoc  dmap           n  d)
          (assoc  activations    start n)
          (assoc  deactivations  (+ start (get d :duration)) n))))

  (defn test-demands [n] (r/map (fn [n] {:name n :startday n :duration  55}) (range-reducer n)))
  (defn add-demands! [dstore ds]
    (with-cells [{dmap          [:demandmap]
                       activations   [:activations]
                       deactivations [:deactivations]
                       :as state}                      dstore] 
      (do (with-transient-cells [dmap activations deactivations]
            (reduce (fn [acc d] 
                      (add-demand!!! dmap activations deactivations d))
                    nil ds)
            (update-state!)))))

  ;;nested cell defs also work fine.
  (defn add-demands!! [dstore ds]
    (with-cells [{dmap          [:demandmap]
                       activations   [:activations]
                       deactivations [:deactivations] 
                       :as state1}              dstore]
      (with-cells [{dmap          [:demandmap]
                         activations   [:activations]
                         deactivations [:deactivations]
                         :as state2}            state1]                                 
        (do (with-transient-cells [dmap activations deactivations]
              (reduce (fn [acc d] 
                        (add-demand!!! dmap activations deactivations d))
                      nil ds)
              (update-state2!))))))
)
