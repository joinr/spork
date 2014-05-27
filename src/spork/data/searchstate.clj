;;Module for defining generic graph search data.  Uses the generic fringe 
;;structures from spork.data.fringe, to provide a generic search state.
;;We'll typically use this with priority queues, for djikstra's algorithm and 
;;the bellman-ford algorithm, but depth-first, breadth-first, and random 
;;searching are also supported.
(ns spork.data.searchstate
  (:require [spork.protocols [core :as generic]]
            [spork.data      [priorityq :as pq]
                             [fringe :as fr]
                             [mutable :as m]])
  (:import  [java.util HashMap ArrayList]))

;;This is slow for recovering paths.
;;We don't need to use persistent stuff to recover paths, it's a small 
;;opportunity to mutably recover paths inside of a lazy computation. 


;(m/defmutable branch [items])
  
;;These allows us to use complex keys in our spt.
(definline ^ArrayList ->branch [^ArrayList x]  `(doto (ArrayList.) (.add ~x)))
(definline branch? [x] `(identical? (class ~x) ~'java.util.ArrayList))
(defn ^ArrayList push-branch [^ArrayList b x]  (doto b (.add x)))

(defmacro get-field [hint obj field]  `(.valAt ~(vary-meta obj assoc :tag hint) ~field))

;;__TODO__ Use transient operations for updating the search state.
;; (defn update-search [state shortest distance fringe]
;;   (-> state
;;       (assoc :shortest shortest)
;;       (assoc :distance distance)
;;       (assoc :fringe fringe)))

(definline update-search [state shortest distance fringe]
  `(-> ~state
       (assoc :shortest ~shortest)
       (assoc :distance ~distance)
       (assoc :fringe ~fringe)))

(definline estimating-conj [estimator fringe sink w target]
  `(generic/conj-fringe ~fringe ~sink (+ ~w (~estimator ~sink ~target))))

;; (defn- estimating-conj [estimator fringe sink w target]
;;   (generic/conj-fringe fringe sink (+ w (estimator sink target))))


;;Experimental performance enhancements...

(definline conj-fringe* [state sink w] 
  `(if-let [e# (:estimator ~state)]
    (estimating-conj e#   (:fringe ~state) ~sink ~w (:target ~state))
    (generic/conj-fringe (:fringe ~state) ~sink ~w)))


;; (defn- conj-fringe*    [state sink w] 
;;   (if-let [e (:estimator state)]
;;     (estimating-conj e   (:fringe state) sink w (:target state))
;;     (generic/conj-fringe (:fringe state) sink w)))

;;When we discover a new path via from source to sink, we add sink to the 
;;shortest path tree, register the distance, and add sink to the fringe.
(definline new-path*   
  [source sink w state] 
  `(update-search ~state 
                  (assoc (:shortest ~state) ~sink ~source)
                  (assoc (:distance ~state) ~sink ~w)
                  (conj-fringe* ~state ~sink ~w)))

;; (defn- new-path*   
;;   "When we discover a new path via from source to sink, we add sink to the 
;;    shortest path tree, register the distance, and add sink to the fringe."
;;   [source sink w {:keys [shortest distance fringe] :as state}]
;;     (update-search state (assoc shortest sink source)
;;                          (assoc distance sink w)
;;                          (conj-fringe* state sink w)))


(definline shorter-path* 
  [source sink wnew wpast state]
  `(update-search ~state (assoc (:shortest ~state) ~sink ~source) ;new spt
                  (assoc (:distance ~state) ~sink ~wnew)  ;shorter distance
                  (conj-fringe* ~state ~sink ~wnew)))

;; (defn- shorter-path*
;;   "When a shorter path is found to a node already on the fringe, we update the 
;;    SPT, distance, and add the sink back to the fringe based on the new path."   
;;   [source sink wnew wpast {:keys [shortest distance fringe] :as state}]
;;     (update-search state (assoc shortest sink source) ;new spt
;;                          (assoc distance sink wnew)  ;shorter distance
;;                          (conj-fringe* state sink wnew)))

(definline equal-path* 
  [source sink state]
  `(let [shortest# (:shortest ~state)
         current#  (get shortest# ~sink)
         newspt#   (assoc shortest# ~sink 
                     (-> (if (branch? current#) current# (->branch current#))
                         (push-branch ~source)))]                 
     (update-search ~state newspt# (:distance ~state) (:fringe ~state))))

;; (defn- equal-path* 
;;   "When we discover equivalent paths, we conj them onto the shortest path tree.
;;    Note, if a better path is found, the other paths will be eliminated."
;;   [source sink {:keys [shortest distance fringe] :as state}]
;;     (let [current (get shortest sink)
;; 		      context (if (branch? current) current (->branch current))
;; 		      newspt  (assoc shortest sink (conj context source))]                 
;; 		     (update-search state newspt distance fringe)))
  

;;OLD an obsolete.
;A general container for any abstract graph search.
;Might shift to a simple map here....not sure yet.
(defrecord searchstate 
  [startnode targetnode shortest distance fringe estimator visited]
  generic/IGraphSearch
  (set-estimator [state e] (searchstate. startnode targetnode shortest distance fringe e visited))
  (set-target   [state nd] (searchstate. startnode nd shortest distance fringe estimator visited))
  (set-start    [state nd] (searchstate. nd targetnode shortest distance fringe estimator visited))
  (new-path     [state source sink w] (new-path* source sink w state))           
  (shorter-path [state source sink wnew wpast]
    (shorter-path* source sink wnew wpast state))
  (equal-path   [state source sink] (equal-path* source sink state))
  (best-known-distance   [state nd] (get distance nd))
  (conj-visited [state source] 
    (assoc state :visited (conj visited source)))
  generic/IFringe 
	  (conj-fringe [state n w] (assoc state :fringe 
                                   (generic/conj-fringe fringe n w)))
	  (next-fringe [state]  (generic/next-fringe fringe))
	  (pop-fringe  [state]  (assoc state :fringe (generic/pop-fringe fringe))))                 

;;A macro to simplify update semantics using record construction.
(defmacro update-searchstate 
  [& {:keys [startnode targetnode shortest distance fringe estimator visited]
      :or {startnode 'startnode shortest 'shortest distance 'distance fringe 'fringe estimator 'estimator visited 'visited}}]
  `(~'searchstate2. 
       ~startnode 
       ~targetnode 
       ~shortest 
       ~distance
       ~fringe
       ~estimator
       ~visited))

;;A persistent implementation of the searchstate.  Still probably
;;slow, and will cause allocation...
(defrecord searchstate2 
  [startnode targetnode shortest distance fringe estimator visited]
  generic/IGraphSearch
  (set-estimator [state e] (searchstate2. startnode targetnode shortest distance fringe e visited))
  (set-target   [state nd] (searchstate2. startnode nd shortest distance fringe estimator visited))
  (set-start    [state nd] (searchstate2. nd targetnode shortest distance fringe estimator visited))
  (new-path     [state source sink w]
    (update-searchstate  
     :shortest (assoc shortest sink source)
     :distance (assoc distance sink w)
     :fringe   (if-let [e estimator]
                 (estimating-conj e   fringe sink w targetnode)
                 (generic/conj-fringe fringe sink w))))
  (shorter-path [state source sink wnew wpast]
    (update-searchstate
         :shortest (assoc shortest sink source) ;new spt
         :distance (assoc distance sink wnew)   ;shorter distance
         :fringe   (if-let [e estimator]
                     (estimating-conj e   fringe sink wnew targetnode)
                     (generic/conj-fringe fringe sink wnew))))        
  (equal-path   [state source sink] 
     (let [current  (get shortest sink)]
       (update-searchstate 
          :shortest (assoc! shortest sink 
                        (-> (if (branch? current) current (->branch current))
                            (push-branch source))))))
  (best-known-distance   [state nd] (get distance nd))
  (conj-visited [state source] 
    (update-searchstate :visited (conj visited source)))
  generic/IFringe 
  (conj-fringe [state n w] 
    (update-searchstate :fringe (generic/conj-fringe fringe n w)))
  (next-fringe [state]  (generic/next-fringe fringe))
  (pop-fringe  [state]  (update-searchstate :fringe (generic/pop-fringe fringe))))

;;A persistent mutable implementation of searchstate using persistent
;;data structures.  Updates are slower, but still in general pretty quick.
                             
(m/defmutable searchstate3
  [startnode targetnode shortest distance fringe estimator visited]
  generic/IGraphSearch
  (set-estimator [state e] (do (set! estimator e) state))
  (set-target   [state nd] (do (set! targetnode  nd) state))
  (set-start    [state nd] (do (set! startnode  nd ) state))   
  (new-path     [state source sink w]
    (do (set! shortest (assoc shortest sink source))
        (set! distance (assoc distance sink w))
        (set! fringe   
              (if-let [e estimator]
                (estimating-conj e   fringe sink w targetnode)
                (generic/conj-fringe fringe sink w))) 
        state))
  (shorter-path [state source sink wnew wpast]
     (do (set! shortest (assoc shortest sink source)) ;new spt
         (set! distance (assoc distance sink wnew))   ;shorter distance
         (set! fringe   
               (if-let [e estimator]
                 (estimating-conj e   fringe sink wnew targetnode)
                 (generic/conj-fringe fringe sink wnew))) 
         state))  
  (equal-path   [state source sink] 
     (let [current  (get shortest sink)]
       (do (set! shortest (assoc shortest sink 
                                  (-> (if (branch? current) current (->branch current))
                                      (push-branch source)))) 
           state)))
  (best-known-distance   [state nd] (get distance nd))
  (conj-visited [state source] 
      (do (set! visited (conj visited source))
          state))
  generic/IFringe 
  (conj-fringe [state n w] 
               (do (set! fringe (generic/conj-fringe fringe n w)) 
                   state))
  (next-fringe [state]  (generic/next-fringe fringe))
  (pop-fringe  [state]  (do (set! fringe (generic/pop-fringe fringe)) state)))

(m/defmutable msearchstate 
  [startnode targetnode  
   ^spork.data.mutable.mutmap shortest 
   ^spork.data.mutable.mutmap distance 
   fringe estimator visited]
  generic/IGraphSearch
  (set-estimator [state e] (do (set! estimator e) state))
  (set-target   [state nd] (do (set! targetnode  nd) state))
  (set-start    [state nd] (do (set! startnode  nd ) state))   
  (new-path     [state source sink w]
    (do (set! shortest (assoc! shortest sink source))
        (set! distance (assoc! distance sink w))
        (set! fringe   
              (if-let [e estimator]
                (estimating-conj e   fringe sink w targetnode)
                (generic/conj-fringe fringe sink w))) 
        state))
  (shorter-path [state source sink wnew wpast]
     (do (set! shortest (assoc! shortest sink source)) ;new spt
         (set! distance (assoc! distance sink wnew))   ;shorter distance
         (set! fringe   
               (if-let [e estimator]
                 (estimating-conj e   fringe sink wnew targetnode)
                 (generic/conj-fringe fringe sink wnew))) 
         state))  
  (equal-path   [state source sink] 
     (let [current  (get shortest sink)]
       (do (set! shortest (assoc! shortest sink 
                                  (-> (if (branch? current) current (->branch current))
                                      (push-branch source)))) 
           state)))
  (best-known-distance   [state nd] (get distance nd))
  (conj-visited [state source] 
      (do (set! visited (conj visited source))
          state))
  generic/IFringe 
  (conj-fringe [state n w] 
               (do (set! fringe (generic/conj-fringe fringe n w)) 
                   state))
  (next-fringe [state]  (generic/next-fringe fringe))
  (pop-fringe  [state]  (do (set! fringe (generic/pop-fringe fringe)) state)))    

;;This is the state we're currently using.
(m/defmutable msearchstate2 
  [startnode targetnode  
   ^java.util.HashMap shortest 
   ^java.util.HashMap distance 
   fringe 
   estimator 
   ^java.util.ArrayList visited
   multipath]
  generic/IGraphSearch
  (set-estimator [state e] (do (set! estimator e) state))
  (set-target   [state nd] (do (set! targetnode  nd) state))
  (set-start    [state nd] (do (set! startnode  nd ) state))            
  (new-path     [state source sink w]
    (do (.put shortest sink source)
        (.put distance sink w)
        (set! fringe   
              (if-let [e estimator]
                (estimating-conj e   fringe sink w targetnode)
                (generic/conj-fringe fringe sink w)))
        state))
  (shorter-path [state source sink wnew wpast]
     (do (.put shortest sink source) ;new spt
         (.put distance sink wnew)   ;shorter distance
         (set! fringe   
               (if-let [e estimator]
                 (estimating-conj e   fringe sink wnew targetnode)
                 (generic/conj-fringe fringe sink wnew))) 
         state))  
  (equal-path   [state source sink] 
     (do (when multipath
           (let [current  (.get shortest sink)]
             (do (.put shortest sink 
                       (-> (if (branch? current) current (->branch current))
                           (push-branch source))))))
         state))
  (best-known-distance   [state nd] (.get distance nd))
  (conj-visited [state source] 
      (do (.add visited source)
          state))
  generic/IClearable
  (-clear [state]  (do (doto shortest (.clear) (.put startnode startnode))
                       (doto distance (.clear) (.put startnode 0))
                       (set! fringe (generic/clear! fringe))
                       state))                       
  generic/IFringe 
  (conj-fringe [state n w] 
               (do (set! fringe (generic/conj-fringe fringe n w)) 
                   state))
  (next-fringe [state]  (generic/next-fringe fringe))
  (pop-fringe  [state]  (do (set! fringe (generic/pop-fringe fringe)) state)))    
                                
(def empty-search  (searchstate. nil nil {} {} nil nil []))
(def empty-search2 (searchstate2. nil nil {} {} nil nil []))
(def empty-search3 (searchstate3. nil nil {} {} nil nil []))
(defn ^searchstate3 ->empty-search-state [] 
  (searchstate3. nil nil {} {} nil nil []))

(defn ^msearchstate2 ->empty-mutable-search-state [] 
  (msearchstate2. nil nil (HashMap. ) (HashMap. ) nil nil (java.util.ArrayList.) nil))

(defn init-search
  "Populates an empty search state with an initial set of parameters.  Allows
   searches to be customized by varying the start, target, and the type of 
   fringe used to prosecute the search."
  [startnode & {:keys [targetnode fringe empty-state] 
                :or   {targetnode ::nullnode fringe fr/depth-fringe empty-state empty-search}}]
  (merge empty-state {:startnode  startnode 
                      :targetnode targetnode
                      :distance {startnode 0}
                      :shortest {startnode startnode}
                      :fringe   fringe}))

(definline init-search!
  "Populates an empty search state with an initial set of parameters.  Allows
   searches to be customized by varying the start, target, and the type of 
   fringe used to prosecute the search."
  [startnode & {:keys [targetnode fringe empty-state] 
                :or   {targetnode ::nullnode fringe fr/depth-fringe empty-state empty-search3}}]
  `(reduce conj! (->empty-search-state) 
           {:startnode  ~startnode 
            :targetnode ~targetnode
            :distance {~startnode 0}
            :shortest {~startnode ~startnode}
            :fringe   ~fringe}))

(defn minit-search
  "Populates an empty search state with an initial set of parameters.  Allows
   searches to be customized by varying the start, target, and the type of 
   fringe used to prosecute the search."
  [startnode & {:keys [targetnode fringe] 
                :or   {targetnode ::nullnode fringe fr/depth-fringe}}]
  (msearchstate.  startnode 
                  targetnode
                  (m/->mutmap [startnode startnode])
                  (m/->mutmap [startnode 0])
                  fringe
                  nil
                  []))

(defn ^HashMap into-hash [xs]
  (let [^HashMap res (HashMap. )]
    (doseq [[k v] xs]
      (.put res k v))
    res))

(defn minit-search2
  "Populates an empty search state with an initial set of parameters.  Allows
   searches to be customized by varying the start, target, and the type of 
   fringe used to prosecute the search."
  [startnode & {:keys [targetnode fringe multipath] 
                :or   {targetnode ::nullnode fringe fr/depth-fringe multipath nil}}]
  (msearchstate2.  startnode 
                   targetnode
                   (doto (HashMap. ) (.put startnode startnode))
                   (doto (HashMap. ) (.put startnode 0))
                   fringe
                   nil
                   (java.util.ArrayList.)
                   multipath))

(definline backtrack [preds startnode tail-path]
 `(when (seq ~tail-path)
    (loop [path#          ~tail-path
           pending-paths# []]
      (let [node# (first path#)] 
        (if (= node# ~startnode) [path# pending-paths#]
            (let [prior#  (get ~preds node#)                  
                  prior-node#   (if (branch? prior#) (first prior#) prior#)
                  branch-paths# (when (branch? prior#) 
                                 (generic/loop-reduce (fn [acc# x#] (conj acc# (cons x# path#))) [] (rest prior#)))]
              (recur (cons prior-node# path#) 
                     (generic/loop-reduce (fn [acc# x#] (conj acc# x#))  pending-paths# branch-paths#))))))))

(definline paths 
  "Given a shortest-path-tree that encodes the predecessors of nodes, yield a 
   sequence of all the paths from start node to end node."
  [preds startnode endnode]
  `(map first 
       (->> (iterate (fn [pair#]
                       (let [current-path#  (nth pair# 0)
                             pending-paths# (nth pair# 1)]
                         (when (not (empty? pending-paths#))
                           (let [res# (backtrack ~preds ~startnode 
                                                (first pending-paths#))
                                 next-path#    (nth res# 0)
                                 new-subpaths# (nth res# 1)]
                             (vector next-path# (into (rest pending-paths#) new-subpaths#))))))
                     (backtrack ~preds ~startnode (list ~endnode)))
            (take-while identity)))) 

(defn path? 
  ([state target] (generic/best-known-distance state target))
  ([state] (path? state (:targetnode state))))

(defn get-paths 
  ([state target] 
    (when (path? state target)
      (paths (:shortest state) (:startnode state) target)))
  ([state] (get-paths state (:targetnode state))))

(defn first-path [state]  
  (let [startnode  (:startnode state)
        targetnode (:targetnode state)
        spt        (:shortest state)]
    (when (generic/best-known-distance state targetnode)
      (loop [node   targetnode
             path   (cons targetnode nil)]
        (if (identical? node startnode) path
            (let [
                  prior   (get spt node)
                  newnode (if (branch? prior) (first prior) prior)]
                (recur newnode (cons newnode path))))))))

(defn  first-path! [state]  
  (let [startnode  (get-field msearchstate2  state  :startnode)
        targetnode (get-field msearchstate2  state  :targetnode)
         ^java.util.HashMap spt        (get-field msearchstate2 state :shortest)]
    (when (generic/best-known-distance state targetnode)
      (loop [node   targetnode
             path   (cons targetnode nil)]
        (if (identical? node startnode) path
            (let [prior   (.get spt node)
                  newnode (if (branch? prior) (first prior) prior)]
                (recur newnode (cons newnode path))))))))

;;A mutable empty depth-first search...
;(def mempty-DFS  (fn [startnode] (minit-search startnode :fringe fr/depth-fringe)))
(definline mempty-DFS [startnode]
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    fr/depth-fringe
                    nil
                    (java.util.ArrayList.)
                    nil))

(definline mempty-BFS [startnode]
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    fr/breadth-fringe
                    nil
                    (java.util.ArrayList.)
                    nil))

(definline mempty-PFS [startnode] 
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    (fr/make-pq)
                    nil
                    (java.util.ArrayList.)
                    nil))

(definline mempty-RFS [startnode] 
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    fr/random-fringe
                    nil
                    (java.util.ArrayList.)
                    nil))


;;An empty priority-first search.  Note: THis uses a mutable priority
;;queue 
(let [init-fringe (fn [startnode] (minit-search2 startnode))]
  (defn mempty-PFS2 [startnode] (assoc! (init-fringe startnode) :fringe (fr/make-pq))))


;;An empty depth-first search.
(def empty-DFS (memoize (fn [startnode] (init-search startnode :fringe fr/depth-fringe))))
;;An empty breadth-first search.
(def empty-BFS (memoize (fn [startnode] (init-search startnode :fringe fr/breadth-fringe))))
;;An empty priority-first search.  Note: THis uses a mutable priority
;;queue 
(let [init-fringe (memoize (fn [startnode] (init-search startnode)))]
  (defn empty-PFS [startnode] (assoc (init-fringe startnode) :fringe (fr/make-pq))))

;;An empty random-first search.
(def empty-RFS (memoize (fn [startnode] (init-search startnode :fringe fr/random-fringe))))

(let [init-fringe (memoize (fn [startnode] (init-search startnode :empty-state empty-search2)))]
  (defn empty-PFS2 [startnode] (assoc (init-fringe startnode) :fringe (fr/make-pq))))

;; (let [init-fringe  (fn [startnode] (init-search! startnode :empty-state empty-search3))]
;;   (defn empty-PFS3 [startnode] (assoc! (init-fringe startnode) :fringe (fr/make-pq))))

(definline empty-PFS3 [startnode] 
  `(assoc! (init-search! ~startnode :empty-state empty-search3) :fringe (fr/make-pq)))

;;Work in progress.
;;Inlined search.
;; (definline empty-PFS3a [startnode]
;;   (assoc! (init-search! startnode :empty-state empty-search3) :fringe (fr/make-pq)))

(def empty-DFS2 (memoize (fn [startnode] (init-search! startnode :fringe fr/depth-fringe :empty-state empty-search2))))
(def empty-DFS3 (fn [startnode] (init-search! startnode :fringe fr/depth-fringe :empty-state empty-search3)))


;;testing
(comment
;; e e e
;; d d b
;; c a a 
;; b
;; a
(def the-spt
  {:a :a, :b :a, :c :b, :d [:c :a], :e [:d :b]})
(def the-state 
  (-> (init-search :startnode :a :targetnode :e)
      (assoc :shortest the-spt)))

;;=>(get-paths the-state)
;;((:a :b :c :d :e) (:a :b :e) (:a :d :e))         
)
