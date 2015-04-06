;;Module for defining generic graph search data.  Uses the generic fringe 
;;structures from spork.data.fringe, to provide a generic search state.
;;We'll typically use this with priority queues, for djikstra's algorithm and 
;;the bellman-ford algorithm, but depth-first, breadth-first, and random 
;;searching are also supported.
(ns spork.data.searchstate
  (:require [spork.protocols [core :as generic]]
            [spork.data      [priorityq :as pq]
                             [fringe :as fr]
                             [mutable :as m]
                             [mpq :as mpq] ;new mutable priority
                                        ;queue, bounded queues.
             ])
  (:import  [java.util HashMap ArrayList]))

;;This is slow for recovering paths.
;;We don't need to use persistent stuff to recover paths, it's a small 
;;opportunity to mutably recover paths inside of a lazy computation. 


;;These allows us to use complex keys in our spt.
(definline ^ArrayList ->branch [^ArrayList x]  `(doto (ArrayList.) (.add ~x)))
(definline branch? [x] `(identical? (class ~x) ~'java.util.ArrayList))
(defn ^ArrayList push-branch [^ArrayList b x]  (doto b (.add x)))

(defmacro get-field [hint obj field]  `(.valAt ~(vary-meta obj assoc :tag hint) ~field))

(definline update-search [state shortest distance fringe]
  `(-> ~state
       (assoc :shortest ~shortest)
       (assoc :distance ~distance)
       (assoc :fringe ~fringe)))

;;This is the state we're currently using.
(m/defmutable msearchstate2 
  [startnode targetnode  
   ^java.util.HashMap shortest 
   ^java.util.HashMap distance 
   fringe 
   ^java.util.ArrayList visited
   multipath]
  generic/IGraphSearch
  (set-target   [state nd] (do (set! targetnode  nd) state))
  (set-start    [state nd] (do (set! startnode  nd ) state))            
  (get-target   [state] targetnode)
  (get-start    [state] startnode)
  (set-multipath [state mp] (do (set! multipath mp) state))
  (visited-nodes [state] visited)
  (new-path     [state source sink w]
    (do (.put shortest sink source)
        (.put distance sink w)
        (set! fringe (generic/conj-fringe fringe sink w))
        state))
  (shorter-path [state source sink wnew wpast]
     (do (.put shortest sink source) ;new spt
         (.put distance sink wnew)   ;shorter distance
         (set! fringe (generic/conj-fringe fringe sink wnew))
         state))  
  (equal-path   [state source sink] 
     (do (when multipath
           (let [current  (.get shortest sink)]
             (when (not= current source)
               (do (.put shortest sink 
                         (-> (if (branch? current) current (->branch current))
                             (push-branch source)))))))
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
                                

(defn ^msearchstate2 ->empty-mutable-search-state [] 
  (msearchstate2. nil nil (HashMap. ) (HashMap. ) nil  (java.util.ArrayList.) nil))


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
  (let [startnode  (get state :startnode )
        targetnode (get state :targetnode )
        spt        (get state :shortest)]
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
                    (java.util.ArrayList.)
                    nil))

(definline mempty-BFS [startnode]
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    fr/breadth-fringe
                    (java.util.ArrayList.)
                    nil))

;; (definline mempty-PFS2 [startnode] 
;;   `(msearchstate2.  ~startnode 
;;                     nil
;;                     (doto (HashMap. ) (.put ~startnode ~startnode))
;;                     (doto (HashMap. ) (.put ~startnode 0))
;;                     (mpq/->min-pq)
;;                     (java.util.ArrayList.)
;;                     nil))


;;This is the old priority-queue based implementation, non-indexed,
;;may be useful in some cases despite the memory overhead.
(definline mempty-PFSQ [startnode] 
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    (fr/make-pq)
                    (java.util.ArrayList.)
                    nil))

;;now using new mutable priority queue, with updated search fringe.
(definline mempty-PFS [startnode] 
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    ;(fr/make-pq)
                    (mpq/->min-pq)
                    (java.util.ArrayList.)
                    nil))

(definline mempty-PFS-bounded [startnode bound] 
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    (mpq/->min-bounded-pq ~bound)
                    (java.util.ArrayList.)
                    nil))

(definline mempty-RFS [startnode] 
  `(msearchstate2.  ~startnode 
                    nil
                    (doto (HashMap. ) (.put ~startnode ~startnode))
                    (doto (HashMap. ) (.put ~startnode 0))
                    fr/random-fringe
                    (java.util.ArrayList.)
                    nil))

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

