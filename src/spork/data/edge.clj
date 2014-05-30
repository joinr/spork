;;A set of definitions for edges in graphs.
;;Work in Progress.  We currently have this defined in network flow.
(ns spork.data.edge
  (:require [spork.data [mutable :as m]]))


;;Note -> protocols are as efficient as interfaces if the dispatch is 
;;performed via a hinted field call.  If dispatch is performed via the 
;;protocol function, it is still within 1.75x as fast as a member 
;;dispatch, which is tolerable.
(defprotocol IEdgeInfo
  (set-from      [e from])
  (set-to        [e to])
  (set-data      [e d])
  (edge-from     [e])
  (edge-to       [e])
  (edge-data     [e])
  (edge-pair     [e]))

(defprotocol IFlowEdgeInfo
  (set-flow      [e flow])
  (set-capacity  [e cap])
  (edge-flow     [e])
  (edge-capacity [e])
  (capacity-to   [e v])
  (inc-flow      [e amt]))

;;Edge Data Types
;;===============

;;We define a persistent record to capture capacitated flow
;;information across edges.
(defrecord einfo [from to capacity flow data]
  IEdgeInfo
  (set-from      [edge f] (einfo. f to capacity flow data))
  (set-to        [edge t] (einfo. from t capacity flow data))
  (set-data      [edge d] (einfo. from to capacity flow d))
  (edge-from     [e] from)
  (edge-to       [e] to)
  (edge-data     [e] data)
  (edge-pair [e] (clojure.lang.MapEntry. from to))
  IFlowEdgeInfo
  (set-flow      [edge new-flow] (einfo. from to  capacity new-flow dir))
  (set-capacity  [edge cap]      (einfo. from to cap flow dir))
  (inc-flow      [edge amt] (einfo. from to   (unchecked-subtract capacity amt)  
                                              (unchecked-add flow amt) dir))
  (edge-flow     [edge] flow)
  (edge-capacity [edge] capacity)  
  (capacity-to   [edge v]   (if (identical? v to) capacity flow)))

;;A mutable edge list.  For mutable stuff.  Mutation.  Mutants.
;;This ought to be good for small graphs.
(m/defmutable meinfo [from to capacity flow data]
  IEdgeInfo
  (set-from      [edge f] (do (set! from f) edge))
  (set-to        [edge t] (do (set! to   t) edge))
  (set-data      [edge d]        (do (set! data d) edge))
  (edge-from     [d]    from)
  (edge-to       [d]    to)
  (edge-data     [e]   data)
  (edge-pair [d]    (clojure.lang.MapEntry. from to))
  IFlowEdgeInfo
  (set-flow      [edge new-flow] (do (set! flow new-flow) edge))
  (set-capacity  [edge cap]      (do (set! capacity cap)  edge))
  (inc-flow      [edge amt]      (do (set! capacity (unchecked-subtract capacity amt))
                                     (set! flow     (unchecked-add flow amt))
                                     edge))
  (edge-flow     [edge] flow)
  (edge-capacity [edge] capacity)
  (capacity-to   [edge v]   (if (identical? v to) capacity flow)))

;;Inline functions for constructing edges.  We adopt the convention 
;;of delineating the arity of these functions in the numerical suffix.

;;These are each 10x faster then the original varargs implementation.
(definline ->edge-info2 
  [from to]
  `(einfo. ~from  ~to posinf 0 nil))

(definline ->edge-info3
  [from to data]
  `(einfo. ~from  ~to posinf 0 ~data))

(definline ->edge-info4 
  [from to capacity flow]
  `(einfo. ~from ~to ~capacity ~flow nil))

;;Constructors for mutable edges.
(definline ->medge-info2 
  [from to]
  `(meinfo. ~from  ~to posinf 0 nil))

(definline ->medge-info3 
  [from to data]
  `(meinfo. ~from  ~to posinf 0 ~data))

(definline ->medge-info4 
  [from to capacity flow]
  `(meinfo. ~from ~to ~capacity ~flow nil))    

(defmacro edge-hint [e] 
  `(vary-meta ~e assoc :tag 'spork.data.edge.IEdgeInfo))

(definline has-capacity-to? [e v]
  `(pos? (capacity-to ~e ~v)))
 
;;Note:
;;Might need to move the direction component into a protocol, or lift
;;it out entirely.

(defn  edge->medge [^einfo edge]
  (meinfo. (.from edge) (.to edge) (.capacity edge) (.flow edge) (.data edge)))

(defn ^einfo medge->edge [^meinfo edge]
  (einfo. (.edge-from edge) (.edge-to edge) (.edge-capacity edge) (.edge-flow edge) (.edge-data edge)))

