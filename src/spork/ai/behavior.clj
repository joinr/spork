;;A library for defining entity behaviors.  Legacy implementation 
;;only covered unit entities, and used a Finite State Machine (FSM).
;;New implementation should be more general, and follows the Behavior 
;;Tree design as presented by A.J. Champagnard.
;;Revised notes:
;;Under the functional paradigm, behaviors are first-class state
;;transition functions.  They compose other first-class state
;;transitions functions to affect a change of state on the simstate.

;;We focus on associating the state transition in the unit/entity
;;behavior.  The other "system" behaviors act a coarse LOD, and really
;;only have one thing they're modifying, so they tend to be a little
;;simpler, BUT the concept is identical (as are the signatures).
;;The AI system is just a (either sequential or concurrent/differential)
;;composition of these independent entity systems.
(ns spork.ai.behavior)

;;Overview
;;========
;;The general idea behind how we motivate entities to do things is to use
;;composeable behaviors - as defined in spork.ai.behavior - composed into
;;behavior "trees".  These trees simplify the typical state-transition
;;model we find in finite-state machines. Where the FSM has zero or more
;;edges - or transitions - between states,  behavior trees focus on a
;;small set of composition operations - called internal or intermediate
;;nodes - that define how to traverse the tree.  So, rather than
;;evaluating the next state to transition to - along with the pre,
;;executing, and post conditions for the state - we walk a tree
;;of behaviors, where nodes along the path dictate consistent
;;idiomatic ways to evaluate child nodes. 

;;Besides composition, the other core concept is that behaviors
;;may return success, failure, or (in other implementations)
;;run to indicate that a behavior node has not yet finished
;;evaluating.  This implementation - focused on unit entity
;;behaviors - takes a fairly naive view and ignores the
;;run evaluation.  Rather, we always succeed or fail.

;;Evaluation in the Behavior Context
;;==================================
;;Unlike traditional entity "update" or "step" functions,
;;we maintain an explicit context in which the behavior is
;;evaluated - the behavior context (typically something implementing
;;the spork.ai.core/IEntityStorage and spork.ai.core/IEntityMessaging
;;protocols for utility). This context provides a consistent
;;accumulation of state through which we can view evaluation
;;of the behavior tree as a reduction, with the behavior
;;context being the accumulated result.  Thus, we traverse
;;the tree with an initial behavior context [reified as  a
;;map with useful keys referencing the simulation
;;context/entity store, the entity being processed, the
;;simulated time of the evaluation, and any additional keys
;;useful to evaluation].  Taken as a lexical environment,
;;the keys of the behavior context form a working set of
;;"variables" or properties that we can either query,
;;update, redefine, add to, or otherwise use to guide behavior
;;evaluation.  Depending on the use case, we may use
;;mutable or persistent environments; the behavior system
;;cares not.

;;When evaluating a behavior tree, we start from the root behavior
;;and use its evaluation rules to proceed with the reduction (i.e.
;;compute a resulting behavior context).  The reduced behavior
;;context is then - typically - processed by merging the
;;entity reference into the simulation context reference, returning
;;the simulation context.  The function that encapsulates this
;;functional form of entity  behavior processing is
;;marathon.ces.behaviorbase/step-entity .

;;Behavior evaluation occurs using the spork.ai.behavior/beval
;;function, which operates similarly to eval but in the domain
;;of behavior trees.  The evaluation rules are fairly simple:

;;If the item is a vector pair that matches [:success|fail|run ctx],
;;the vector is returned as the output for beval.

;;If the item to be evaluated is a function, then it
;;is applied to the current accumulated context to determine
;;the next behavior to beval.  This means that
;;functions may return a final result ala [:success|:fail|:run ctx]
;;or they may return another behavior (function or node) which
;;will continue to be evaluated against the context.

;;If the item to be evaluated is a behavior node - anything
;;implemented the spork.ai.IBehaviorTree protocol - then it
;;is beval'd with the current accumulated context
;;(delegating to the behave function of the IBehaviorTree).

;;The current implementation assumes that the highest-level
;;of evaluation - as in marathon.ces.behaviorbase/step-entity
;;will always be successful.  Anything else is an error (even
;;returning [:fail ...].

;;Behavior Functions
;;=================
;;Callers may define functions that operate on the
;;behavior context directly; in some cases this is a useful
;;- if low level - approach to defining behaviors.  
;;Arbitrary functions that map a context to a [:success ...]
;;or a [:fail ...] may be used as behaviors, and will
;;operate correctly under beval.

;;For convenience, and to focus on behavior tree
;;traversal as an "evaluation", the  spork.ai.behavior/befn
;;macro provides a convenient way to define aforementioned
;;behavior functions with convenient destructuring and
;;behavior result packing built in.  Using the befn
;;macro - to define behavior functions - is similar to
;;the standard clojure.core/defn form, with a change
;;the context:  The function arguments correspond to
;;a map-destructing of the behavior context, and
;;where specified by a type hint, will compile to
;;fast field-based accessors for the specific
;;behavior context.  To elide repetitive use of
;;(success ...) and the like, and to align with
;;clojure's idiom of using nil for failure,
;;nil results are automatically converted to
;;(failure ...) evaluations.  Otherwise,
;;behavior evaluation continues as per beval -
;;the caller can immediately return from the behavior
;;using (success ctx) or yield another behavior
;;as a return value - which will effectively
;;continue evaluation using the new behavior

;;Behavior Nodes
;;==============
;;Aside from encoding custom functionality with raw functions,
;;pre-existing behavior nodes provide an expressive domain
;;specific language for defining behavioral "flow control"
;;in a composeable manner.  They effectively define
;;custom behavior functions - again returning
;;[:success|:fail|:run ctx] behind a unified protocol.
;;The magic lies in how a behavior node executes and
;;interprets the traversal of its children.  For example,
;;the ->or behavior corresponds to a logical or of all
;;child nodes (or clauses).  Upon evaluation, ->or will
;;reduce its children - in order - returning on the first
;;[:success ctx] it finds, else [:fail ctx].  This is
;;similar to the 'or macro in clojure.  Similarly, the
;;->and will return at the first sign of a failed child
;;node, else return [:success ctx] as its behavior reduction.
;;In-order, i.e. left-to-right node traversal is a common
;;idiom (although not a constraint) in behavior trees, and
;;allows one to follow the behavior "logic" in a simple,
;;consistent manner by following the traversal.

;;These nodes provide a simple way to compose behaviors
;;and to communicate success/failure throughout the traversal.
;;These trees may be embedded as children of like nodes,
;;creating sophisticatd behaviors with a declarative
;;specification.  Callers are advised to use the canonical
;;behavior nodes where possible to utilize their expressive
;;power, readability, and re-use..


;;Legacy Notes
;;============
;;The remaining notes and inline comments are pending a re-write,
;;but generally follow the preceding overview and implement the
;;minutae of a functional behavior tree.


;;Transitiong To Behavior Trees
;;=============================
;;Changing from the current policy-driven FSM, we need to start at the
;;choke-points so-to-speak, and then grow from there.  The nice thing
;;about behaviors is that they compose nicely....so you can build much
;;more complex behaviors from smaller, simpler behaviors.  We should
;;also be able to analyze, and possibly "compile" behavior trees at
;;some point.  For now though, we'll focus on building at least two
;;fundamental behaviors.

;;The philosophy behind our behavior tree is that the tree contains
;;all the possible actions a unit entity could take, inlcuding the
;;sequences and conditions (and joint conditions) therein.  The tree
;;should structure the logic of our entity's behavior such that its
;;depth-first traversal embodies the "hierarchy" of concerns for the
;;entity.  Typically, the AI folks will dictate that this is analagous
;;to the hierarchy of needs;  higher-priority needs occur earlier in
;;the tree;  With our naive tree, we always check from the root and
;;traverse until a stopping criterion is met;  this means we can
;;accomplish the same reactive "philosophy" of the existing FSM;
;;we should be able to make simple changes to the state of our
;;entity, and the behavior tree - upon re-evaluation - should be
;;able to suss out what it should be doing.


;;One strategy could be to isolate the state-independent parts of the 
;;existing logic and identify them as behaviors; in other words, 
;;remove all state-changes.  We then have the idiom of defining 
;;small, self-contained behaviors, and where we previously "wanted"
;;a state change, we realize a sequence of the simple behavior, 
;;and the state change.

;;This doesn't refactor anything, but it does allow us to translate
;;fairly directly to a behavior tree representation.

;;For instance, the control flow of the Moving state would establish
;;the context of a move based on policy, and then evaluate an
;;instantaneous call to ChangeState afterwards.  We can view that 
;;as a behavior like the following: 
;;               Moving         
;;[SetNextMove SetWaitTime LogMove WaitInNextState]

;;So one strategy is to just break out all of the implicit unit entity
;;state changes we're performing and them build our behaviors out 
;;of them.  As we go along, we can re-use previous actions, or 
;;where apprioriate, entire behaviors.

;;We may give some thoughts to extensions for our naive behavior tree
;;as well: 
;;  Behavior zippers (so we can remember the path we followed to our
;;                    current child)
;;  Specific types that denote success, running, failure

;;Implementation
;;==============

;;For now, we'll let the behavior tree assume it has everything it
;;needs in its context. 
;;The context is a simple map; we may move to a record type as an
;;optimization later, particularly if there are well-known fields 
;;that we'll be accessing frequently.  The context acts as a
;;"blackboard" for the nodes in the behavior tree to work with.

;;Note-> there are opportunities for using STM and exploiting
;;parallelism here; if we implement a parallel node, we may 
;;enjoy the benefits of "fast" entity updates.  On the other hand, 
;;since supply updating takes the preponderance of our time, 
;;we can still get a lot of bang-for-the-buck by updating individual
;;units in parallel batches.  Parallelizing the behavior tree may 
;;not be all that necessary.

;;Note: if we do implement a parallel node (even if executed
;;serially), we can have competing concerns executed in parallel 
;;(i.e. listen for messages, and also update over time slices).

;;aux function.
(defmacro case-identical?
  "Like case, except uses identical? directly to create the cases, rather than 
   the hash-based case function that's default.  Seems 3x faster than built in 
   clojure.core/case if you're using keyword literals....Beware the downfall of 
   using/expecting identity-based comparison on anything else though.  This is a 
   specific use case.  Caller beware."
  [e & clauses]
  (let [v       (gensym "v")
        default (when (odd? (count clauses)) (last  clauses))                   
        pairs      (partition 2 clauses)
        assoc-test (fn assoc-test [m test expr]
                     (if (contains? m test)
                       (throw (IllegalArgumentException. (str "Duplicate case test constant: " test)))
                       (assoc m test expr)))
        pairs (reduce
               (fn [m [test expr]]
                 (if (seq? test)
                   (reduce #(assoc-test %1 %2 expr) m test)
                   (assoc-test m test expr)))
               {} pairs)
        tests (reduce (fn [acc [case res]]
                        (-> acc 
                            (conj `(identical? ~v ~case))
                            (conj res))) [] pairs) ]
    (if (odd? (count clauses))
      `(let [~v ~e]    (cond ~@tests :else ~default))
      `(let [~v ~e]    (cond ~@tests
                             :else (throw (IllegalArgumentException. (str "No matching clause: " ~v)))))
        )))

(defmacro swap!!
  ([atom f]
   (let [atm (with-meta (gensym "atm") {:tag 'clojure.lang.IAtom})]
     `(let [~atm ~atom]
        (.swap ~atm  ~f))))
  ([atom f x]
   (let [atm (with-meta (gensym "atm") {:tag 'clojure.lang.IAtom})]
     `(let [~atm ~atom]
        (.swap ~atm ~f ~x))))
  ([atom f x y]
   (let [atm (with-meta (gensym "atm") {:tag 'clojure.lang.IAtom})]
     `(let [~atm ~atom]
        (.swap ~atm ~f ~x ~y))))
  ([atom f x y & args]
   (let [atm (with-meta (gensym "atm") {:tag 'clojure.lang.IAtom})]
     `(let [~atm ~atom]
        (.swap ~atm ~f ~x ~y ~args)))))

;;Behavior Tree Core
(defprotocol IBehaviorTree
  (behave [b ctx]))
  
(defrecord bnode [type status f data]
  IBehaviorTree
  (behave [b ctx] (f ctx))
  clojure.lang.Named 
  (getName [b] (name type))
  ;; clojure.lang.IFn 
  ;; (invoke [obj arg] (f arg))
  )

;;we'd like to keep both the symbolic function and the compiled function around;
;;Currently, paying the cost for the symbolic function is a bit too high,
;;specifically the cost of creating lots of hash-maps.  It may be better to
;;allow the ability to inline the nodes directly...

;; (definline second! [coll]
;;   (let [c (with-meta (gensym "coll") {:tag 'clojure.lang.Indexed})]
;;     `(let [~c ~coll]
;;        (.nth ~c 1 nil))))

(definline val! [coll]
  (let [c (with-meta (gensym "coll") {:tag 'clojure.lang.MapEntry})]
    `(let [~c ~coll]
       (.val ~c ))))

(defmacro with-result [[[l r] res] & expr]
  (let [result (with-meta (gensym "result") {:tag 'clojure.lang.MapEntry})
        ]
    `(let [~result  ~res
          ~l (.key ~result)
           ~r (.val ~result)]
       ~@expr)))

;;note, originally used satisfies? but extends? is much faster..
(defn behavior? [obj] (extends? IBehaviorTree (class obj)))

;;We can extend our interpreter to understand more...
;;Right now, it only understands functions and behavior nodes.
;;Functions are expected to be :: context -> 'a.
;;What if they return a behavior?  It's useful to
;;implictly evaluate the resulting behavior with the given context...
;;acting as an implict pipeline.  Is this akin to a stack-based language
;;where we're passing arguments implictly (via the stack)?
;; (defn beval
;;   "Maps a behavior tree onto a context, returning the familiar 
;;   [[:fail | :success | :run] resulting-context] pair."
;;   [b ctx]
;;   (cond (behavior? b) (behave b ctx) ;;same as beval....
;;         (fn? b)       (b ctx)))

;;are there any atomic behaviors that we can define beval with?
;;I.e. leaves in the computation....
;;As stated, behave always maps context to [[fail success run] context]
;;Ah...but functions can return behaviors or modified contexts.
;;If it returns a vector, we should terminate evaluation.
;; (defn beval
;;   "Maps a behavior tree onto a context, returning the familiar 
;;   [[:fail | :success | :run] resulting-context] pair."
;;   [b ctx]    
;;   (cond (vector?   b)   b ;;result with context stored in meta.        
;;         (fn?       b)  (beval (b ctx) ctx) ;;apply the function to the current context
;;         :else (behave b ctx) ;;evaluate the behavior node.
;;                                         ;(throw (Exception. (str ["Cannot evaluate" b " in " ctx])))
;;         ))

;; (definline beval
;;   "Maps a behavior tree onto a context, returning the familiar 
;;   [[:fail | :success | :run] resulting-context] pair."
;;   [b ctx]    
;;  `(cond (vector?   ~b)   ~b ;;result with context stored in meta.        
;;         (fn?       ~b)  (beval (b ctx) ctx) ;;apply the function to the current context
;;         :else (behave b ctx) ;;evaluate the behavior node.
;;                                         ;(throw (Exception. (str ["Cannot evaluate" b " in " ctx])))
;;         ))

(defmacro behave! [b ctx]
  `(.behave ~(with-meta b {:tag 'spork.ai.behavior.IBehaviorTree}) ~ctx))

(definline beval
  "Maps a behavior tree onto a context, returning the familiar 
  [[:fail | :success | :run] resulting-context] pair."
  [b ctx]
  (let [beh (with-meta  (gensym "behavior") {:tag 'spork.ai.behavior.IBehaviorTree})]
    `(loop [b#   ~b
            ctx# ~ctx] 
       (cond (vector?   b#)   b# ;;result with context stored in meta.        
             (fn?       b#)  (recur (b# ctx#) ctx#) ;;apply the function to the current context
             :else      (recur (behave! b# ctx#) nil) ;;evaluate the behavior node.
                                        ;(throw (Exception. (str ["Cannot evaluate" b " in " ctx])))
        ))))

;;if we have a nested set of behaviors, we can compile the behavior to
;;get more performance. 
;; (defn compile! [nd]
;;    (if (fn? nd) nd
;;        (:f nd)))

;;perhaps a better option here is to use type wrappers.

;;we could probably just make these functions...
;;convenience? macros...at least it standardizes success and failure,
;;provides an API for communicating results.

(defmacro success [expr]  `(clojure.lang.MapEntry. :success ~expr));`(vector :success ~expr))
(defmacro fail [expr]     `(clojure.lang.MapEntry. :fail ~expr))    ;`(vector :fail ~expr))
(defmacro run [expr]      `(clojure.lang.MapEntry. :run ~expr))     ;`(vector :run ~expr))

(defn success? "Indicates if the behavior succeded."
  [^clojure.lang.MapEntry res]
  (identical? :success (.key res)))

;;Behavior Nodes
;;==============
;;These are basic implementations of behaviors that form
;;a useful Embedded Domain Specific Language for defining behavior trees.
;;The system is flexible enough to allow arbitrary functions to act as behaviors,
;;so the host language (clojure) can be used pervasively alongside the EDSL.

;;note, behaviors are perfect candidates for zippers...
(defn ->leaf
  "Defines a leaf-node, the simplest behavior that applies function f to the context."
  [f]    (->bnode  :leaf nil  (fn [ctx]  (f ctx)) f))

(defn ->pred
  "Given a function pred :: ctx->boolean, applies the predicate against the context to 
  determine success or failure."
  [pred] 
  (if (behavior? pred) 
    pred ;behaviors can act as predicates, since they return success/failure.
    (->bnode :pred nil  
             (fn [ctx] (if (pred ctx) (success ctx) (fail ctx))) pred)))

(defn ->and
  "Semantically similar to (and ....), reduces over the children nodes xs, 
   short-circuiting the reduction if failure is encountered or a behavior is still
   running."
  [xs]
  (->bnode  :and nil
     (fn [ctx]
      (reduce (fn [acc child]
                (with-result [[res ctx] (beval child (val! acc))]
                  (case-identical? res
                    :run       (reduced (run ctx))
                    :success   (success ctx)
                    :fail      (reduced (fail ctx))))) (success ctx) xs))
     xs))

;; (defmacro ->and!
;;   "Semantically similar to (and ....), reduces over the children nodes xs, 
;;    short-circuiting the reduction if failure is encountered or a behavior is still
;;    running."
;;   [xs ctx]
;;   `(reduce (fn [acc# child#]
;;                (let [[res# ctx#] (beval child# (val! acc#))]
;;                   (case-identical? res#
;;                     :run       (reduced (run ctx#))
;;                     :success   (success ctx#)
;;                     :fail      (reduced [:fail ctx#])))) (success ~ctx) ~xs))

(defmacro ->and!
  "Semantically similar to (and ....), reduces over the children nodes xs, 
   short-circuiting the reduction if failure is encountered or a behavior is still
   running."
  [xs ctx]
  (let [v (with-meta (gensym "v") {:tag 'clojure.lang.IPersistentVector})]
    `(let [~v ~xs
           bound# (.count ~v)]
       (loop [idx# 0
              acc# (success ~ctx)]
         (cond (== idx# bound#)  acc#
               (reduced? acc#) @acc#
               :else
               (let [[res# ctx#] (beval (.nth ~v idx#) (val! acc#))]
                 (recur  (unchecked-inc idx#)         
                         (case-identical? res#                                   
                                          :success   (success ctx#)
                                          :fail      (reduced (fail ctx#))
                                          :run       (reduced (run ctx#))))))))))
;;this is broken...
(defn ->reduce [f xs]
  (throw (Exception. "->reduce is not implemented..."))
  (fn [ctx] 
    (reduce (fn [acc child]
              (let [[res acc] (beval child (val! acc))]
                (case-identical? res
                  :run       (reduced (run acc))
                  :success   (success acc)
                  :fail      (fail acc)))) (success ctx) xs)))


;;We should eliminate the reduce funcall..
;;Lots of overhead due to this getting called repeatedly...
;;Seq is used a lot in behaviors....so instead of reduce,
;;if we shift it to a loop, then we get something a bit faster.

;;Verify this, I think the semantics are wrong.
(defn ->seq
  "Defines a sequential node, more or less the bread-and-butter of behavior tree architecture.
   A sequential node will traverse xs, in order, only short circuiting if a node is running.  
   After the reduction is complete, the value of the sequence is successful."
  [xs]
  (->bnode  :seq nil
     (fn [ctx]
      (reduce (fn [acc child]
                (with-result [[res ctx] (beval child (val! acc))]
                  (case-identical? res
                    :run       (reduced (run ctx))
                    :success   (success ctx)
                    :fail      (fail ctx)))) (success ctx) xs))
     xs))

(defn ->or
  "Defines a behavior node that short-circuits upon finding any success from xs, returning 
   success for the entire subtree.  Else, failure."
  [xs]
  (->bnode  :or nil 
     (fn [ctx]
       (reduce (fn [acc child]
                 (with-result [[res ctx] (beval child (val! acc))]
                   (case-identical? res
                     :run       (reduced (run ctx))
                     :success   (reduced (success ctx))
                     :fail      (fail ctx)))) (success ctx) xs))
     xs))

(defn ->not
  "Semantically similar to (not ..), logically inverts the result of the 
  behavior b, where (not :success) => :failure, (not :failure) => :success, 
  (not :run) => :run, since running is not determined."
  [b]
  (->bnode  :not nil
      (fn [ctx] (with-result [[res ctx] (beval b ctx)]
                   (case-identical? res
                     :run     (run     ctx)
                     :success (fail    ctx)
                     :fail    (success ctx))))
      b))

;;if a behavior fails, we return fail to the parent.
;;we can represent a running behavior as a zipper....
;;alternatively, we can just reval the behavior every time (not bad).
(defn ->alter
  "Defines a behavior node that always succeeds, and applies f to the context.
  Typically used for updating the context."
  [f] (->bnode :alter nil (fn [ctx]
                            (success (f ctx))
                            ) nil))

(defn ->elapse
  "Convenience node that allows us to update a time value in the blackboard."
  [interval]                            
    (->alter #(update-in % [:time] + interval)))

(defn always-succeed
  "Always force success by returning a successful context."
  [b]
  (fn [ctx] (success (val! (beval b ctx)))))
(defn always-fail
  "Always force failure by returning a failed context."
  [b]
  (fn [ctx] (fail (val! (beval b ctx)))))
;;a behavior that waits until the time is less than 10.
(defn ->wait-until
  "Observes the context, using pred (typically some eventful condition), to 
   determing if the behavior is still running (pred is false), or pred occurred."
  [pred]
  (->bnode  :wait-until nil 
          (fn [ctx] (if (pred ctx) (success ctx) (run ctx)))    nil))

;;do we allow internal failure to signal external failure?
(defn ->while
  "Emulates the semantics of (while ...) in behaviors, using pred to 
  determine if evaluation should continue.  If evaluation proceeds, 
  returns the result of evaluating b against the context, else failure."
  [pred b]
  (->bnode :while nil   
           (fn [ctx] (if (pred ctx) 
                       (beval b ctx)
                       (fail ctx))) 
           b))
          
(defn ->elapse-until
  "Returns a behavior that repeatedly causes time to elapse, by interval,
   up to a specified time."
  [t interval]
  (->while #(< (:time %) t)
            (->elapse interval)))

(defn ->do
  "Emulates side-effecting in behaviors, evalates f against the context, then 
   returns a successful context regardless of f's result."
  [f] 
  (fn [ctx] (success (do (f ctx) ctx))))

(defn ->if
  "Emulates (if ...) semantics in behaviors.  Depending on the result of 
   applying pred to the context, either evaluates btrue or bfalse (if present)."
  ([pred btrue]
      (->and [(->pred pred)
              btrue]))
  ([pred btrue bfalse]
     (->or (->and [(->pred pred)
                   btrue])
           bfalse)))         

;;For reference, these are the nodes that define our dsl:
(def behavior-nodes 
  '[beval
    success?
    success
    run
    fail
    behave
    ->seq
    ->elapse
    ->not
    ->do
    ->alter
    ->elapse-until
    ->leaf
    ->wait-until
    ->if
    ->and
    ->pred
    ->or
    ->bnode
    ->while
    always-succeed
    always-fail])


;;__Evaluating Behaviors in Context__
;;A binding for the default context.  If no context is provided,
;;we use this for implicit context, and require that it is bound
;;during evaluation.
(def ^:dynamic *behavior-context*)
;;Auxilliary function.
;;This just does the plumbing for us and lifts keys out of the environment.
;;either define the context as a vector of args, which is bound to the
;;environment, or as a map...
;;we'd like to allow destructuring...

;;we can make key-fn smarter....and allow type-hinted
;;optimizations to take hold.  Specifically, we can allow a
;;{:fields [x y z] :as ^BehaviorContext benv} to generate
;;optimized code.

(defn static? [field]
  (java.lang.reflect.Modifier/isStatic
   (.getModifiers field)))
(defn get-record-field-names [record]
  (->> record
       .getDeclaredFields
       (remove static?)
       (map #(.getName %))
       (remove #{"__meta" "__extmap"})
       (map symbol)))

;;typed destructuring...
;;Should make performance faster using direct method access rather
;;than function call overhead associated with generic "get"
(defmacro kwinfo [vars]
  (let [args (first (rest &form))
        m    (if-let [res (get args :as)]
               res
               (gensym "map"))
        type (get (meta m) :tag
                  (get (meta args) :tag  'clojure.lang.ILookup))
        m   (with-meta m {:tag type})]
    `{:fields '[~@(:keys args)]
      :symb   '~m
      :type   ~type}))

(defn kwinfo->mapspec [{:keys [fields symb type] :as m}]  
  (cond (isa? type clojure.lang.IRecord) ;we have fields
        (let [known-fields (set (get-record-field-names type))
              map-fields (group-by (fn [s] (if (known-fields s)
                                             :fields
                                             :keys)) fields)]
          (assoc m :fields (:fields map-fields)
                   :keys   (:keys map-fields)))              
        (isa? type clojure.lang.ILookup) ;we can use .valAt
        (-> m (dissoc :fields)
              (assoc :keys fields))
        :else
        (-> m (dissoc :fields :type)              
              (assoc :keys fields  ))))
        ;;use get... 

(defmacro mapspec->arg-body [ms & body]
  (let [{:keys [fields keys symb type]}  (if (map? ms) ms (eval ms))
                                        ;symb
        tsymb (with-meta symb {:tag (if (identical? clojure.lang.ILookup
                                                        type)
                                          'clojure.lang.ILookup
                                          type)})
        get-field (fn [f]
                    (let [getter (symbol (str "." (name f)))]
                      `[~f (~getter ~symb)]))
        get-key   (fn [k]
                    `[~k (.valAt  ~tsymb  ~(keyword (name k)))])]
    `(fn [~symb]
       (let [;~tsymb ~symb
             ~@(reduce (fn [acc [l r]] (conj acc l r))
                      []
                      (concat (for [f fields]
                                (get-field f))
                               (for [k keys]
                                 (get-key k))))]
        ~@body))))

(defmacro hinted-key-fn 
  [vars & body]
  (if (map? vars)
    `(mapspec->arg-body ~(kwinfo->mapspec (eval `(kwinfo ~vars)))
                        ~@body )
    (let [args (rest &form)
          type (get (meta args) :tag 'clojure.lang.ILookup)
          argmap (with-meta `{:keys [~@vars] :as ~'context} {:tag type})]
      `(fn  ; [{:keys [~@vars] :as ~'context}]
            [~argmap]
            ~@body)
      `(mapspec->arg-body ~(kwinfo->mapspec (eval `(kwinfo ~argmap)))
                        ~@body ))))
      
  
;;now we can extract hinted fields based on the types...


;;we need to xref the fields with the record's fields to find out
;;which fields are accessible via direct field access at compile
;;time.

;;We can enforce a type-hint in conjunction with the fields
;;definition...either by metadata or using the keyword
;;arg in the map.  Clojure convention prefers metadata for
;;typehinting...
(defmacro key-fn
  [vars & body]
  (if (map? vars)
    `(fn [~vars]
       ~@body)      
    `(fn [{:keys [~@vars] :as ~'context}]
       ~@body)))



;;we're going to transform a (fn ... [args] body) into something
;;like (defn ~name ~doc? [args] body)
;;so really, just replacing (fn []) with (defn ~name ~doc) in the outer
;;form...
(defmacro fn->defn
  ([name-opts expr]
   (let [name-opts      (if (coll? name-opts) name-opts
                            [name-opts])
         [fst args body] (loop [xpr expr]
                           (if (= (name (first xpr)) "fn")
                             xpr
                             (recur (macroexpand-1 xpr))))]
     `(defn ~@name-opts ~args ~body)))
  ([name docstring expr]
   `(fn->defn [~name ~docstring] ~expr)))


;;While elegant (maybe), this solution produces a couple of costs that
;;are pretty heavy (particularly for the primary purpose of this library:
;;run-time simulation stuff).  Whenever we evaluate a befn, we end up
;;establishing >= 1 new binding for *behavior-context*.  This is
;;problematic.....because even small innocuous behaviors invoke it (almost
;;everything is defined via befn...so bindings abound).  The consequence is
;;that in order to eval the binding form, we have to create a new hashmap
;;and push the bindings using push-thread-bindings, which has a serious
;;performance penalty.  So, we should opt to NOT do this....instead, we
;;should try to find a way to pass references to the context via
;;args.

;;behavior functions automatically provide us with implicit failure if we
;;return nil.  Note the binding of the symbl 'ctx . Since we're using the
;;key-fn macro, when we unpack the key-fn, we automatically bind the map
;;containing its args to a 'ctx var in the lexical scope of the function.
;;Thus, we are guaranteed to have 'ctx available for binding in dynamic scope.
;;bevals the whatever body evaluates to, in the *ctx*.  This should let us
;;get away from having to define explicit continuation of evaluation,
;;and move behavior composition into spork.ai.behavior/beval where it belongs,
;;or into the specific nodes or functions for custom control flow.

;;An alternative that avoids all this overhead, is to kind of do what
;;we're already doing - pass the context along via args.  The thing is,
;;we want bind! and friends to map nicely to the "current context", i.e.
;;the context arg passed in the lexical scope.
;; (defmacro befn
;;   ([vars body]
;;    (let [ctx-name  (if (map? vars) (get vars :as 'context) 'context)]
;;      `(key-fn ~vars
;;               (binding [~'spork.ai.behavior/*behavior-context* ~ctx-name]
;;                 (if-let [res# ~body]
;;                   (spork.ai.behavior/beval res# spork.ai.behavior/*behavior-context*)
;;                   (fail ~'spork.ai.behavior/*behavior-context*))))))
;;   ([name vars body]
;;    (let [ctx-name (if (map? vars) (get vars :as 'context) 'context)]
;;      `(fn->defn ~name
;;                 (key-fn ~vars
;;                         (binding [~'spork.ai.behavior/*behavior-context* ~ctx-name]
;;                           (if-let [res# ~body]
;;                             (spork.ai.behavior/beval res# spork.ai.behavior/*behavior-context*)
;;                             (fail ~'spork.ai.behavior/*behavior-context*)))))))
;;   ([name docstring vars body]
;;    `(befn [~name ~docstring] ~vars ~body)))

;;we could establish local-functions...they only get compiled once, as anonymous
;;inner classes...Another option is to just call
;;bind! merge! and friends via protocol definitions that act on the
;;context, and supply the context as a record type...

(defmacro befn
  ([vars body]
   (let [ctx-name  (if (map? vars) (get vars :as 'context) 'context)]
     `(hinted-key-fn ~vars
                (if-let [res# ~body]
                  (spork.ai.behavior/beval res# ~ctx-name)
                  (fail ~ctx-name)))))
  ([name vars body]
   (let [ctx-name (if (map? vars) (get vars :as 'context) 'context)]
     `(fn->defn ~name
                (hinted-key-fn ~vars                        
                        (if-let [res# ~body]
                          (spork.ai.behavior/beval res# ~ctx-name)
                          (fail ~ctx-name))))))
  ([name docstring vars body]
   `(befn [~name ~docstring] ~vars ~body)))

;;These are primitive actions...
;;We should probably include these in the entity environment...
;;bind the keys to vals in the environmental context, returning a successful
;;computation.
;;This is similar to monadic bind, at least in meaning.  We associate new
;;values to keys in the environment local to the compuatation (psuedo monad).
;;Note: we could easily alter bind! to use atoms instead, and take
;;advantage of mutation.  This is a trivial optimization to exploit
;;in the future.
;;Note: could be a macro....may be more efficient (not creating intermediate
;;map or doing a reduction).
(defn bind!
  ([kvps ctx]
   (success 
    (reduce-kv (fn [acc k v]
                 (assoc acc k v)) ctx kvps)))
  ([kvps]
   (fn [ctx]
     (success 
      (reduce-kv (fn [acc k v]
                   (assoc acc k v)) ctx kvps)))))

;  (bind! kvps spork.ai.behavior/*behavior-context*)))

(defmacro inline-merge! [m kvps]
  (let [acc (with-meta (gensym "acc") {:tag 'clojure.lang.Associative})]    
    `(let [~acc ~m]
       (-> ~acc
           ~@(for [[k v] kvps]
               `(.assoc ~k ~v))))))

;;we're calling bind! a lot, so if we inline it, it should alleviate
;;the creation of tons of intermediate maps...spending a lot of time
;;building maps...we can compile that away.
(defmacro bind!!
  ([kvps ctx] `(success (inline-merge! ~ctx ~kvps )))
  ([kvps]
   `(fn [ctx#]
      (bind!! ~kvps ctx#))))

;;assumes we have atomic places defined for our kvps.r
(defmacro push!!
  ([atm k v ctx]
   (let [m (with-meta (gensym "m") {:tag 'clojure.lang.Associative})]
     `(success (do (swap!! ~atm (fn [~m]
                                  (.assoc ~m ~k ~v)))
                ~ctx))))
  ([atm k v]
   `(fn [ctx#]
      (push! ~atm ~k ~v ctx#))))

(defn push!
   {:inline (fn [atm k v ctx] `(push!! ~atm ~k ~v ~ctx))
   :inline-arities #{4}
   :added "1.0"}
  ([atm k v ctx]
   (push!! atm k v ctx))
  ([atm k v]
   (fn [ctx]
     (push!! atm k v ctx))))

;this is about 6 times faster...
(definline merge1 [l r]
  (let [acc (with-meta (gensym "acc") {:tag 'clojure.lang.Associative})]
    `(reduce-kv (fn [~acc k# v#]
                  (.assoc ~acc k# v#)) ~l ~r)))
(defn merge!
  ([atm kvps ctx]
   (success (do (swap!! atm (fn [m] (merge1 m kvps)))
                ctx)))
  ([atm kvps]
   (fn [ctx] (merge! atm kvps ctx))))

(defmacro merge!!
  ([atm kvps ctx]
   `(success (do (swap!! ~atm (fn [m#] (inline-merge! m# ~kvps)))
                ~ctx)))
  ([atm kvps] `(fn [ctx#] (merge!! ~atm ~kvps ctx#))))

;;removes bindings..
(defn drop!
  ([ks ctx]
   (success (reduce (fn [acc k] (dissoc acc k)) ctx ks)))
  ([ks] (fn [ctx] (drop! ks ctx))))


(defmacro ->? [vars & body] `(->pred (key-fn ~vars ~@body)))  
(defn ->prop? [k v] (->pred (fn [ctx] (= (get ctx k) v))))
(defmacro ->let [[symbs ctx] & body]
  `(fn [{:keys [~@symbs] :as ~ctx}]
     (if-let [inner# ~@body]
       (if (spork.ai.behavior/behavior? inner#)
         (spork.ai.behavior/beval
          inner# ~ctx)
         inner#)
       (fail ~ctx))))

(defn return! [res]
  (if (success? res)
    (val! res)
    (throw (Exception. (str [:failed-behavior res])))))
