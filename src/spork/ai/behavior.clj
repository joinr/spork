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

(defn behavior? [obj] (satisfies? IBehaviorTree obj))

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
(defn beval
  "Maps a behavior tree onto a context, returning the familiar 
  [[:fail | :success | :run] resulting-context] pair."
  [b ctx]
  (cond (vector?   b)   b ;;result with context stored in meta.
        (behavior? b)  (behave b ctx) ;;evaluate the behavior node.                        
        (fn?       b)  (beval (b ctx) ctx) ;;apply the function to the current context
        :else (throw (Exception. (str ["Cannot evaluate" b " in " ctx])))))

;;we could probably just make these functions...
;;convenience? macros...at least it standardizes success and failure,
;;provides an API for communicating results.
(defmacro success [expr]  `(vector :success ~expr))
(defmacro fail [expr]     `(vector :fail ~expr))
(defmacro run [expr]      `(vector :run ~expr))

(defn success? "Indicates if the behavior succeded."
  [res]
  (identical? :success (first res)))

;;Behavior Nodes
;;==============
;;These are basic implementations of behaviors that form
;;a useful Embedded Domain Specific Language for defining behavior trees.
;;The system is flexible enough to allow arbitrary functions to act as behaviors,
;;so the host language (clojure) can be used pervasively alongside the EDSL.

;;note, behaviors are perfect candidates for zippers...
(defn ->leaf
  "Defines a leaf-node, the simplest behavior that applies function f to the context."
  [f]    (->bnode  :leaf nil  (fn [ctx]  (f ctx)) nil))
(defn ->pred
  "Given a function pred :: ctx->boolean, applies the predicate against the context to 
  determine success or failure."
  [pred] 
  (if (behavior? pred) 
    pred ;behaviors can act as predicates, since they return success/failure.
    (->bnode :pred nil  
             (fn [ctx] (if (pred ctx) (success ctx) (fail ctx))) nil)))
(defn ->and
  "Semantically similar to (and ....), reduces over the children nodes xs, 
   short-circuiting the reduction if failure is encountered or a behavior is still
   running."
  [xs]
  (->bnode  :and nil
     (fn [ctx]
      (reduce (fn [acc child]
                (let [[res ctx] (beval child (second acc))]
                  (case res
                    :run       (reduced (run ctx))
                    :success   (success ctx)
                    :fail      (reduced [:fail ctx])))) (success ctx) xs))
     xs))

;;Verify this, I think the semantics are wrong.
(defn ->seq
  "Defines a sequential node, more or less the bread-and-butter of behavior tree architecture.
   A sequential node will traverse xs, in order, only short circuiting if a node is running.  
   After the reduction is complete, the value of the sequence is successful."
  [xs]
  (->bnode  :seq nil
     (fn [ctx]
      (reduce (fn [acc child]
                (let [[res ctx] (beval child (second acc))]
                  (case res
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
                 (let [[res ctx] (beval child (second acc))]
                   (case res
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
      (fn [ctx] (let [[res ctx] (beval b ctx)]
                   (case res
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
  [f] (->bnode :alter nil (fn [ctx] (success (f ctx))) nil))

(defn ->elapse
  "Convenience node that allows us to update a time value in the blackboard."
  [interval]                            
    (->alter #(update-in % [:time] + interval)))

(defn always-succeed
  "Always force success by returning a successful context."
  [b]
  (fn [ctx] (success (second (beval b ctx)))))
(defn always-fail
  "Always force failure by returning a failed context."
  [b]
  (fn [ctx] (fail (second (beval b ctx)))))
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
