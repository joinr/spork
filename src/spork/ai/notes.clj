;;Legacy notes detailing the implementation of finite state machines, and
;;the translation toward behavior trees.
(ns spork.ai.notes)

;;If we can hone in on a protocol for all states/behaviors, then
;;we have the same notion of atomic and composite behaviors.

;;States are simple behaviors, they do not overtly determine
;;what the transition is.
;;State transitions are captured explicitly via a graph in the FSM
;;or implicitly via allowing the states to call eachother directly.

;;The legacy scheme mixes these together....our states are all
;;functions that transform units, they accomplish system-wide
;;state transitions by firing events, and updating other parts of
;;the system.  In some cases, we allow the state to invoke another
;;immediately, affecting an instantaneous transfer.

;;Most states are driven by the unit's policy....the policy provides
;;a state-transition graph of some of the states, along with wait
;;times.

;;Some state transitions are not encoded in policy, but affected
;;via function calls that codify the implicit transitions.

;;There is a main "Driver" or loop that prosecutes state transitions
;;until the state machine reaches a (currently) halting state.  This
;;is typically indicated by running out of states to transition to,
;;or running out of time (having time remaining in the current state).


;;It probably behooves us to formalize the implicit conventions above.
;;Some basic principles:
;;  We have one or more states (functions) that serve to transition
;;  an entity relative to the current context.
;;  State transitions happen relative to time....
;;  State transitions may happen instantaneously, i.e. blip states.
;;  State transitions also happen relative to the whole context,
;;  since information is propogated about the entity's changes...
;;  When an entity has time remaining in a state, we simply update
;;  the entity with said state.
;;   [In behavior trees, this is known as being "done" in the current
;;   behavior"]
;;  States may have pre and post conditions (i.e. on-enter and on-exit
;;  conditions).
;;  Updates, over a period of time, may trigger moving to a new state
;;  (or behavior).
;;  External forces can cause the state to become invalidated, or
;;  there may be a directed state transition that occurs....

;;How can we formulate the building blocks for these concerns, so that
;;it becomes easy to define isolated states, their effects on the
;;entity in question and the broader simcontext, and the ordering
;;of states?

;;One observation is that the only difference between state machines
;;and behavior trees is the ordering of the state change mechanism.
;;FSMs are defined by nodes in the graph (the states), and the arcs
;;that connect them (arc weights include some notion of transition
;;action, i.e. "wait 10 seconds" or "go immediately" or "trigger this
;;event then change".

;;Behavior trees impose an ordering by embedding states in, at a
;;minumum, a tree structure.  The current state exists as a leaf node
;;somewhere in this structure.  State changes happen as the
;;current behavior completes, any sibling behaviors are visited in
;;an depth-first in-order traversal.

;;BTs are pretty damn convenient from an FP perspective (nice and
;;composeable)....is there anything that we can't compose with states
;;though?  

;;Legacy Implementation
;;======================

;;In an effort to simplify the original implementation for entity
;;behaviors, I am porting them to composeable behavior trees.
;;The current task is to determine how to map the existing FSM
;;into a set of behaviors.

;;The API for the legacy implementation is focused on two functions:
;;update      :: unitdata -> deltat -> unitdata
;;changeState :: unitdata -> tostate -> deltat -> *duration ->
;;              *followingstate -> unitdata

;;Both functions incorporate the notion of possible time delta;  
;;Both functions also return the updated unit.

;;update is the more standard usage, as it's purely a function of
;;time.  The 90% use-case is that the simulation is ticking, prompting
;;time changes.  We actually have several concurrent simulations (unit
;;entities) that are flowing through the supply.  The simulation clock
;;is serving as a central point for coordinating time changes.
;;As the clock ticks, we advance time.  Units are scheduled for
;;updating at specific intervals (typically at their request).
;;The update consists of translating a deltat into an effect on
;;the unit (and transitively the broader simulation writ large).
;;So, we have a somewhat complex unit behavior codified as an FSM
;;that follows a script (a rotational policy), and advances the unit's
;;position over multiple time-slices (deltat's).  Note that the
;;time-slices are not arbitrary....for instance the units will request
;;updates for themselves at eventful points; say according to a
;;policy, their next move should be to go transition to a new location
;;and wait for 100 units of time.  They make the state change, i.e.
;;record the new location, record the fact that they are waiting for
;;100, and request an update at 100 steps after the current time.
;;Thus, in the ideal case, the entity will automatically follow its
;;built-in behavior, which is to move periodically from place to place
;;as a function of its current policy position and the time in that
;;position, according to a scripted policy it's following.
;;This is a happy-medium for re-usable, data-driven behavior, in that
;;the entity is just a policy-interpreter with a swath of pre-defined
;;behavior.

;;The other 10% use case is when we need to directly intervene in
;;an entity's life outside of the passage of time; specifically
;;an external event "causes" the entity to change its state,
;;typically to a state determined a-priori.  ChangeState gives us
;;access to the entity's internal mechanisms to accomplish this,
;;perhaps via an event or an arbitrary (formerly) method call.
;;In the case of unit entities, we see this happening when there
;;are unscheduled deployments, or unplanned withdraws from a demand,
;;etc.  Basically, we deviate from the current script, and drop the
;;entity somewhere else in the script.  So long as we stay "somewhere"
;;in the script, the entity can pick up and continue following along
;;without caring why, or caring where it came from.

;;In fact, ChangeState is a primitive call for other states to use,
;;and serves as what would be more commonly found as the familiar,
;;"onEnter, onUpdate, onExit" set of methods in the State design
;;pattern.  Other states, like MoveState, are typical targets for
;;ChangeState, since 99% of the entity's behavior is following a
;;script that tells it where to move and for how long.

;;If we're thinking of moving to a distributed environment,
;;We can (probably should?) unify the interface as having each entity
;;exist as a formal process that receives messages.  In this case,
;;even time sliced updates are messages.  Currently it's a technical
;;distinction.

;;The centers of gravity for a useful entity (something that moves of
;;its own volition, enough for a supply simulation), is to have the
;;behavior implement (update), and understand how to read from a
;;script (i.e. the unit's policy).

;;We can generalize and implement an "onEvent" or similar function
;;later, behind which we can wrap our changestate ops.

;;Also note that many of the states operate not only on the
;;unit entity, but on the simulation context; in many cases,
;;information is propogated via the event system (typically
;;instantaneous events, mostly for logging, ui stuff).

;;So, we are likely to include, either directly or in a
;;contextual binding, the current simulation context, when
;;we go to update one or more units.

