(ns spork.util.generators)

;helper functions....I need these somewhere else, since they're universal.
(defn unfold
  "unfold takes a generating function, f :: state -> state | nil,
   a halting function, halt?:: state -> bool, and an intial state s.  Returns
   a sequence of the application of (f (f (f s))) while not halt?"
  [halt? f s]
  (take-while #(not (halt? %)) (iterate f s)))     

(defn generate
  "generate is akin to unfold, except it uses recursion instead of sequences to
   avoid overhead associated with sequences - if needed.  Bear in mind that
   unfold may be about 5x slower due to uses of seqs (from naive testing), which
   makes generate more useful when performance matters.  Takes a generating
   function, f :: state -> state | nil, a halting function,
   halt?:: state -> bool, and an intial state s."
  [halt? f s]
  (loop [state s
         nextstate s]
    (if (halt? nextstate)
      state
     (recur  nextstate (f nextstate)))))
