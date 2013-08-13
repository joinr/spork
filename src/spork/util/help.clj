(ns spork.util.help
  (:use [clojure.pprint :only [pprint cl-format]]
        [clojure.repl :only [apropos doc]]
        [clojure.string :only [split]])) 


(defn format-eighty [words]
  (cl-format nil "~{~<~%~1,40:;~A~> ~}" words))

(declare get-help tutorial)

(defn help-msg [& lines]
      (apply str (interleave lines (repeat \newline))))

(def help "Most users will type this into the repl, not knowing lisp style." 
  (clojure.core/reify 
    Object 
    (toString [o] 
      (help-msg "Hello!" "I assume you typed 'help' at the REPL."  
   "By typing 'help' and hitting return, you caused the lisp reader to evaluate"
   "the var (sort of like a variable in other languages) bound to 'help'"
   "Most of the time, you want to evaluate expressions of the form:"
   "(my expression goes here), which evaluates the entire expression"
   "- sort of like a function call in other languages -"
   "The general help system works in this way, which is more Lispy."
   "To get more help, try evaluating the expression (get-help)"
   "Just type (get-help) and hit the Enter or Return key!"
   "For a general overview, type (tutorial) to get a smattering of topics!"))
    )) 

(def known-topics (atom {}))
(defn defhelp
  "Adds a new help topic to the general help system."
  [topic msg]
  (let [tops @known-topics]
    (if (not (contains? tops topic)) 
      (swap! known-topics assoc topic msg))))
      
(def useful-commands ['doc 'apropos 'def 'defn])

(def apropos* 
  "A prettier version of apropos"
  (comp pprint apropos))
  

;load basic help topics....
(#(doseq [[k v] %] (defhelp k v))   
{:repl   
 (help-msg 
   "You are currently at the REPL."
   "REPL stands for Read Evaluate Print Loop"
   "Lisp, and thus Clojure, are dynamic languages that let you"
   "code in an interactive style, more akin to sculpting."
   "Technically, you are in a namespace in the REPL, likely user."
   "  To find out what namespace you're in, you can evaluate *ns*"
   "You have access to everything Clojure has evaluated up until now."   
   "  To find out which namespaces have been loaded, evaluate (all-ns)"
   "To find out more about namespaces, evaluate (doc ns) or (apropos \"ns\")")
 :lisp  
 (help-msg 
   "Lisp is an ancient and powerful language, created by John McCarthy"
   "Lisp has many dialects, but all share the basic notions of Lisp:"
   "  Minimal Syntax. Expressions are lists of symbols."
   "  Code is data.  Expressions, as lists, can be manipulated."
   "  Language extension.  Since code is data, and Lisp is code, Lisp is data."
   "    Lisp can be extended, new language features/languages defined by you."
   "Lisp facilitates multiple paradigms of programming naturally:"
   "  Imperative..do this then that, change x to 2, then do..."
   "  Object Oriented...data, functions, and procedures encapsulated in objects"
   "  Functional Programming...everything is a pure function of input->output"
   "  and many more... "
   )
 :clojure 
 (help-msg 
   "Clojure is a modern and flexible member of the Lisp family"
   "  Clojure lives on the Java Virtual Machine."
   "  Clojure can interop seamlessly with dirty, but useful Java."
   "Unlike Common Lisp, Clojure prefers a functional programming style"
   "  Clojure favors immutable values 99% of the time."
   "    Once values are defined, they don't change."
   "    Mutability (changing things) is not common."
   "    Functions and function composition are the tools of the trade."
   "    Operations on data structures return new structures." 
   "      The input structures persist/are not mutated in the process."   
   "  For the other 1%, Clojure offers special structures for handling state.")
 :util   
 (help-msg
   "The default utilies library, under the namespace (util.[somelib])"
   "Core utilities include file operations, clipboard manipulation, help"
   "simple table operations, xml, zip file manipulation, date time."
   "Many of these are included by default during scripting sessions.")
 :basics
 (help-msg 
   "The REPL can serve as an incredibly useful guide to Clojure."
   "Useful commands include:"
   (str useful-commands)
   "You can read the documentation for anything - if it has a docstring"
   "Just call (doc expr), where expression is the symbol of interest"
   "For instance, (doc defhelp) evaluates to:"
   (with-out-str (print (doc defhelp))))
 :functions
 (help-msg
   "Functional programming is built on something called the Lambda Calculus."
   "  Clojure, as a functional programming language, centers on this heavily."
   "  Functions are the primary building blocks for Clojure programs."
   "Named functions can be defined using defn:"
   "  (defn myfunc [msg] (println msg))"
   "One feature of functional programming is the use of lambda functions."
   "  Lambda functions are akin to anonymous functions"
   "  They are practically useful because we can inline function definitions."
   "  This allows a very flexible programming style."
   "Clojure provides 2 ways of producing these lambda functions:"
   "  Use fn, a primitive version of defn, to define anonymous functions:"
   "    (fn [arg] (println (str arg)))"
   "    This looks exactly like defn, except the function has no name."
   "    Thus, no symbol refers to the function."
   "  If we prefix # to an expression, we can create one:"
   "    #(println (str %))"
   "     The argument is implied by %.  Multiple arg versions use %1,%2,..."
   "     This is equivalent to our previous example."
   "  We can compose functions...technically creating a new lambda function.."
   "    (comp println str))"
   "  We can also use partial application to fill in arguments..."
   "    (fn [user] (println ((partial apply str :user) user)))"
   "We can use functions as first class values:"
   "  Clojure's function 'map' takes a function an a sequence of values."
   "  map applies the function to each value in the sequence,"
   "  returning a corresponding sequence of results."
   "We can add 5 to every item in the sequence [1 2 3 4]:"
   "  (map (fn [n] (+ 5 n)) [1 2 3 4])"
   "  -> (6 7 8 9)"
   "  Equivalently..."
   "  (map #(+ 5 %) [1 2 3 4])"
   "  (map (partial + 5) [1 2 3 4])"
   "Clojure has a very powerful and flexbile sequence library built around"
   "this idiom.")
 :expressions
 (help-msg
   "Lisp dialects use parenthesis () to denote lists."
   "Lisp dialects process lists of expressions by by applying the first symbol"
   "to the remaining symbols in the list."
   "Thus, (println 1) is like saying 'return the result of feeding 1 to println'"
   "Primitive values, like numbers and strings, cannot be applied."
   "So the expression (1 2 3) would throw an error."
   "  What is the result of feeding 2 3 to 1?  Doesn't make sense for numbers"
   "So the rule of thumb is...only put symbols that map to functions up front."
   "  (somefunction arg1 arg2 arg...)"
   "A function of no arguments:"
   "  (myfunction)")
  :data-structures
  (help-msg 
    "Clojure has several built-in data structures."
    "Each of these structures is persistent, which means that operations"
    "do not mutate the underlying structure, but return a new structure"
    "that is equivalent to the change.  Operations are pure functions."
    "This allows versions of the structure to persist, and is highly useful."
    "The main structures are:"
    "                       read in-line , created from function"
    "  list:                   '(1 2 3 4), (list 1 2 3 4)"
    "  vector                   [1 2 3 4], (vec 1 2 3 4)"
    "  set:                    #{1 2 3 4}, (set 1 2 3 4)"
    "  hash-map:    {:a 1 :b 2 :c 3 :d 4}, (hash-map :a 1 :b 2 :c 3 :d 4}"
    "Each structure has its own uses and properties, but all structures can"
    "be seen as abstract sequences.  Clojure's sequence libraries are built"
    "around this idea.  Thus, we can apply the map function broadly:"
    "   (= (map inc '(1 2 3 4)) (map inc [1 2 3 4]))"
    "   -> true"
    )
   :primitives 
   (help-msg 
     "Clojure has several primitive values."
     "Numbers: '(1,2,3,4, 3000.2)"
     "Ratios: 22/4"
     (str "characters: " \\ \a)
     "Strings: \"this is a string\" "
     "Any many more, but these are the main ones.") })

(defn- general-help [topic] (apropos* topic)) 

(defn list-topics [] (keys @known-topics))
(defn help-topics "Lists all help topics" [] 
  (println 
   (help-msg   
    "Known topics include:"
    (list-topics)    
    "To get help with a listed topic, evaluate (get-help topic)"
    "  (get-help :repl), when evaluated, would provide repl help"
    "For more general help, or to search symbols, you can use Clojure's apropos:"
    "  Try (apropos \"sometopic\"), or use appropos* for a prettier version")))

(defn get-help 
  "get-help provides a starting points for folks to navigate the REPL."
  ([topic] (if (= topic :tutorial) (tutorial) 
             (println  (get @known-topics topic 
                            (str "That topic is not known.")))))
  ([] (help-topics)))

(defn tutorial []
  (println  
    (doseq [topic [:lisp :clojure :expressions :repl :basics :functions 
                   :primitives :data-structures]]
      (println (str topic \newline (with-out-str (get-help topic)))))))
  
  