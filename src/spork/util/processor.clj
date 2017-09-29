;NOTE -> currently this file is under heavy revision, it is a dumping ground, 
;not intended for compilation and use! 

;This is a simple set of utilities for defining post processors for data files.
;The need originated in Marathon scripting...and resulted in a more general 
;library.  Enter the generic processor.

;Processors will typically operate on files, and utilize a set of common data to 
;provide a consistent set of output files with canonical fields.

;Processors must define dependencies, as well as resources they produce...not 
;unlike a build tool.

(ns spork.util.processor
  (:require [clojure [set :as set]]
            [spork.util [io :as io]]))

(comment
;A processor is a named task that requires preconditions to hold, and provides 
;a set of post conditions, or effects.  A typical precondition would be the 
;existence of a resource..say a file or an in-memory map or something.

(defprotocol IProcessor 
  (can-process? [p state]
     "Determine if the processor can be run in the given state.")
  (process [p state] "Apply the processor to the process state")
  (process-name [p] "Get the name of the process"))

;The processfunc is a function (ProcessState->ProcessState), where ProcessState
;is a simple map of resource names to resources.  The idea is that we define
;individual sub processes, along with their preconditions (resources required
;prior to running), and their postconditions (resources provided as a result of
;running), then apply the processfunc.
(defrecord processor [name pre post processfunc]
  IProcessor
  (can-process? [p state]
                (or (empty? pre)
                    (every? identity
                            (for [[condition f] pre]
                              (f condition state)))))
  (process [p state] 
           (if (can-process? p state)
             (processfunc state)
             (throw (Exception. (str "Cannot execute process " name)))))
  (process-name [p] name))

(def #^:dynamic *processors* (atom {})) ;a db of processors
(defn add-processor [p]
  (swap! *processors* (assoc (process-name p) p)))

(defprotocol IProcess 
  (do-process [p & env] 
  "Applies post processing to an environment.  A process is a function, where 
   process :: Env -> Env, where Env is a map of keyvals, typically containing 
   the path and any other structure accumulated during processing (which may 
   be useful downstream).  If no args are specified, the default process is 
   to compute highwater results, fillrates, and build an audit trail."))

;This will probably be adapted further, currently not used.  The idea is to 
;define inputs for processes, as well as outputs, to allow for a GPS-type 
;system of goal-programming, in which pre and post-conditions, combined with 
;a map of actions, will allow the processor to figure out how to accomplish 
;a set of goals.
(defprotocol IConditionalProcess
  (pre-conditions [p env] "A set of pre-conditions to execute p.")
  (post-conditions [p env] "A set of post-conditions that p will impart."))

(extend-protocol IProcess
  nil ;a nil process returns the environment, if any.
    (do-process [p & env] (first env))
  clojure.lang.PersistentVector ;vectors are seen as a serial set of processes. 
    (do-process [p & env] 
      (reduce (fn [env proc] (if-let [res (proc env)] res env))           
              (get-environment (first env)) p))
  clojure.lang.PersistentArrayMap ;maps are seen as independent processes.  
    (do-process [p & env]
      (let [ps (seq p)] ;execute processes in parallel
        (->> (pmap (fn [[pname proc]] (fn [env] (proc env))) ps) 
          (reduce (fn [env f] (merge env (f env))) ;returning a merged env
                  (get-environment (first env)))))))

;process combinators.
(defn proc
  "Build a sequential process from one or more items."
  [& itms] (vec itms))

(defn proc-parallel
  "Build a parallel process from one or more items."
  [& itms] (into {} (map-indexed vector itms)))

(defn proc-effect
  "Convert a function of no args, f, into a process by making it into 
   a function that takes an environment argument."
  [thunk] 
  [(fn [env] (do (thunk) env))]) 

(defn proc-map 
  "Map function f to env"
  [f]
  [(fn [env] (f env))])

(defn proc-if
  "Apply process p when pred holds."
  [pred p] 
  [(fn [env] (if (pred env) (p env) env))])  

(defn proc-log
  "Prints a simple message to the console, for logging."
  [msg ]
  (proc-effect (println msg)))

(defn proc-read-file
  [{:keys [resname path]}]
  "Reads a file from path, binding merging the result into the environment."
  (fn [env] (merge env {resname (slurp path)})))    


)
;a sample of compiling an audit trail from a marathon run.
(comment 
	(defprocess compute-trends [rootdir]
	  (let [readme io/readme        
	        folderspec {"Output" readme 
	                    "Input"  readme}]
	  (with-dir rootdir
	    (reading-files [trends (relative-path rootdir ["DemandTrends.txt"]) 
	                    titles (relative-path rootdir ["TitleDef.txt"])]
	       (with-path (relative-path *dir* ["Output"]) [highpath ["highwater.txt"]
	                                                    fillpath ["fillstats.txt"]]
	         (do         
	           (compute-highwater trends titles highpath)
	           (compute-fillstats highpath titles fillpath)))))))
)

  
  
  
    

    

  


     
   
