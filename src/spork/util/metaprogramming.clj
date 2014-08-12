;A collection of macros and functions useful for metaprogramming and library
;development. 
(ns spork.util.metaprogramming)


(definline id 
  "Like identity, but acts at compile time.  Acts as a stand-in for 
   identity to avoid un-necessary function calls."
  [expr]
  `~expr)

(defmacro tagged 
  "Like gensym, but tags the symbol with a type hint."
  [type name]
  `(with-meta (gensym ~name) {:tag ~type}))

(defmacro defmany
  "Define multiple definitions inline.  Takes a collection of bindings, and 
   converts them into def expressions."
  [bindings]
    `(do ~@(->> (partition 2 bindings)
             (map (fn [b] `(def ~(first b) ~(second b)))))))

(defn keyvals->constants
  "Given a map of {symbol val} or {:symbol val}, 
   creates def bindings for each symbol."
  [m]
  (let [as-symbol (fn [k] 
                    (cond (symbol? k) k
                          (keyword? k) (symbol (subs (str k) 1))
                          :else (throw (Exception. "unknown symbol type"))))]
    (eval `(defmany ~(flatten (for [[k v] m] [(as-symbol k) v]))))))

;defines a path to a resource, specifically a function that can get a nested 
;resource from an associative structure.
;A ton of our work will be in dissecting nested structures, particularly the 
;simcontext.
(defmacro defpath
  "Allows definitions of nested paths into associative structures.  Creates 
   a function, named pathname, that consume a map and apply get-in 
   using the supplied path denoted by a sequence of literals, ks."
  [pathname & ks] 
  `(do 
     (defn ~(symbol (str "get-" pathname)) 
       ~(str "Accessor for associatives. Fetches " pathname)   
       [m#] (get-in m# ~@ks))
     (defn ~(symbol (str "set-" pathname))    
       ~(str "Accessor for associatives. Sets " pathname " to second arg.")
       [m# v#] (assoc-in m# ~@ks v#))
     (defn ~(symbol (str "update-" pathname)) 
       ~(str "Accessor for associatives. Sets " pathname 
             " to second arg applied to current val at " pathname)
       [m# f#]
       (if-let [current-val# (get-in m# ~@ks)]
         (assoc-in m# ~@ks (f# current-val#))))))         

(defmacro defpaths
  "Allows multiple paths to be defined at once, with the possibility of sharing 
   a common prefix.  Consumes a map of [pathname path] and applies defpath to 
   each in turn.  A common prefix may be supplied to the paths. "
  ([kvps]         `(defpaths [] ~kvps))
  ([prefix kvps]  
     (let [prefix (if (coll? prefix) prefix [prefix])]
       `(do ~@(map (fn [[n p]] `(defpath ~n ~(into prefix p))) kvps)))))

(defn key->symb [k]  (symbol (subs (str k) 1)))
(defn key->gensymb [k]  (symbol (str (subs (str k) 1) \#)))
(defn key->var [k]  (symbol (str \* (subs (str k) 1) \*)))

(defmacro binding-keys [opts & body]
  `(let ~(reduce-kv (fn [acc k v] 
                           (-> acc
                               (conj (key->var k))
                               (conj v)))
                         []
                         (if (map? opts) opts (eval opts)))
     ~@body))


;;One useful extrapolation I've found is defining macros that take 
;;"hooks", or forms that allow the user access to forms within.
;;I have built this pattern by hand so far, but I need to encode it
;;into a metaprogramming macro.


;;A useful binding form that replaces the - unperformant - 
;;varargs idiom in clojure with something that 
;;takes a map of varargs and unpacks it.

(defn blah [x & {:keys [op y z] :or {op + y 2 z 3}}]
  (op x y z))


;; (defn blah-opt 
;;   ([x opts]
;;      (let [op (get opts :op +)
;;            y  (get opts :y 2)
;;            z  (get opts :z 3)]
;;        (op x y z)))
;;   ([x]      
;;      (let [op  +
;;            y   2
;;            z   3]
;;        (op  y z))))


   
(comment 
(defmacro defn-curried-options
  [name doc args [user-opts opts-val] body]
  (let [folded-body (clojure.walk/postwalk-replace {user-opts opts-val} body)]
    `(defn ~name ~doc  
       ([~@args ~user-opts] 
          ~body)
       ([~@args] 
          ~folded-body))))

(defn symbolize [k] 
  (cond (symbol? k)  k
        (keyword? k) (symbol (subs  (str k) 1))
        (string? k)  (symbol k)
        :else (throw (Exception. (str "No way to make a symbol from " k)))))

(defmacro with-keys [ks env & body]
  `(let [~'*env* ~env
         ~@(mapcat (fn [k] `(~(symbolize k) (get ~'*env* ~(if (symbol? k) (list 'quote k) 
                                                                       k)))) ks)]
     ~@body))    

;;This provides a much faster alternative to the RestFn generating,
;;yet "idiomatic" form of defn, where optional args are elided as 
;;maps or vectors.  Here, we imply that the optional args will
;;definitely take the form of a finite map as a final arg. 
;;Otherwise, a normal function body with pre-evaluated defaults is used.
(defmacro defn-curried-options
  [name doc args opts-map body]
  (let [locals (vec (keys opts-map))]
    `(with-keys  ~locals  ~opts-map
      (defn ~name ~doc  
        ([~@args ~'user-env] 
           (with-keys ~locals ~'user-env
             ~body))
        ([~@args]  ~body)))))

(defn split-args-by [symb coll ]
  (loop [xs coll
         acc []]
    (if (empty? xs) [acc nil]
        (let [x  (first xs)
              ys (rest  xs)]
          (if (= x symb) [acc (first ys)]
            (recur ys 
                   (conj acc x)))))))

(defn options->spec [m]
  (let [vars     (get m :keys)
        defaults (get m :or)
        name     (get m :as 'options)]
    [vars defaults name]))
        
;;the easiest thing to do is to over-ride the 
;;defn behavior and augment it...
;;something like, with-curried-varargs 
(defmacro defn-curried-options
  [name doc args body]
  (let [[args opts-map]           (split-args-by '&optional args)
        [locals defaults mapname] (options->spec opts-map)]
    `(let [~mapname  ~defaults
           ~opts-map ~mapname]
         (defn ~name ~doc  
           ([~@args ~'user-env]  
              (let [~mapname (reduce-kv (fn [m# k# v#] (assoc m# k# (get ~'user-env v#)))
                                        ~defaults ~locals)
                    ~opts-map ~mapname]
                ~body))              
           ([~@args]  ~body)))))
)
