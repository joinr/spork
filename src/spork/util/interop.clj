;;Namespace for useful functions to make interop
;;with the host platform easier.  Initially
;;targetted at java
(ns spork.util.interop)

(defmacro expose-private-accessors
  "Given a java class, klass, and a seq of symbols defining assumably private 
   fields in said class, returns a map of keyworded getters and setters, that 
   associate with functions that operate on said object to access the fields.
   Yields public function definitions of the form set-fieldname, get-fieldname 
   for each field."
  [klass & fields]
  (let [m (with-meta (gensym "m" ) {:tag 'java.lang.reflect.Field})
        o (with-meta (gensym "o")  {:tag klass})
        fld-get-sets (vec (for [f fields]
                            [(str f)
                             (symbol (str "get-" f))
                             (symbol (str "set-" f))]))]
    `(do ~@
                  (for [[fld getter setter]  fld-get-sets]
                    `(let [~m   (.getDeclaredField ~klass  ~fld)
                           ~'_  (.setAccessible ~m true)]        
                       (defn ~getter [~o]    (.get ~m ~o))
                       (defn ~setter [~o v#] (do (.set ~m ~o v#) ~o)))))))
