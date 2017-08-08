;;serialization utilities.
;;Primarily wrappers around nippy to
;;support compressed output and
;;serializing spork datastructures
;;like entity stores and tables.
(ns spork.util.serial
  (:require [clojure.java.io]
            [spork.util.table :as tbl]
            [spork.entitysystem.store :as store] ;temporary
            [clojure.core.rrb-vector :as rrb]
            [taoensso.nippy :as nippy]))

(defn ^bytes slurp-bytes [path]
  (with-open [in (clojure.java.io/input-stream path)]
    (let [l (.length (clojure.java.io/file path))
          buf (byte-array l)]
      (.read in buf 0 l)
      buf)))

(defn spit-bytes [^bytes ba target]
  (with-open [out (clojure.java.io/output-stream target)]
    (.write out ba)))

;;We're producing pretty big entries this way...
;;another way to do it may be to lz4 compress each day at a time,
;;then when reading, read daily entries?  Dunno...

;;we're going to avoid compression for the time being...
;;until I can figure out a way to stream it (possible with gzip..)

;;This is a bad idea for large items, because it's all chucked in
;;memory.  For things like histories, it's better to freeze them incrementally/streaming.
(defn freeze-to! [o target & {:keys [compressor] :or {compressor nippy/lz4hc-compressor}}]
    (with-open [buff (clojure.java.io/output-stream target)
                dos (java.io.DataOutputStream. buff)]
      (nippy/freeze-to-out! dos  o)))

(defn thaw-from! [target & {:keys [compressor] :or {compressor nippy/lz4hc-compressor}}]
    (with-open [fis (clojure.java.io/input-stream target)
                dis (java.io.DataInputStream. fis)]
  (nippy/thaw-from-in! dis )))


(defn freeze-to [o target & {:keys [compressor] :or {compressor nippy/lz4hc-compressor}}]
  (spit-bytes (nippy/freeze o {:compressor compressor}) target))
(defn thaw-from [target & {:keys [compressor] :or {compressor nippy/lz4hc-compressor}}]
  (nippy/thaw (slurp-bytes target) {:compressor compressor}))

;;streaming versions....
(defn serialize
  [v]
  (let [buff (java.io.ByteArrayOutputStream. 1024)]
    (with-open [dos (java.io.ObjectOutputStream. buff)]
      (.writeObject dos v))
    (.toByteArray buff)))

(defn deserialize [bytes]
  (with-open [dis (java.io.ObjectInputStream.
                   (java.io.ByteArrayInputStream. bytes))]
    (.readObject dis)))

(def primitives  #{:long   
                    :double
                    :int    
                    :float        
                    :boolean
                    :char
                    :short})
(defn typed-col
  ([t]
   (let [prim (or (primitives t)                 
                 :object)]
     (with-meta (rrb/vector-of prim) {:column-type t})))
  ([t xs] (if (primitives t)
            (into (typed-col t) xs)
            (with-meta (rrb/vec xs) {:column-type t}))))

(defn col-type [xs]  (get (meta xs) :column-type))

;;this actually does really well...compresses from 200+ mb down to 5...
(defn freeze-table [t target]
  (let [m {:fields  (tbl/table-fields t)
           :columns (tbl/table-columns t)}]           
    (freeze-to m target)))

(defn thaw-table [target]
  (let [res (thaw-from target)
        columns (mapv (fn [col]
                        (if-let [t (primitives (col-type col))]
                          (into (typed-col t) col)
                          col))
                      (:columns res))]
    (merge tbl/empty-table {:fields res :columns columns})))

;;If we used records instead of deftype, we'd be able to
;;serialize these a bit mo easily, since all the clojure
;;framework goes to work.  

;;Nippy doesn't currently support deftypes explicitly
;;temporary extension to allow entities to be serialized/deserialized as maps.
; A unique (namespaced) type identifier
(nippy/extend-freeze spork.entitysystem.store.entity :spork/entity 
  [x data-output]
  (nippy/freeze-to-out!  data-output (into {} (seq x))))

;;This works perfectly!
(nippy/extend-thaw :spork/entity ; Same type id
  [data-input]
  (nippy/thaw-from-in!  data-input))

(defn ->reference [class hash val])

;;one idea is to freeze a representation of the atom,
;;If we have, a lazy seq of atoms, at each stage,
;;the atom will change...
;;So, we really want to freeze reference types by their
;;identity...
;;If we have a structure, and that identity is shared
;;throughout the structure, then we should respect
;;the identities when we thaw.

(defn as-reference [x])
  
;;i.e. {:class atom :val 2}
(defn thaw-reference [x references]
  (let [h (:hash x)]
    (if-let [x (get references h)]
      (do (reset! x (:value h))
          x)
      ;;the ref.
      ;;new...
      (let [new-ref (atom x)
            _       (swap! references assoc h new-ref)]
        new-ref))))

(defn freeze-reference [r references]
  (let [h (hash r)]    
    (if-let [x (get references h)]
      (assoc x :value @x) 
      ;;new...
      (let [new-ref (->reference (class r) h @r)
            _ (swap! references assoc h new-ref)]
        new-ref))))
    
(nippy/extend-freeze clojure.lang.Atom :clojure/atom
  [x data-output]
  (nippy/freeze-to-out!  data-output (->reference clojure.lang.Atom (hash x) @x)))


;;This works perfectly!
(nippy/extend-thaw :clojure/atom ; Same type id
  [data-input]
  (nippy/thaw-from-in!  data-input))

;;We'd have to do the same thing for anything else that's
;;a deftype, like stuff in the spork libs...
