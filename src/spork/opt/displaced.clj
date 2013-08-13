(ns spork.opt.displaced)

;;A protocol for mapping to and from primitive arrays of doubles.
(defprotocol IdoublesEncoder 
  (encode-doubles [e s] "Maps s to an array of doubles."))
(defprotocol IdoublesDecoder
  (decode-doubles [d xs] 
    "Maps an array of doubles onto another domain."))

;Float arrays are viewed as simple representations.
(extend-type (double-arr)
  IdoublesEncoder
   (encode-doubles [e xs] e)
  IdoublesDecoder   
   (decode-doubles [d xs] xs))

;;A protocol for mapping to and from primitive arrays of bytes.
(defprotocol IBitsEncoder 
  (encode-bytes [e s] "Maps s to an array of bytes."))
(defprotocol IBitsDecoder
  (decode-bytes [d xs] 
    "Maps an array of bytes onto another domain."))

;;Some array-based math stuff.  This is preferable to using persistent vectors,
;;since we're typically going to be discarding the results (except when we 
;;find good solutions).  As a result, we'll deal with float arrays using these
;;wrapper functions.

;;NOTE - We'll probably replace these with something like the HipHip array lib.
;;=============================================================================

(defn sum-doubles-doubles [^doubles a1 ^doubles a2]
  (clojure.core/amap ^doubles a1 idx ^doubles ret  
                     (+  (aget ret idx)
                         (aget a2 idx))))

(defn ^doubles sum-doubles-doubles [^doubles a1 ^doubles a2]
  (clojure.core/amap ^doubles a1 idx ^doubles ret  
                     (+  (aget ret idx)
                         (aget a2 idx))))

(defn sum-ints-doubles 
  [^ints s ^doubles xs]
  (clojure.core/amap ^ints s idx ^ints ret  
                     (+  (aget ret idx)
                         (int (aget xs idx)))))

(defn sum-ints-ints [^ints a1 ^ints a2]
  (clojure.core/amap ^ints a1 idx ^ints ret  
     (+  (aget ret idx)
         (aget a2 idx))))

;;Matrix implementation. 
(defprotocol IDoubleMatrix 
  (dimensions [m])
  (^double get-matrix [m i j])
  (set-matrix [m i j ^double x])
  (map-matrix [m1 m2 f] [m1 f]))

(extend-protocol IDoubleMatrix
  (double-matrix)
  (dimensions  [m]   
    [(alength ^doubles m) (alength ^doubles (aget ^doubles m 0))])
  (get-matrix  [m i j] (aget ^doubles m i j))
  (set-matrix  [m i j x] (aset ^doubles m i j ^double x))
  (map-matrix  
    ([m1 m2 f] 
      (let [[height width] (dimensions m1)
            h (long height)
            w (long width)
            ret (make-array Double/TYPE h w)]
        (loop [i 0]
          (if (> i h) 
            ret
            (do 
              (loop [j 0]
                (when (< j w)
                  (aset ret i j (f (get-matrix m1 i j) 
                                   (get-matrix m2 i j)))
                  (recur (unchecked-inc j))))
              (recur (unchecked-inc i)))))))
    ([m1 f] 
      (let [[height width] (dimensions m1)
            h (long height)
            w (long width)
            ret (make-array Double/TYPE h w)]
        (loop [i 0]
          (if (> i h)
            ret 
            (do 
              (loop [j 0]
                (when (< j w)
                  (aset ret i j (f (get-matrix m1 i j)))
                  (recur (unchecked-inc j))))
              (recur (unchecked-inc i)))))))))



;;Right now, these mutate-in-place perturbations don't have any performance 
;;advantage over the array-cloning versions above.  They may for giant arrays.
;;As a result, they are not the default.
(comment 
(defn perturb-doubles! 
  [^doubles s ^doubles xs]
  (let [l (alength s)]
	  (loop  [idx 0]
	    (if (< idx  l)
	      (do (clojure.core/aset s idx   
	               (+  (aget s idx)
	                   (aget xs idx)))
	          (recur (unchecked-inc idx)))
	      s))))

(defn perturb-doubles! 
  [^doubles s ^doubles xs]
  (let [l (int (alength s))]
	  (loop  [idx (int 0)]
	    (when (< idx  l)
	      (clojure.core/aset s idx   
           (+  (aget s idx)
               (aget xs idx)))
       (recur (unchecked-inc idx)))
	      )
   s))

(defn read-doubles! 
  [^doubles s n]
  (let [n (int n)]
    (loop  [idx (int 0)]
      (if (< idx n)    
        (do (aget s 1)
          (recur (unchecked-inc idx)))
        s))))

(defn perturb-ints! 
  [^ints s ^doubles xs]
  (loop  [idx 0]
    (if (< idx  (alength s))
      (do (clojure.core/aset s idx   
               (+  (aget s idx)
                   (int (aget xs idx))))
          (recur (unchecked-inc idx)))
      s))))

(defmacro record-template [type-name fields & imps]
  (let [absolute-class-name (symbol (str (ns-name *ns*) \. type-name))
        ctor       (symbol (str "->" type-name))
        map-ctor   (symbol (str "map->" type-name))
        imp-names  (map first imps)
        imp-specs  (map second imps)
        field-keys (map keyword fields)
        class-fields (map (fn [fld] (symbol (str "-" fld))) fields)
        unsupported-op '(throw (java.lang.UnsupportedOperationException.))
        this       (gensym "this_") ;re-used gensyms.
        G          (gensym "G_")
        k          (gensym "k_")
        v          (gensym "v_")
        drifting-fields (fn [symb] 
                          (->> (repeat (vec fields))
                               (take (count fields))
                               (map-indexed  
                                 (fn [i fld-vec] (assoc fld-vec i symb)))))]    
  `(let
     []
     (declare ~ctor)
     (declare ~map-ctor)
     (deftype* ~type-name ~absolute-class-name [~@field-keys ~'__meta ~'__extmap]
       :implements   [clojure.lang.IRecord
                      clojure.lang.IHashEq
                      clojure.lang.IObj
                      clojure.lang.ILookup
                      clojure.lang.IKeywordLookup
                      clojure.lang.IPersistentMap
                      java.util.Map
                      java.io.Serializable
                      ~@imp-names]
  (~'entrySet [~this] (clojure.core/set ~this))
  (~'values   [~this] (clojure.core/vals ~this))
  (~'keySet   [~this] (clojure.core/set (keys ~this)))
  (~'clear    [~this] ~unsupported-op)
  (~'putAll   [~this] ~unsupported-op)
  (~'remove   [~this ~k] ~unsupported-op)
  (~'put      [~this ~k ~v]  ~unsupported-op)
  (~'get [~this ~k] (~'.valAt ~this ~k))
  (~'containsValue [~this ~v]  
     (clojure.core/boolean 
       (clojure.core/some #{~v} (clojure.core/vals ~this))))
  (~'isEmpty [~this]  (= 0 (~'.count ~this)))
  (~'size [~this]     (~'.count ~this))
  (~'without [~this ~k] 
    (if  (clojure.core/contains? #{~@field-keys} ~k)
      (clojure.core/dissoc (clojure.core/with-meta  (clojure.core/into {} ~this)  ~'__meta) ~k)
      (new ~type-name ~@fields ~'__meta (clojure.core/not-empty (clojure.core/dissoc ~'__extmap ~k)))))
  (~'assoc [~this ~k ~G]
     (clojure.core/condp clojure.core/identical? ~k    
      ~@(interleave field-keys 
          (map (fn [flds] `(new ~type-name ~@flds ~'__meta ~'__extmap))
               (drifting-fields G)))               
      (new ~type-name ~@fields ~'__meta  (clojure.core/assoc ~'__extmap ~k ~G))))
;           :x (new ~type-name G# y z ~'__meta ~'__extmap)
;           :y (new ~type-name x G# z ~'__meta ~'__extmap)
;           :z (new ~type-name x y G# ~'__meta ~'__extmap)
  (~'iterator [~this] (clojure.lang.SeqIterator. (~'.seq ~this)))
  (~'seq [~this] (clojure.core/seq 
                 (clojure.core/concat
                      ~(vec (for [[fld-key fld] (map vector field-keys fields)]
                              `(new clojure.lang.MapEntry ~fld-key ~fld)))
                      ~'__extmap)))
;                      [(new clojure.lang.MapEntry :x x)
;                       (new clojure.lang.MapEntry :y y)
;                       (new clojure.lang.MapEntry :z z)]
  (~'entryAt [~this ~k]
     (let  [~v (~'.valAt ~this ~k ~this)]
       (when-not  (clojure.core/identical? ~this  ~v)
         (clojure.lang.MapEntry. ~k ~v))))
  (~'containsKey [~this ~k] 
    (clojure.core/not 
      (clojure.core/identical?  ~this (~'.valAt ~this ~k ~this))))
;                      (= x (. ~G -x))
;                      (= y (. ~G -y))
;                      (= z (. ~G -z))
  (~'equiv [~this ~G]
   (clojure.core/boolean
    (clojure.core/or
     (clojure.core/identical? ~this ~G)
     (clojure.core/when  (clojure.core/identical? 
                           (clojure.core/class ~this)  
                           (clojure.core/class ~G))
       (clojure.core/let [~G ~G] 
         (clojure.core/and
           ~@(for [[fld class-fld] (map vector fields class-fields)]
               `(= ~fld (. ~G ~class-fld)) ))
           (= ~'__extmap (. ~G ~'__extmap)))))))
  (~'cons [~this e#] (#'clojure.core/imap-cons ~this e#))
  (~'empty [~this]
   (throw (java.lang.UnsupportedOperationException.
            (str "Can't create empty: " ~absolute-class-name))))
  (~'count [~this] (clojure.core/+ ~(count fields) (clojure.core/count ~'__extmap)))
;     :x  (reify clojure.lang.ILookupThunk
;           (get [thunk gtarget]
;             (if (identical? (class gtarget) gclass)
;                 (. gtarget -x)
;                 thunk)))
;     :y (reify clojure.lang.ILookupThunk
;          (get [thunk gtarget]
;            (if
;              (identical? (class gtarget) gclass)
;              (. gtarget -y)
;              thunk)))
;     :z (reify clojure.lang.ILookupThunk
;          (get [thunk gtarget]
;            (if
;              (identical? (class gtarget) gclass)
;              (. gtarget -z)
;              thunk)))
  (~'getLookupThunk [~this ~k]
   (let [~'gclass (clojure.core/class ~this)]
    (clojure.core/case  ~k
      ~@(interleave field-keys 
          (map (fn [fld] 
                 `(reify clojure.lang.ILookupThunk
                    (clojure.core/get [~'thunk ~'gtarget]
                      (if (clojure.core/identical? 
                            (clojure.core/class gtarget) ~'gclass)
                        (. ~'gtarget ~fld)
                        ~'thunk)))) class-fields)) 
     nil)))
  (~'valAt [~this ~k else#] 
         (clojure.core/case ~k ~@(interleave field-keys fields)
           (clojure.core/get ~'__extmap ~k else#)))
  (~'valAt [~this ~k] (~'.valAt ~this ~k nil))
  (~'withMeta [~this ~G] (new ~type-name ~@fields ~G ~'__extmap))
  (~'meta [~this] ~'__meta)
  (~'equals [~this ~G] (clojure.lang.APersistentMap/mapEquals ~this ~G))
  (~'hashCode [~this]  (clojure.lang.APersistentMap/mapHash ~this))
  (~'hasheq   [~this]  (clojure.core/bit-xor 1458456690 
                           (clojure.lang.APersistentMap/mapHasheq ~this)))
  ~@imp-specs)
 (clojure.core/import ~absolute-class-name)
 (clojure.core/defn ~ctor 
  ~(str "Positional factory function for class " absolute-class-name)
  [~@fields ]
  (new ~absolute-class-name ~@fields))
 (clojure.core/defn ~map-ctor
   ~(str "Factory function for class " absolute-class-name 
         ", taking a map of keywords to field values.")
   ([m#] (~(symbol (str absolute-class-name "/create")) m#)))
 ~absolute-class-name)))

;  clojure.lang.IPersistentStack
;  (peek [this]
;    (when-not (.isEmpty this)
;      (let [f (first priority->set-of-items)]
;        (MapEntry. (first (val f)) (key f)))))

;  (pop [this]
;    (if (.isEmpty this) (throw (IllegalStateException. "Can't pop empty priority map"))
;      (let [f (first priority->set-of-items),
;            item-set (val f)
;            item (first item-set),
;            priority (key f)]
;        (if (= (count item-set) 1)
;          ;If the first item is the only item with its priority, remove that priority's set completely
;          (PersistentPriorityMap.
;            (dissoc priority->set-of-items priority)
;            (dissoc item->priority item)
;            (meta this))
;          ;Otherwise, just remove the item from the priority's set.
;          (PersistentPriorityMap.
;            (assoc priority->set-of-items priority (disj item-set item)),
;            (dissoc item->priority item)
;            (meta this))))))


;;Implementations are pending for the lexmap.      
;;      java.io.Serializable  ;Serialization comes for free with the other things implemented
;;      clojure.lang.MapEquivalence
;;      Map ;Makes this compatible with java's map
;;      (size [this] (count item->priority))
;;      (isEmpty [this] (zero? (count item->priority)))
;;      (containsValue [this v] (contains? (priority->set-of-items this) v))
;;      (get [this k] (.valAt this k))
;;      (put [this k v] (throw (UnsupportedOperationException.)))
;;      (remove [this k] (throw (UnsupportedOperationException.)))
;;      (putAll [this m] (throw (UnsupportedOperationException.)))
;;      (clear [this] (throw (UnsupportedOperationException.)))
;;      (keySet [this] (set (keys this)))
;;      (values [this] (vals this))
;;      (entrySet [this] (set this))
  
