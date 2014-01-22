;;Helper functions and macros for working with multidimensional arrays.
(ns spork.util.array)

(defn ^longs  longs-2d [w h]
  (make-array Long/TYPE w h))
(defn ^doubles doubles-2d [w h]
    (make-array Double/TYPE w h))

;;from christophe Garande   - for working on 2d arrays efficiently.
(defmacro deep-aget 
  ([hint array idx]
    `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
    `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
       (deep-aget ~hint a# ~@idxs))))                   

;;from christophe Garande   - for working on 2d arrays efficiently.
(defmacro deep-aset [hint array & idxsv]
  (let [hints '{booleans boolean
                bytes byte
                chars char
                longs long
                ints int
                shorts short
                doubles double
                floats float} 
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~'objects ~array ~@idxs)
                        array)
        a-sym (with-meta (gensym "a") {:tag hint})]
      `(let [~a-sym ~nested-array]
         (aset ~a-sym ~idx ~v))))  


;;Functions for providing effecient keyed access to 
;;arrays.  Came up in the context of graph algos...
;;We want the speed of random access lookup, the storage
;;contiguity of arrays, etc, but the flexibility of interfacing with
;;nodes that are mapped to nice literal values (like keywords) rather
;;than simple indices.
(defn bindings->case [l rs inner]
  `(~l  ~(seq (into '[case r] (reduce (fn [acc r] 
                                   (-> acc (conj r) (conj (inner l r)) )) [] rs)))))
(defn case-map [m inner] 
  (let [xs (set (keys m))]
   (concat '(case l) 
     (reduce (fn [acc l] 
               (let [binds (bindings->case l (disj xs l) inner)]
                 (-> acc (conj (first binds)) (conj (second binds))))) [] xs))))

(defmacro keyed-array-getter [keymap hint] 
  (let [hints '{longs   long 
                doubles double}
        inner (fn [l r] `(arr/deep-aget ~hint ~'arr ~(get keymap l) ~(get keymap r)))
        m (case-map keymap inner)]
    `(~(with-meta 'fn {:tag (get hints hint)}) [~(with-meta 'arr {:tag hint}) ~'l ~'r]
          ~m)))

(defmacro keyed-array-setter [keymap hint] 
  (let [hints '{longs   long 
                doubles double}
        inner (fn [l r] `(arr/deep-aset ~hint ~'arr ~(get keymap l) ~(get keymap r) ~'v))
        m (case-map keymap inner)]
    `(~(with-meta 'fn {:tag (get hints hint)}) 
      [~(with-meta 'arr {:tag hint}) ~'l ~'r ~(with-meta 'v {:tag (get hints hint)})]
          ~m)))  
