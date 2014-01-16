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

