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
;; (defn bindings->case [l rs inner]
;;   `(~l  ~(seq (into '[case r] (reduce (fn [acc r] 
;;                                    (-> acc (conj r) (conj (inner l r)) )) [] rs)))))
;; (defn case-map [m inner] 
;;   (let [xs (set (keys m))]
;;    (concat '(case l) 
;;      (reduce (fn [acc l] 
;;                (let [binds (bindings->case l (disj xs l) inner)]
;;                  (-> acc (conj (first binds)) (conj (second binds))))) [] xs))))

;;Alternative formulation...should still be fastish...

;; (defn bindings->case [l rs inner]
;;   `(~l  ~(seq (into '[case r] (reduce (fn [acc r] 
;;                                    (-> acc (conj r) (conj (inner l r)) )) [] rs)))))
;; (defn case-map [m inner] 
;;   (let [xs      (set (keys m))
;;         fronts  (mapcat (fn [x] (let [nodes (disj xs x)] (for [n nodes] [x n]))) xs)
;;         backs   (map (fn [lr] [ (second lr) (first lr)]) fronts)
;;         cases   (distinct (concat fronts backs))]    
;;     (concat '(case l) 
;;       (reduce (fn [acc lr] 
;;                 (-> acc (conj lr) (conj (inner (first lr) (second lr))))) [] cases))))

;; (defmacro keyed-array-getter [keymap hint] 
;;   (let [hints '{longs   long 
;;                 doubles double}
;;         km (if (map? keymap) keymap (eval keymap))
;;         inner (fn [l r] `(deep-aget ~hint ~'arr ~(get km l) ~(get km r)))
;;         m  (case-map km inner)]
;;     `(~(with-meta 'fn {:tag (get hints hint)}) [~(with-meta 'arr {:tag hint}) ~'l ~'r]
;;           ~m)))

;; (defmacro keyed-array-setter [keymap hint] 
;;   (let [hints '{longs   long 
;;                 doubles double}        
;;         km (if (map? keymap) keymap (eval keymap))
;;         inner (fn [l r] `(deep-aset ~hint ~'arr ~(get km l) ~(get km r) ~'v))
;;         m (case-map  km inner)]
;;     `(~(with-meta 'fn {:tag (get hints hint)}) 
;;       [~(with-meta 'arr {:tag hint}) ~'l ~'r ~(with-meta 'v {:tag (get hints hint)})]
;;           ~m)))  


(defmacro keyed-array-setter [keymap hint] 
  (let [hints '{longs long
                doubles double}]    
    `(let [km# ~(if (map? keymap) keymap (eval keymap))]
       (~(with-meta 'fn {:tag (get hints hint)}) 
        [~(with-meta 'arr {:tag hint}) l# r# ~(with-meta 'v {:tag (get hints hint)})]
          (deep-aset  ~hint ~'arr (get km# l#) (get km# r#) ~'v)))))

(defmacro keyed-array-getter [keymap hint] 
  (let [hints '{longs long
                doubles double}]    
     `(let [km# ~(if (map? keymap) keymap (eval keymap))]
        (~(with-meta 'fn {:tag (get hints hint)}) 
         [~(with-meta 'arr {:tag hint}) l# r#]
           (let [~(with-meta 'left {:tag 'long}) (get km# l#)
                 ~(with-meta 'right {:tag 'long}) (get km# r#)]
             (deep-aget ~hint ~'arr ~'left ~'right))))))
;)
;;FYI, the case expansion is somewhat useless for large arrays.  The
;;generated code is too large.
;;array testing....
(comment 
(def the-nodes (into {} (for [n (range 100)] [(keyword (str "A" n)) n])))


(defn ^long  get-table! [^longs arr ^long x ^long y]          (deep-aget longs arr x y))
(defn ^longs set-table! [^longs arr ^long x ^long y ^long v]  (deep-aset longs arr x y v))

(defn ^long  kget-table!  [^longs arr x  y]          (deep-aget longs arr (get the-nodes x) (get the-nodes y)))
(defn ^longs kset-table!  [^longs arr  x y ^long v]  (deep-aset longs arr (get the-nodes x) (get the-nodes y) v))

(def getter! (keyed-array-getter the-nodes longs))
(def setter! (keyed-array-setter the-nodes longs))

(let [^longs the-array (make-array Long/TYPE 100 100)]
  (do  (println "Getting")
       (time (dotimes [i 100000]
               (get-table! the-array 50 50)))
       (time (dotimes [i 100000]
               (kget-table! the-array :A50 :A50)))
       (time (dotimes [i 100000]
               (getter! the-array :A50 :A50)))
       (println "Setting")
       (time (dotimes [i 100000]
               (set-table! the-array 50 50 10)))
       (time (dotimes [i 100000]
               (kset-table! the-array :A50 :A50 10)))
       (time (dotimes [i 100000]
               (setter! the-array :A50 :A50 10)))))
)
;; (fn  [arr l r]
;;  (case  l
;;   :A2  (case   r
;;          :A1 2
;;          :A4 2
;;          :A3 3
;;          :A0 4)
;;   :A1  (case   r
;;          :A2   2
;;          :A4   3
;;          :A3   4
;;          :A0   5)
;;   :A4  (case   r
;;          :A2   2
;;          :A1   3
;;          :A3   4
;;          :A0   5)
;;   :A3  (case   r
;;          :A2   2
;;          :A1   3
;;          :A4   4
;;          :A0   5))
;;  )

;; (fn [l r] 
;;  (case [l r]
;;    [:A0 :A1] (deep-aget longs the-array 0 1)
;;    [:A0 :A2] (deep-aget longs the-array 0 1)
;;    [:A0 :A3] (deep-aget longs the-array 0 1)
;;    [:A0 :A4] (deep-aget longs the-array 0 1)
;;    [:A1 :A0] (deep-aget longs the-array 0 1)
;;    [:A1 :A2] (deep-aget longs the-array 0 1)
;;    [:A1 :A3] (deep-aget longs the-array 0 1)
;;    [:A1 :A4] (deep-aget longs the-array 0 1)
;;    [:A2 :A0] (deep-aget longs the-array 0 1)
;;    [:A2 :A1] (deep-aget longs the-array 0 1)
;;    [:A2 :A3] (deep-aget longs the-array 0 1)
;;    [:A2 :A4] (deep-aget longs the-array 0 1)
;;    [:A3 :A0] (deep-aget longs the-array 0 1)
;;    [:A3 :A1] (deep-aget longs the-array 0 1)
;;    [:A3 :A2] (deep-aget longs the-array 0 1)
;;    [:A3 :A4] (deep-aget longs the-array 0 1)
;;    [:A4 :A0] (deep-aget longs the-array 0 1)
;;    [:A4 :A1] (deep-aget longs the-array 0 1)
;;    [:A4 :A2] (deep-aget longs the-array 0 1)
;;    [:A4 :A3] (deep-aget longs the-array 0 1)))
