;;A namespace for exploring speed ups using indexed traversals on
;;cache coherent chunked sequences and arrays.
(ns spork.util.indexed
  (:require [spork.data [mutable :as m]]))


(def xs (vec (range 1000000)))

;;traversing a vector

(defmacro iter! [idx coll & body]
  `(let [n#   (count ~coll)]
     (loop [~idx  0]
       (if (== ~idx n#) ~coll
         (do ~@body
             (recur (unchecked-inc ~idx)))))))

(defn  iter [f ^objects arr]  
  (iter! idx arr (f (aget ^objects arr idx))))

(defn  iteri [f ^objects arr]  
  (iter! idx arr (f idx (aget ^objects arr idx))))

(defn  map! [f ^objects arr]  
  (iter! idx arr (aset ^objects arr idx (f (aget ^objects arr idx)))))

(defn  map-indexed! [f ^objects arr]  
  (iter! idx arr (aset ^objects arr idx (f idx (aget ^objects arr idx)))))


(defn  filter! [pred ^objects arr]
  (let [acc (m/make-array-list)]
    (do  (iter! idx arr 
           (let [v (aget ^objects arr idx)] 
             (when (pred v) (m/add-list acc v))))
         (to-array acc))))

(defn  partition! [pred ^objects arr]
  (let [l (m/make-array-list)
        r (m/make-array-list)]
    (do  (iter! idx arr 
           (let [v (aget ^objects arr idx)] 
             (if (pred v) (m/add-list l v) (m/add-list r v))))
         (list (to-array l) (to-array r)))))

(defn some! [f ^objects arr]
  (let [n   (count arr)]
    (loop [idx  0]
      (if (== idx n) nil
          (let [v  (aget arr idx)]
            (if (f v) v
                (recur (unchecked-inc idx))))))))

(defn some-index! [f  ^objects arr]
  (let [n   (count arr)]
    (loop [idx  0]
      (if (== idx n) nil
          (if (f (aget arr idx)) idx
              (recur (unchecked-inc idx)))))))

(defn reverse! [^objects arr]
  (let [n   (dec (count arr))
        res (aclone arr)]
    (loop [idx n]
      (if (== idx -1) res
          (do  (aset res (- n idx) (aget arr idx))
               (recur (unchecked-dec idx)))))))
(defn nreverse! [^objects arr]
  (loop [l 0
         r (dec (count arr))]
    (if (< r l) arr
        (let [right (aget arr r)]
          (do  (aset  arr r (aget arr l))                 
               (aset  arr l right)
               (recur (unchecked-inc l) (unchecked-dec r)))))))

(defn fold!  [f state ^objects arr]
  (let [s (atom state)
        n (count arr)]
    (loop [idx 0]
      (if (== idx n) @s
      (do (reset! s (f @s (aget arr idx)))
          (recur (unchecked-inc idx)))))))

(defn fold-between!  [f state from to ^objects arr]
  (assert (and (> to from) (> from -1) (< to (count arr))))
  (loop [idx from
         s   state]
    (if (== idx to) (f s (aget arr idx))
        (recur (unchecked-inc idx) (f s (aget arr idx))))))

(defn fold-back!  [f state ^objects arr]
  (loop [idx (dec (count arr))
         s state]
    (if (== idx 0) (f state (aget arr idx)))
        (recur (unchecked-inc idx) (f state (aget arr idx)))))
       
;;it would be nice...
;;to define a for-loop/recur...           
         
;;finding in a vector




