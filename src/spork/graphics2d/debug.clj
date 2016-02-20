
(ns spork.graphics2d.debug
  (:require [spork.graphics2d.canvas :refer :all]
            [spork.graphics2d.swing :as provider]))

;;primitive shape analysis...
;;these functions help us to deconstruct the operations implicit in our shape
;;combinators, and let us get at the ast behind them, as well as the
;;primitive instructions.
;;Note: perhaps a "better" model would be to revert to the
;;scene-graph API I built originally (of which sketch was a
;;distillation).  

;;note: this only works with swing atm...it should, ideally,
;;be decoupled from the backend.
(defn analyze
  "Allows us to break a shape down into its primitive drawing 
   instructions."
  [shp]
  (let [{:keys [x y width height]} (shape-bounds shp)
        width (+ x width)
        height (+ y height)
        dg (provider/->debug-graphics width height)
        init-stroke (get-stroke dg)]
    (vary-meta (get-path (draw-shape shp dg)) ;;if the shape has meta, we want to capture in instructions.
               assoc :init-stroke init-stroke)))

;;simple compiler for graphics instructions...
;;we basically trim down the state changes required to draw the shape.
(defn simplify [xs]
  (transduce (comp (partition-by first)
                   (map  (fn xd [instructions]
                           (case (ffirst instructions)
                             :translate    (reduce (fn collapse [[_ x y] [_ x2 y2]]
                                                                [:translate (+ x x2) (+ y y2)])
                                                              instructions)
                             :scale        (reduce (fn collapse [[ins x y] [_ x2 y2]]
                                                                [:scale (* x x2) (* y y2)])
                                                              instructions)
                             (last instructions))))                 
                   (filter (fn f [x]
                             (case (first x)
                               :translate (or (not (zero? (nth x 1))) (not (zero? (nth x 2))))
                               :scale     (and (not= (nth x 1) 1.0) (not= (nth x 2) 1.0))
                               true))))
             (completing
              (fn xd [acc x] (conj acc x)))
             (vary-meta [] assoc :init-stroke (:init-stroke (meta xs)))
             xs))

(defn deconstruct
  "Given a shp, returns all the primitive shapes and their transforms.  Also records any state changes 
  in order."
  [shp]
  (let [xs (simplify (analyze shp))
        m  (meta xs)]
    (transduce (filter (fn [xs]
                         (not (#{:translate :scale :rotate} (first xs)))))
               (completing
                (fn [acc shp]
                  (conj acc shp)))
               (vary-meta [] assoc :init-stroke (:init-stroke m))
               xs)))

(defn draw-instructions
  "Given a sequence of graphics instructions (xs), draw them sequentially to 
   the canvas c."
  [xs c]
  (reduce (fn [c x]
            (case (first x)
              :line    (let [[_ clr x y x2 y2 xform] x]
                          (-> (set-transform c xform)
                              (draw-line clr x y x2 y2)))
              :string  (let [[_ clr font s x y xform] x]
                         (-> (set-transform c xform)
                             (draw-string clr font s x y)))
              :image   (let [[_  img transparency x y xform] x]
                         (-> (set-transform c xform) 
                             (draw-image img transparency x y)))
              :stroke  (set-stroke c (second x))
              :alpha   (set-alpha  c (second x))
              (:begin :end) c ;no-ops
              (throw (Exception. (str "unhandled instruction:" x)))))
          c xs))

(defn node? [x]
  (#{:stroke
    :alpha
    :translate
    :rotate
     :font} (first x)))

(def ops #{:begin :end})
(defn closes? [[l t1] [r t2]]
  (and (identical? l :begin)
       (identical? r :end)
       (= t1 t2)))
(defn primitive? [itm]
  (do ;(println itm)
      (not (ops (first itm)))))

;;note on "naming"
;;we can use metadata via the ^tag to indicate shapes that we'd like to
;;"name" in a particular group of shapes.  Given that information, we
;;have the opportunity to communicate said information in the node's
;;output when rendering.  This is actually going pretty ass-backwards.
;;                                        ;

;;we know that getting a segment, or block, from a flat seq will leave us
;;with [seg unparsed] as the result.
;;this is ridiculous, I should be able to pop out a block parser in no time.
;;we either have a primitive, which we conjoin onto the acc, or a segment,
;;which we get via get-segments again.
;;we need to keep track of the fact that we're in a segment though.
;;as we recurse, we need to know what we're looking for to close the current
;;segment at each level of recursion.
(defn first-vector [xs]
  (let [x (first xs)]
    (if (vector? x) x nil)))

;;note: we may have implicit groups...specifically custom rendering
;;functions that aren't prepended with :begin and :end.
;;in this case, it's just a primitive vov...
;;so, we want to 
(defn next-segment
  [l init xs]
   (loop [acc         init
          remaining   xs]    
     (if-let [x   (first-vector remaining)]
       (cond (primitive? x) (recur (conj acc x) (rest remaining)) ;;add it to the acc
             (and l (closes? l x))   ; we have a left already...
             ;;we completed a segment.
             (do ;(println [(clojure.string/join (repeat @lvl "-")) :closing x])
                                        ;(swap! lvl dec)
               [(conj acc x) (rest remaining)])
              :else 
              (let [;; _ (swap! lvl inc)
                    ;; _ (println [(clojure.string/join (repeat @lvl "-")) :recursing x])
                    [seg rem] (next-segment x [x] (rest remaining))]
                (recur (conj acc seg) rem)))
       acc)))

(defn get-segments [xs]
  (let [res (next-segment nil [] xs)]
    (if (primitive? (first res))
      (-> (into '[[:begin group]]
                res)
          (conj '[:end group])))))             

(defn segments->tree [vov]
  (cond (keyword? (first vov)) vov
        (vector?  (first vov))                     
        (let [props     (second (first vov))
              op        (if (map? props) :shape
                            :group)
              n         (case  op
                          (:group :shape) 1
                         2)
              [t & xs :as nd]   (nth vov (dec n))
              tl (fn [xs] (if (== n 2) (butlast xs) xs))]
          (with-meta 
            {:node nd  :children (vec (map segments->tree  (tl  (butlast  (drop n  vov)))))}
            {:properties props}
          ))))

(defn shape->nodes [shp]
  (->> (analyze shp)
       (get-segments)
       (segments->tree)))
