;;Implementation of several abstractions for drawing dynamic trends, such as animated
;;or live data in the form of area charts, scatter charts, stacked area, etc.
;;Probably should move this into something like spork.geometry or somewhere
;;other than the main ns.
(ns spork.trends
  (:require [spork.graphics2d [canvas :as canvas]]
            [spork.sketch :refer :all]
            [spork.geometry.shapes :refer :all]
            [spork.util.temporal :as temp]))

(defn unroll-map [m]
  (for [[tr xs] m
        x xs]
    (into [tr] x)))

(defn roll-map [txys]
  (reduce (fn [acc [tr x y]]
            (assoc acc tr (conj (get acc tr []) [x y]))) {} txys))


;;__Abstract Trend Type__
;;We'll work with trends a lot, specifically live or dynamic trends.  These trends
;;are typically (x,y) cartesian coordinates associated with trend-name.
(defprotocol ITrends
  (add-points  [ts trxys])
  (drop-points [ts trxys])
  (set-order   [ts order])
  (set-color   [ts color]))

;;One common idiom is to use "slices" to dynamically update the latest points in the trend.
;;If we think of a time series, with the "Right" edge being the latest point in the series,
;;we can add a slice of points - across multiple trends - efficiently.  This is primarily to
;;ease the cost of drawing and redrawing trends.  Depending on the underlying implementation,
;;we may retain only the current trendline, or we may dynamically recompute the trends in
;;some fashion.  Either way, adding "slices" of trends at a particular time is useful.
(defn add-slices [tr xs]
  (reduce (fn [tr sl] (add-points tr sl)) tr xs))

(defn add-slice   [tr s] (add-slices tr [s]))

;;__Stacked Area Charts__

;;A stacked area chart is pretty easy too....
;;If we're careful.
;;We can make it dynamic pretty simply as well, probably loads faster than
;;jfreechart.  We're just filling in a polygon over time.  Actually, we're
;;drawing lines over time, and not erasing them....so, all we "really"
;;have to do is figure out how to draw a slice, and then advance multiple
;;slices across a canvas that doesn't erase (basically drawing to a
;;dirty buffer)

(defrecord stacked-trends [trends colors buffer latest]
  canvas/IShape
  (draw-shape [shp c]
    (canvas/draw-shape buffer c)))
  
(defn ->dynamic-stack [trend->color w h]
  (->stacked-trends (zipmap (keys trend->color) (repeat []))
                    trend->color
                    (spork.graphics2d.image/make-image w h)))

(defn ->stacked-samples [trend-pts trend-order]
  (let [samples     (temp/sample-trends (unroll-map trend-pts) first second (fn [[tr x y]] y))
        ts          (temp/minimum-samples (temp/get-samples  [samples]))
        new-trends  (atom (transient {}))
        prev        (atom 0)]
    (->> ts
         (reduce 
          (fn [acc x]
            (do (reset! prev 0)
                (doseq [[tr y] (sort-by (comp trend-order first) (temp/samples-at samples x))]
                  (do 
                    (swap! acc (fn [m] (let [xs (get m tr [])]
                                         (assoc! m tr (conj xs [x (+ y @prev)])))))
                    (swap! prev + y)))
                acc))
          new-trends
          )
         (deref)
         (persistent!))))

(defn ->stacked-area [trend-pts trend-order trend-color]
  (let [areas   (mapv (fn [[tr xys]]
                        (->area (trend-color tr) 0 0 xys))
                      (sort-by (fn [[tr _]] (-  (trend-order tr)))
                               (seq (->stacked-samples trend-pts trend-order))))]
    areas))

(defn ->adjacent-area [trend-pts trend-order trend-color]
  (let [areas   (mapv (fn [[tr xys]]
                        (->area (trend-color tr) 0 0 xys))
                      (sort-by (fn [[tr _]]  (trend-order tr))
                               (seq  trend-pts)))]
    (stack areas)))

;;can we introduce multiple samplers?
;;A discrete sampler for every batch of known points?
;;Maybe introduce operations like union-samplers....so we can
;;check to see if there are samples in other (later) samplers,
;;rather than having to maintain a monolothic sampler?  This seems more in line
;;with the notion of having a sampling tree.
(defrecord dynamic-trends [trend-points trend-order trend-color areas bnds]
  canvas/IShape
  (shape-bounds [s] bnds)
  (draw-shape [s c] (if areas
                      (canvas/draw-shape areas c))
                      c)
  ITrends
  (add-points  [ts trxys]
    (let [new-points (reduce (fn [acc [tr x y]]
                               (assoc acc tr (conj (get acc tr []) [x y])))
                             trend-points
                             trxys)
          new-areas  (->stacked-area new-points trend-order trend-color)]
      (dynamic-trends. new-points trend-order trend-color new-areas
                       (canvas/shape-bounds new-areas)))) 
  (drop-points [ts trxys]  ts)
  (set-order [ts order] (dynamic-trends. trend-points order trend-color (->stacked-area trend-points order trend-color) bnds))
  (set-color [ts color] (dynamic-trends. trend-points trend-order color (->stacked-area trend-points trend-order color) bnds)))

(defn ->trend-line [trend-points trend-order trend-color]
  (let [xs (sort-by (fn [x] (trend-order (first x))) trend-points)
        ynow (atom 0)]
    (reduce (fn [acc [tr x y]]
              (let [res (conj acc (->rectangle (trend-color tr) 0 @ynow 1 y))]
                (swap! ynow + y)
                res)) []
                xs)))

;;We need to cover the case in which there is nothing to add...
;;Note, we're also inferring the x coord of the trendline from the points....
;;If there's nothing to add, we don't update the trendline.
;;Or we leave the trendline at its last position.

;;We only ever draw the most recent trend-point.  Basically
;;We draw stacked lines.  So, one for each trend.
(defrecord dirty-trends [trend-points trend-order trend-color trend-line x]
  canvas/IShape
  (shape-bounds [s] (assoc (canvas/shape-bounds trend-line) :x x))
  (draw-shape [s c] (if (seq trend-points)
                      (canvas/draw-shape trend-line c)
                      c))                      
  ITrends
  (add-points  [ts trxys]
    ;; (let [new-points (reduce (fn [acc [tr x y]]
    ;;                            (assoc acc tr (conj (get acc tr []) [x y])))
    ;;                          trend-points
    ;;                          trxys)]
      (dirty-trends. trxys trend-order trend-color
                     (->trend-line trxys trend-order trend-color)
                     (or (second (first trxys)) x)))
  (drop-points [ts trxys]  ts)
  (set-order [ts order] (dirty-trends. trend-points order trend-color (->trend-line trend-points order  trend-color) x))
  (set-color [ts color] (dirty-trends. trend-points trend-order color (->trend-line trend-points trend-order color)  x)))

(defn ->dirty-trends [trxys trend-order trend-color]
  (dirty-trends. trxys trend-order trend-color
                 (when trxys (->trend-line trxys trend-order trend-color))
                 (when trxys (second (first trxys)))))


    
;;used to be ->dtrends
(defn ->dtrends
  [xytrs order color]
  (add-points 
   (->dynamic-trends {} order color nil nil)
   xytrs))

;;Our other option is to just extend to trends protocol to
;;atom, and infer that the stored data is a dtrends object.
(defn ->live-trends [xys trend-order trend-color]
  (let [trends  (atom (if (seq xys)
                        (->dtrends xys trend-order trend-color)
                        (->dynamic-trends xys trend-order trend-color
                                          nil
                                          (spork.protocols.spatial/bbox 0 0 1 1))))]
    (reify
      clojure.lang.IDeref
      (deref [obj]        (deref trends))
      canvas/IShape
      (shape-bounds [s]   (canvas/shape-bounds @trends))
      (draw-shape   [s c] (canvas/draw-shape   @trends c))
      ITrends
      (add-points   [ts trxys] (do (swap! trends add-points trxys) ts))
      (drop-points  [ts trxys]  ts)
      (set-order [ts order]   (do (swap! trends set-order order) ts))
      (set-color [ts color]   (do (swap! trends set-color color) ts)))))

(defn ->dirty-live-trends [xys trend-order trend-color]
  (let [trends  (atom  (->dirty-trends  xys trend-order trend-color
                                       ))
                        ]
    (reify
      clojure.lang.IDeref
      (deref [obj]        (deref trends))
      canvas/IShape
      (shape-bounds [s]   (canvas/shape-bounds @trends))
      (draw-shape   [s c] (canvas/draw-shape   @trends c))
      ITrends
      (add-points  [ts trxys] (do (swap! trends add-points trxys) ts))
      (drop-points [ts trxys]  ts)
      (set-order [ts order]   (do (swap! trends set-order order) ts))
      (set-color [ts color]   (do (swap! trends set-color color) ts)))))

(defn ->empty-trends [order color]
  (->dynamic-trends {} {:a 0 :b 1 :c 2}
                    {:a :red :b :blue :c :green}
                    nil
                    (spork.protocols.spatial/bbox 0 0 1 1)))


(defn trends-from [trs & {:keys [ctor]
                        :or {ctor ->dirty-live-trends}}]
  (let [[l r] (reduce (fn [[l r] [k idx v]]
                        [(assoc l k idx)
                         (assoc r k v)])
                      [{} {}] 
                      (map-indexed (fn [idx [k v]]
                                     [k idx v])
                                    (partition 2 trs)))]
    (ctor [] l r)))
