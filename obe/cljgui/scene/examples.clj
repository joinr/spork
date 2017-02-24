(ns spork.cljgui.scene.examples
  (:use  [spork.mvc]
         [spork.cljgui.components swing]
         [spork.cljgui.scene scenegraph]
         [spork.geometry shapes]
         [spork.events observe native]))

;a simple scene for drawing mathematical plots...

(defn- evenize [n]
  (if (even? n) n
    (inc n)))

(defn ->gridlines [width height & {:keys [color xmax xscale ymax yscale]
                                   :or {color :black
                                        xmax width
                                        ymax height
                                        xscale (/ width 50)
                                        yscale (/ height 50)}}]
	  (let [hs (for [i (map inc (range (/ xmax xscale)))]
	              (->line color
	                   (* i (int xscale)) 0
	                   (* i (int xscale)) width))
	        vs (for [i (map inc (range (/ ymax yscale)))]
	            (->line color
	                   0 (* i (int yscale))
	                   height (* i (int yscale)) ))]
	    (->group (concat vs hs))))


(defn- square [n] (* n n))
(defn plot-function
  "Evaluate f across all samples.  While doing so, effeciently gather the
   maxima and minima in each direction."
  [f samples]
  (loop [l 0, r 0, u 0, d 0, xs samples, coords []]
    (if-let [x (first xs)]
         (let [y (f x), lnxt (min x l), rnxt (max x r), unxt (max y u),
               dnxt (min y d)]
           (recur lnxt rnxt unxt dnxt (rest xs) (conj coords [x y])))
         {:xmin l :xmax r :ymax u :ymin d :coords coords})))


(defn- mirror-range
  [n] (concat (map negate (reverse (map inc (range (dec n))))) (range n)))


(defn ->line-segments [color coords]
  (->group (for [[[x1 y1] [x2 y2]] (partition 2 1 coords)]
             (->line color x1 y1 x2 y2))))

;(defn ->points [color coords]
;  (->group (for [[x y] coords]
;             (->translation (* xscale x) (* yscale (- y 2))
;                            (->relative-rectangle :black 3)))))

(defn ->dotplot
  "Project a function f, onto a regular grid. For each sample in f, f should
   produce a value.  The input (sample) vs output (value) is projected onto
   a 2D plane, with input serving as the horizontal or x axis, output serving
   as the vertical or y axis."
  [f samples width height & {:keys [plotter] :or {plotter nil}}]
  (let [{:keys [xmin xmax ymax ymin coords]} (plot-function f samples)
        xscale (/ width (- xmax xmin))
        yscale (/ (halve height) (- ymax ymin))
        plot   (->scale 1.0 -1.0
                  (->group
                    (for [[x y] coords]
                      (->translation (* xscale x) (* yscale (- y 2))
                             (->relative-rectangle :black 3)))))]
   ;(println (format "Xscale %s YScale %s" (str xscale) (str yscale)))
    (->group [(make-sprite (->gridlines width height :xscale (/ width 10)) :bitmask 0 0)
              (->translation (/ width 2) (/ height 2) plot)])))

(defn simple-plot [& {:keys [f samples] :or {f square
                                             samples (mirror-range 20)}}]
  (paint-scene (->dotplot square samples 500 500)))
