(ns spork.cljgui.scenes
  (:use [cljgui gui scenegraph behavior] ))


;a simple scene for drawing mathematical plots...

(defn- evenize [n] 
  (if (even? n) n
    (inc n)))

;(gridlines 300 300 :xscale 100 :yscale 100 :centered true)
;(defn get-offset [w 300 scale 100]
;  (let [n (/ w scale)]
;    (if odd? n) 
;    (/ 
;  

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
	    (concat vs hs)))

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

(defn ->dotplot
  "Project a function f, onto a regular grid. For each sample in f, f should 
   produce a value.  The input (sample) vs output (value) is projected onto 
   a 2D plane, with input serving as the horizontal or x axis, output serving 
   as the vertical or y axis."
  [f samples width height]
  (let [{:keys [xmin xmax ymax ymin coords]} (plot-function f samples)
        xscale (/ width (- xmax xmin))
        yscale (/ (halve height) (- ymax ymin))
        plot   (->scale 1.0 -1.0
                  (for [[x y] coords]
                     (->translation (* xscale x) (* yscale y)
                          (->relative-rectangle :black 4))))]
   ;(println (format "Xscale %s YScale %s" (str xscale) (str yscale)))
    [(->gridlines width height )
     (->translation (/ width 2) (/ height 2) 
         plot)]))            
 
  

  