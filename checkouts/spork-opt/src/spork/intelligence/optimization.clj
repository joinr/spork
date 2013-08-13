;work in progress of a port from toby segaran's collective intelligence.
;this is really just a stochastic optimization package at the moment.  I ganked 
;his intersecting lines example from the book.  I have some problems with
;line-generation, in that currently a line of [0,0] [0,0] is valid.  I need to 
;fix this.  The system eventually optimizes by pushing "most" everything 
;into a corner :/ 
(ns spork.intelligence.optimization
  (:use     [clojure.contrib.combinatorics])
  (:require [spork.intelligence [annealing :as ann]]))

(defn between [l u]
  (fn [n] (and (>= n l) (<= n u))))

(def normal (between 0 1))

(defn crosses? 
  ([x1 y1 x2 y2 x3 y3 x4 y4] 
	  (let [dx1 (- x2 x1) ;3
	        dy1 (- y2 y1) ;2           
	        dx2 (- x4 x3) ;4
	        dy2 (- y4 y3) ;3           
	        den (- (* dy2 dx1) ;3 * 3 = 9
	               (* dx2 dy1))]; 4 * 2 = 8  == (9 - 8) = 1
	    (if (not= 0 den)
	      (let [dy3 (- y1 y3) ;-2
	            dx3 (- x1 x3) ;-3
	            ua (/ (- (* dx2 dy3) (* dy2 dx3)) den) ; (-8 - (-9)) 1 / 1  
	            ub (/ (- (* dx1 dy3) (* dy1 dx3)) den)] ; (-6 - (-6)
	        (and (normal ua) (normal ub))))
     ))
  ([[x1 y1 x2 y2] [x3 y3 x4 y4]]
    (crosses? x1 y1 x2 y2 x3 y3 x4 y4)))

(defn manhattan
  "Calculate the Manhattan distance between two cartesian coordinates."
  ([x1 y1 x2 y2] (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))
  ([[x1 y1] [x2 y2]] (manhattan  x1 y1 x2 y2))
  ([[x1 y1 x2 y2]] (manhattan  x1 y1 x2 y2)))

(defn objective [lns]
  (let [lnpairs (partition        
  
;(def line1 [0 1 3 3])
;(def line2 [0 1 4 4])
;
;(def line2 [2 2 -50 100])

(defn random-line [xlim ylim & integers]
  (let [drawf (if integers rand-int rand)] 
    [(drawf xlim) (drawf ylim) (drawf xlim) (drawf ylim)]))

(defn- random-linestream* [xlim ylim & integers]
    (lazy-seq (cons (random-line xlim ylim integers)
                    (random-linestream* xlim ylim integers))))

(defn random-linestream [xlim ylim & integers] 
  (distinct (random-linestream* xlim ylim integers)))

(defn generate-lines [xlim ylim n & integers]
  (into [] (take n (random-linestream xlim ylim integers))))

;This is a naive line-counting algorithm...it may be more 
;efficient to use a line-sweep algorithm from computational 
;geometry.  This is simple enough for small N.

(defn count-crossings [ls]
    (reduce + (map #(if (apply crosses? %) 1 0) (combinations ls 2))))

(defn line-shifter [xlim ylim step]
  (let  [bracket (fn [lower upper v] 
                   (cond (< v lower) lower
                         (> v upper) upper
                         :else v))
         rand-step (fn [] (- (rand-int (inc step)) step))
         br-step #(bracket 0 %1 (+ %2 (rand-step)))
         x-step (partial br-step xlim) 
         y-step (partial br-step ylim)         
        line-shift (fn [[x1 y1 x2 y2]] 
                     [(x-step x1) (y-step y1) (x-step x2) (y-step y2)])]
    (fn [ls]
      (let [idx (rand-int (count ls))]
        (assoc ls idx (line-shift (get ls idx)))))))                                           

(def svghead "\"<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">")
(defn line->svgline [[x1 y1 x2 y2]]
  (format 
   "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" style=\"stroke:rgb(255,0,0)
;stroke-width:2\"/>" x1 y1 x2 y2))
  
(defn lines->svg [ls]
  (str (apply str svghead \newline (map #(str (line->svgline %) \newline) ls)) 
       \newline "</svg>"))


;next step is to optimize.....spread the points out so that we get fewer
;intersections...
;let's use the handy-dandy functions from the annealing library!
(defn line-annealable [ls xlim ylim step]
  (ann/make-annealable 
    ls ;initial value
    #(* 100 (count-crossings %));costf
    (line-shifter xlim ylim step))) ;neighborhood fn

;a simple test...which technically works, but it fails in spirit.
(defn line-test [xlim ylim n step]
  (let [lns (generate-lines xlim ylim n :ints)]
    (-> (line-annealable lns xlim ylim step)
        (ann/solution-seq)))) 

(def improving (line-test 600 800 10 100))
    
    
    
    
    







