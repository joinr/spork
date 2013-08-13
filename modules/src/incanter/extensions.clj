;A set of useful extensions for incanter that Marathon uses.
(ns spork.incanter.extensions
  (:use [incanter core charts])
  (:import [org.jfree.data.xy XYDataset XYSeries XYSeriesCollection]))

(defn ^XYSeries make-xyseries
  "Primitive function to bind two lists of x and y coordinates into a
   jfreechart series."
  [label xs ys]
  (let [s (XYSeries. (str label))]
    (doseq [[x y] (map vector xs ys)]
      (. add s x y))
    s))

(defn ^XYSeriesCollection map->seriescoll
  "Primitive function to trasnfer a map, where the
   assoc'd values are coordinate lists - [[xs] [ys]]. Returns a
   series collection object for charting."
  [m]
  (let [^XYSeriesCollection sc (XYSeriesCollection.)]
    (doseq [[lbl [xs ys]] m]
      (.addSeries sc (make-xyseries lbl xs ys)))
    sc))

(defn empty-xy "An Empty scatter plot. Returns a JFreechart chart."
  ([data] (set-theme-default
            (org.jfree.chart.ChartFactory/createScatterPlot
              "No Title" "X" "Y" data
              org.jfree.chart.plot.PlotOrientation/VERTICAL true false false)))
          ([] (empty-xy (XYSeriesCollection.))))

(defn add-point-series
  "Convenience function for adding a series of x y points to a chart."
  [chrt xs ys label]
  (doto chrt (add-points xs ys :series-label label)))


(defn add-linear-series
  "Convenience function for adding a series of x y points to a chart Each
   point is connected by a line."  
  [chrt xs ys label]
  (add-lines chrt xs ys :series-Label label))

(defn add-line-horizontal
  "Adds a horizontal line from [xO -> xl], at height y,
   with series name label."
  [chart xO xl y label]
  (add-lines chart [xO xl] [y y] : series-label label))

(defn add-line-vertical
  "Adds a vertical line from [yO -> y1], at width x,
   with series name label."
  [chart yO y1 x label]
  (add-lines chart [x x] [yO y1] :series-Label label))

(defn table->dataset
  "Converts a simple table into an incanter dataset. Actually, the table is
   more or less an incanter dataset format."
  [t]
  (incanter.core/dataset (:fields t) (:records t)))

