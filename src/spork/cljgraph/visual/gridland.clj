(ns spork.cljgraph.visual.gridland
  (:import [java.awt Graphics Graphics2D Polygon Point Rectangle Shape
                     Point2D Rectangle2D AffineTransform]
           [javax.swing JFrame JPanel]
           [java.awt.event ActionListener ActionEvent 
                           MouseListener KeyListener]))

(defn empty-frame [] (JFrame.))
(defn make-visible [frm]     
  (doto frm 
    (.pack) 
    (.setVisible true)))

(defn add-view [frm view] 
 (do (.add (.getContentPane frm) view) 
         frm))

;This serves as a testbed for visualizing various graph algorithms, specifically
;shortest paths, etc. 

;What I'd like to do is steal some inspiration from Matt Buckland, and use his 
;excellent examples in his Game AI book.

;Matt's world exists largely in a grid. 
;He breaks up a view into nxn tiles.
;He then defines sets of polygons that fill said world, and act as boundaries
;for any path-finding algorithm. 

;Coordinates in the grid-world are actually centered on a tile.
;This only really matters for drawing purposes.

(defn make-grid [n]
  (into {} (for [x (range n) 
        y (range n)] [[x y] nil])))

(defn get-grid [g x y]
  (get g [x y]))

(defn draw-line [g [x1 y1 x2 y2]]
  (.drawLine g x1 y1 x2 y2))
