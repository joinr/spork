;;A simple handle for bindings to the underlying spork graphics
;;package.  This is a convenience namespace that pulls in all the
;;low-level dependencies, and provides a user-friendly API for newbs
;;to perform declarative rendering.  I really need to pull in the 
;;full scene graph library for this.  For now, it's being used as 
;;a sort of skeleton scene graph, for simple 2d diagrams and plotting.
(ns spork.sketch
  (:require [spork.graphics2d.canvas :refer :all]
            [spork.graphics2d [image :as image]
                              [swing :as provider]]
            [spork.protocols [spatial :as space]]
            [spork.geometry.shapes :refer :all]
            [spork.cljgui.components [swing :as gui]]))

;;These are brittle, but work until I found a better way around the problem.
(def ^:dynamic *font-height* 14)
(def ^:dynamic *font-width*  5.5)

(def ^:dynamic *current-sketch* nil)
      
(defn sketch-image [the-shapes]
  (->> the-shapes 
       (image/shape->img)
       (gui/view)))

;;image combinators 
(defn beside [s1 s2]
  (let [bounds1 (shape-bounds s1)
        bounds2 (shape-bounds s2)
        hmax (max (:height bounds1) (:height bounds2))
        width (+ (:width bounds1) (:width bounds2))
        new-bounds (space/bbox 0 0 width hmax)]
  (reify IShape 
    (shape-bounds [s] new-bounds)
    (draw-shape   [s c] (with-translation (:width bounds1) 0 
                          (draw-shape s1 c) (partial draw-shape s2))))))

(defn background [color shp]
  (let [{:keys [x y width height]} (shape-bounds shp)]
    [(->rectangle color 0 0 (+ x width) (+ y height))
     shp]))

(defn translate [tx ty shp]
  (reify IShape 
    (shape-bounds [s] (space/translate-bounds tx ty (shape-bounds shp)))
    (draw-shape   [s c] (with-translation tx ty 
                          c (partial draw-shape shp)))))

(defn above [s1 s2]
  (let [bounds1 (shape-bounds  s1)
        bounds2 (shape-bounds  s2)
        wmax   (max (:width bounds1)  (:width bounds2))
        height (+   (:height bounds1) (:height bounds2))
        new-bounds (space/bbox 0 0 wmax height)]
  (reify IShape 
    (shape-bounds [s] new-bounds)
    (draw-shape   [s c] (with-translation  0 (:height bounds1) 
                          (draw-shape s1 c) (partial draw-shape s2))))))
(defn fade [alpha shp]
  (reify IShape 
    (shape-bounds [s] (shape-bounds shp))
    (draw-shape   [s c] (with-alpha  alpha 
                          c (partial draw-shape shp)))))
(defn rotate [theta shp]
  (reify IShape 
    (shape-bounds [s]   (space/rotate-bounds theta (shape-bounds shp)))
    (draw-shape   [s c] (with-rotation theta  c (partial draw-shape shp)))))


;; (defn spin [theta shp]
;;   (let [bounds (shape-bounds shp)
;;         centerx (:x 
;;   (reify IShape 
;;     (shape-bounds [s]   (space/rotate-bounds theta (shape-bounds shp)))
;;     (draw-shape   [s c] (with-rotation theta  c (partial draw-shape shp)))))


(defn scale [xscale yscale shp]
  (reify IShape 
    (shape-bounds [s]   (space/scale-bounds xscale yscale (shape-bounds shp)))    
    (draw-shape   [s c] (with-scale xscale yscale c (partial draw-shape shp)))))

(defn cartesian [shp]
  (let [bounds    (shape-bounds shp)
        reflected (scale 1.0 -1.0 shp)]
    (reify IShape 
      (shape-bounds [s] bounds)
      (draw-shape [s c] 
        (with-translation 0 (:height bounds) c 
          (partial draw-shape reflected))))))

(defn outline [s & {:keys [color] :or {color :black}}]
  (let [bounds (shape-bounds s)]
    [s
     (->wire-rectangle color (:x bounds) (:y bounds) (:width bounds) (:height bounds))]))

(defn stack [shapes] (reduce above  shapes))
(defn shelf [shapes] (reduce beside shapes))

;;work in progress.
;(defn at-center [shp]
;  (let [bounds (shape-bounds shp)
;        centerx (/ (:width bounds) 2.0)
;        centery (/ (:heigh bounds) 2.0)]    
;  (reify IShape 
;    (shape-bounds [s] bounds)
;    (draw-shape   [s c] (with-translation centerx centery c
;                          (partial draw-shape shp))))))

(defn ->label [txt x y & {:keys [color] :or {color :black}}]
  (reify IShape
    (shape-bounds [s]   (space/bbox x y (* (count txt) *font-width*) *font-height*))    
    (draw-shape   [s c] (draw-string c color :default txt x (+ y (- *font-height* 2))))))

;;Drawing events and tracks.

(def simple-activity {:start 0 :duration 100 :name "The Activity!" :quantity 10})
(def simple-track
  [{:start 15 :duration 25   :name "A" :quantity 10}
   {:start 200 :duration 250 :name "B" :quantity 10}
   {:start 150 :duration 10  :name "C" :quantity 10}
   {:start 22  :duration 5   :name "D" :quantity 10}])
(def random-track 
  (vec  (map (fn [idx] {:start (inc (rand-int 600)) :duration (inc (rand-int 100)) :name (str "event_" idx) :quantity (inc (rand-int 30))})
             (range 100))))

(defn ->labeled-box  [txt label-color color x y w h]
  (let [r          (->rectangle  color x y w h)
        half-dur   (/ w 2.0)
        centerx    (+ x 1)
        half-width (* (/ (count txt) 2.0) *font-width*)        
        scalex     1.0 ;(if (< half-width half-dur) 1.0  (/ half-dur half-width))
        centery    10
        label      (scale scalex 1.0 (->label txt centerx centery :color label-color))]
    (reify IShape 
      (shape-bounds [s]   (shape-bounds r))
      (draw-shape   [s c] (draw-shape [r label] c)))))
  
(defn ->activity 
  [{:keys [start duration name quantity]} & {:keys [color-map label-color] 
                                             :or {color-map {} label-color :white}}]
  (let [h  10 ;(* quantity 10)
        b  (->labeled-box name label-color (get color-map name :blue) start 0 duration h)]
    (outline b)))

(def  ->vline (image/shape->img (->line :black 0 0 0 10)))
(defn ->axis [min max step-width]
  (let [tick   (fn [x] (translate x 0 ->vline))]        
    (translate 0 *font-height*     
     (image/shape->img 
       [(->line :black min 0 max 0)
        (image/shape->img 
          (into [] (map tick (range min (inc max) step-width))))]))))

;;should render us a track of each event, with a track-name to the
;;left of the track.  Events in the track are rendered on top of each other.
(defn ->track [records & {:keys [track-name track-height track-width] 
                          :or   {track-name (str (gensym "Track ")) 
                                 track-height 10 
                                 track-width  800}}]
  (let [label  (->label (str track-name) 0 0)
        lwidth (:width (shape-bounds label))
        sorted (sort-by (juxt :start :duration) records)
        [elevated hmax] (reduce (fn [[xs height] x] 
                           (let [act (->activity x)]
                             [(conj xs (translate 0.0 height act)) 
                              (+ height (:height (shape-bounds act)))]))
                                [[] 0] sorted)
        hscale 0.5
        pad    (when (> (:start (first sorted)) 0)
                  (->rectangle :white 0 0 (:start (first sorted)) track-height))
        ] ;(/ track-height hmax)]        
     (beside [(->rectangle :lightgray 0 0 lwidth (/ hmax 2.0))
              label] 
             (outline
              (scale 1.0 hscale (cartesian 
                                (into (conj [] pad) elevated)))))))

(defn ->tracks [track-seq]
;  (let [hline (->line :black 0 0 0 200)]
    (->> (for [[name records] track-seq]
           (->track records :track-name name))
 ;        (interleave (repeat hline))
         (vec)
         (stack)))


;;testing
(comment 

(sketch-image (stack [(->track random-track) (->track simple-track)]))

(require '[spork.util [sampling :as s]])
(require '[spork.util [stats :as stats]])

(def sampler (s/sample-data))

(def random-tracks 
  {:SAMPLE (s/sample-from sampler (s/->constrain {:tfinal 1000} :sample))
   :CASE1  (s/sample-from sampler (s/->constrain {:tfinal 1000} :case1))})

(let [sampler {:foo {:name "foo" :start 0 :duration 1}
               :bar {:name "bar" :start 0 :duration 10}
               :baz {:name "baz" :start 0 :duration 5}
               :random-records  (s/->transform 
                                 {:start    (stats/uniform-dist 0 2000)
                                  :duration (stats/normal-dist 100 50)}
                                 (s/->choice [:foo :bar :baz]))
               :clamped-records  (s/->constrain {:tfinal 5000 
                                                 :duration-max 5000}
                                                 [:random-records])}]  
  (defn random-track! [& {:keys [n] :or {n 10}}]
    (->> :clamped-records
         (s/->replications n)
         (s/->flatten)
         (s/sample-from sampler))))

(defn random-tracks! [& {:keys [n] :or {n 4}}]
  (sketch-image
   (->tracks (zipmap (map #(str "Track" %) (range n)) (repeatedly random-track!)))))

)
