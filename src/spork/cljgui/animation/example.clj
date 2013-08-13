(ns spork.cljgui.animation.example
  (:use [cljgui.graphics2d.canvas]
        [cljgui.geometry.shapes]
        [cljgui.behavior]
        [cljgui.scene.scenegraph]
        [cljgui.components.swing])
  (:import [java.awt Graphics2D]
           [java.awt.event ActionListener]
           [javax.swing Timer JPanel]))

;what we really should do is extend the drawing to IShapes 
;this will let us maintain the state of the shape records, and call their 
;draw-shape functions....

(def greencircle (->group 
                   [(->relative-circle :green 100)
                    (->relative-ring :black 100)]))
(def bluecircle (->group 
                  [(->relative-circle :blue 100)
                   (->relative-ring :black 100)]))

(defn compose-scene [s1 s2]  (->group [s1 s2]))

(def greenandblue
      (compose-scene (translate-scene -35 35 greencircle)
                     (translate-scene 35 -35 bluecircle)))

(defn compose-b [scene1b scene2b]
  (lift2 compose-scene scene1b scene2b))

(defn translate-b [xb yb sceneb] 
  (lift3 translate-scene xb yb sceneb))

(defn scale-b [xb yb sceneb]
  (lift3 scale-scene xb yb sceneb))

(defn rotate-b [thetab sceneb]
  (lift2 rotate-scene thetab sceneb))

(defn fade-b [alphab sceneb]
  (lift2 fade-scene alphab sceneb))

(defn offset-scale-b [scale xb yb sceneb]
  (let [[scaledx scaledy] (map #(map-b * scale-b %) [xb yb])]
  (scale-b 
    (translate-b scaledx scaledy sceneb))))

(defn orbit [radius speed drawb]
  (let [pos (lift2 * wiggle (forever radius))]
    (->> drawb 
      (translate-b pos (wait 0.5 pos))
      (faster speed))))

;use FRP to compose a bunch of functional drawings...
;we take an initial drawing behavior (animation) and 
;a number of trails back we want to go.  The idea is that 
;we compute a new behavior, which is the composition of the 
;drawing behavior, with multiple "past" versions of itself.
;This allows us to draw a trail.  We use n to denote the number of 
;items that are trailing the original.  Note, if N is large, or if we 
;have an infinite handler, that would me we're just statically grawing 
;the shape as a function of time.  This could lead to a space leak, so 
;we might want to handle that separately....
(defn trail
  "Trail returns a behavior that creates a time-shifted duplicate which appears
   to follow the input behavior."
  [drawb]
  (compose-b 
    drawb 
    (wait 0.25 drawb)))

(defn fading-trail [fade-rate tdelay drawb] 
  (compose-b 
    drawb 
    (fade-b (forever fade-rate) (wait tdelay drawb))))

;allow for multiple trails....n back, on the caveat that the intervals 
;exist over the domain. I.e., as we go back in "time", if t is invalid (i.e. 
;only positive values are allowed for t), then we stop trailing.
;note....interval will ultimately be multiplied by pi, so any real "difference"
;for wait should be relative to pi.  In this case, 2PI is a full cycle.
;Something that tricked me was that intuitive intervals (i.e. 1, 2 ,3) 
;seemd fine, and should produce cool trails of drawings that are animated 
;1, 2, and 3 time-steps behind the other...leaving a nice trail.
;However, the primary mover function driving most of the animation right now, 
;wiggle, projects time into a sin wave, multiplying the input value by pi.
;Thus, an interval of 20 produces the same effect as 2PI, or any other even
;multiple, while 1 produces the same effect as PI, as does 19, or any odd 
;multiple of PI.  Therefore, we need to think relative to pi (for now) to get
;meaningful transforms out of this.  Really, it's time/cycle.  That's what 
;interval should be....

;Another way around this is to just divide the cycle into n intervals, and fill
;em.  Yea, let's do that....
;This is equivalent to dividing the cycle, and taking the first n values.
(defn n-trail
  "Returns a behavior that is the composition of n duplicates of the 
   drawingbehavior, where the nth duplicate is delayed - time shifted - by 
   (+ (/ 1 (- n 1)) (/ 1 n)"
  [n drawb]
  (let [interval (/ 1 n)
        trails (map (fn [w]  (fade-b (forever (- 1 w)) (wait (negate w) drawb)))                                          
                    (take (dec n) (iterate #(+ interval %) interval)))
        composition (reduce compose-b (concat (reverse trails) [drawb]))] 
	  (sample 
	    (fn [bc] (composition bc)))))

(defn n-trail-solid
  "Returns a behavior that is the composition of n duplicates of the 
   drawingbehavior, where the nth duplicate is delayed - time shifted - by 
   (+ (/ 1 (- n 1)) (/ 1 n)"
  [n drawb]
  (let [interval (/ 1 n)
        trails (map (fn [w]  (wait (negate w) drawb))                                          
                    (take (dec n) (iterate #(+ interval %) interval)))
        composition (reduce compose-b (concat (reverse trails) [drawb]))] 
	  (sample 
	    (fn [bc] (composition bc)))))


(defn rand-trail
  "Create n duplicates of the drawingbehavior, where the nth duplicate 
   is delayed - time shifted - by a random value between 0 and 1."
  [n drawb]
  (let [trails (map (fn [w] (fade-b (forever (- 1 w)) 
                                    (wait (negate w) drawb))) 
                      (take (dec n) (repeatedly (fn [] (rand)))))
        composition (reduce compose-b (concat (reverse trails) [drawb]))] 
	  (sample 
	    (fn [bc] (composition bc)))))                 

(defn n-decaying [n drawb] 
    (let [interval (/ 1 n)
        trails (map (fn [w]  
                               
                               (scale-b 
                                 (forever (- 1 w))
                                 (forever (- 1 w))
                                 (fade-b 
                                   (forever (- 1 w)) (wait (negate w) drawb))))                                          
                    (take (dec n) (iterate #(+ interval %) interval)))
        composition (reduce compose-b (concat (reverse trails) [drawb]))] 
	  (sample 
	    (fn [bc] (composition bc)))))

;Animation utilities and demos....
(defn now [] (System/currentTimeMillis))

(defn af [anim]
  (let [[w h] [600 600]
        midw  (/ w 2) 
        midh (/ h 2)
        background (make-sprite (->rectangle :white 0 0 w h) :opaque 0 0) 
        starttime (ref (now))
        ^JPanel panel 
                (paintpanel w h 
                  (fn [^Graphics2D g]
                    (let [elapsed (/ (- (now) @starttime) 1000.0)
                          scene (translate-scene midw midh 
                                   (->> @anim
                                     (read-value elapsed)))]
                      (do
                        (draw-shape background g)
                        ;(.translate g (int midw) (int midh))
                        (render scene g)))))
                        ;(.translate g (int (negate midw));have to move back.... 
                        ;              (int (negate midh))))))) 
        x (add-watch anim :newanimation 
           (fn [k r oldstate newstate] 
             (if-not (or (nil? panel) (not (.isVisible panel)))
               (.repaint panel)
               (remove-watch r k))))
        tmr (Timer. 16 ;about 60 fps
                  (proxy [ActionListener] []
                     (actionPerformed [e] 
                        (.repaint panel))))]
      (do (.start tmr)
          (toggle-top (display (empty-frame) 
                               (stack 
                                 (label "Functional Reactive Animation!")
                                   panel))))))         

(defn animation-app [initframe]
  (let [mutableanimation (ref initframe)]
    (do (run-app (fn []  (af mutableanimation)))
      (fn  [newanim]
        (dosync (ref-set mutableanimation newanim))))))        




;(def stillframe (forever greenandblue))
(def stillframe (forever greenandblue))

(def activeanimator (ref nil))
 
(defn animator? []
  (not= nil @activeanimator))

(defn get-animator []
  (if (animator?) 
    @activeanimator
	  (dosync (ref-set activeanimator (animation-app stillframe))
	    @activeanimator)))

(defn animator [scenebehavior]
  ((get-animator) scenebehavior))
  
(defn reset [] 
  (animator stillframe))

(defn vertical [] 
  (animator (translate-b (forever 0) wiggle100 stillframe)))

(defn horizontal []
  (animator (translate-b wiggle100 (forever 0) stillframe)))

(defn diagonal [] 
  (animator (translate-b wiggle100 wiggle100 stillframe)))


(defn horizontal-trail []
  (animator (trail (translate-b wiggle100 (forever 0) stillframe))))

(defn horizontal-two []
  (animator (n-trail 2 (translate-b wiggle100 (forever 0) stillframe))))

(defn rotating []
  (animator (orbit 100 1 stillframe )))

(defn rotate-trail []
  (animator (compose-b 
              (n-trail 10 (orbit 100 1 stillframe))
              stillframe)))

(defn horizontal-n
  ([n] (animator (->> stillframe
                   (translate-b wiggle100 (forever 0))
                   (n-trail n))))
  ([] (horizontal-n 10)))
  
(defn random-trail 
  ([n] (animator (->> stillframe
                   (translate-b wiggle100 (forever 0))
                   (rand-trail n))))
  ([] (random-trail 10)))

(defn skating []
    (->> stillframe 
      (orbit 100 1)
      (translate-b wiggle100 (forever 0))
      (n-trail 20)
      (animator)))

(defn figure8 []    
    (->> stillframe 
      (orbit 30 0.5)
      (translate-b wiggle100 (forever 0))
      (n-trail 20)
      (animator)))


(defn rotation []
  (animator 
     (->> stillframe
       (rotate-b (wiggle-between (* 2 pi))))))

(defn tumble []
  (->> stillframe
    (rotate-b (wiggle-between (* 2 pi)))
    (translate-b wiggle100 (forever 0.0))
    (n-trail 20)
    (animator)))

(defn waltz [n]
  (->> stillframe
    (rotate-b (wiggle-between pi))
    (orbit 100 0.8)
    (n-trail n)
    (faster 0.5)
    (animator)))

(defn waltz-solid [n]
  (->> stillframe
    (rotate-b (wiggle-between pi))
    (orbit 100 0.8)
    (n-trail-solid n)
    (faster 0.5)
    (animator)))

(defn wigglewaggle []
  (->> stillframe 
    (translate-b (waggle-between 100) (wiggle-between 100))
    (animator)))

(defn separate []
  (let [green (->> (forever greencircle) 
                (translate-b (wiggle-between 300) (waggle-between 300))
                (n-trail 15))
        blue (->> (forever bluecircle)
               (translate-b (waggle-between 200) (wiggle-between 200))
               (n-trail 15))]
    (->> (compose-b green blue)
      (animator))))


(defn scaled []
  (->> stillframe 
    (scale-b wiggle  wiggle)
    (translate-b wiggle100 waggle100)
    (animator)))
     
(defn decaying []
  (let 
    [green (->> (forever  greencircle) 
                (translate-b (wiggle-between 300) (waggle-between 300))
                (n-decaying 15))
    blue (->> (forever bluecircle)
               (translate-b (waggle-between 200) (wiggle-between 200))
               (n-decaying 15))]
    (->> (compose-b green blue)
       (animator))))

(defn round-particles [n size]
  (let [particle (make-sprite (->relative-circle :red size) :bitmask 0 0)]
     (->> 
       (forever particle)
       (rotate-b (wiggle-between pi))
       (orbit 100 0.8)
       (n-trail-solid n)
       (faster 0.5)
       (animator))))

(defn square-particles [n size]
     (->> 
       (forever  (->relative-rectangle :red size))
       ;(rotate-b (wiggle-between pi))
       (orbit 100 0.8)
       (n-trail-solid n)
       (faster 0.5)
       (animator)))
