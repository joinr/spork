(ns spork.cljgui.examples.drawing  
  (:use [spork.cljgui.components.swing] 
        [spork.cljgui.asyncdrawing]
        [spork.cljgui.events base observe native]))

(defn random-lines-demo
  "Shows how to display a 'somewhat' complicated gui.  We use a paintbot to 
   manage drawing state asynchronously, which allows us to ignore the problems 
   of running on swing's Event Dispatch Thread.  The paintbot allows us to 
   add shapes, clear shapes, etc.  in a thread-safe, mutable context.  Note the
   ease of composition for building interactive guis.  In my opinion, this would
   be a LOT rougher in Java."
  [& {:keys [pb stretched?] 
      :or {pb (make-paintbot) 
           stretched? true}}]
  (let [panelfunc (if stretched? get-stretched-panel get-panel)
        resetbtn (button "Click to clear lines!")
        lbl (label "These two are mirrored")
        addline (fn [e] 
                  (add-shape pb (->line :black (rand 500) (rand 500) 
                                        (rand 500) (rand 500) )))  
        counter (label "0")
        zero-counter (fn [c e] (change-label c (fn [_] "0") e))
        shapecounter (shelf (label "Shape Count: ")
			                         counter)                      
        
        clearclick (->> (get-observer resetbtn :action)
                     (:actionPerformed)
                     (subscribe 
                       (fn [e]
                         (zero-counter counter e)
                         (clear-shapes pb))))
        
        drag-lines (fn [panel]  
                     (do (->> (get-observer panel :mouse)
                           (:dragged)
                           (map-obs #(doto % (addline)))
                           (subscribe 
                             (partial change-label counter 
                                      (fn [_] (str (count (:shapes @pb))))))) 
                       panel))] 
    (make-modelview 
      pb 
      (stack shapecounter
					        (drag-lines (panelfunc pb))                
					        resetbtn
					        (drag-lines (panelfunc pb)))
					     {:clearclick clearclick 
					      :drag-lines drag-lines})))

;;Example from Joy of Clojure
(defn example [] 
  (display (empty-frame) 
     (let [g1 (txt 10 "Charlemagne")
           g2 (txt 10 "Pippin")
           r (txt 3 "10")
           d (txt 3 "5")]
       (splitter 
         (stack 
           (shelf (label "Player 1") g1)
           (shelf (label "Player 2") g2)
           (shelf (label "Rounds " ) r
                        (label "Delay ") d))
         (stack 
           (grid-container 21 11 #(label "-"))
           (button "Go!" (fn [e] (alert (str (.getText g1) " vs. "
                                             (.getText g2) " for "
                                             (.getText r) " rounds, every "
                                             (.getText d) " seconds.")))))))))

