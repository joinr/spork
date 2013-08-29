(ns spork.cljgui.examples.reactive
  (:use [spork.cljgui.components.swing] 
        [spork.cljgui.asyncdrawing]
        [spork.cljgui.events base observe native]))

(defn observable-demo [] 
	;testing.  this is a simple simulation.....
  (let 
    [balance (fn [limit & source]
               (let [signal (if (first source) 
                                (first source) 
                                (make-observable))]
                 (->> signal
                   (split-obs #(<= % limit)) 
                   (splitmap-obs 
                     (fn [l] [:low l])
                     (fn [r] [:high r]))
                   (merge-obs)
                   (map-obs (fn [[vtype volume]]
                              (let [msg (case vtype
                                        :low "Volume is loooooow :"
                                        :high "Volume is HIGH!!! :")]
                                (str msg volume)))))))	                                                                                	
	  ;create a volume thingy....if volume goes over 50 it thinks its loud.
     b (->> (balance 50)
	         (subscribe println))]
	  ;feed a stream of "events", i.e. notifications to the observable
		(doseq [v (range 50)] (notify! b (rand-int 100)))))