;(ns spork.cljgui.replbox
;  (:import [javax.swing JTextPane JTextArea])
;  (:require [cljgui [gui :as gui
;                    asnycdrawing :as async]]))
;
;(defn textpane [] 
;  (JTextPane.))
;
;(defn make-replbox
;  "Builds a simple text box modelview, in which the model is 
;   an agent that evals input from the textbox, and returns the evaluated 
;   result to an outputbox.  Click the button to eval!  Has the benefit of 
;   allowing access to the cljgui namespace.... 
;   Simple, and dumb, for now!"
;  []
;  (let [inputbox 
;  
