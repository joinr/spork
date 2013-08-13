(ns spork.cljgui.protocols)

;GUIComponent defines an abstract interface for us to interrogate some data
;and learn about useful bits relative to the Model-View-Controller paradigm.
;Specifically, we can derive events associated with the component (events 
;are actually IObservable instances, even for wrapped GUI components.), the 
;model associated with the component, a view of the component (usually a 
;Swing Panel, but not always!), and any extra state associated with the 
;component.  State is usually a dumping ground for - useful - metadata.  
(defprotocol IGUIComponent
  (get-events [g])
  (get-model [g])
  (get-view [g])
  (get-state [g]))

;A common containter for handling MVC relations.  I found that returning only 
;a swing panel was rather limiting, especially since I'm capturing individual 
;events, as well as building new events for certain widgets. 
;Model is generally some state (usually in an agent) associated with 
;a view.  For basic swing panels, the model IS the panel.  Typical swing 
;development encapsulates important state inside of a class that extends 
;a basic Swing interface, like JFrame, JPanel, etc.  You then interact with
;the model through public methods on the class.  To facilitate clojurian 
;development, I keep access to the underlying model, including its 
;data.  We typically use asynchronous workers (agents) to deal with maintaining
;the model and its data, which also allows us to ignore the single-threaded 
;issues relative to Swing.  A view is usually a swing-panel associated with the 
;underlying model. 

(defrecord modelview [model view control state]
  IGUIComponent
  (get-events [g] control)
  (get-model [g] model)
  (get-view [g] view)
  (get-state [g] state))
  
(defn make-modelview
  "Multi-arity constructor for new modelview records.  Some MVCs will not 
   have any extra state associated with them, so we provide a default 
   empty map."
  ([model view control] (->modelview model view control {}))
  ([model view control state] 
    (->modelview model view control state)))

(defn add-state 
  "Add meta data to the state map in the MVC."
  [mvc k v]
  (assoc-in mvc [:state k] v))