;A separate namespace for model-view-controller protocols and paradigms.
(ns spork.cljgui.mvc)

;IModelViewControl defines an abstract interface for us to interrogate some data
;and learn about useful bits relative to the Model-View-Controller paradigm.
;Specifically, we can derive events associated with the component (events 
;are actually IObservable instances, even for wrapped GUI components.), the 
;model associated with the component, a view of the component (usually a 
;Swing Panel, but not always!), and any extra state associated with the 
;component.  State is usually a dumping ground for - useful - metadata.  
(defprotocol IModelViewControl
  (mvc-events [g])
  (mvc-model [g])
  (mvc-view [g])
  (mvc-state [g]))

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

(defrecord modelview [id model view control state]
  IModelViewControl
  (mvc-events [g] control)
  (mvc-model [g] model)
  (mvc-view [g] view)
  (mvc-state [g] state))
  
(defn make-modelview
  "Multi-arity constructor for new modelview records.  Some MVCs will not 
   have any extra state associated with them, so we provide a default 
   empty map.  Creates a unique name for the mvc if none is provided."
  ([model view control] (->modelview (gensym "mvc") model view control {}))
  ([model view control state] 
    (->modelview (gensym "mvc") model view control state))
  ([id model view control state] 
    (->modelview id model view control state)))

(defn add-state 
  "Add meta data to the state map in the MVC."
  [mvc k v]
  (assoc-in mvc [:state k] v))

;(defn mvc-value
;  "Return the current value of the model.  For mutable models, represented by
;   mutable types, the model is dereferenced to acquire its current state."
;  [mvc]
;  (when (mvc-model mvc)
;    (if (satisfies? IDeref mvc)
;      @mvc 
;      mvc)))

;how can we combine models? 
;Can we just nest them?  
;Note -> the views are not affected....the views - particularly for swing guis, 
;are already rendered....
;maybe the view should be a function of the model?
;That would facilitate composing one or more MVC's together, then building the
;views...


;we should probably have some generic stuff for ripping apart model views.

;Could use some nice combinators...

;(defn merge-mvc 
;  "Combine two mvc's into a new mvc, which subscribes to the events of all the 
;   children."
;  [mvc1 mvc2])