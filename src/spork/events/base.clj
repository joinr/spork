;;Tom Spoon Feb 13 2012.
;;Base protocols and structures for
;;generic event information.  
(ns spork.events.base)

(defprotocol IEvent 
  (event-source [e] "Derive the event source of event e.")
  (event-time [e] "Report the timestamp for event e.")
  (event-data [e] "Return (usually a map) of data associated with event e.")
  (event-id [e] "Return a (usually numeric) ID associated with event e.")
  (event-type [e] "Return (usually string) event-type associated with event e.")
  (event-from [e] "Return the originating source of event e, usually numeric." )
  (event-to [e] "Return the intended destination for event e, if any."))
  
;(defprotocol IEventSource
;  (event-categories [eventsource] "List the event categories supported")
;  (get-events [eventsource & categories] 
;   "Return a map of {:category {:eventname observable}} pairs."))
  
             
(defrecord Event [id evtype t from to data]
  IEvent 
	  (event-source [e] from)
	  (event-time [e] t)
	  (event-data [e] data)
    (event-type [e] evtype)
	  (event-id [e] id)
	  (event-from [e] from)
	  (event-to [e] to))

;I haven't really used this yet, although for generic observables, I 
;probably will.  So far, the swing stuff has the protocol extended to it...
(def empty-event (Event. -1 :none -1 :anonymous :global nil))
(defn make-event 
  [id evtype & {:keys [t from to data] 
                 :or {t 0 from :anonymous to :global data nil} 
                 :as opts}]
    (Event. id evtype t from to data))
