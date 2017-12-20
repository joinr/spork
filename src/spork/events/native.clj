;Tom Spoon Feb 13 2012
;This provides default implementations for common events in Java (or the 
;native system we're operating on).  Right now, I tried to pull out common 
;methods from Swing and AWT that would be useful.

;Our goal here is to provide functions that can map a Java component into 
;one or more observables.  Additionally, we want to de-encapsulate the actual 
;event data, which is hidden by default inside of Java's methods, so that we 
;can treat event instances as abstract sequences.  To facilitate this, we 
;also build in default functionality to treat "most" java events as generic 
;IEvents, from the cljgui.events.base namespace.  We retain all of the domain
;specific data from each Java Event class, and package it as a map accessible 
;via (event-data [e]) from IEvent.

;To recap:
;  Interfacing Java with Clojure Observables entails
;  A. Extending consistent operations from the IEvent protocol to Java events 
;     so that clojure libraries can apply generic operations to observable 
;     IEvent streams.
;  B. Attaching listeners (mouse, keybord, etc.) that handle event actions 
;     by notifying observables, which are exposed to Clojure.  Attachment 
;     functions hide all the underlying Java plumbing from the caller, and 
;     simply take a Java component as input, returning a map of keyed 
;     observables as a result.  The java component is mutated as a side effect.
;  C. Define complex event routing via functional composition of Observables.
;  D. Make GUI development much less painful, but just as robust.

;;NOTE - 2017:  This was intended to wrap the wierd interface-based
;;adapter-based event system in Swing with something far more sane, based
;;off of events-as-data (as defined in my port of the F# reactive observable
;;system in events.observe).  Things have changed since 2011/12, so I'd
;;recommend possibly using a different library for swing gui work.
;;This is only maintained for legacy compatibility until some migration
;;occurs.
(ns spork.events.native
  (:use [spork.events.base]
        [spork.events.observe])
  (:require [clojure [reflect :as r]
                    [string :as s]])
  (:import  
    [java.util EventListener EventObject]
    [java.awt.event 
     ActionEvent ActionListener 
     ComponentEvent ComponentListener
     AdjustmentEvent AdjustmentListener
     FocusEvent FocusListener 
     ContainerEvent ContainerListener
     HierarchyEvent HierarchyListener
     KeyEvent KeyListener KeyAdapter
     ItemEvent ItemListener
     MouseEvent MouseListener MouseMotionListener MouseWheelListener MouseAdapter 
     TextEvent TextListener
     WindowEvent WindowFocusListener WindowListener WindowAdapter 
     ]
    [javax.swing JPanel]
     [javax.swing.event 

      ChangeEvent ChangeListener
      DocumentEvent DocumentListener 

       ]))

;The typical workflow for attaching a listener to something that produces events
;is:
;Implement a Listener interface/s relative to the events of interest
;  Typical Java interfaces are broken up into groupings of events....
;  The consistency amongst groups is that every method in the Listener interface
;  has the same signature:: SomeEvent -> void 

;  Another way of looking at it, is that are n discrete events per interface, 
;  all of which initially start with the same data type.

;  .Net handles this by publishing specific events of interest, and allowing 
;  things to subscribe to them (mutable, but also practical).

;  What'd be useful, is to implement the .Net view of things using the 
;  events described in the existing Java Listener interfaces.

;  For each interface, extract an eventname from the delineated methodnames in 
;  the listener interface.

;  For classes that support the interface, establish a mapping between the 
;  class, and the events it supports.  This allows dynamic event hookups. 

;  I.e. a JPanel supports component events, because it can attach a 
;  ComponentListener.  

;  For a given native eventful object, use an intermediate protocol to abstract
;  out the mapping of available events.

;  We need to transform an event tag, into an Observable that corresponds to 
;  (recieves notification from) the underlying Java Listener method.

;  From there, the observable framework takes over.  We just pass around 
;  data (type-hinted for the Java stuff), and use our combinators to define 
;  complex, side-effecting behavior.


(def nosource :nosource)
(def globalevent :global)

(defn make-aevent 
  "Simple constructor for action events.  Mostly for testing."
  ([source id command t]
    (ActionEvent. source id command t))
  ([id command t] (ActionEvent. nosource id command t)))
                 

;                           Wrapping....
;This section concerns converting most Java Event classes into IEvents by 
;wrapping them, or extending the IEvent protocol to them.  

;we tie into the default methods guaranteed by the java Event base class.
(def default-event 
  {:event-source (fn [e] (.getSource e))
   :event-time (fn [e] 0) ;no default notion of time.
   :event-data (fn [e] (.getActionCommand e))
   :event-type (fn [e] (.getID e))
   :event-from (fn [e] (.getSource e))
   :event-to (fn [e] globalevent)})

(defn- get-properties
  "return a function that reflects on a class and returns all (getXXXX) methods
   and all (isXXX) methods..."
  [klass]
  (let [zero-args? (fn [r] (= 0 (count (:parameter-types r))))
        has-args? (fn [r] (contains? r :parameter-types))
        properties (filter
                      #(and 
                        (has-args? %)
                        (zero-args? %)
                        (let [nm (str (:name %))] 
                          (and (> (count nm) 3)
                             (or
                               (= (subs nm 0 2) "is")
                               (= (subs nm 0 3) "get"))))) 
                      (:members (r/reflect klass)))]
    properties)) 

(defn- cut-string [instr tobecut]
  (.replace instr tobecut ""))

(defn- get-propertymap
  "generate a list of key-val pairs, where keys are keyworded method names
   without 'get' or 'is', and vals are anonymous functions that apply the 
   get or is method to an instance of the class"
  [names]
  (let [methodcall  (fn [pname] 
                      (let [n (str "." pname)]
                       (list  (symbol n) 'k)))
        get-key (fn [pname]
                  (let [cut (if (= (subs pname 0 2) "is")
                              "is" "get")]
                    (keyword (cut-string pname cut))))
        property-map (fn [pname]
                       (let [k (get-key pname)]
                         (list k (methodcall pname))))]
    (cons 'hash-map (mapcat property-map names))))

;tie together the property map into a single anonymous function.
;eval it to turn the symbol list into an actual function.
(defn getmethods [klass]
  (let [ps (map (comp str :name) (get-properties klass))]
    (eval (list 'fn '[k] (get-propertymap ps))))) 

;use a macro to easily define event wrappers for a Java XXXEvent class, along 
;with replacement methods (or even additional methods) to add to the property
;map.
(defmacro wrap-events [awtclass methodmap]
  `(let [base# (merge default-event ~methodmap)] 
    (extend ~awtclass IEvent
      (merge base# {:event-data (getmethods ~awtclass)}))))

;Extend the ability to wrap MANY java events...
(defmacro wrap-many [& [classpairs]]
  (let [cp (for [[k p] classpairs] ;(fn [[[k p] _]] 
                 (list 'wrap-events k p))]
   `(do (list 
           ~@cp))))
(defn if-map [m init x]
  (reduce (fn [acc [k f]]
            (if (f x)
              (conj acc k)
              acc)) init (seq m)))
;Wrap an assload of event types, so we can use generic IEvent ops on
;them in observable combinators.  
(wrap-many 
	 [[ActionEvent {:event-time (fn [e] (.getWhen e))}]
	  [ComponentEvent {}]
	  [AdjustmentEvent {}] 
	  [ContainerEvent {}]
	  [FocusEvent {}]
	  [HierarchyEvent {}] 
    [KeyEvent {}]
	  [ItemEvent {}]
	  [MouseEvent {}]
	  [TextEvent {}]
	  [WindowEvent {}]])

	  ;[InputEvent {}]
	  ;[PaintEvent {}]

(defn key-modifiers
  "Fetches a set of modifiers associated with the current event."
  [e]
  (if-map {:shift #(.isShiftDown %)
                   :alt #(.isAltDown %)
                   :control #(.isControlDown %)}
                  #{} e))
   
(defn has-modifier? [evt m]
  (if-let [mods (key-modifiers evt)]
    (contains? mods m)))
   
(defn control-down?
  "Control key depressed during event."
  [evt] (has-modifier? evt :control))
(defn alt-down?
  "Alt key depressed during event."
  [evt] (has-modifier? evt :alt))
(defn shift-down?
  "Shift key depressed during event."
  [evt] (has-modifier? evt :shift))
(defn has-modifiers?
  "All modifiers in ms present in event."
  [evt ms]
  (if-let [mods (key-modifiers evt)]
    (every? mods ms)))
(defn has-char? [evt c]
  (= (-> (event-data evt) :KeyChar) c))

(defn make-key-filter
  "Builds a function that parses key events from cljgui.events.native, 
   to support event filtering.  Valid input may be a keyword drawn from 
   #{:newline, :control, :shift, :alt} where the presence of the key modifier
   will trigger a true, or a normal character value, or vector of values that
   are parsed individually, and composed to form a logical AND filter function.
   This facilitates building key-strokes and command key functionality using 
   the compositional framework for handling events."
  [x]
  (cond  (vector? x)   
           (let [filters (map make-key-filter x)]
             (fn [e] (loop [res true
                            xs  filters]
                       (if (empty? xs)
                         res
                         (if ((first xs) e)
                           (recur res (rest xs))
                           nil)))))
         (keyword? x) (case x 
                        :newline (make-key-filter \newline)
                        :control control-down?
                        :shift   shift-down?
                        :alt     alt-down?
                        (throw (Exception. (str "Unknown exit key " x))))
         (char? x) #(has-char? % x)
         :else (throw (Exception. (str "Invalid exit key " x)))))
   
;                        Attaching
;   
;Now we have a way of feeding an XXXEvent class and "wrapping" it into 
;an IEvent, x, where the underlying data is exposed as a simple map in the 
;IEvent, accessible via (event-data x)

;What about creating observables that interface with GUI components? 
;Components are responsible for registering subscribers themselves, 
;which is handled via the .addXXXListener method on the instance of a 
;component.  We'd like to abstract this plumbing away from client code.
;Ideally, we'd just have a client apply a function to a GUI component and 
;get some identifiable event streams back, which they could compose using 
;observable combinators.

;We now define a library for easily converting GUI components into maps of 
;observed IEvent streams. 

(defn- ignore [_] nil)

(defn get-mouse
  "Return an anonymous proxy for MouseAdapter that uses function handlers for 
   each event in args.  If no handlers are supplied, the MouseAdapter will not 
   respond to events."
  [{:keys [clicked dragged entered exited moved pressed
                          released wheelmoved]}]
  (proxy [MouseAdapter] [] 
    (mouseClicked [e] (clicked e))
    (mouseDragged [e] (dragged e))
    (mouseEntered [e] (entered e))
    (mouseExited [e] (exited e))
    (mouseMoved [e] (moved e))
    (mousePressed [e] (pressed e))
    (mouseReleased [e] (released e))
    (mouseWheelMoved [e] (wheelmoved e))))


(defn get-keys
  "Return an anonymous proxy for KeyAdapter that uses function handlers for 
   each event in args.  If no handlers are supplied, the KeyAdapter will not
   respond to events."
  [{:keys [pressed released  typed]}]
  (proxy [KeyAdapter] [] 
    (keyPressed [e] (pressed e))
    (keyReleased [e] (released e))
    (keyTyped [e] (typed e))))


(defn get-window
  "Return an anonymous proxy for WindowAdapter that uses function handlers for 
   each event in args.  If no handlers are supplied, the KeyAdapter will not
   respond to events."
  [{:keys [activated closed closing deactivated deiconified gained-focus 
           lost-focus opened state-changed]}]
  (proxy [WindowAdapter] [] 
    (windowActivated [e] (activated e))
    (windowClosed [e] (closed e))
    (windowClosing [e] (closing e))
    (windowDeactivated [e] (deactivated e))
    (windowDeiconified [e] nil)
    (windowGainedFocus [e] (gained-focus e))
    (windowIconified [e] nil)
    (windowLostFocus [e] (lost-focus e))
    (windowOpened [e](opened e))
    (windowStateChanged [e] (state-changed e))))

(defn get-eventroute
  "Return a map of {ename [event fobservation]}, where fobservation is an anonymous 
   function that routes an argument to the notify! of an anonymous observer.
   The effect is, given an eventname, we return a map of the eventname and a
   function that wraps an observable.  The purpose here is to facilitate
   wrapping of Java events, to provide observables amenable to combinators in 
   the observe library."
  [ename]
  (let [newevent (make-observable)] 
   {ename [newevent 
           (fn [e] (notify! newevent e))]}))

(defn eroutes->events [eventroutes]
  (zipmap (keys eventroutes) (for [[event obs] (vals eventroutes)] event)))

(defn eroutes->routes [eventroutes]
  (zipmap (keys eventroutes) (for [[event obs] (vals eventroutes)] obs)))


(defn simple-mouse [target]
  (let [eroute (get-eventroute :clicked)
        clickf (first (vals (eroutes->routes eroute)))]
    (doto target 
      (.addMouseListener 
        (proxy [MouseAdapter] []
          (mouseClicked [e] (clickf e)))))
    (first (vals (eroutes->events eroute)))))

(defn- make-alistener [f]
  (reify ActionListener
    (^void actionPerformed [this ^ActionEvent e] (f e))))

(defn action-observer
  "Defines a simple action observer for a Java component.  The component must  
   have the capacity to add an ActionListener.  We return the result of wrapping 
   the ActionListner's actionPerformed event in an observable.  Thus, we 
   can observe a stream of ActionEvents (generated by the unseen actionListener)
   by interacting with the returned observable.  This allows us to turn action 
   events into generic observables, rather than hiding the event data behind 
   method implementations."
  [target] 
  (let [eroute (get-eventroute :actionPerformed)
        route (first (vals (eroutes->routes eroute)))
        observable (first (vals (eroutes->events eroute)))]
    (do (.addActionListener target 
          (make-alistener (fn [^ActionEvent e] (notify! observable e))))
      {:actionPerformed observable})))

(defn mouse-observer 
  "Defines a simple mouse observer for a Java component.  The component must  
   have the capacity to add a MouseListener.  We return the result of wrapping 
   the MouseListener's events in several observables.  Thus, we 
   can observe a stream of MouseEvents (generated by the unseen MouseListener)
   by interacting with the returned observable.  This allows us to turn Mouse 
   Events into generic observeables."
  [target]
  (let [eventroutes 
        (reduce merge {}
                (map get-eventroute [:clicked :dragged :entered :exited :moved 
                                     :pressed :released :wheelmoved]))
        adapter (get-mouse (eroutes->routes eventroutes))]
  (do 
    (.addMouseListener target adapter)
    (.addMouseMotionListener target adapter)
    (.addMouseWheelListener target adapter)
    (eroutes->events eventroutes))))  

(defn window-observer 
  "Defines a simple window observer for a Java component.  The component must  
   have the capacity to add a WindowListener.  We return the result of wrapping 
   the WindowListener's events in several observables.  Thus, we 
   can observe a stream of WindowEvents (generated by the unseen WindowListener)
   by interacting with the returned observable.  This allows us to turn Window 
   Events into generic observeables."
  [target]
  (let [eventroutes 
        (reduce merge {}
                (map get-eventroute [:activated :closed :closing :deactivated 
                                     :deiconified :gained-focus :lost-focus 
                                     :opened :state-changed]))
        adapter (get-window (eroutes->routes eventroutes))]
  (do 
    (.addWindowListener target adapter)
    (eroutes->events eventroutes)))) 
  
;weird...tried using MouseEvent/BUTTON1, etc.  but case wouldn't work.
(defn get-button [^MouseEvent e]
  (case (.getButton e)
    1 :left
    2 :middle
    3 :right 
    :none))

(defn left-button? [^MouseEvent e]
  (= (get-button e) :left))
(defn right-button? [^MouseEvent e]
  (= (get-button e) :right))
(defn middle-button? [^MouseEvent e]
  (= (get-button e) :middle))

;(defn mouse-xy [^MouseEvent e]
;  [(.getX e) (.getY e)])

(defn key-observer
  "Defines a simple key observer for a Java component.  The component must  
   have the capacity to add a KeyListener.  We return the result of wrapping 
   the KeyListener's events in several observables.  Thus, we 
   can observe a stream of KeyEvents (generated by the unseen KeyListener)
   by interacting with the returned observable.  This allows us to turn Key 
   Events into generic observeables."
  [target]
  (let [eventroutes 
        (reduce merge {}
                (map get-eventroute [:pressed :released :typed]))]
  (do 
    (.addKeyListener target (get-keys (eroutes->routes eventroutes)))
    (eroutes->events eventroutes))))


;(defn window-observer
;  "Defines a simple window observer for a Java frame.  The component must  
;   have the capacity to add a WindowListener.  We return the result of wrapping 
;   the WindowListener's events in several observables.  Thus, we 
;   can observe a stream of WindowEvents (generated by the unseen WindowListener)
;   by interacting with the returned observable.  This allows us to turn Window 
;   Events into generic observeables."
;  [target]
;  (let [eventroutes 
;        (reduce merge {}
;                (map get-eventroute [:activated :closed :closing ]))]
;  (do 
;    (.addWindowListener target (get-window (eroutes->routes eventroutes)))
;    (eroutes->events eventroutes))))

(def observer-map {:mouse mouse-observer 
                   :key key-observer 
                   :action action-observer})

(defmulti get-observer
  "A single multimethod to get observable events from a target Swing component.
   This allows the use to get select categories of events (i.e. we may not 
   need key events, or mouse events, or may need both.
   category is a keyword in the set #{:mouse :key :action}.  The function will
   return a map of observable events, relative to indicated category."
  (fn [target category] category))
(defmethod get-observer nil [target category] {})
(defmethod get-observer :mouse [target category] (mouse-observer target))
(defmethod get-observer :key [target category] (key-observer target))
(defmethod get-observer :action [target category] (action-observer target))
(defmethod get-observer :window [target category] (window-observer target))
               
;Finally, to make event-access easy, we extend the IEventSource protocol 
;from cljgui.events.base to a couple of Swing components...
;Our implementations here are memoized, since we don't want to return new
;event listeners every time we call get-events.  We'd like to hook up 
;observables once, and work on them.  
;(extend-protocol IEventSource
;  JPanel
;    (event-categories [panel] #{:mouse :component :window})
;    (get-events [panel & categories] 
;       (let [cs (if (seq categories) 
;                  (filter #{:mouse :component :window} categories)
;                 [:mouse])]
;         (reduce #(merge %1 {%2 (get-observer panel %2)}) {} cs))))
                                             
         
              

;Testing....

;starting with the ActionListener....it's simple and is used by the Swing timer.
(comment 

(use 'cljgui.components.swing)
(def btn (button "Test!"))
(def inbox (text-field "blah"))
(def mevents (mouse-observer btn))
(->> (:clicked mevents)
  (map-obs event-data) 
  (subscribe println))
(->> (get-observer inbox :key)
     (:pressed)
     (map-obs (juxt event-data  key-modifiers))     
     (subscribe (fn [e](pprint e))))
(def frm (empty-frame))
(toggle-top (display frm (stack btn
                                inbox)))

)


;(defn actionlog [e]
;  (->> e 
;    (map-obs (fn [^ActionEvent evt] (


;(defn make-timer 
;  ([speed listenf] (Timer. speed (make-alistener listenf)))
;  ([speed] (make-timer speed (fn [_] nil))))


;BUSTED!
;(defn agent-timer [ticklimit] 
;  (let [state (atom {:timer nil :time 0 :ticks 0})       
;        t  (make-timer 200)
;        _ (doto t
;            (.addActionListener
;               (make-alistener 
;	             (fn [e]
;	                   (let [s @state]
;	                   (if (<= ticklimit (:tick s))
;	                     (swap! state merge {:time (inc (:time s))
;	                                         :ticks (inc (:ticks s))})
;                      (.stop t)))))))]
;    (agent (swap! state merge {:timer t}))))
;
;(defn get-timer [agt] (:timer @agt))
;(defn start-atimer [agt] (.start (get-timer agt)))
;(defn stop-atimer [agt] (.stop (get-timer agt)))
;
;(defn listen-timer [agt f] (.addActionListener (get-timer agt) f))

;(defn timed-logging
;  (let [atimer (agent-timer 100)
;        incoming (make-observable)
;        pushf (fn [^ActionEvent actionevt] 
;                (notify! incoming actionevt))
;        log (->> incoming
;               (map-obs .getWhen)
;               (subscribe println))]    


;;        -----Currently Unused Event/Listener classes------
;      CaretEvent CaretListener
;      AncestorEvent AncestorListener      
;      HyperlinkEvent HyperlinkListener
;      InternalFrameEvent InternalFrameListener
;      ListDataEvent ListDataListener
;      ListSelectionEvent ListSelectionListener
;      MenuDragMouseEvent MenuDragMouseListener
;      MenuEvent MenuEvent
;      MenuKeyEvent MenuKeyListener
;      PopupMenuEvent PopupMenuListener
;      RowSorterEvent RowSorterListener
;      TableColumnModelEvent TableColumnModelListener
;      TableModelEvent TableModelListener
;      TreeExpansionEvent TreeExpansionListener
;      TreeModelEvent TreeModelListener
;      TreeSelectionEvent TreeSelectionListener
;      UndoableEditEvent UndoableEditListener



    
    
