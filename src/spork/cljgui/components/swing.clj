 (ns spork.cljgui.components.swing
  (:require [spork.graphics2d [canvas :as j2d]
                              [swing :as jgraphics]]
            [spork.mvc :refer :all]
            [spork.events [base :as events]
                          [observe :as obs]
                          [native :as native]])
  (:import [java.util Vector]
        ;   [java.io OutputStream PrintStream]
           [javax.swing JFrame JPanel Box BoxLayout JTextField JSplitPane
            JLabel JButton JOptionPane JScrollPane Timer SwingUtilities
            JFileChooser JTable JFrame JMenuBar JMenu JMenuItem JTextArea
            JList ListModel ListSelectionModel AbstractListModel JTabbedPane JComponent ImageIcon]
           [javax.swing.event ChangeListener]
           [java.awt GridBagConstraints GridBagLayout BorderLayout FlowLayout 
                     GridLayout Component Graphics Graphics2D Dimension Insets]           
           [java.awt.event ActionListener MouseListener ComponentListener
            MouseAdapter MouseEvent WindowAdapter WindowEvent HierarchyListener HierarchyEvent]
           [java.awt.image BufferedImage]
           [spork.cljgui.components PaintPanel]
           ))

 
(defn get-events [obj]  (:event-stream (meta obj)))
 
(defn ->ui-spec [name & {:keys [doc event-type event] :as opts}]
  (merge {:name name}  opts))
 
(defmacro with-disposable [bindings & body]
  "Blatantly ripped from clojure.core !  with-open repurposed... "
  (assert (vector? bindings) "a vector for its binding")
  (assert (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-disposable ~(subvec bindings 2) ~@body)
                                (finally
                                  (. ~(bindings 0) ~'dispose))))
    :else (throw (IllegalArgumentException.
                   "with-open only allows Symbols in bindings"))))
 
;Macros for adding action listeners...

;Courtesy of Stuart Sierra.
(defmacro with-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

(defmacro with-resize [component event & body]
  `(. ~component addComponentListener
      (proxy [java.awt.event.ComponentListener] []
        (componentResized [~event] ~@body))))

(defmacro with-moved [component event & body]
	`(. ~component addComponentListener
	      (proxy [java.awt.event.ComponentListener] []
	        (componentResized [~event] ~@body))))

(defn run-app [app] 
  (if (javax.swing.SwingUtilities/isEventDispatchThread)
    (app)
    (SwingUtilities/invokeLater app)))
 
(defn run-now [app]
  (if (javax.swing.SwingUtilities/isEventDispatchThread)
    (app)
    (SwingUtilities/invokeAndWait app)))

;;Composable gui components....
;; (defn ^JFrame empty-frame 
;;   ([] (JFrame.))
;;   ([^String title] (JFrame. title)))

(defn ^JFrame empty-frame 
  ([] (empty-frame ""))
  ([^String title] 
     (let [frm (if (= title "") 
                 (JFrame.) 
                 (JFrame. title))]
;            onclose (proxy [WindowAdapter] []
 ;                     (windowClosing [^WindowEvent e]
 ;                       (.dipose frm)))]        
        (doto frm  (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
            ))))

;; (defn ^JFrame still-frame 
;;   ([] (let [frm (JFrame.)
;;             onclose (proxy [WindowAdapter] []
;;                       (windowClosing [^WindowEvent e]
;;                         (.dipose)))]
        
;;         (doto frm (.addWindowListener onclose))))
;;   ([^String title] (JFrame. title)))

(defn ^JFrame add-menu [^JMenuBar mb  ^JFrame frm]
  (do (.setJMenuBar frm mb)
    frm))

(defn ^JFrame make-visible [^JFrame frm]     
  (doto frm 
    (.pack) 
    (.setVisible true)))

(defn ^JFrame add-view [^JFrame frm ^JPanel view] 
 (do (.add (.getContentPane frm) view) 
         frm))

(defn visible? [^JFrame frm] (.isVisible frm))
(defn ^JFrame toggle-top [^JFrame frm]
  (doto frm (.setAlwaysOnTop (not (.isAlwaysOnTop frm))))) 

(defn clear-contents [^JFrame frm]
  (let [cp (.removeAll (.getContentPane frm))]
  (doto frm 
    (.repaint))))

(defn display-stretched [^JFrame frm ^JPanel pane]
  (doto frm 
     (add-view pane)
     (make-visible)))
    
;Display any generic swing component.  All implementations support JComponent.
(defn  display [^JFrame frm ^JPanel pane]
	  (doto frm
	    (clear-contents)
            (.add pane BorderLayout/CENTER)
	    (make-visible)))

(defn display-simple [^JPanel pane]
  (doto (empty-frame)
    (clear-contents)
    (.add pane)
    (make-visible)))

(defn alert 
  ([msg] (alert nil msg))
  ([frame msg] 
    (javax.swing.JOptionPane/showMessageDialog frame msg)))


(defn ^JMenu menu [name & items]
  (let [m (JMenu. (str name))]
    (doseq [item items]
        (.add m item))
    m))

(defn ^JMenuItem menu-item [name & {:keys [action-handler]
                                    :or   {action-handler (fn [] nil)}}]
  (let [m (JMenuItem. (str name))]
    (do (with-action m e
          (action-handler))
      m)))

(defn ^JMenuBar menu-bar [& menuseq]
  (let [mb (JMenuBar.)]
    (doseq [m (seq menuseq)]
      (cond (= (class m) JMenu) (.add mb m)
            (coll? m)             
            (let [child (menu (first m) (rest m))]
              (.add mb child))))      
    mb))


(defn map->menu [name action-map]
  (reduce (fn [m [item-name & [v]]]
            (let [itm (if (map? v)
                        (map->menu (str item-name) v)
                        (menu-item item-name  
                                   :action-handler v))]
              (doto m 
                (.add itm)))) (menu name) (seq action-map))) 

(defn menu-spec->ui-map [spec & {:keys [keywordize?] :or 
                                        {keywordize? true}}]
  (reduce (fn [acc k]
            (let [event-type 
                  (if keywordize? ((comp keyword 
                                         clojure.string/lower-case) k)
                    k)]
            (assoc acc k (->ui-spec k :doc (get spec k)
                                      :event-type event-type 
                                      :event event-type))))
                   {} (vec (keys spec))))

(defn reactive-menu-spec 
  "Converts a specification of reactive ui elements into their observable 
   events."
  [menu-spec & {:keys [subscribers]}]
  (let [ui-map (menu-spec->ui-map menu-spec)
        event-stream (obs/make-observable)
        _ (when subscribers (doseq [s subscribers]
                              (obs/subscribe! event-stream s)))
        activate! (fn [name]
                    (let [ui (get ui-map name)]
                      (obs/notify! event-stream (get ui :event-type))))]
    (-> (into {} (for [[k v] ui-map]
                          [k (fn [] (activate! k))])) 
         (with-meta {:ui-map ui-map
                     :event-stream event-stream}))))

(defn map->reactive-menu [name spec]
  (let [reactive-spec (reactive-menu-spec spec)]
    {:view (map->menu name reactive-spec)
     :control (meta reactive-spec)}))

  
(defn repaint [^JPanel p]
  (.repaint p))

;Code adapted from Staurt Sierra's excellent blog post.
(def c (GridBagConstraints.))

(defmacro set-grid! [constraints field value]
  `(set! (. ~constraints ~(symbol (name field)))
         ~(if (keyword? value)
            `(. java.awt.GridBagConstraints
                ~(symbol (name value)))
            value)))


(defmacro grid-bag-layout [container & body]
  (let [c (gensym "c")
        cntr (gensym "cntr")]
    `(let [~c (new java.awt.GridBagConstraints)
           ~cntr ~container]
       ~@(loop [result '() body body]
           (if (empty? body)
             (reverse result)
             (let [expr (first body)]
               (if (keyword? expr)
                 (recur (cons `(set-grid! ~c ~expr
                                          ~(second body))
                              result)
                        (next (next body)))
                 (recur (cons `(.add ~cntr ~expr ~c)
                              result)
                        (next body)))))))))

(defn shelf [& components]
  (let [shelf (JPanel.) ]
    (.setLayout shelf (FlowLayout.))
    (doseq [c components] (.add shelf c))
    shelf))

(defn stack [& components]
  (let [stack (Box. BoxLayout/PAGE_AXIS)]
    (doseq [c components]
      (.setAlignmentX c Component/CENTER_ALIGNMENT)
      (.add stack c))
    stack))

(defn splitter [top bottom]
  (doto (JSplitPane.)
    (.setOrientation JSplitPane/VERTICAL_SPLIT)
    (.setLeftComponent top)
    (.setRightComponent bottom)))

(defn button 
  ([text] (JButton. text))
  ([text f]
	  (doto (JButton. text)
		    (.addActionListener 
		      (proxy [ActionListener] []
		        (actionPerformed [e] (f e)))))))

;implement scroll pane? 

(defn ^JScrollPane scroll-pane [content]
  (JScrollPane. content))

(defn ^JTabbedPane tab-pane [content]
  (JTabbedPane. content))

(defn add-tabs [^JTabbedPane  t tabs]
  (reduce (fn [acc [k v]]
            (doto acc (.addTab (str k) v)))
          t (seq tabs)))

(defn ^JTabbedPane tabs [content-map]
  (add-tabs (JTabbedPane. JTabbedPane/TOP) content-map))

(defn label [txt] (JLabel. txt))

(defn txt [cols t]
  (doto (JTextField.)
    (.setColumns cols)
    (.setText t)))

(defn grid-container [x y f]
  (let [g (doto (JPanel.)
            (.setLayout (GridLayout. x y)))]
    (dotimes [i x]
      (dotimes [j y]
        (.add g (f i j))))
    g))

;; (defn grid []
;;   (doto (JPanel. (GridBagLayout.))    
;;     (grid-bag-layout 
;;      :fill :BOTH, :insets (Insets. 5 5 5 5)
;;      :gridx 0, :gridy 0, ;:weightx 1 :weighty 1
;;      (JButton. "One")
;;      :gridy 1
;;      (JButton. "Two")
;;      :gridx 1 :gridy 0 :gridheight 2
;;      (JButton. "Three"))))

;; (defn grid [x y f]

(def text-directions {:left JTextField/LEADING 
                      :centered JTextField/CENTER
                      :right JTextField/RIGHT})

(defn text-field [value & {:keys [direction] :or {direction :left}}]
  (doto (JTextField. value 15)
    (.setEnabled true)
    (.setHorizontalAlignment 
      (get text-directions direction JTextField/LEADING))))

(defn text-area
  "Creates a potentially editable area of text.  The difference between
   this and the text-field"
  [& {:keys [initial-value rows cols enabled?]
      :or {initial-value ""
           rows 20
           cols 80
           enabled? true}}]
  (doto (JTextArea. rows cols)
    (.setText initial-value)
    (.setEnabled enabled?)))

(defn text-box [& {:keys [initial-value width height]
                   :or {initial-value ""
                        width 500
                        height 500}}] 
  (let [txt  (JTextArea. 5 30)]
    (doto (JScrollPane. txt)
      (.setPreferredSize (Dimension. width height))
      (.setEnabled true))))
       
(defn change-label
  "Changes the text value of label L."
  ([^JLabel lab f e] 
    (.setText lab (f e)))
  ([^JLabel lab e]
    (.setText lab (str e))))

(defn select-file
  "Initiates a file selection dialogue using a Swing file chooser.  
   Returns the path to the selected file."
  ([initpath]
  (let [fc (JFileChooser. initpath)
        res (. fc showOpenDialog nil)]
    (if (= res (. JFileChooser APPROVE_OPTION))
      (str (.getSelectedFile fc))
      nil)))
  ([] (select-file (System/getProperty "user.dir"))))

(defn- folder-chooser [initpath]
  (let [f (JFileChooser. initpath)]
    (do 
      (.setFileSelectionMode f JFileChooser/DIRECTORIES_ONLY)
      f)))

(defn select-folder
  "Initiates a folder selection dialogue using a swing folder chooser.
   Returns the path to the selected folder."  
  ([initpath]   (let [f (folder-chooser initpath)
                      res (. f showOpenDialog nil)]
                  (if (= res (. JFileChooser APPROVE_OPTION))
                    (str (.getSelectedFile f))
                    nil)))
  ([] (select-folder (System/getProperty "user.dir"))))

;again, inspired by the awesome work from Incanter!  

;NOTE-> the Vector class (which the jtable wants to use) is 
;using reflection here, probably going to be slow for large 
;data sets....


(defn ^JTable ->swing-table [column-names column-vals 
                           & {:keys [sorted] :or {sorted false}}]
  (doto (JTable. (Vector. (map #(Vector. %) column-vals))
                             (Vector. column-names))
                    (.setAutoCreateRowSorter sorted)))

(defn ^ListModel ->list-model [coll & {:keys [item-at] 
                                       :or {item-at  (fn [index] 
                                                       (nth coll index))}}]
  (proxy [javax.swing.AbstractListModel] []
    ;(addListDataListener [l] nil) 
    (getElementAt [index] (item-at index))
    (getSize [] (count coll)))) 
    ;(removeListDataListener [l] nil))) 

(def selection-modes 
  {:multi-select  ListSelectionModel/MULTIPLE_INTERVAL_SELECTION
   :single-select ListSelectionModel/SINGLE_SELECTION
   :single-interval ListSelectionModel/SINGLE_INTERVAL_SELECTION})

(defn set-selection-mode [obj mode]
  (doto obj  
    (.setSelectionMode
      (get selection-modes mode
           (throw (Exception. (str "Unknown selection mode " mode)))))))

(defn ^JList ->swing-list [xs & {:keys [selection-mode] 
                                 :or   {selection-mode :multi-select}}]
  (JList. (->list-model xs)))


(defn ensure-delivery
  "If a promise has not been fulfilled, forces the value into the promise and 
   fulfills it.  Used for cross thread synchronization."
  [p v]
  (if (not (realized? p))
    (deliver p v)))
    
(defn seq-dialogue
  "Present the user with a choice of items from the sequence.  For maps, the
   list presented will be the keys of the map.  When user selects a key, the 
   corresponding item is returned.  For vectors and lists, the string 
   representation of the items are the items listed.  Do NOT use this with 
   infinite sequences....that'd be bad."
  [xs & {:keys [selection-mode]}]   
  (let [itms (if (map? xs)  
               (vec (keys xs))
               (vec xs))
        item-at   (if (map? xs) 
                    (fn [n] (get xs (get itms n)))
                    (fn [n] (nth xs n)))
        listbox (->swing-list itms :selection-mode selection-mode)
        mouse-events (native/get-observer listbox :mouse)
        selected (promise)
        get-list-vals (if (= selection-mode :single-select) 
                        #(.getSelectedValue listbox)
                        #(seq (.getSelectedValues listbox)))
        cancel   (->> (obs/make-observable)
                      (obs/subscribe 
                        (fn [_] (ensure-delivery selected nil))))
        ok       (->> (obs/make-observable)
                      (obs/subscribe 
                        (fn [_] (ensure-delivery selected (get-list-vals))))) 
        double-click (->> (-> mouse-events :clicked)                       
                       (obs/map-obs events/event-data) ;extract event to a map
                       (obs/filter-obs (fn [e] (>= (:ClickCount e) 2))))
        click-ok (obs/subscribe (fn [_] (obs/notify! ok :ok)) double-click)]
     (make-modelview {:selected selected} 
                     listbox                         
                     {:double-click double-click
                      :ok ok  
                      :cancel cancel})))

(defmulti ^JTable as-JTable (fn [o & opts] (class o)))

(defn table 
  ([data] (as-JTable data :sorted true))
  ([name data] (shelf (label name)
                      (as-JTable data :sorted true))))

(defn ^JFrame ->scrollable-view [content & {:keys [title width height] :or {title "" width 400 height 600}}] 
  (doto (empty-frame title)
    (.add (JScrollPane. content))
    (.setSize width  height)
    (.pack)
    (.setVisible true)))

(defn choose-from
  "Given a sequence of xs, presents the user with a view of the sequence, and 
   allows them to select an entry.  Selection is confirmed by double clicking
   or clicking Okay."
  [xs & {:keys [prompt] 
         :or {prompt "Double click on a single item, or select and hit ok."}}] 
  (let [{:keys [model view control state]} (seq-dialogue xs)
        get-selection #(deref (:selected model))]
    (with-disposable 
      [lbox (display-simple 
              (scroll-pane         
                (stack
                  (label prompt)
                  view
                  (shelf 
                    (button "Ok" 
                      (fn [e] (obs/notify! (:ok control) :clicked))) 
                    (button "Cancel"
                      (fn [e] (obs/notify! (:cancel control) :cancelled)))))))]
      (let [on-close (->> (native/get-observer lbox :window) 
                       (:closing)
                       (obs/subscribe (fn [e]
                                          (obs/notify! (:cancel control)
                                                       :cancelled))))]
        (get-selection)))))

(defn yes-no-dialogue
  "A simple yes/no dialogue presented to the user.  Depending on which 
   way the user decides, either the yes or no continuation will be called.
   Each continuation should be a thunk, or a delayed computation."
  [prompt] 
  (let [input (promise) 
        cancel   (->> (obs/make-observable)
                      (obs/subscribe 
                        (fn [_] (ensure-delivery input nil))))
        yes       (->> (obs/make-observable)
                      (obs/subscribe 
                        (fn [_] (ensure-delivery input :yes))))]
    (make-modelview {:selected input} 
                     (stack (label prompt)
                            (shelf (button "Yes" 
                                     (fn [e] (obs/notify! yes :y)))
                                   (button "No" 
                                     (fn [e] (obs/notify! cancel nil)))))
                      {:yes yes
                       :no  cancel
                       :cancel cancel})))

(defn yes-no-box
  "Prompts the user to select yes or no from a button selection.  Returns either
   :yes or nil."
  [& [prompt]]
  (let [{:keys [model view control state]} (yes-no-dialogue prompt)
        get-selection #(deref (:selected model))]
    (with-disposable [yes-no (display (empty-frame) view)]
      (let [on-close (->> (native/get-observer yes-no :window) 
                     (:closing)
                     (obs/subscribe (fn [_] (obs/notify! (:cancel control)
                                                         :cancelled))))]
        (get-selection)))))

(defn text-dialogue
  "Presents the user with a simple input box to enter information into."
  [prompt & {:keys [text-component exit-on] 
             :or {text-component (text-field "")
                  exit-on :newline}}]
  (let [inbox    text-component      
        read-box (fn [] (.getText inbox))
        input    (promise) 
        key-events   (native/get-observer inbox :key)
        exit-key   (->> (:pressed key-events) 
                     (obs/filter-obs (native/make-key-filter exit-on))
                     (obs/subscribe 
                   (fn [_] (ensure-delivery input (read-box)))))                        
        cancel   (->> (obs/make-observable)
                      (obs/subscribe 
                        (fn [_] (ensure-delivery input nil))))
        ok       (->> (obs/make-observable)
                      (obs/subscribe 
                        (fn [_] (ensure-delivery input (read-box)))))]
    (make-modelview {:selected input} 
                     (stack (label prompt)
                            inbox)
                      {:exit-key exit-key
                       :ok ok  
                       :cancel cancel})))
(defn input-box
  "Presents the user with an input box, ok, and cancel buttons, with 
   a caller-supplied prompt provided.  "
  [& {:keys [prompt text-component exit-on]
      :or    {prompt "" 
              text-component (text-field "") 
              exit-on :newline}}] 
  (let [{:keys [model view control state]} 
        (text-dialogue prompt :text-component text-component 
                              :exit-on exit-on)
        get-selection! #(deref (:selected model))]
    (with-disposable 
      [ibox (display-simple 
                (stack
                  (scroll-pane view)
                  (shelf 
                    (button "Ok" 
                      (fn [e] (obs/notify! (:ok control) :clicked)))
                    (button "Cancel" 
                      (fn [e] (obs/notify! (:cancel control) :cancelled))))))]
      (let [on-close (->> (native/get-observer ibox :window) 
                       (:closing)
                       (obs/subscribe (fn [_] (obs/notify! (:cancel control)
                                             :cancelled))))]
        (get-selection!)))))

(defn memo-box 
  "Presents the user with a text area, which allows them to type in multiple 
   lines of text.  Default exit-key behavior is ctrl-enter.  This is identical
   to the input box, only it provides extra functionality, and can be used to
   engage in significantly larger text dialogues."
   [& {:keys [prompt exit-on] :or {prompt "" exit-on [:control :newline]}}]
   (input-box :prompt prompt :text-component (text-area) :exit-on exit-on)) 

(defn simple-log 
  "Provides a model-view-controller that appends text to a scrollable text 
   area.  Provides events for append, clear.  Text is not editable 
   by mouse/keyboard input."
  [& {:keys [title editable?] :or {title "Log" editable? false}}]
  (let [logbox (text-area :enabled? editable?)
        append-text! (fn [x] (.append logbox (str x)))
        clear-text!  (fn []  (.setText logbox ""))]
    (make-modelview
      {:get-text (fn [] (.getText logbox))} 
      (scroll-pane logbox)
      {:append-text (->> (obs/make-observable)
                      (obs/subscribe append-text!))
       :write-line  (->> (obs/make-observable)
                      (obs/subscribe  #(append-text! (str % \newline))))
       :clear-text  (->> (obs/make-observable)
                      (obs/subscribe clear-text!))}
      nil)))

;;Incorporated from elsewhere...jvmnotebook.org or something.
;(defn textarea-stream-class [textarea]
;  (proxy [OutputStream] []
;		 (write [data]
;				(. textarea (append "!!!")))))
;
;(defn init-textarea-stream [textarea]
;  (let [out (new PrintStream (textarea-stream-class textarea))]	
;	;; Redirect standard output stream to the TextAreaOutputStream
;    (. System (setOut out))
;	;; Redirect standard error stream to the TextAreaOutputStream
;	  (. System (setErr out))))
;
;(defn stream->observable [])
;(defn observed-err [])



(defn simple-repl
  "Makes a very rudimentary embedded repl.  This is still in Alpha, needs to 
   get the kinks worked out."
  []
  (let [name-space  (ns-name *ns*) 
        add-prompt  (fn [s] (str "=> " s))
        terminal    (simple-log :editable? false)        
        history     (atom [])
        input       (text-area)
        write-line! (-> terminal :control :write-line) 
        input-keys  (native/get-observer input :key)
        read!       (fn [] (read-string (.getText input)))
        eval-       (fn [form] [form (try  (eval form)
                                       (catch Exception e 
                                         (str "Exception " (.getMessage e))))])        
        print!      (fn [[form res]] (do (swap! history conj form) 
                                   (obs/notify! write-line!                                      
                                      (add-prompt form))
                                    (obs/notify! write-line! 
                                      (with-out-str (print res)))))
        repl!       (->> (:pressed input-keys)
                      (obs/filter-obs (native/make-key-filter 
                                        [:control :newline]))
                      (obs/subscribe 
                        (fn [_] (print! (eval- (read!))))))
        mvc (make-modelview 
              {:history history}
              (splitter (stack (label "Output") 
                               (:view terminal))
                        (stack (label "Input")
                               input))
              {:eval (->> (obs/make-observable)
                          (obs/subscribe (fn [form] (print! (eval- form)))))
               :clear (:clear-text terminal)})]
    (do (obs/notify! (-> mvc :control :eval) 
                     `(~'do (~'ns ~name-space)
                          (~'use 'clojure.repl)))
      mvc)))
       

;(defn dialogue-parser
;  "Given a tree of choices, accumulates a dialogue with the user."
;  [choice-tree go-back] 

;(defn survey
;  "Given a map of {choicename choices}, constructs a wizard-like dialogue 
;   context that creates new views for each set of choices.  Along with the 
;   view, "
;  [choices]
;  (reduce (fn [acc [name choice]] 
;            (
  

(comment 
;example of a multiple-choice dialogue
  (def choices (pmap 
                 (fn [i] (choose-from (zipmap [:tom :dick :harry] [0 1 2])))
                 (range 3)))

;(defn object-bounds->world-bounds 
;  ([x y width height] 
;    (if (and (zero? x) (zero? y))
;      [x y width height]
;      [0 0 (+ width (Math/abs x)) (+ height (Math/abs y))]             
;      )))

; MouseListener mouseListener = new MouseAdapter() {
;     public void mouseClicked(MouseEvent e) {
;         if (e.getClickCount() == 2) {
;             int index = list.locationToIndex(e.getPoint());
;             System.out.println("Double clicked on Item " + index);
;          }
;     }
; };
; list.addMouseListener(mouseListener);

  )



;This eff'd up because of a circular dependency.....
;If call a namespace from another namespace that depends on it...you introduce 
;a circular dependency, and reval everything in the other namespace....it's 
;bad....

(defmulti view (fn [obj & options] (class obj)))
        

(defn- guitype [c]
  (cond 
    (= (class c) spork.mvc.modelview) :modelview
    (or (instance? javax.swing.JComponent c)
        (instance? java.awt.Component c) )   :component
    (satisfies? j2d/IShape c) :shape
    :else nil))
    
;define a multimethod for displaying things....this stemmed from an original 
;lone function called display, that was centered around viewing a JPanel in 
;a JFrame.  Now that we have modelviews, I'd like to be able to display them 
;as well.  In essence, we want to keep the structure of modelviews accessible, 
;and facilitate interop.
(defmulti display (fn [frame contents] (guitype contents)))

;Display any generic swing component.  All implementations support JComponent.
(defmethod  display :component [^JFrame frm ^JPanel pane]
	  (doto frm
	    (clear-contents)
	    (.setContentPane (doto (JPanel.) 
	                       (.add pane BorderLayout/CENTER)))
	    (make-visible)))
  
(defmethod display :modelview [^JFrame frm mvc]
  (display frm (mvc-view mvc)))

;rather than dealing with all of the linear operations using the 
;internal state of the graphics object....why don't we just provide 
;functions for handling transforms?  Seems like it'd be easier...

;That way, we can define a coordinate system.  Usually, when we create a panel
;the starting width/height will define a coordinate system.
;We can then  draw (relative) points to this coordinate system using the 
;identity transform.  

;What happens when we scale the component?  We're still drawing to the 
;coordinate system....

;;__TODO__ Rewrite paintpanel using the mvc stuff and reactives.  It's an old 
;;implementation


(defn close-listener [closef]
  (proxy [WindowAdapter] []
    (windowClosing [^WindowEvent e]
      (closef))))

;;loses type info..
(defn dispose-all! [xs]
  (doseq [x xs]
    (.dispose x)))

(defmacro window-disposer [xs]  
  `(proxy [WindowAdapter] []
     (windowClosing [^WindowEvent e]
       (do ~@(map (fn [x] `(.dispose ~x)) xs)))))

(defn hierarchy-listener [f]
  (proxy [HierarchyListener] []
    (hierarchyChanged [^HierarchyEvent e]
      (f e))))

(def logger (atom nil))
;(add-watch logger :gui (fn ()))
(defn log! [msg] (swap! logger conj msg))

;;Note -> if we don't call the parent's paintcomponent, swing will not 
;;repaint the component like it should, which messes up scrollpanes
;;and introduces graphical artifacts.
(defn paintpanel
  "Create a JPanel with its paint method overriden by paintf, which will be 
   called using g.  We can get mutable behavior by passing a function that 
   evals and applies a ref'd function for paintf, or we can keep the painting 
   static.  Note, paintpanel is static, in that resize behavior will not scale
   the original coordinate system.  It will NOT stretch.  Use paintpanel for 
   simple situations where you have fixed dimensions."
  ([width height paintf]
   (let [painter  (atom paintf)
         meta-map (atom {:paintf painter})
         panel    (proxy [JPanel clojure.lang.IMeta clojure.lang.IObj] []
                    (paintComponent [^Graphics g]
                      (let [^JComponent this this
                            _  (proxy-super paintComponent g)]
                        (paintf (jgraphics/->canvas-graphics g width height))))
                    (removeNotify [] (do (println "removing!")
                                         (proxy-super removeAll)
                                         (proxy-super removeNotify)))
                    (withMeta [m] (reset! meta-map m))
                    (meta [] @meta-map))
           savelistener (proxy [MouseAdapter] []
                          (mouseClicked [^MouseEvent e]
                             (if (= (.getButton e) MouseEvent/BUTTON3)
                               (let [savepath 
                                     (str (str (System/getProperty "user.home") 
                                               "\\" "SavedBuffer.png")) ]
                                 (do (let [buffer (jgraphics/make-imgbuffer width height)
                                           bg     (j2d/bitmap-graphics buffer)
                                           _      (paintf bg)]
                                       (j2d/write-image buffer savepath nil))
                                     (alert (str "Saved image to " savepath)))))))]                                      
          (doto panel                                                     
            (.setPreferredSize (Dimension. width height))
            (.addMouseListener  savelistener)
            (.setName  (str (gensym "Canvas"))))))
  ([paintf] (paintpanel 250 250 paintf)))

;;Note -> if we don't call the parent's paintcomponent, swing will not 
;;repaint the component like it should, which messes up scrollpanes
;;and introduces graphical artifacts.
(defn new-paintpanel
  "Create a JPanel with its paint method overriden by paintf, which will be 
   called using g.  We can get mutable behavior by passing a function that 
   evals and applies a ref'd function for paintf, or we can keep the painting 
   static.  Note, paintpanel is static, in that resize behavior will not scale
   the original coordinate system.  It will NOT stretch.  Use paintpanel for 
   simple situations where you have fixed dimensions."
  ([width height paintf]
   (let [^PaintPanel panel    (PaintPanel. width height  paintf)
         savelistener (proxy [MouseAdapter] []
                          (mouseClicked [^MouseEvent e]
                             (if (= (.getButton e) MouseEvent/BUTTON3)
                               (let [savepath 
                                     (str (str (System/getProperty "user.home") 
                                               "\\" "SavedBuffer.png")) ]
                                 (do (let [buffer (jgraphics/make-imgbuffer width height)
                                           bg     (j2d/get-graphics (j2d/bitmap-graphics buffer))
                                           _      (paintf bg)]
                                       (j2d/write-image buffer savepath nil))
                                     (alert (str "Saved image to " savepath)))))))]                                      
          (doto panel                                                     
            (.setPreferredSize (Dimension. width height))
            (.addMouseListener  savelistener)
            (.setName  (str (gensym "Canvas"))))))
  ([paintf] (new-paintpanel 250 250 paintf)))

;;if we use jlabel and bufferedimage, imageicon stuff, we can avoid overriding any
;;swing methods and dealing with hacking protected methods, in theory.
(defn ^JLabel canvas [^BufferedImage buffer]  (JLabel. (ImageIcon. buffer)))        

(defn cached-paintpanel
  "Create a JPanel with its paint method overriden by paintf, which will be 
   called using g.  We can get mutable behavior by passing a function that 
   evals and applies a ref'd function for paintf, or we can keep the painting 
   static.  Note, paintpanel is static, in that resize behavior will not scale
   the original coordinate system.  It will NOT stretch.  Use paintpanel for 
   simple situations where you have fixed dimensions."
  ([width height paintf]
     (let [buffer   (atom (jgraphics/make-imgbuffer  width height))           
           painter  (atom paintf)
           meta-map (atom {:buffer @buffer :paintf painter})
           bg     (jgraphics/->canvas-graphics (j2d/bitmap-graphics @buffer) width height)
           p      (fn [^Graphics2D g]
                    (do (paintf bg) 
                        (j2d/draw-image g @buffer :opaque 0 0)))
           panel  (proxy [JPanel clojure.lang.IMeta clojure.lang.IObj]   []
                    ;; (paintComponent [^Graphics g]
                    ;;   (let [^JComponent this  this ;hat tip to stackoverflow, this was tricky!
                    ;;         _  (proxy-super paintComponent g)]
                    ;;     (p g)))
                    (paint [^Graphics g]
                      (let [^JComponent this  this ;hat tip to stackoverflow, this was tricky!
                            _  (proxy-super paintComponent g)]
                        (p g)))
                    (removeNotify   [] (do (println "removing!")
                                           (proxy-super removeAll)
                                           (.dispose bg)
                                           (.flush ^BufferedImage @buffer)
                                           (reset! buffer nil)
                                           (proxy-super removeNotify)))
                    (meta [] @meta-map)
                    (withMeta [m] (reset! meta-map m)))
           savelistener (proxy [MouseAdapter] []
                          (mouseClicked [^MouseEvent e]
                             (if (= (.getButton e) MouseEvent/BUTTON3)
                               (let [savepath 
                                     (str (str (System/getProperty "user.home") 
                                               "\\" "SavedBuffer.png")) ]
                               (do (j2d/write-image (aget buffer 0) savepath nil)
                                 (alert (str "Saved image to " savepath)))))))]                                      
         (doto panel                                                     
           (.setPreferredSize (Dimension. width height))
           (.addMouseListener  savelistener)
           (.setName (str (gensym "Canvas"))))))
  ([paintf] (cached-paintpanel 250 250 paintf)))

;; (defn cached-paintpanel2
;;   "Create a JPanel with its paint method overriden by paintf, which will be 
;;    called using g.  We can get mutable behavior by passing a function that 
;;    evals and applies a ref'd function for paintf, or we can keep the painting 
;;    static.  Note, paintpanel is static, in that resize behavior will not scale
;;    the original coordinate system.  It will NOT stretch.  Use paintpanel for 
;;    simple situations where you have fixed dimensions."
;;   ([width height paintf]
;;    (let [painter  (atom paintf)
;;          buffer   (atom (jgraphics/make-imgbuffer width height))
;;          bg       (.getGraphics @buffer)
;;          panel    (with-meta (PaintPanel.)
;;                     {:buffer @buffer
;;                      :paintf painter})
;;          paint!   (fn [^Graphics2D g] (do (@painter bg)
;;                                           (.repaint panel)))
;;          savelistener (proxy [MouseAdapter] []
;;                         (mouseClicked [^MouseEvent e]
;;                           (if (= (.getButton e) MouseEvent/BUTTON3)
;;                             (let [savepath 
;;                                   (str (str (System/getProperty "user.home") 
;;                                             "\\" "SavedBuffer.png")) ]
;;                               (do (j2d/write-image (aget buffer 0) savepath nil)
;;                                   (alert (str "Saved image to " savepath)))))))
;;          _            (swap! (.state panel) merge  {:paintf paint! :width width :height height
;;                                                     :buffer @buffer})]
;;          (doto panel                                                     
;;            (.setPreferredSize    (Dimension. width height))
;;            (.addMouseListener     savelistener)
;;            (.setName (str (gensym "Canvas"))))))
;;   ([paintf] (cached-paintpanel2 250 250 paintf)))

(defmethod display :shape [^JFrame frm s]
  (let [{:keys [x y width height]} (j2d/shape-bounds s)]    
    (display frm (paintpanel 
                     (inc (+ width x)) 
                     (inc (+ height y)) #(j2d/draw-shape s %)))))

(defn repaint [^JPanel p] (.repaint p))
                  
(defn stretchable-panel
  "Create a JPanel with its paint method overriden by paintf, which will be 
   called using g.  We can get mutable behavior by passing a function that 
   evals and applies a ref'd function for paintf, or we can keep the painting 
   static.  Note, paintpanel is static, in that resize behavior will not scale
   the original coordinate system.  It will stretch.  Use stretchpanel for 
   situations where you have user resizing.  Note, the behavior will only take
   effect when Resize events are generated.  This means that you must use 
   stretchpanel in a layout manager that supports it."
  ([iwidth iheight paintf]
     (let [buffer (ref (jgraphics/make-imgbuffer iwidth iheight))
           bg  (ref (j2d/bitmap-graphics @buffer)) ;this is really our drawing surface...
           
           aspect  (/ iheight iwidth )
           w (ref iwidth) ;our nominal coordinate width
           h (ref iheight) ;our nominal coordinate height           
           offset (ref [0 0]) ;no offset to begin with applied.           
           p (fn [^Graphics2D g]
               (do                   
                 (paintf @bg) ;paint to the buffer
                 (j2d/draw-image g @buffer :opaque 0 0))) ;draw buffer to any graphics.
           panel (doto 
                   (proxy [JPanel] [] 
                     (paintComponent [g]  (do  (proxy-super paintComponent g)  
                                               (p g)))
                     (removeNotify [] (do (println "removing!")
                                          (proxy-super removeAll)
                                          (.dispose bg)
                                          (.flush buffer)
                                          (proxy-super removeNotify))))
                   (.setPreferredSize (Dimension. iwidth iheight)))
           resize (fn [wnew hnew] ;if w and h have changed, need to change the buffer
                    (let [wprev @w
                          hprev @h
                          ^BufferedImage oldbuff @buffer]
                          
	                    (if (or (not= wnew wprev) (not= hnew hprev))
                       (dosync                         
                         (let [[xoff yoff] [(- wprev wnew) (- hprev hnew)]
                               ^BufferedImage newbuff (jgraphics/make-imgbuffer wnew hnew)
                               ^Graphics2D bgraph (j2d/bitmap-graphics newbuff)]                               
                           (dosync 
                             (ref-set offset [xoff yoff])
                             (ref-set buffer newbuff)
                             (ref-set bg (doto bgraph                                                                                     
                                           (.scale (double (/ wnew wprev)) 
                                                   (double (/ hnew hprev))
                                           )))
                             (.setPreferredSize panel (Dimension. wnew hnew))
                             ))))))]
       (doto (JPanel. (GridBagLayout.))                                                                                                         
         (grid-bag-layout (JPanel. (GridBagLayout.)) 
                      :fill :BOTH, :insets (Insets. 0 0 0 0)
                      :gridx 0, :gridy 0, :weightx 1 :weighty 1 
                      
                     (doto panel 
                            (.setPreferredSize (Dimension. iwidth iheight))
                            (.addComponentListener 
                              (proxy [ComponentListener] []
                                (componentMoved [e] nil)
                                (componentHidden [e] nil)
                                (componentResized [e]
                                  (let [w (.getWidth panel)
                                        h (.getHeight panel)]                                    
                                    (if (and (> w 0) (> h 0))
                                      (let [areaw (/ (* w w ) aspect)
                                            areah (* h h aspect)]
                                        (if (<= areaw areah) ;minimize 
                                          (resize w
                                              (* aspect w))
                                          (resize h
                                              (/ h aspect)))))))                                   
                                (componentShown [e] nil)))  )))))
  ([paintf] (stretchable-panel 250 250 paintf)))


(defmethod view JFrame [frm & [s]] 
    (let [mvc (case (guitype s)
                :modelview s
                :shape (let [{:keys [x y width height]} (j2d/shape-bounds s)]
                         (make-modelview s 
                           (paintpanel (inc (+ x width)) ;;check this...for duplicate.
                                       (inc (+ y height))
                                            #(j2d/draw-shape s %)) {}))
                :component (make-modelview s s {}))]                
      [(display frm s)  mvc]))

(defmethod view JTable [t & {:keys [title] :or {title "Table"}}]
  (->scrollable-view t :title title))

;; (defmethod view :default [s & {:keys [title cached?] :or {title "Shape" :cached? false}}] 
;;   (if (satisfies? j2d/IShape s)
;;     ;(->scrollable-view s :title title)))
;; ;    (view (empty-frame) s)))
;;     (let [{:keys [x y width height]} (j2d/shape-bounds s)]
;;       (->scrollable-view (
;;                           (if cached? 
;;                             cached-paintpanel
;;                             paintpanel)
;;                           (inc (+ x width)) ;;check this...for duplicate.
;;                           (inc (+ y height))
;;                           #(j2d/draw-shape s %)) :title title))))

(defn atom? [x]  (instance? clojure.lang.Atom  x))
(defn add-repaint-watch! [atm ^JPanel pnl]
  (if (atom? atm)
    (let [repaint    (keyword (gensym "repainter"))
          closer     (hierarchy-listener (fn [^HierarchyEvent e]
                                           (let [^Component c (.getComponent e)]
                                             (when (and (not (.isDisplayable c))
                                                        (nil? pnl))
                                               (remove-watch atm repaint)))))] 
      (do (add-watch atm repaint (fn [k r old new] (.repaint pnl)))
          (.addHierarchyListener pnl closer)
          pnl))
    pnl))
        

;;creates a mouse adapter that listen to mouse events from the panel and
;;translates them into pan and zoom alterations.
;; (defn ->navigator [& {:keys [pan zoom]}
;;                    {:or {pan canvas/*pan*
;;                          zoom canvas/*zoom*}}]
;;   (let [last-point (atom nil)        
;;         deltax (^int fn [^int x] (- (aget xy 0) 
        
;;   (proxy [MouseAdapter] []
;;     (mouseClicked [^MouseEvent e]
;;       (if (= (.getButton e) MouseEvent/BUTTON3)))
;;     (mouseDragged [^MouseEvent e]
       
;;       ))

(defmethod view :default [s & {:keys [title cached?] :or {title "Shape" :cached? false}}] 
  (if (satisfies? j2d/IShape s)
    (let [{:keys [x y width height]} (j2d/shape-bounds s)
          paintf (if (atom? s) (fn [c] (j2d/draw-shape @s c))
                     (fn [c] (j2d/draw-shape s c)))
          panel  (->> (;(if cached? 
                         ;cached-paintpanel
                         ;paintpanel
                                        ;)
                       new-paintpanel
                       (inc (+ x width)) ;;check this...for duplicate.
                       (inc (+ y height))
                       paintf)                      
                      (add-repaint-watch! s))
           ]
      (->scrollable-view panel :title title))))

(defn swing-canvas [width height] 
  (let [frm (empty-frame)
        painter (atom (fn [g] nil))        
        panel   (doto 
                  (proxy [JPanel] [] (paintComponent [g]  (@painter g)))
                  (.setPreferredSize (Dimension. width height)))
        _       (add-watch painter :repaint (fn [k r o n]
                                             (repaint panel)))]
    (do (doto (display frm panel) (toggle-top))
      {:frame frm 
       :painter painter 
       :panel panel})))   

;(defn view 
;  "Bread and butter function to quickly visualize anything that can be 
;   painted into a frame.  Uses the implementation of display for s.  If 
;   no frame is provided, an empty one is created with a simple paint panel."
;  ([^JFrame f s]
;    (let [mvc (case (guitype s)
;                :modelview s
;                :shape (let [{:keys [width height]} (shape-bounds s)]
;                         (make-modelview s 
;                             (paintpanel width height #(draw-shape s %)) {}))
;                :component (make-modelview s s {}))]                
;      [(display f s)  mvc]))
;  ([s] (view (empty-frame) s)))
;

;  "Bread and butter function to quickly visualize anything that can be 
;   painted into a frame.  Uses the implementation of display for s.  If 
;   no frame is provided, an empty one is created with a simple paint panel."
 
 


 

 
