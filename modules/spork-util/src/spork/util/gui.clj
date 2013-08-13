;TOM SPOON 10 July
;A simple package for enabling file and folder selection dialogues, assumably 
;from the command line.  Uses Swing GUI componenets.
;Also exposes a few simple methods for viewing data.  The inspiration for a 
;generic 'view' method came from Incanter, which is REALLY nice.  I just 
;wanted a lightweight swing viewer for certain instances.  For heavier 
;stuff, use cljgui, which has a full-featured user interface framework.

;Note -> this should be supplanted by a more powerful framework, namely 
;cljgui.  The intent is to provide a lightweight set of operations for building
;simple user interfaces and CRUD applications.
(ns spork.util.gui
  (:import [java.util Vector]
           [javax.swing JFrame JPanel Box BoxLayout JTextField JSplitPane
            JLabel JButton JOptionPane JScrollPane Timer SwingUtilities
            JFileChooser JTable JFrame JMenuBar JMenu JMenuItem]
           [javax.swing.event ChangeListener]
           [java.awt GridBagConstraints GridBagLayout BorderLayout FlowLayout 
                     GridLayout Component]           
           [java.awt.event ActionListener MouseListener ComponentListener 
                           MouseAdapter MouseEvent]))

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


;;Composable gui components....
(defn ^JFrame empty-frame 
  ([] (JFrame.))
  ([title] (JFrame. title)))

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
	    (.setContentPane (doto (JPanel.) 
	                       (.add pane BorderLayout/CENTER)))
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
        (.add g (f))))
    g))

(defn text-field [value]
  (doto (JTextField. value 15)
    (.setEnabled false)
    (.setHorizontalAlignment JTextField/RIGHT)))

(defn change-label
  "Changes the text value of label L."
  [^JLabel lab f e] 
  (.setText lab (f e)))

 (defn run-app [app] 
   (if (javax.swing.SwingUtilities/isEventDispatchThread)
       (app)
     (SwingUtilities/invokeLater app)))
 
 (defn run-now [app]
   (if (javax.swing.SwingUtilities/isEventDispatchThread)
       (app)
     (SwingUtilities/invokeAndWait app)))


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

(defn ^JTable ->swing-table [column-names column-vals 
                           & {:keys [sorted] :or {sorted false}}]
  (doto (JTable. (Vector. (map #(Vector. %) column-vals))
                             (Vector. column-names))
                    (.setAutoCreateRowSorter sorted)))

(defmulti ^JTable as-JTable (fn [o & opts] (class o)))

(defn table 
  ([data] (as-JTable data :sorted true))
  ([name data] (shelf (label name)
                      (as-JTable data :sorted true))))

(defn ^JFrame ->scrollable-view [content & {:keys [title]}] 
  (doto (JFrame. title)
    (.add (JScrollPane. content))
    (.setSize 400 600)
    (.setVisible true)))

;This eff'd up because of a circular dependency.....
;If call a namespace from another namespace that depends on it...you introduce 
;a circular dependency, and reval everything in the other namespace....it's 
;bad....

(defmulti view (fn [obj & options] (class obj)))


