(ns spork.cljgui.gl)

;This is a Clojure port of the JOGL example in Wikipedia
;Author: Curran Kelleher
;License: Public Domain
;dependencies: jogl.jar, gluegen-rt.jar

;for the REPL (use C-x C-e):
(comment
;these are the default paths of the jars in Ubuntu
(add-classpath "file:/usr/share/java/jogl.jar")
(add-classpath "file:/usr/share/java/gluegen-rt.jar")
)

(import '(java.awt Frame)
  '(java.awt.event WindowListener WindowAdapter KeyListener KeyEvent)
  '(javax.media.opengl GLCanvas GLEventListener GL GLAutoDrawable)
  '(javax.media.opengl.glu GLU)
  '(com.sun.opengl.util Animator))
(def rotateT 0)
(def glu (new GLU))
(def canvas (new GLCanvas))
(def frame (new Frame "Jogl 3D Shape/Rotation"))
(def animator (new Animator canvas))
(defn exit "Stops animation and closes the OpenGL frame." []
(.stop animator)
(.dispose frame))

(.addGLEventListener
canvas
(proxy [GLEventListener] []
 (display
  [#^GLAutoDrawable drawable]
  (doto (.getGL drawable)
    (.glClear (. GL GL_COLOR_BUFFER_BIT))
    (.glClear (. GL GL_DEPTH_BUFFER_BIT))
    (.glLoadIdentity)
    (.glTranslatef 0 0 -5)

    (.glRotatef rotateT 1 0 0)
    (.glRotatef rotateT 0 1 0)
    (.glRotatef rotateT 0 0 1)
    (.glRotatef rotateT 0 1 0)
              
    (.glBegin (. GL GL_TRIANGLES))

    ; Front
    (.glColor3f 0 1 1)
    (.glVertex3f 0 1 0)
    (.glColor3f 0 0 1)
    (.glVertex3f -1 -1 1)
    (.glColor3f 0 0 0)
    (.glVertex3f 1 -1 1)


    ; Right Side Facing Front
    (.glColor3f 0 1 1)
    (.glVertex3f 0 1 0)
    (.glColor3f 0 0 1)
    (.glVertex3f 1 -1 1)
    (.glColor3f 0 0 0)
    (.glVertex3f 0 -1 -1)

    ; Left Side Facing Front
    (.glColor3f 0 1 1)
    (.glVertex3f 0 1 0)
    (.glColor3f 0 0 1)
    (.glVertex3f 0 -1 -1)
    (.glColor3f 0 0 0)
    (.glVertex3f -1 -1 1)

    ;Bottom
    (.glColor3f 0 0 0)
    (.glVertex3f -1 -1 1)
    (.glColor3f 0.1 0.1 0.1)
    (.glVertex3f 1 -1 1)
    (.glColor3f 0.2 0.2 0.2)
    (.glVertex3f 0 -1 -1)

    (.glEnd))
  (def rotateT (+ 0.2 rotateT)))

 (displayChanged [drawable m d])

 (init
  [#^GLAutoDrawable drawable]
  (doto (.getGL drawable)
    (.glShadeModel (. GL GL_SMOOTH))
    (.glClearColor 0 0 0 0)
    (.glClearDepth 1)
    (.glEnable (. GL GL_DEPTH_TEST))
    (.glDepthFunc (. GL GL_LEQUAL))
    (.glHint (. GL GL_PERSPECTIVE_CORRECTION_HINT)
         (. GL GL_NICEST)))
  (.addKeyListener
   drawable
   (proxy [KeyListener] []
     (keyPressed
  [e]
  (when (= (.getKeyCode e) (. KeyEvent VK_ESCAPE))
    (exit))))))

 (reshape
  [#^GLAutoDrawable drawable x y w h]
  (when (> h 0)
    (let [gl (.getGL drawable)]
  (.glMatrixMode gl (. GL GL_PROJECTION))
  (.glLoadIdentity gl)
  (.gluPerspective glu 50 (/ w h) 1 1000)
  (.glMatrixMode gl (. GL GL_MODELVIEW))
  (.glLoadIdentity gl))))))

(doto frame
(.add canvas)
(.setSize 640 480)
(.setUndecorated true)
(.setExtendedState (. Frame MAXIMIZED_BOTH))
(.addWindowListener
 (proxy [WindowAdapter] []
   (windowClosing [e] (exit))))
(.setVisible true))
(.start animator)
(.requestFocus canvas)