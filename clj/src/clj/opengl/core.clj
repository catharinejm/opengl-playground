(ns opengl.core
  (:import [org.lwjgl Sys BufferUtils]
           [org.lwjgl.glfw GLFW Callbacks GLFWKeyCallback GLFWvidmode]
           [org.lwjgl.opengl GL11 GLContext]
           [org.lwjgl.system MemoryUtil]
           [java.nio ByteBuffer]))

(defrecord State
    [error-callback key-callback window])

(defn init
  ^State []
  (let [error-callback (Callbacks/errorCallbackPrint)]
    (GLFW/glfwSetErrorCallback error-callback)
    (when-not (identical? (GLFW/glfwInit) GL11/GL_TRUE)
      (throw (IllegalStateException. "Unable to initialize GLFW")))
    (GLFW/glfwDefaultWindowHints)
    (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GL11/GL_FALSE)
    (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GL11/GL_TRUE)
    (let [width 640
          height 480
          window (GLFW/glfwCreateWindow width height "Hello from Clojure!"
                                        MemoryUtil/NULL MemoryUtil/NULL)
          _ (when (nil? window)
              (throw (RuntimeException. "Failed to create the GLFW window")))
          key-callback (proxy [GLFWKeyCallback] []
                         (invoke [window key scancode action mods]
                           (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                                      (= action GLFW/GLFW_RELEASE))
                             (GLFW/glfwSetWindowShouldClose window GL11/GL_TRUE))))
          _ (GLFW/glfwSetKeyCallback window key-callback)
          vidmode (GLFW/glfwGetVideoMode (GLFW/glfwGetPrimaryMonitor))]
      (GLFW/glfwSetWindowPos window
                             (/ (- (GLFWvidmode/width vidmode) width) 2)
                             (/ (- (GLFWvidmode/height vidmode) height) 2))
      (GLFW/glfwMakeContextCurrent window)
      (GLFW/glfwSwapInterval 1)
      (GLFW/glfwShowWindow window)
      (map->State {:error-callback error-callback
                   :key-callback key-callback
                   :window window}))))

(defn run-loop
  [^long window]
  (GLContext/createFromCurrent)
  (let [update-interval 2.5]
    (loop [last-time (GLFW/glfwGetTime)
           num-frames 0]
      (when (identical? (GLFW/glfwWindowShouldClose window) GL11/GL_FALSE)
        (let [wbuf (BufferUtils/createIntBuffer 1)
              hbuf (BufferUtils/createIntBuffer 1)
              _ (GLFW/glfwGetFramebufferSize window wbuf hbuf)
              fbw (.get wbuf 0)
              fbh (.get hbuf 0)
              ratio (/ fbw (float fbh))]
          (GL11/glViewport 0 0 fbw fbh)
          (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT
                                GL11/GL_DEPTH_BUFFER_BIT))

          (GL11/glMatrixMode GL11/GL_PROJECTION)
          (GL11/glLoadIdentity)
          (GL11/glOrtho (- ratio) ratio -1.0 1.0 1.0 -1.0)

          (GL11/glMatrixMode GL11/GL_MODELVIEW)
          (GL11/glLoadIdentity)
          (GL11/glRotated (* (GLFW/glfwGetTime) 50) 0 0 1)

          (GL11/glBegin GL11/GL_TRIANGLES)

          (GL11/glColor3f 1.0 0.0 0.0)
          (GL11/glVertex3f -0.6 -0.4 0.0)
          (GL11/glColor3f 0.0 1.0 0.0)
          (GL11/glVertex3f 0.6 -0.4 0.0)
          (GL11/glColor3f 0.0 0.0 1.0)
          (GL11/glVertex3f 0.0 0.6 0.0)

          (GL11/glEnd))

        (GLFW/glfwSwapBuffers window)
        (GLFW/glfwPollEvents)

        (let [tdelta (- (GLFW/glfwGetTime) last-time)]
          (if (>= tdelta update-interval)
            (let [ms-per-frame (/ (* tdelta 1000.0) num-frames)
                  fps (/ num-frames update-interval)]
              (GLFW/glfwSetWindowTitle window (str "ms/f: " ms-per-frame " FPS: " fps))
              (recur (+ last-time update-interval)
                     0))
            (recur last-time (inc num-frames))))))))

(defn run
  []
  (println (str "Hello LWJGL " (Sys/getVersion) "!"))
  (let [state (init)]
    (try
      (run-loop (.window state))

      (GLFW/glfwDestroyWindow (:window state))
      (.release (:key-callback state))
      (finally
        (GLFW/glfwTerminate)
        (.release (:error-callback state))))))


(defn -main [& args]
  (run))
