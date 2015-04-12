( sbtns opengl.core
  (:import [org.lwjgl Sys]
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
    (let [width 300
          height 300
          window (GLFW/glfwCreateWindow width height "Hello from Clojure!"
                                        MemoryUtil/NULL MemoryUtil/NULL)
          _ (when (nil? window)
              (throw (RuntimeException. "Failed to create the GLFW window")))
          key-callback (proxy [GLFWKeyCallback] []
                         (invoke [window key scancode action mods]
                           (when (and (identical? key GLFW/GLFW_KEY_ESCAPE)
                                      (identical? action GLFW/GLFW_RELEASE))
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
  (GL11/glClearColor 1.0 1.0 0.0 0.0)
  (while (identical? (GLFW/glfwWindowShouldClose window) GL11/GL_FALSE)
    (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT
                          GL11/GL_DEPTH_BUFFER_BIT))
    (GLFW/glfwSwapBuffers window)
    (GLFW/glfwPollEvents)))

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

