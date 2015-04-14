package opengl

import org.lwjgl.{Sys, BufferUtils}
import org.lwjgl.glfw._
import org.lwjgl.opengl._

import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil._

trait BaseWindow {
  val errorCallback = errorCallbackPrint(System.err)
  val keyCallback = new GLFWKeyCallback {
      override def invoke(window: GLFWWindow, key: Int, scancode: Int, action: Int, mods: Int): Unit = {
        if (key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE)
          glfwSetWindowShouldClose(window, GL_TRUE)
      }
  }

  def width: Int
  def height: Int

  def run(): Unit = {
    System.out.println(s"Hello LWJGL ${Sys.getVersion}!")

    try {
      val window = init()
      loop(window)

      glfwDestroyWindow(window)
      keyCallback.release()
    } finally {
      glfwTerminate()
      errorCallback.release()
    }
  }

  def init(): GLFWWindow = {
    glfwSetErrorCallback(errorCallback)

    if (glfwInit() != GL_TRUE)
      throw new IllegalStateException("Unable to initialize GLFW")

    glfwDefaultWindowHints();
    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
    glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);

    val window: GLFWWindow = glfwCreateWindow(width, height, "Hello from Scala!", NULL, NULL)
    if (window == NULL)
      throw new RuntimeException("Failed to create the GLFW window!")

    glfwSetKeyCallback(window, keyCallback)

    val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor())
    glfwSetWindowPos(window,
      (GLFWvidmode.width(vidmode) - width) / 2,
      (GLFWvidmode.height(vidmode) - height) / 2)

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)
    glfwShowWindow(window)

    window
  }

  def getFBDims(window: GLFWWindow): (Int, Int) = {
      val fbWidBuf = BufferUtils.createIntBuffer(1)
      val fbHtBuf = BufferUtils.createIntBuffer(1)
      glfwGetFramebufferSize(window, fbWidBuf, fbHtBuf)
      (fbWidBuf.get(0), fbHtBuf.get(0))
  }

  def loop(window: GLFWWindow): Unit = {
    GLContext.createFromCurrent()
    println(s"OpenGL Version: ${glGetString(GL_VERSION)}")
    var lastTime = glfwGetTime()
    var numFrames = 0
    val updateInterval = 2.5

    while (glfwWindowShouldClose(window) == GL_FALSE) {
      numFrames += 1
      val currentTime = glfwGetTime()
      if (currentTime - lastTime >= updateInterval) {
        val msPerFrame = (currentTime - lastTime) * 1000.0 / numFrames
        glfwSetWindowTitle(window, s"ms/f: $msPerFrame, FPS: ${1/msPerFrame*1000.0}")
        lastTime += updateInterval
        numFrames = 0
      }

      val (fbWid, fbHt) = getFBDims(window)

      loopBody(fbWid, fbHt)

      glfwSwapBuffers(window)
      glfwPollEvents()
    }
  }

  def loopBody(fbWidth: Int, fbHeight: Int): Unit
}
