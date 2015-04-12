package opengl

import org.lwjgl.Sys
import org.lwjgl.glfw._
import org.lwjgl.opengl._

import java.nio.ByteBuffer

import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil._

object Main {

  private var errorCallback: GLFWErrorCallback = null
  private var keyCallback: GLFWKeyCallback = null
  private var window: Long = 0L

  def run(): Unit = {
    System.out.println(s"Hello LWJGL ${Sys.getVersion}!")

    try {
      init()
      loop()

      glfwDestroyWindow(window)
      keyCallback.release()
    } finally {
      glfwTerminate()
      errorCallback.release()
    }
  }

  def init(): Unit = {
    errorCallback = errorCallbackPrint(System.err)
    glfwSetErrorCallback(errorCallback)

    if (glfwInit() != GL_TRUE)
      throw new IllegalStateException("Unable to initialize GLFW")

    glfwDefaultWindowHints();
    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);
    glfwWindowHint(GLFW_RESIZABLE, GL_TRUE);

    val width = 300
    val height = 300

    window = glfwCreateWindow(width, height, "Hello from Scala!", NULL, NULL)
    if (window == NULL)
      throw new RuntimeException("Failed to create the GLFW window!")

    keyCallback = new GLFWKeyCallback() {
      override def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int): Unit = {
        if (key == GLFW_KEY_ESCAPE && action == GLFW_RELEASE)
          glfwSetWindowShouldClose(window, GL_TRUE)
      }
    }
    glfwSetKeyCallback(window, keyCallback)

    val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor())
    glfwSetWindowPos(window,
      (GLFWvidmode.width(vidmode) - width) / 2,
      (GLFWvidmode.height(vidmode) - height) / 2)

    glfwMakeContextCurrent(window)
    glfwSwapInterval(1)
    glfwShowWindow(window)
  }

  def loop(): Unit = {
    GLContext.createFromCurrent()
    glClearColor(1.0f, 1.0f, 0.0f, 0.0f)

    while (glfwWindowShouldClose(window) != GL_TRUE) {
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glfwSwapBuffers(window)
      glfwPollEvents()
    }
  }

  def main(args: Array[String]) = run()
}
