package opengl

import org.lwjgl.{Sys, BufferUtils}
import org.lwjgl.glfw._
import org.lwjgl.opengl._

import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL11._
import org.lwjgl.system.MemoryUtil._

object GLFWDemo extends BaseWindow { 
  val width = 640
  val height = 480

  def loopBody(fbWidth: Int, fbHeight: Int): Unit = {
    val ratio: Float = fbWidth / fbHeight.asInstanceOf[Float]

    glViewport(0, 0, fbWidth, fbHeight)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-ratio, ratio, -1.0f, 1.0f, 1.0f, -1.0f)

    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glRotated(glfwGetTime() * 50, 0, 0, 1)

    glBegin(GL_TRIANGLES)

    glColor3f(1.0f, 0.0f, 0.0f)
    glVertex3f(-0.6f, -0.4f, 0.0f)
    glColor3f(0.0f, 1.0f, 0.0f)
    glVertex3f(0.6f, -0.4f, -0.0f)
    glColor3f(0.0f, 0.0f, 1.0f)
    glVertex3f(0.0f, 0.6f, 0.0f)

    glEnd()
  }
}
