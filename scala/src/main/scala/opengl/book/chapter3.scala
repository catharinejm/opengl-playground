package opengl.book

import opengl._
import Utils._

import org.lwjgl.{Sys, BufferUtils}
import org.lwjgl.glfw._
import org.lwjgl.opengl._

import java.nio.ByteBuffer

import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._
import org.lwjgl.system.MemoryUtil._

object Chapter3 extends BaseWindow {
  val vertexShader = """
#version 400

layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
out vec4 ex_Color;

void main(void)
{
  gl_Position = in_Position;
  ex_Color = in_Color;
}
"""
  var vertexShaderId = 0

  val fragmentShader = """
#version 400

in vec4 ex_Color;
out vec4 out_Color;

void main(void)
{
  out_Color = ex_Color;
}
"""
  var fragmentShaderId = 0
  var programId = 0
  var vaoId = 0
  var vboId = 0

  var bufferId = 0
  val indexBufferIds = BufferUtils.createIntBuffer(2)
  var activeIndexBuffer = 0

  val width = 600
  val height = 600

  override val keyCallback = new GLFWKeyCallback {
      override def invoke(window: GLFWWindow, key: Int, scancode: Int, action: Int, mods: Int): Unit = {
        if (action == GLFW_RELEASE) key match {
          case GLFW_KEY_ESCAPE => glfwSetWindowShouldClose(window, GL_TRUE)
          case GLFW_KEY_T =>
            activeIndexBuffer = if (activeIndexBuffer == 1) 0 else 1
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBufferIds.get(activeIndexBuffer))
          case _ =>
        }
      }
  }

  override def setup(): Unit = {
    createShaders()
    createVBO()
  }

  override def cleanup(): Unit = {
    destroyShaders()
    destroyVBO()
  }

  override def windowHints(): Unit = {
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
  }

  def createShaders(): Unit = {
    glGetError()

    vertexShaderId = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(vertexShaderId, vertexShader)
    glCompileShader(vertexShaderId)

    fragmentShaderId = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(fragmentShaderId, fragmentShader)
    glCompileShader(fragmentShaderId)

    programId = glCreateProgram();
    glAttachShader(programId, vertexShaderId)
    glAttachShader(programId, fragmentShaderId)
    glLinkProgram(programId)
    glUseProgram(programId)

    val errorCheckValue = glGetError()
    if (errorCheckValue != GL_NO_ERROR)
      throw new GLException("could not create shaders", errorCheckValue)
  }


  def createVBO(): Unit = {
    val verticesBuffer = vertexBuffer(
	    Vertex(Vec4(0.0f, 0.0f, 0.0f, 1.0f), ColorF(1.0f, 1.0f, 1.0f, 1.0f)),
	    // Top
	    Vertex(Vec4(-0.2f, 0.8f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 0.0f, 1.0f)),
	    Vertex(Vec4(0.2f, 0.8f, 0.0f, 1.0f), ColorF(0.0f, 0.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(0.0f, 0.8f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(0.0f, 1.0f, 0.0f, 1.0f), ColorF(1.0f, 0.0f, 0.0f, 1.0f)),
	    // Bottom
	    Vertex(Vec4(-0.2f, -0.8f, 0.0f, 1.0f), ColorF(0.0f, 0.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(0.2f, -0.8f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 0.0f, 1.0f)),
	    Vertex(Vec4(0.0f, -0.8f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(0.0f, -1.0f, 0.0f, 1.0f), ColorF(1.0f, 0.0f, 0.0f, 1.0f)),
	    // Left
	    Vertex(Vec4(-0.8f, -0.2f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 0.0f, 1.0f)),
	    Vertex(Vec4(-0.8f, 0.2f, 0.0f, 1.0f), ColorF(0.0f, 0.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(-0.8f, 0.0f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(-1.0f, 0.0f, 0.0f, 1.0f), ColorF(1.0f, 0.0f, 0.0f, 1.0f)),
	    // Right
	    Vertex(Vec4(0.8f, -0.2f, 0.0f, 1.0f), ColorF(0.0f, 0.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(0.8f, 0.2f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 0.0f, 1.0f)),
	    Vertex(Vec4(0.8f, 0.0f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 1.0f, 1.0f)),
	    Vertex(Vec4(1.0f, 0.0f, 0.0f, 1.0f), ColorF(1.0f, 0.0f, 0.0f, 1.0f))
    )

	  val indicesBuffer = byteBuffer(
      // Top
      0, 1, 3,
	    0, 3, 2,
	    3, 1, 4,
	    3, 4, 2,
	    // Bottom
	    0, 5, 7,
	    0, 7, 6,
	    7, 5, 8,
	    7, 8, 6,
	    // Left
	    0, 9, 11,
	    0, 11, 10,
	    11, 9, 12,
	    11, 12, 10,
	    // Right
	    0, 13, 15,
	    0, 15, 14,
	    15, 13, 16,
	    15, 16, 14
    )

    val altIndicesBuffer = byteBuffer(
      // Outer square border:
      3, 4, 16,
      3, 15, 16,
      15, 16, 8,
      15, 7, 8,
      7, 8, 12,
      7, 11, 12,
      11, 12, 4,
      11, 3, 4,
      
      // Inner square
      0, 11, 3,
      0, 3, 15,
      0, 15, 7,
      0, 7, 11
    )

    glGetError()
    vaoId = glGenVertexArrays()
    glBindVertexArray(vaoId)

    vboId = glGenBuffers()
    glBindBuffer(GL_ARRAY_BUFFER, vboId)
    glBufferData(GL_ARRAY_BUFFER, verticesBuffer, GL_STATIC_DRAW)

    glVertexAttribPointer(0, Vec4.size, GL_FLOAT, false, Vertex.byteSize, 0)
    glVertexAttribPointer(1, ColorF.size, GL_FLOAT, false, Vertex.byteSize, Vertex.colorOffset)
    glEnableVertexAttribArray(0)
    glEnableVertexAttribArray(1)    

    glGenBuffers(indexBufferIds)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBufferIds.get(0))
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indicesBuffer, GL_STATIC_DRAW)

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBufferIds.get(1))
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, altIndicesBuffer, GL_STATIC_DRAW)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBufferIds.get(0))

    val errorCheckValue = glGetError()
    if (errorCheckValue != GL_NO_ERROR)
      throw new GLException("Could not create a VBO", errorCheckValue)
  }

  def destroyShaders(): Unit = {
    glGetError()

    glUseProgram(0)

    glDetachShader(programId, vertexShaderId)
    glDetachShader(programId, fragmentShaderId)

    glDeleteShader(fragmentShaderId)
    glDeleteShader(vertexShaderId)

    glDeleteProgram(programId)

    val errorCheckValue = glGetError()
    if (errorCheckValue != GL_NO_ERROR)
      throw new GLException("Could not destroy the shaders", errorCheckValue)
  }

  def destroyVBO(): Unit = {
    glGetError()

    glDisableVertexAttribArray(1)
    glDisableVertexAttribArray(0)
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDeleteBuffers(vboId)

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDeleteBuffers(indexBufferIds)

    glBindVertexArray(0)
    glDeleteVertexArrays(vaoId)

    val errorCheckValue = glGetError()
    if (errorCheckValue != GL_NO_ERROR)
      throw new GLException("Could not destroy the VBO", errorCheckValue)
  }

  def loopBody(fbWidth: Int, fbHeight: Int): Unit = {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    if (activeIndexBuffer == 0)
      glDrawElements(GL_TRIANGLES, 48, GL_UNSIGNED_BYTE, NULL)
    else
      glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_BYTE, NULL)
  }
}
