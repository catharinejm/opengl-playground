package opengl.book

import opengl._

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

  val width = 600
  val height = 600

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

  def vertexBuffer(vertices: Vertex*): ByteBuffer = {
    val buffer = BufferUtils.createByteBuffer(vertices.length * Vertex.byteSize)
    vertices foreach (_ fillBuffer buffer)
    buffer.rewind()
    buffer
  }

  def createVBO(): Unit = {
    val verticesBuffer = vertexBuffer(
      Vertex(Vec4(-0.8f, -0.8f, 0.0f, 1.0f), ColorF(1.0f, 0.0f, 0.0f, 1.0f)),
      Vertex(Vec4(0.0f,  0.8f, 0.0f, 1.0f), ColorF(0.0f, 1.0f, 0.0f, 1.0f)),
      Vertex(Vec4(0.8f, -0.8f, 0.0f, 1.0f), ColorF(0.0f, 0.0f, 1.0f, 1.0f))
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

    glBindVertexArray(0)
    glDeleteVertexArrays(vaoId)

    val errorCheckValue = glGetError()
    if (errorCheckValue != GL_NO_ERROR)
      throw new GLException("Could not destroy the VBO", errorCheckValue)
  }

  def loopBody(fbWidth: Int, fbHeight: Int): Unit = {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glDrawArrays(GL_TRIANGLES, 0, 4)
  }
}
