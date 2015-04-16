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

import breeze.linalg._
import breeze.numerics._

object Chapter4 extends BaseWindow {
  val width = 800
  val height = 600

  var projectionMatrixUniformLocation = 0
  var viewMatrixUniformLocation = 0
  var modelMatrixUniformLocation = 0
  var bufferIds = Array(0,0,0)
  var shaderIds = Array(0,0,0)

  var projectionMatrix = Matrix16.Identity
  var viewMatrix = Matrix16.Identity
  var modelMatrix = Matrix16.Identity

  val projectionMatrixBuffer = Matrix16.initBuffer(projectionMatrix)
  val viewMatrixBuffer = Matrix16.initBuffer(viewMatrix)
  val modelMatrixBuffer = Matrix16.initBuffer(modelMatrix)

  var cubeRotation = 0f
  var lastTime = 0.0

  override def windowHints(): Unit = {
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
  }

  override def setup(): Unit = {
    glGetError()
    glClearColor(0,0,0,0)

    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LESS)
    throwIfGlError("Could not set OpenGL depth testing options")

    glEnable(GL_CULL_FACE)
    glCullFace(GL_BACK)
    glFrontFace(GL_CCW)
    throwIfGlError("Could not set OpenGL culling options")

    createCube()
  }

  def createCube(): Unit = {
    val verticesBuffer = vertexBuffer(
      Vertex(Vec4(-.5f, -.5f,  .5f, 1), ColorF(0, 0, 1, 1)),
      Vertex(Vec4(-.5f,  .5f,  .5f, 1), ColorF(1, 0, 0, 1)),
      Vertex(Vec4( .5f,  .5f,  .5f, 1), ColorF(0, 1, 0, 1)),
      Vertex(Vec4( .5f, -.5f,  .5f, 1), ColorF(1, 1, 0, 1)),
      Vertex(Vec4(-.5f, -.5f, -.5f, 1), ColorF(1, 1, 1, 1)),
      Vertex(Vec4(-.5f,  .5f, -.5f, 1), ColorF(1, 0, 0, 1)),
      Vertex(Vec4( .5f,  .5f, -.5f, 1), ColorF(1, 0, 1, 1)),
      Vertex(Vec4( .5f, -.5f, -.5f, 1), ColorF(0, 0, 1, 1))
    )

    val indicesBuffer = intBuffer(
      0,2,1,  0,3,2,
      4,3,0,  4,7,3,
      4,1,5,  4,0,1,
      3,6,2,  3,7,6,
      1,6,5,  1,2,6,
      7,5,6,  7,4,5
    )

    shaderIds(0) = glCreateProgram()
    throwIfGlError("Could not create the shader program")
    shaderIds(1) = loadShader("SimpleShader.fragment.glsl", GL_FRAGMENT_SHADER)
    shaderIds(2) = loadShader("SimpleShader.vertex.glsl", GL_VERTEX_SHADER)
    glAttachShader(shaderIds(0), shaderIds(1))
    glAttachShader(shaderIds(0), shaderIds(1))

    glLinkProgram(shaderIds(0))
    throwIfGlError("Could not link the shader program")

    modelMatrixUniformLocation = glGetUniformLocation(shaderIds(0), "ModelMatrix")
    viewMatrixUniformLocation = glGetUniformLocation(shaderIds(0), "ViewMatrix")
    projectionMatrixUniformLocation = glGetUniformLocation(shaderIds(0), "ProjectionMatrix")
    throwIfGlError("Could not get shader uniform locations")

    bufferIds(0) = glGenVertexArrays()
    throwIfGlError("Could not generate VAO")
    glBindVertexArray(bufferIds(0))
    throwIfGlError("Could not bind VAO")

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    throwIfGlError("Could not enable vertex attributes")

    bufferIds(1) = glGenBuffers()
    bufferIds(2) = glGenBuffers()

    glBindBuffer(GL_ARRAY_BUFFER, bufferIds(1))
    glBufferData(GL_ARRAY_BUFFER, verticesBuffer, GL_STATIC_DRAW)
    throwIfGlError("Could not bind the VBO to the VAO")

    glVertexAttribPointer(0, 4, GL_FLOAT, false, Vertex.byteSize, 0)
    glVertexAttribPointer(1, 4, GL_FLOAT, false, Vertex.byteSize, Vertex.colorOffset)
    throwIfGlError("Could not set VAO attributes")

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, bufferIds(2))
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indicesBuffer, GL_STATIC_DRAW)
    throwIfGlError("Could not bind the IBO to the VAO")

    glBindVertexArray(0)
  }

  def destroyCube(): Unit = {
    glDetachShader(shaderIds(0), shaderIds(1))
    glDetachShader(shaderIds(0), shaderIds(2))
    glDeleteShader(shaderIds(1))
    glDeleteShader(shaderIds(2))
    glDeleteProgram(shaderIds(0))
    throwIfGlError("Could not destroy the shaders")

    glDeleteBuffers(bufferIds(1))
    glDeleteBuffers(bufferIds(2))
    glDeleteVertexArrays(bufferIds(0))
    throwIfGlError("Could not destroy the buffer objects")
  }

  def drawCube(): Unit = {
    var cubeAngle: Float = 0
    var now = glfwGetTime()
    if (lastTime == 0.0)
      lastTime = now

    cubeRotation += 45.0f * (now - lastTime)
    cubeAngle = math.toRadians(cubeRotation)
    lastTime = now

    modelMatrix = Matrix16.rotateAboutX(Matrix16.Identity, cubeAngle)
    modelMatrix = Matrix16.rotateAboutY(modelMatrix, cubeAngle)

    Matrix16.fillBuffer(modelMatrix, modelMatrixBuffer)
    glUniformMatrix4fv(modelMatrixUniformLocation, false, modelMatrixBuffer)
    glUniformMatrix4fv(viewMatrixUniformLocation, false, viewMatrixBuffer)
    throwIfGlError("Could not set the shader uniforms")

    glBindVertexArray(bufferIds(0))
    throwIfGlError("Could not bind the VAO for drawing")

    glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0)
    throwIfGlError("Could not draw the cube")

    glBindVertexArray(0)
    glUseProgram(0)
  }
}
