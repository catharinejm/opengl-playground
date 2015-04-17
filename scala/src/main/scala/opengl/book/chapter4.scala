package opengl.book

import math._

import opengl._
import Utils._
import Errors._

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

object Chapter4 extends BaseWindow {
  var width = 800
  var height = 600

  var projectionMatrixUniformLocation = 0
  var viewMatrixUniformLocation = 0
  var modelMatrixUniformLocation = 0
  var bufferIds = Array(0,0,0)

  var programId = 0
  var fragmentShaderId = 0
  var vertexShaderId = 0

  var projectionMatrix = Matrix16.createProjectionMatrix(
    60,
    width.asInstanceOf[Float] / height,
    1.0f,
    100.0f
  )
  var viewMatrix = Matrix16.Identity
  var modelMatrix = Matrix16.Identity

  var cubeRotation = 0.0
  var lastTime = -1.0

  override def windowHints(): Unit = {
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
  }

  def printMatrix(name: String, m: java.nio.FloatBuffer): Unit = {
    println(name)
    for {
      i <- 0 to 3
      val off = i*4
      j <- 0 to 3
    } {
      print(s"${m.get(off+j)} ")
      if (j == 3)
        println()
    }
    println()
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

    Matrix16.translate(viewMatrix, 0, 0, -2)
    printMatrix("ViewMatrix", viewMatrix)

    createCube()
  }

  override def cleanup(): Unit = {
    destroyCube()
  }

  override def setCallbacks(window: GLFWWindow): Unit = {
    glfwSetWindowSizeCallback(window, new GLFWWindowSizeCallback {
      override def invoke(window: GLFWWindow, w: Int, h: Int): Unit = {
        width = w
        height = h
        glViewport(0, 0, width, height)
        projectionMatrix.put(Matrix16.createProjectionMatrix(
          60,
          width.asInstanceOf[Float] / height,
          1.0f,
          100.0f
        ))
        projectionMatrix.rewind()
        glUseProgram(programId)
        glUniformMatrix4fv(projectionMatrixUniformLocation, false, projectionMatrix)
        glUseProgram(0)
      }
    })
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

    programId = glCreateProgram()
    throwIfGlError("Could not create the shader program")
    val shaderPath = "./src/main/scala/opengl/book"
    fragmentShaderId = loadShader(s"$shaderPath/SimpleShader.fragment.glsl", GL_FRAGMENT_SHADER)
    vertexShaderId = loadShader(s"$shaderPath/SimpleShader.vertex.glsl", GL_VERTEX_SHADER)
    glAttachShader(programId, fragmentShaderId)
    glAttachShader(programId, vertexShaderId)

    glLinkProgram(programId)
    throwIfGlError("Could not link the shader program")

    modelMatrixUniformLocation = glGetUniformLocation(programId, "ModelMatrix")
    viewMatrixUniformLocation = glGetUniformLocation(programId, "ViewMatrix")
    projectionMatrixUniformLocation = glGetUniformLocation(programId, "ProjectionMatrix")
    throwIfGlError("Could not get shader uniform locations")

    bufferIds(0) = glGenVertexArrays()
    throwIfGlError("Could not generate VAO")
    glBindVertexArray(bufferIds(0))
    throwIfGlError("Could not bind VAO")

    glEnableVertexAttribArray(0)
    glEnableVertexAttribArray(1)
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
    glDetachShader(programId, fragmentShaderId)
    glDetachShader(programId, vertexShaderId)
    glDeleteShader(fragmentShaderId)
    glDeleteShader(vertexShaderId)
    glDeleteProgram(programId)
    throwIfGlError("Could not destroy the shaders")

    glDeleteBuffers(bufferIds(1))
    glDeleteBuffers(bufferIds(2))
    glDeleteVertexArrays(bufferIds(0))
    throwIfGlError("Could not destroy the buffer objects")
  }

  def drawCube(): Unit = {
    val now = glfwGetTime()
    if (lastTime < 0)
      lastTime = now

    cubeRotation += 45.0 * (now - lastTime)
    val cubeAngle = toRadians(cubeRotation).asInstanceOf[Float]
    lastTime = now

    modelMatrix = Matrix16.Identity
    Matrix16.rotateAboutY(modelMatrix, cubeAngle)
    Matrix16.rotateAboutX(modelMatrix, cubeAngle)

    glUseProgram(programId)
    glUniformMatrix4fv(modelMatrixUniformLocation, false, modelMatrix)
    throwIfGlError("Could not set the model matrix uniform")
    glUniformMatrix4fv(viewMatrixUniformLocation, false, viewMatrix)
    throwIfGlError("Could not set the view matrix uniform")
    glUniformMatrix4fv(projectionMatrixUniformLocation, false, projectionMatrix)
    throwIfGlError("Could not set the projection matrix uniform")

    glBindVertexArray(bufferIds(0))
    throwIfGlError("Could not bind the VAO for drawing")

    glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0)
    throwIfGlError("Could not draw the cube")

    glBindVertexArray(0)
    glUseProgram(0)
  }

  override def periodicPrint(): Unit = {
    printMatrix("ViewMatrix", viewMatrix)
    printMatrix("ModelMatrix", modelMatrix)
    printMatrix("ProjectionMatrix", projectionMatrix)
  }

  def loopBody(fbWidth: Int, fbHeight: Int): Unit = {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    drawCube()
  }
}
