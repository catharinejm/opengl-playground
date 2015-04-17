package opengl.book

import java.nio.{ByteBuffer, IntBuffer, FloatBuffer}

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL20._

import opengl._
import opengl.Errors._

import math._

object Matrix16 {
  def Identity = Array[Float](
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  )
  def Zeros = Array[Float](
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
  )

  implicit def dToF(d: Double): Float = d.asInstanceOf[Float]

  def cotan(n: Float) = 1.0f / math.tan(n).asInstanceOf[Float]

  def multiply(m1: Array[Float], m2: Array[Float]) = {
    val out = Identity
    for {
      row <- 0 to 3
      val rowOff = row*4
      col <- 0 to 3
    } {
      out(rowOff + col) =
        m1(rowOff + 0) * m2(col + 0) +
          m1(rowOff + 1) * m2(col + 4) +
          m1(rowOff + 2) * m2(col + 8) +
          m1(rowOff + 3) * m2(col + 12)
    }
    out
  }

  def scale(m: Array[Float], x: Float, y: Float, z: Float): Array[Float] = {
    val scaleM = Identity
    scaleM(0) = x
    scaleM(5) = y
    scaleM(10) = z
    multiply(m, scaleM)
  }

  def translate(m: Array[Float], x: Float, y: Float, z: Float): Array[Float] = {
    val trans = Identity
    trans(12) = x
    trans(13) = y
    trans(14) = z
    multiply(m, trans)
  }

  def rotateAboutX(m: Array[Float], angle: Float): Array[Float] = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation(5) = cosine
    rotation(6) = -sine
    rotation(9) = sine
    rotation(10) = cosine

    multiply(m, rotation)
  }

  def rotateAboutY(m: Array[Float], angle: Float): Array[Float] = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation(0) = cosine
    rotation(8) = sine
    rotation(2) = -sine
    rotation(10) = cosine

    multiply(m, rotation)
  }

  def rotateAboutZ(m: Array[Float], angle: Float): Array[Float] = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation(0) = cosine
    rotation(1) = -sine
    rotation(4) = sine
    rotation(5) = cosine

    multiply(m, rotation)
  }

  def createProjectionMatrix(
    fovy: Float, aspect: Float, nearPlane: Float, farPlane: Float
  ): Array[Float] = {
    val out = Zeros
    val yScale = cotan(toRadians(fovy/2))
    val xScale = yScale / aspect
    val frustumLen = farPlane - nearPlane

    out(0) = xScale
    out(5) = yScale
    out(10) = -((farPlane + nearPlane) / frustumLen)
    out(11) = -1
    out(14) = -((2 * nearPlane * farPlane) / frustumLen)

    out
  }

  def fillBuffer(m: Array[Float], buf: FloatBuffer): Unit = {
    buf.mark()
    buf.put(m)
    buf.reset()
  }

  def initBuffer(m: Array[Float]): FloatBuffer = {
    val b = BufferUtils.createFloatBuffer(16)
    fillBuffer(m, b)
    b
  }
}

object Utils {
  def loadShader(filename: String, shaderType: Int): Int = {
    val source = io.Source.fromFile(filename).mkString
    val shaderId = glCreateShader(shaderType)
    if (shaderId != 0) {
      glShaderSource(shaderId, source)
      glCompileShader(shaderId)
      throwIfGlError("Could not compile shader file $filename")
    } else
      throw new RuntimeException("Failed to create shader")
    shaderId
  }

  def vertexBuffer(vertices: Vertex*): ByteBuffer = {
    val buffer = BufferUtils.createByteBuffer(vertices.length * Vertex.byteSize)
    vertices foreach (_ fillBuffer buffer)
    buffer.rewind()
    buffer
  }

  def byteBuffer(bytes: Byte*): ByteBuffer = {
    val buffer = BufferUtils.createByteBuffer(bytes.length)
    bytes foreach (buffer put _)
    buffer.rewind()
    buffer
  }

  def intBuffer(ints: Int*): IntBuffer = {
    val buffer = BufferUtils.createIntBuffer(ints.length)
    ints foreach (buffer put _)
    buffer.rewind()
    buffer
  }
}
