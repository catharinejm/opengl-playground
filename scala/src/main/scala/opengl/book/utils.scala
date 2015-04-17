package opengl.book

import java.nio.{ByteBuffer, IntBuffer, FloatBuffer}

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL20._

import opengl._
import opengl.Errors._

import math._

object Matrix16 {
  def Identity = {
    val b = BufferUtils.createFloatBuffer(16)
    b.put(0, 1f)
    b.put(5, 1f)
    b.put(10, 1f)
    b.put(15, 1f)
    b
  }
  def Zeros = {
    val b = BufferUtils.createFloatBuffer(16)
    BufferUtils.zeroBuffer(b)
    b.rewind()
    b
  }

  implicit def dToF(d: Double): Float = d.asInstanceOf[Float]

  def cotan(n: Float) = 1.0f / math.tan(n).asInstanceOf[Float]

  def multiply(m1: FloatBuffer, m2: FloatBuffer) = {
    val out = Identity
    for {
      row <- 0 to 3
      val rowOff = row*4
      col <- 0 to 3
    } {
      out.put(rowOff + col,
        m1.get(rowOff + 0) * m2.get(col + 0) +
          m1.get(rowOff + 1) * m2.get(col + 4) +
          m1.get(rowOff + 2) * m2.get(col + 8) +
          m1.get(rowOff + 3) * m2.get(col + 12)
      )
    }
    out
  }

  def scale(m: FloatBuffer, x: Float, y: Float, z: Float): Unit = {
    val scaleM = Identity
    scaleM.put(0, x)
    scaleM.put(5, y)
    scaleM.put(10, z)
    m.put(multiply(m, scaleM))
    m.rewind()
  }

  def translate(m: FloatBuffer, x: Float, y: Float, z: Float): Unit = {
    val trans = Identity
    trans.put(12, x)
    trans.put(13, y)
    trans.put(14, z)
    m.put(multiply(m, trans))
    m.rewind()
  }

  def rotateAboutX(m: FloatBuffer, angle: Float): Unit = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation.put(5, cosine)
    rotation.put(6, -sine)
    rotation.put(9, sine)
    rotation.put(10, cosine)

    m.put(multiply(m, rotation))
    m.rewind()
  }

  def rotateAboutY(m: FloatBuffer, angle: Float): Unit = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation.put(0, cosine)
    rotation.put(8, sine)
    rotation.put(2, -sine)
    rotation.put(10, cosine)

    m.put(multiply(m, rotation))
    m.rewind()
  }

  def rotateAboutZ(m: FloatBuffer, angle: Float): Unit = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation.put(0, cosine)
    rotation.put(1, -sine)
    rotation.put(4, sine)
    rotation.put(5, cosine)

    m.put(multiply(m, rotation))
    m.rewind()
  }

  def createProjectionMatrix(
    fovy: Float, aspect: Float, nearPlane: Float, farPlane: Float
  ): FloatBuffer = {
    val out = Zeros
    val yScale = cotan(toRadians(fovy/2))
    val xScale = yScale / aspect
    val frustumLen = farPlane - nearPlane

    out.put(0, xScale)
    out.put(5, yScale)
    out.put(10, -((farPlane + nearPlane) / frustumLen))
    out.put(11, -1)
    out.put(14, -((2 * nearPlane * farPlane) / frustumLen))

    out
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
