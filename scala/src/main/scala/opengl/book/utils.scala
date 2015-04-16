package opengl.book

import java.nio.{ByteBuffer, IntBuffer, FloatBuffer}

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL20._

import breeze.linalg._
import breeze.numerics._

import opengl._
import opengl.Errors._

object Matrix16 {
  def Identity = DenseMatrix.eye[Float](4)
  def Zeros = DenseMatrix.zeros[Float](4,4)

  def cotan(n: Float) = 1.0f / tan(n)

  def scale(m: DenseMatrix[Float], x: Float, y: Float, z: Float): DenseMatrix[Float] = {
    val scaleM = Identity
    scaleM(0,0) = x
    scaleM(1,1) = y
    scaleM(2,2) = z
    m :* scaleM
  }

  def translate(m: DenseMatrix[Float], x: Float, y: Float, z: Float): DenseMatrix[Float] = {
    val trans = Identity
    trans(3,0) = x
    trans(3,1) = y
    trans(3,2) = z
    m :* trans
  }

  def rotateAboutX(m: DenseMatrix[Float], angle: Float): DenseMatrix[Float] = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation(1,1) = cosine
    rotation(1,2) = -sine
    rotation(2,1) = sine
    rotation(2,2) = cosine

    m :* rotation
  }

  def rotateAboutY(m: DenseMatrix[Float], angle: Float): DenseMatrix[Float] = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation(0,0) = cosine
    rotation(2,0) = sine
    rotation(0,2) = -sine
    rotation(2,2) = cosine

    m :* rotation
  }

  def rotateAboutZ(m: DenseMatrix[Float], angle: Float): DenseMatrix[Float] = {
    val rotation = Identity
    val sine: Float = sin(angle)
    val cosine: Float = cos(angle)

    rotation(0,0) = cosine
    rotation(0,1) = -sine
    rotation(1,0) = sine
    rotation(1,1) = cosine

    m :* rotation
  }

  def createProjectionMatrix(
    fovy: Float, aspect: Float, nearPlane: Float, farPlane: Float
  ): DenseMatrix[Float] = {
    val out = DenseMatrix.zeros[Float](4,4)
    val yScale = cotan(toRadians(fovy/2))
    val xScale = yScale / aspect
    val frustumLen = farPlane - nearPlane

    out(0,0) = xScale
    out(1,1) = yScale
    out(2,2) = -((farPlane + nearPlane) / frustumLen)
    out(2,3) = -1
    out(3,2) = -((2 * nearPlane * farPlane) / frustumLen)

    out
  }

  def fillBuffer(m: DenseMatrix[Float], buf: FloatBuffer): Unit = {
    buf.mark()
    m.values foreach (buf put _)
    buf.reset()
  }

  def initBuffer(m: DenseMatrix[Float]): FloatBuffer = {
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
