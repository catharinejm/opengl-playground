package opengl

import java.nio.ByteBuffer

object Size {
  val ofByte = 1
  val ofShort = 2
  val ofInt = 4
  val ofLong = 8
  val ofFloat = 4
  val ofDouble = 8
}

case class Vec4(x: Float, y: Float, z: Float, w: Float)
object Vec4 {
  val size = 4
  val byteSize = size*Size.ofFloat
}
case class ColorF(r: Float, g: Float, b: Float, a: Float)
object ColorF {
  val size = 4
  val byteSize = size*Size.ofFloat
}

case class Vertex(vec: Vec4, color: ColorF) {
  def fillBuffer(buf: ByteBuffer): Unit = {
    toSeq foreach (buf putFloat _)
  }

  def toSeq: Seq[Float] = this match {
    case Vertex(Vec4(x,y,z,w),ColorF(r,g,b,a)) => Seq(x,y,z,w,r,g,b,a)
  }
}
object Vertex {
  val size = Vec4.size + ColorF.size
  val byteSize = Vec4.byteSize + ColorF.byteSize
  val colorOffset = Vec4.byteSize
}
