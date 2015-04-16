package opengl

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._

object Errors {
  def glErrorString(glError: Int): String =
    glError match {
      case GL_NO_ERROR => "No error"
      case GL_INVALID_ENUM => "Invalid Enum"
      case GL_INVALID_VALUE => "Invalid value"
      case GL_INVALID_OPERATION => "Invalid operation"
      case GL_INVALID_FRAMEBUFFER_OPERATION => "Invalid framebuffer operation"
      case GL_OUT_OF_MEMORY => "Out of memory"
      case GL_STACK_UNDERFLOW => "Stack underflow"
      case GL_STACK_OVERFLOW => "Stack overflow"
      case _ => s"Unknown error ($glError)"
    }

  def throwIfGlError(msg: String) = {
    val errorVal = glGetError
    if (errorVal != GL_NO_ERROR)
      throw new GLException(msg, errorVal)
  }
}


class GLException(msg: String = null) extends RuntimeException(msg) {
  def this(msg: String, glError: Int) = this(s"$msg: ${Errors.glErrorString(glError)}")
}
