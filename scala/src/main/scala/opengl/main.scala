package opengl

import opengl.book._

object Main {
  def main(args: Array[String]) =
    args.headOption match {
      case None | Some("glfw") => GLFWDemo.run()
      case Some("ch2") => Chapter2.run()
      case Some("ch3") => Chapter3.run()
      case Some("ch4") => Chapter4.run()
      case Some(x) => println(s"invalid option: $x")
    }
}
