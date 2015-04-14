package opengl.book

import opengl._

import org.lwjgl.{Sys, BufferUtils}
import org.lwjgl.glfw._
import org.lwjgl.opengl._

import java.nio.{ByteBuffer, IntBuffer}

import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import org.lwjgl.system.MemoryUtil._


trait Chapter2 {
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

  val fragmentShader = """
#version 400

in vec4 ex_Color;
in vec4 out_Color;

void main(void)
{
  out_Color = ex_Color;
}
"""
}

object Chapter2_1 extends BaseWindow with Chapter2 {
  val width = 800
  val height = 600
  def loopBody(fbWidth: Int, fbHeight: Int): Unit = {}
}
