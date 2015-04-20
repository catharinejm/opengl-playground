{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import System.FilePath ((</>))

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as U

import qualified Util as W

main :: IO ()
main = do
  win <- W.initialize "My first triangle"
  prog <- initResources
  W.mainLoop (draw prog win) win
  W.cleanup win

initResources :: IO Program
initResources = do
  -- compile vertex shader
  vs <- U.loadShader GL.VertexShader $ shaderPath </> "triangle.v.glsl"
  fs <- U.loadShader GL.FragmentShader $ shaderPath </> "triangle.f.glsl"
  p <- U.linkShaderProgram [vs, fs]
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  (Program p) <$> GL.get (GL.attribLocation p "coord2d")
    <*> U.makeBuffer GL.ArrayBuffer vertices

draw :: Program -> GLFW.Window -> IO ()
draw (Program program attrib buf) win = do
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]
  (width, height) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  GL.currentProgram $= Just program
  GL.vertexAttribArray attrib $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer $= Just buf
  GL.vertexAttribPointer attrib $=
    (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)
  GL.drawArrays GL.Triangles 0 3 -- 3 vertices
  GL.vertexAttribArray attrib $= GL.Disabled

data Program = Program GL.Program MatrixLocs Buffers
data MatrixLocs = MatrixLocs { projection :: GL.AttribLocation
                             , view :: GL.AttribLocation
                             , model :: GL.AttribLocation
                             }
data Buffers = Buffers { vao :: GL.BufferObject
                       , vbo :: GL.BufferObject
                       , ibo :: GL.BufferObject
                       }

shaderPath :: FilePath
shaderPath = "."

vertices :: [Float]
vertices = [ 0.0, 0.8
           ,-0.8,-0.8
           , 0.8,-0.8
           ]
