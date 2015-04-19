{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import System.FilePath ((</>))
import qualified Data.Vector.Storable as V

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
  vs <- U.loadShader GL.VertexShader $ shaderPath </> "triangle.v.glsl"
  fs <- U.loadShader GL.FragmentShader $ shaderPath </> "triangle.f.glsl"
  p <- U.linkShaderProgram [vs, fs]
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  Program p <$> GL.get (GL.attribLocation p "coord2d")

draw :: Program -> GLFW.Window -> IO ()
draw (Program program attrib) win = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  (width, height) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  GL.currentProgram $= Just program
  GL.vertexAttribArray attrib $= GL.Enabled
  V.unsafeWith vertices $ \ptr ->
    GL.vertexAttribPointer attrib $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
  GL.drawArrays GL.Triangles 0 3
  GL.vertexAttribArray attrib $= GL.Disabled


data Program = Program GL.Program GL.AttribLocation

shaderPath :: FilePath
shaderPath = "."

vertices :: V.Vector Float
vertices = V.fromList [ 0.0, 0.8
                      ,-0.8,-0.8
                      , 0.8,-0.8
                      ]
