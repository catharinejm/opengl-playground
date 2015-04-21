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
  putStrLn "hi"
  -- win <- W.initialize "My first triangle"
  -- prog <- initResources
  -- W.mainLoop (draw prog win) win
  -- W.cleanup win

initResources :: IO Program
initResources = do
  -- compile vertex shader
  vs <- U.loadShader GL.VertexShader $ shaderPath </> "triangle.v.glsl"
  fs <- U.loadShader GL.FragmentShader $ shaderPath </> "triangle.f.glsl"
  p <- U.linkShaderProgram [vs, fs]
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  vao <- U.makeVAO $ do
    vbo <- U.makeBuffer GL.ArrayBuffer vertices
    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (4*8) U.offset0)
    GL.vertexAttribPointer (GL.AttribLocation 1) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (4*8) $ U.offsetPtr (4*4))
    ibo <- U.makeBuffer GL.ElementArrayBuffer indices
    return ()
  (Program p vao) <$> loadAttribs p
  where
    loadAttribs p = do
      proj <- GL.get (GL.attribLocation p "ProjectionMatrix")
      v <- GL.get (GL.attribLocation p "ViewMatrix")
      mod <- GL.get (GL.attribLocation p "ModelMatrix")
      return MatrixLocs { projection = proj
                        , view = v
                        , model = mod
                        }

-- draw :: Program -> GLFW.Window -> IO ()
-- draw (Program program attrib buf) win = do
--   GL.clearColor $= GL.Color4 0 0 0 1
--   GL.clear [GL.ColorBuffer]
--   (width, height) <- GLFW.getFramebufferSize win
--   GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

--   GL.currentProgram $= Just program
--   GL.vertexAttribArray attrib $= GL.Enabled
--   GL.bindBuffer GL.ArrayBuffer $= Just buf
--   GL.vertexAttribPointer attrib $=
--     (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0)
--   GL.drawArrays GL.Triangles 0 3 -- 3 vertices
--   GL.vertexAttribArray attrib $= GL.Disabled

data Program = Program GL.Program GL.VertexArrayObject MatrixLocs
data MatrixLocs = MatrixLocs { projection :: GL.AttribLocation
                             , view :: GL.AttribLocation
                             , model :: GL.AttribLocation
                             }
data Buffers = Buffers { vbo :: GL.BufferObject
                       , ibo :: GL.BufferObject
                       }

shaderPath :: FilePath
shaderPath = "."

vertices :: [Float]
vertices = [ -0.5, -0.5,  0.5, 1,   0, 0, 1, 1
           , -0.5,  0.5,  0.5, 1,   1, 0, 0, 1
           ,  0.5,  0.5,  0.5, 1,   0, 1, 0, 1
           ,  0.5, -0.5,  0.5, 1,   1, 1, 0, 1
           , -0.5, -0.5, -0.5, 1,   1, 1, 1, 1
           , -0.5,  0.5, -0.5, 1,   1, 0, 0, 1
           ,  0.5,  0.5, -0.5, 1,   1, 0, 1, 1
           ,  0.5, -0.5, -0.5, 1,   0, 0, 1, 1
           ]

indices :: [Int]
indices = [ 0,2,1,  0,3,2
          , 4,3,0,  4,7,3
          , 4,1,5,  4,0,1
          , 3,6,2,  3,7,6
          , 1,6,5,  1,2,6
          , 7,5,6,  7,4,5
          ]
