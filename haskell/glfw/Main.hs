{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import Control.Applicative
import System.FilePath ((</>))
import Control.Monad.Trans
import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as U

import qualified Util as W

import Numeric.LinearAlgebra.HMatrix
import MatrixOps
import GHC.Float
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = do
  let width = 800
      height = 600
  win <- W.initialize width height "My first cube"
  prog <- initResources
  Just time <- GLFW.getTime
  let projMat = makeProjMatrix 60 (fromIntegral width / fromIntegral height) 1 100
      initialDrawState = DrawState { lastTime = time, cubeRotation = 0, projectionMatrix = projMat }
  U.printError
  W.mainLoop (evalStateT (draw prog win) initialDrawState) win
  W.cleanup win

initResources :: IO Program
initResources = do
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.depthFunc $= Just GL.Less
  GL.cullFace $= Just GL.Back
  GL.frontFace $= GL.CCW
  -- compile shaders
  vs <- U.loadShader GL.VertexShader $ shaderPath </> "SimpleShader.vertex.glsl"
  fs <- U.loadShader GL.FragmentShader $ shaderPath </> "SimpleShader.fragment.glsl"
  p <- U.linkShaderProgram [vs, fs]
  vao <- U.makeVAO $ do
    vbo <- U.makeBuffer GL.ArrayBuffer vertices
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (4*8) U.offset0)
    GL.vertexAttribPointer (GL.AttribLocation 1) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float (4*8) $ U.offsetPtr (4*4))
    ibo <- U.makeBuffer GL.ElementArrayBuffer indices
    return ()
  (Program p vao (Shaders vs fs)) <$> loadAttribs p
  where
    loadAttribs p = do
      proj <- GL.get (GL.uniformLocation p "ProjectionMatrix")
      v <- GL.get (GL.uniformLocation p "ViewMatrix")
      mod <- GL.get (GL.uniformLocation p "ModelMatrix")
      return MatrixLocs { projection = proj
                        , view = v
                        , model = mod
                        }

destroyResources :: Program -> IO ()
destroyResources (Program prog vao shaders mlocs) = do
  GL.detachShader prog $ vertex shaders
  GL.detachShader prog $ fragment shaders
  GL.deleteObjectName $ vertex shaders
  GL.deleteObjectName $ fragment shaders
  GL.deleteObjectName prog
  U.deleteVAO vao
  return ()


draw :: Program -> GLFW.Window -> StateT DrawState IO ()
draw (Program program vao _ mlocs) win = do
  state @ DrawState { lastTime, cubeRotation, projectionMatrix } <- get
  Just now <- liftIO GLFW.getTime
  let newRot = cubeRotation + 45.0 * (now - lastTime)
      angle = (double2Float newRot) * 180.0 / pi
  put state { lastTime = now, cubeRotation = newRot }
  liftIO $ do GL.clear [GL.ColorBuffer, GL.DepthBuffer]
              GL.currentProgram $= Just program
              let yrot = rotateAboutY (ident 4) angle
              U.uniformMat (model mlocs) $= (glMat $ rotateAboutX yrot angle)
              U.uniformMat (view mlocs) $= viewMatrix
              U.uniformMat (projection mlocs) $= glMat projectionMatrix
              U.withVAO vao $ do
                GL.drawElements GL.Triangles 36 GL.UnsignedInt U.offset0
                

data Program = Program GL.Program GL.VertexArrayObject Shaders MatrixLocs
data Shaders = Shaders { vertex :: GL.Shader
                       , fragment :: GL.Shader
                       }
data DrawState = DrawState { lastTime :: Double
                           , cubeRotation :: Double
                           , projectionMatrix :: Matrix Float
                           }
data MatrixLocs = MatrixLocs { projection :: GL.UniformLocation
                             , view :: GL.UniformLocation
                             , model :: GL.UniformLocation
                             }

toGLList :: [[Float]] -> [[GL.GLfloat]]
toGLList = unsafeCoerce

glMat = toGLList . toLists 

viewMatrix = glMat $ translate (ident 4) 0 0 (-2)

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
