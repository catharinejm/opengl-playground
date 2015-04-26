{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import System.FilePath ((</>))
import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as U
import Graphics.GLUtil (ShaderProgram(..))

import qualified Util as W

import Numeric.LinearAlgebra.HMatrix
import MatrixOps
import GHC.Float
import Unsafe.Coerce (unsafeCoerce)

import Types

main :: IO ()
main = do
  let width = 800
      height = 600
  win <- W.initialize width height "My first cube"
  prog <- initResources
  Just time <- GLFW.getTime
  let projMat = makeProjMatrix 60 (fromIntegral width / fromIntegral height) 1 100
      initialDrawState = DrawState { lastTime = time
                                   , cubeRotation = 0
                                   , projectionMatrix = projMat
                                   , lastWrite = time
                                   , numFrames = 0
                                   }
  U.printError
  evalStateT (draw prog win) initialDrawState
  destroyResources prog
  W.cleanup win

initResources :: IO Program
initResources = do
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.depthFunc $= Just GL.Less
  GL.cullFace $= Just GL.Back
  GL.frontFace $= GL.CCW
  -- compile shaders
  shaderProg@ShaderProgram {program} <-
    U.loadShaderProgram [(GL.VertexShader, shaderPath </> "SimpleShader.vertex.glsl")
                        ,(GL.FragmentShader, shaderPath </> "SimpleShader.fragment.glsl")
                        ]
  vao <- U.makeVAO $ do
    U.makeBuffer GL.ArrayBuffer vertices
    U.enableAttrib shaderProg "in_Position"
    U.setAttrib shaderProg "in_Position" GL.ToFloat
      (GL.VertexArrayDescriptor 4 GL.Float 0 U.offset0)

    U.makeBuffer GL.ArrayBuffer colors
    U.enableAttrib shaderProg "in_Color"
    U.setAttrib shaderProg "in_Color" GL.ToFloat (GL.VertexArrayDescriptor 4 GL.Float 0 U.offset0)

    U.makeBuffer GL.ElementArrayBuffer indices
    return ()
  (Program program vao) <$> loadAttribs program
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
destroyResources (Program prog vao mlocs) = do
  shaders <- GL.get $ GL.attachedShaders prog
  mapM_ (GL.detachShader prog) shaders
  mapM_ GL.deleteObjectName shaders
  GL.deleteObjectName prog
  U.deleteVAO vao
  return ()


draw :: Program -> GLFW.Window -> StateT DrawState IO ()
draw prog@(Program glprog vao mlocs) win = do
  close <- liftIO $ GLFW.windowShouldClose win
  unless close $ do
    Just now <- liftIO GLFW.getTime
    DrawState { lastWrite, numFrames } <- get
    modify $ \s -> s { numFrames = numFrames + 1 }
    let doWrite = now - lastWrite >= 2.5
    when doWrite $ do
      let msPerFrame = (now - lastWrite) * 1000.0 / fromIntegral numFrames
          fps = fromIntegral numFrames / (now - lastWrite)
      liftIO $ GLFW.setWindowTitle win ("ms/f: "++(show msPerFrame)++", FPS: "++(show fps))
      modify $ \s -> s { lastWrite = lastWrite + 2.5
                       , numFrames = 0
                       }
    DrawState { lastTime, cubeRotation, projectionMatrix } <- get
    let newRot = cubeRotation + 45.0 * (now - lastTime)
        angle = (double2Float newRot) * pi / 180.0
    modify $ \s -> s { lastTime = now, cubeRotation = newRot }
    liftIO $ do GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                withProgram glprog $ do
                  let yrot = rotateAboutY (ident 4) angle
                      mod = rotateAboutX yrot angle
                      proj = projectionMatrix
                  when doWrite $ do
                    putStrLn $ "Model Matrix\n"++(show mod)
                    putStrLn $ "View Matrix\n"++(show viewMatrix)
                    putStrLn $ "Projection Matrix\n"++(show proj)

                  U.uniformMat (model mlocs) $= glMat mod
                  U.uniformMat (view mlocs) $= glMat viewMatrix
                  U.uniformMat (projection mlocs) $= glMat proj
                  U.withVAO vao $ do
                    GL.drawElements GL.Triangles 36 GL.UnsignedInt U.offset0
                GLFW.swapBuffers win
                GLFW.pollEvents
    draw prog win

withProgram :: GL.Program -> IO () -> IO ()
withProgram prog action = do
  GL.currentProgram $= Just prog
  action
  GL.currentProgram $= Nothing

toGLF :: Float -> GL.GLfloat
toGLF = unsafeCoerce

toGLList :: [[Float]] -> [[GL.GLfloat]]
toGLList [] = []
toGLList (f:fs) = map toGLF f : toGLList fs

glMat = toGLList . toLists . tr

viewMatrix = translate (ident 4) 0 0 (-2)

shaderPath :: FilePath
shaderPath = "."

vertices :: [Float]
vertices = [ -0.5, -0.5,  0.5, 1
           , -0.5,  0.5,  0.5, 1
           ,  0.5,  0.5,  0.5, 1
           ,  0.5, -0.5,  0.5, 1
           , -0.5, -0.5, -0.5, 1
           , -0.5,  0.5, -0.5, 1
           ,  0.5,  0.5, -0.5, 1
           ,  0.5, -0.5, -0.5, 1
           ]

colors :: [Float]
colors = [ 0, 0, 1, 1
         , 1, 0, 0, 1
         , 0, 1, 0, 1
         , 1, 1, 0, 1
         , 1, 1, 1, 1
         , 1, 0, 0, 1
         , 1, 0, 1, 1
         , 0, 0, 1, 1
         ]



indices :: [Int]
indices = [ 0,2,1,  0,3,2
          , 4,3,0,  4,7,3
          , 4,1,5,  4,0,1
          , 3,6,2,  3,7,6
          , 1,6,5,  1,2,6
          , 7,5,6,  7,4,5
          ]
