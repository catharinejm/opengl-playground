{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import System.FilePath ((</>))
import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.RWS.Strict
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as U

import qualified Util as W

import Numeric.LinearAlgebra.HMatrix
import MatrixOps
import GHC.Float
import Unsafe.Coerce (unsafeCoerce)
import Data.Int
import Data.List

import Types

main :: IO ()
main = do
  let width = 800
      height = 600
  win <- W.initialize width height "My first cube"
  prog <- initResources width height
  Just time <- GLFW.getTime
  eventQueue <- newTQueueIO :: IO (TQueue Event)
  let initialDrawState = DrawState { lastTime     = time
                                   , cubeXRotation = 0
                                   , cubeYRotation = 0
                                   , lastWrite    = time
                                   , numFrames    = 0
                                   , viewMatrix   = translate (ident 4) 0 0 (-2)
                                   , rotateXSpeed = 0.0
                                   , rotateYSpeed = 0.0
                                   , rotateZSpeed = 0.0
                                   }
      initialEnv = Env { initWidth  = width
                       , initHeight = height
                       , window     = win
                       , eventQueue
                       }
  GLFW.setWindowSizeCallback win $ Just $ windowSizeCallback prog
  GLFW.setKeyCallback        win $ Just $ keyCallback eventQueue
  evalRWST (initMatrices prog >> draw prog win) initialEnv initialDrawState
  destroyResources prog
  W.cleanup win

windowSizeCallback :: Program -> GLFW.WindowSizeCallback
windowSizeCallback (Program glprog _ mlocs) win w h = do
  let proj = makeProjMatrix 60 (fromIntegral w / fromIntegral h) 1 100
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  withProgram glprog $ do
    U.uniformMat (projection mlocs) $= glMat proj

keyCallback :: TQueue Event -> GLFW.KeyCallback
keyCallback tq window k sc ka mk = atomically $ writeTQueue tq $ EventKey window k sc ka mk

initMatrices :: Program -> RWST Env () DrawState IO ()
initMatrices (Program glprog _ mlocs) = do
  viewMat <- gets viewMatrix
  width <- asks initWidth
  height <- asks initHeight
  liftIO $ withProgram glprog $ do
    U.uniformMat (view mlocs) $= glMat viewMat
    let proj = makeProjMatrix 60 (fromIntegral width / fromIntegral height) 1 100
    U.uniformMat (projection mlocs) $= glMat proj


initResources :: Int -> Int -> IO Program
initResources width height = do
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.depthFunc  $= Just GL.Less
  GL.cullFace   $= Just GL.Back
  GL.frontFace  $= GL.CCW
  GL.viewport   $= (GL.Position 0 0, GL.Size 800 600)
  -- compile shaders
  shaderProg <- U.loadShaderProgram [(GL.VertexShader, shaderPath </> "SimpleShader.vertex.glsl")
                                    ,(GL.FragmentShader, shaderPath </> "SimpleShader.fragment.glsl")
                                    ]
  vao <- U.makeVAO $ do
    U.makeBuffer GL.ArrayBuffer vertices
    U.enableAttrib shaderProg "in_Position"
    U.enableAttrib shaderProg "in_Color"
    U.setAttrib shaderProg "in_Position" GL.ToFloat
      (GL.VertexArrayDescriptor 4 GL.Float (4*8) U.offset0)
    U.setAttrib shaderProg "in_Color" GL.ToFloat
      (GL.VertexArrayDescriptor 4 GL.Float (4*8) $ U.offsetPtr (4*4))
    U.makeBuffer GL.ElementArrayBuffer indices
    return ()
  let program = (U.program shaderProg)
      vmLoc = U.getUniform shaderProg "ViewMatrix"
      pmLoc = U.getUniform shaderProg "ProjectionMatrix"
      mmLoc = U.getUniform shaderProg "ModelMatrix"
  return $ Program program vao MatrixLocs { projection = pmLoc
                                          , view = vmLoc
                                          , model = mmLoc
                                          }

destroyResources :: Program -> IO ()
destroyResources (Program prog vao _) = do
  shaders <- GL.get $ GL.attachedShaders prog
  mapM_ (GL.detachShader prog) shaders
  mapM_ GL.deleteObjectName shaders
  GL.deleteObjectName prog
  U.deleteVAO vao
  return ()


draw :: Program -> GLFW.Window -> RWST Env () DrawState IO ()
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
      liftIO $ GLFW.setWindowTitle win
        ("Haskell: ms/f: "++(show msPerFrame)++", FPS: "++(show fps))
      modify $ \s -> s { lastWrite = lastWrite + 2.5
                       , numFrames = 0
                       }
    DrawState { lastTime, cubeXRotation, cubeYRotation, rotateXSpeed, rotateYSpeed } <- get
    let newXRot = cubeXRotation + 45.0 * rotateXSpeed * (now - lastTime)
        newYRot = cubeYRotation + 45.0 * rotateYSpeed * (now - lastTime)
        xangle = (double2Float newXRot) * pi / 180.0
        yangle = (double2Float newYRot) * pi / 180.0
    modify $ \s -> s { lastTime = now, cubeXRotation = newXRot, cubeYRotation = newYRot }
    liftIO $ do GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                withProgram glprog $ do
                  let yrot = rotateAboutY (ident 4) yangle
                      mod = rotateAboutX yrot xangle
                  U.withVAO vao $ do
                    U.uniformMat (model mlocs) $= glMat mod
                    GL.drawElements GL.Triangles 36 GL.UnsignedInt U.offset0
                GLFW.swapBuffers win
                GLFW.pollEvents
    processEvents
    draw prog win

processEvents :: RWST Env () DrawState IO ()
processEvents = do
  tq <- asks eventQueue
  me <- liftIO $ atomically $ tryReadTQueue tq
  case me of
   Just e -> do
     processEvent e
     processEvents
   Nothing -> return ()

processEvent :: Event -> RWST Env () DrawState IO ()
processEvent ev =
  case ev of
   (EventKey win key sc keyState modKeys) ->
     if keyState == GLFW.KeyState'Pressed then
       case key of
        GLFW.Key'Q -> close
        GLFW.Key'Escape -> close
        GLFW.Key'Up -> modify $ \s -> s { rotateXSpeed = speedUp (rotateXSpeed s) }
        GLFW.Key'Down -> modify $ \s -> s { rotateXSpeed = speedDown (rotateXSpeed s) }
        GLFW.Key'Left -> modify $ \s -> s { rotateYSpeed = speedDown (rotateYSpeed s) }
        GLFW.Key'Right -> modify $ \s -> s { rotateYSpeed = speedUp (rotateYSpeed s) }
        _ -> return ()
     else
       return ()
     where
       close = liftIO $ GLFW.setWindowShouldClose win True
       speedUp v = if v < 0 then
                     let q = v / 1.3
                     in if q > -0.1 then 0.0
                        else q
                   else
                     let p = v * 1.3
                     in if p < 0.1 then 0.1
                        else p
       speedDown v = -(speedUp (-v))

withProgram :: GL.Program -> IO () -> IO ()
withProgram prog action = do
  GL.currentProgram $= Just prog
  action
  GL.currentProgram $= Nothing

toGLLists :: [[Float]] -> [[GL.GLfloat]]
toGLLists = unsafeCoerce

glMat = toGLLists . toLists

shaderPath :: FilePath
shaderPath = "."

vertices :: [Float]
vertices = [ -0.5, -0.5,  0.5, 1,  0, 0, 1, 1
           , -0.5,  0.5,  0.5, 1,  1, 0, 0, 1
           ,  0.5,  0.5,  0.5, 1,  0, 1, 0, 1
           ,  0.5, -0.5,  0.5, 1,  1, 1, 0, 1
           , -0.5, -0.5, -0.5, 1,  1, 1, 1, 1
           , -0.5,  0.5, -0.5, 1,  1, 0, 0, 1
           ,  0.5,  0.5, -0.5, 1,  1, 0, 1, 1
           ,  0.5, -0.5, -0.5, 1,  0, 0, 1, 1
           ]


indices :: [Int32]
indices = [ 0,2,1,  0,3,2
          , 4,3,0,  4,7,3
          , 4,1,5,  4,0,1
          , 3,6,2,  3,7,6
          , 1,6,5,  1,2,6
          , 7,5,6,  7,4,5
          ]
