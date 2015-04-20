{-# LANGUAGE NamedFieldPuns #-}

module Util where


import Control.Applicative
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO

import qualified Graphics.GLUtil as U

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
                                      GLFW.setWindowShouldClose window True

initialize :: String -> IO GLFW.Window
initialize title = do
  GLFW.setErrorCallback $ Just errorCallback
  successfulInit <- GLFW.init
  if not successfulInit then exitFailure else do
    GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
    mw <- GLFW.createWindow 800 600 title Nothing Nothing
    case mw of
     Nothing -> GLFW.terminate >> exitFailure
     Just window -> do
       Just mon <- GLFW.getPrimaryMonitor
       Just videoMode <- GLFW.getVideoMode mon
       let w = GLFW.videoModeWidth videoMode
           h = GLFW.videoModeHeight videoMode
       GLFW.setWindowPos window ((w - 800) `div` 2) ((h - 600) `div` 2)
       GLFW.makeContextCurrent mw
       GLFW.swapInterval 1
       GLFW.setKeyCallback window (Just keyCallback)
       return window

cleanup :: GLFW.Window -> IO ()
cleanup win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess


mainLoop :: IO () -> GLFW.Window -> IO ()
mainLoop draw w = do
  close <- GLFW.windowShouldClose w
  unless close $ do
    draw
    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop draw w
