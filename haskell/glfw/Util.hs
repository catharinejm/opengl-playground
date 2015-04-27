{-# LANGUAGE NamedFieldPuns #-}

module Util where


import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO

import qualified Graphics.GLUtil as U

import Types

errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

initialize :: Int -> Int -> String -> IO GLFW.Window
initialize width height title = do
  GLFW.setErrorCallback $ Just errorCallback
  successfulInit <- GLFW.init
  if not successfulInit then exitFailure else do
    GLFW.defaultWindowHints
    GLFW.windowHint $ GLFW.WindowHint'Visible False
    GLFW.windowHint $ GLFW.WindowHint'Resizable True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
    mw <- GLFW.createWindow width height title Nothing Nothing
    case mw of
     Nothing -> GLFW.terminate >> exitFailure
     Just window -> do
       Just mon <- GLFW.getPrimaryMonitor
       Just videoMode <- GLFW.getVideoMode mon
       let w = GLFW.videoModeWidth videoMode
           h = GLFW.videoModeHeight videoMode
       GLFW.setWindowPos window ((w - width) `div` 2) ((h - height) `div` 2)
       GLFW.makeContextCurrent mw
       GLFW.swapInterval 1
       GLFW.showWindow window
       return window

cleanup :: GLFW.Window -> IO ()
cleanup win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess


mainLoop :: GLFW.Window -> StateT DrawState IO ()
mainLoop w = do
  liftIO $ do
    close <- GLFW.windowShouldClose w
    unless close $ do
      GLFW.swapBuffers w
      GLFW.pollEvents
  mainLoop w
