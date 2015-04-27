module Types where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Numeric.LinearAlgebra.HMatrix
import Control.Concurrent.STM (TQueue)

data Program = Program GL.Program GL.VertexArrayObject MatrixLocs
data DrawState = DrawState { lastTime     :: !Double
                           , lastWrite    :: !Double
                           , cubeRotation :: !Double
                           , numFrames    :: !Int
                           , viewMatrix   :: !(Matrix Float)
                           , rotateXSpeed :: !Float
                           , rotateYSpeed :: !Float
                           , rotateZSpeed :: !Float
                           }
data MatrixLocs = MatrixLocs { projection :: !GL.UniformLocation
                             , view       :: !GL.UniformLocation
                             , model      :: !GL.UniformLocation
                             }

data Event =
  EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys

data Env = Env { eventQueue :: TQueue Event
               , window     :: !GLFW.Window
               , initWidth  :: !Int
               , initHeight :: !Int
               }
