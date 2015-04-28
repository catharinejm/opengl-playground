module Types where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Numeric.LinearAlgebra.HMatrix
import Control.Concurrent.STM (TQueue)

data Program = Program GL.Program GL.VertexArrayObject MatrixLocs
data DrawState = DrawState { lastTime      :: !Double
                           , lastWrite     :: !Double
                           , cubeXRotation :: !Double
                           , cubeYRotation :: !Double
                           , numFrames     :: !Int
                           , viewMatrix    :: !(Matrix Float)
                           , rotateXSpeed  :: !Double
                           , rotateYSpeed  :: !Double
                           , rotateZSpeed  :: !Double
                           }
data MatrixLocs = MatrixLocs { projection :: !GL.UniformLocation
                             , view       :: !GL.UniformLocation
                             , model      :: !GL.UniformLocation
                             }

data Event = Event !Double !EventInfo
           deriving (Eq, Show)

data EventInfo =
  KeyEventInfo { eventWindow  :: !GLFW.Window
               , key          :: !GLFW.Key
               , scanCode     :: !Int
               , keyState     :: !GLFW.KeyState
               , modifierKeys :: !GLFW.ModifierKeys
               }
  deriving (Show, Eq)

data Env = Env { eventQueue :: TQueue Event
               , envWindow  :: !GLFW.Window
               , initWidth  :: !Int
               , initHeight :: !Int
               }
