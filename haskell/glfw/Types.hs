module Types where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Numeric.LinearAlgebra.HMatrix

data Program = Program GL.Program GL.VertexArrayObject MatrixLocs
data DrawState = DrawState { lastTime :: Double
                           , lastWrite :: Double
                           , cubeRotation :: Double
                           , projectionMatrix :: Matrix Float
                           , numFrames :: Int
                           }
data MatrixLocs = MatrixLocs { projection :: GL.UniformLocation
                             , view :: GL.UniformLocation
                             , model :: GL.UniformLocation
                             }

