module Types where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Numeric.LinearAlgebra.HMatrix

data Program = Program GL.Program GL.VertexArrayObject Shaders MatrixLocs
data Shaders = Shaders { vertex :: GL.Shader
                       , fragment :: GL.Shader
                       }
data DrawState = DrawState { lastTime :: Double
                           , lastWrite :: Double
                           , cubeRotation :: Double
                           , projectionMatrix :: Matrix Float
                           }
data MatrixLocs = MatrixLocs { projection :: GL.UniformLocation
                             , view :: GL.UniformLocation
                             , model :: GL.UniformLocation
                             }

