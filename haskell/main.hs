import Graphics.UI.GLUT
import Bindings
import Data.IORef

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Haskell OpenGL"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  angle <- newIORef 0.0
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)
  idleCallback $= Just (idle angle delta)
  displayCallback $= display angle pos
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  closeCallback $= Just (putStrLn "see ya!")
  mainLoop
