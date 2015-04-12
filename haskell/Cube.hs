module Cube where

import Graphics.UI.GLUT

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cube :: GLfloat -> IO ()
cube w = do
  renderPrimitive Quads $ mapM_ vertex3f
    [ ( w,  w,  w), ( w,  w, -w), ( w, -w, -w), ( w, -w,  w),
      ( w,  w,  w), ( w,  w, -w), (-w,  w, -w), (-w,  w,  w),
      ( w,  w,  w), ( w, -w,  w), (-w, -w,  w), (-w,  w,  w),
      (-w,  w,  w), (-w,  w, -w), (-w, -w, -w), (-w, -w,  w),
      ( w, -w,  w), ( w, -w, -w), (-w, -w, -w), (-w, -w,  w),
      ( w,  w, -w), ( w, -w, -w), (-w, -w, -w), (-w,  w, -w) ]


cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
              [ ( w,-w, w),( w, w, w),( w, w, w),(-w, w, w),
                (-w, w, w),(-w,-w, w),(-w,-w, w),( w,-w, w),
                ( w,-w, w),( w,-w,-w),( w, w, w),( w, w,-w),
                (-w, w, w),(-w, w,-w),(-w,-w, w),(-w,-w,-w),
                ( w,-w,-w),( w, w,-w),( w, w,-w),(-w, w,-w),
                (-w, w,-w),(-w,-w,-w),(-w,-w,-w),( w,-w,-w) ]

