module MatrixOps where

import Numeric.LinearAlgebra.HMatrix


updateIdent = accum (ident 4) const


-- All constructed matrices below use Row Major indexing since GLUtil assumes RowMajor
-- openglbook.com Chapter 4 has Column Major matrices, so these are flipped


rotateAboutX :: Matrix Float -> Float -> Matrix Float
rotateAboutX m angle =
  let sine = sin angle
      cosine = cos angle
      rot = updateIdent [((1,1), cosine)
                        ,((1,2), sine)
                        ,((2,1), -sine)
                        ,((2,2), cosine)
                        ]
  in m <> rot

rotateAboutY :: Matrix Float -> Float -> Matrix Float
rotateAboutY m angle =
  let sine = sin angle
      cosine = cos angle
      rot = updateIdent [((0,0), cosine)
                        ,((2,0), -sine)
                        ,((0,2), sine)
                        ,((2,2), cosine)
                        ]
  in m <> rot

rotateAboutZ :: Matrix Float -> Float -> Matrix Float
rotateAboutZ m angle =
  let sine = sin angle
      cosine = cos angle
      rot = updateIdent [((0,0), cosine)
                        ,((0,1), sine)
                        ,((1,0), -sine)
                        ,((1,1), cosine)
                        ]
  in m <> rot

scale :: Matrix Float -> Float -> Float -> Float -> Matrix Float
scale m x y z = m <> (diag $ 4 |> [x,y,z,1])

translate :: Matrix Float -> Float -> Float -> Float -> Matrix Float
translate m x y z = m <> updateIdent [((0,3), x)
                                     ,((1,3), y)
                                     ,((2,3), z)
                                     ]

makeProjMatrix :: Float -> Float -> Float -> Float -> Matrix Float
makeProjMatrix fovy aspect near far =
  accum zeros const [((0,0), xscale)
                    ,((1,1), yscale)
                    ,((2,2), -((far + near) / frustum))
                    ,((2,3), -((2 * near * far) / frustum))
                    ,((3,2), -1)
                    ]
  where
    zeros = (4><4) $ repeat 0.0
    yscale = cotan (fovy / 2)
    xscale = yscale / aspect
    frustum = far - near
    cotan a = 1.0 / tan (pi * a / 180.0)
