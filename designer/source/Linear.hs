-- grid is a game written in Haskell
-- Copyright (C) 2018 karamellpelle@hotmail.com
-- 
-- This file is part of grid.
-- 
-- grid is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- grid is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with grid.  If not, see <http://www.gnu.org/licenses/>.
--
module Linear
  (
    Mat4 (..),
    mat4X,
    mat4Y,
    mat4Z,
    mat4W,

    makeMat4,
    mat4Translation,
    mat4TranslationAnti,
    mat4Perspective,
    mat4Ortho,
    mat4Ortho2D,
    mat4ViewABC,

    mat4Translate2,
    mat4Translate3,
    
    Vec4 (..),
    (*>),
    (<+>),
    normVec4,
  ) where


import Data.Monoid


data Mat4 = 
    Mat4 !Float !Float !Float !Float
         !Float !Float !Float !Float
         !Float !Float !Float !Float
         !Float !Float !Float !Float


mat4X :: Mat4 -> (Float, Float, Float, Float)
mat4X (Mat4 x0 x1 x2 x3 
            y0 y1 y2 y3
            z0 z1 z2 z3
            w0 w1 w2 w3) = 
    (x0, x1, x2, x3)


mat4Y :: Mat4 -> (Float, Float, Float, Float)
mat4Y (Mat4 x0 x1 x2 x3 
            y0 y1 y2 y3
            z0 z1 z2 z3
            w0 w1 w2 w3) = 
    (y0, y1, y2, y3)

mat4Z :: Mat4 -> (Float, Float, Float, Float)
mat4Z (Mat4 x0 x1 x2 x3 
            y0 y1 y2 y3
            z0 z1 z2 z3
            w0 w1 w2 w3) = 
    (z0, z1, z2, z3)

mat4W :: Mat4 -> (Float, Float, Float, Float)
mat4W (Mat4 x0 x1 x2 x3 
            y0 y1 y2 y3
            z0 z1 z2 z3
            w0 w1 w2 w3) = 
    (w0, w1, w2, w3)


instance Show Mat4 where 
    show mat = show (mat4X mat) ++ "\n" ++ 
               show (mat4Y mat) ++ "\n" ++ 
               show (mat4Z mat) ++ "\n" ++
               show (mat4W mat)


instance Monoid Mat4 where
    mempty = mat4Identity
    mappend = appendMat4


appendMat4 :: Mat4 -> Mat4 -> Mat4
appendMat4 (Mat4 bx0 bx1 bx2 bx3 
                 by0 by1 by2 by3
                 bz0 bz1 bz2 bz3
                 bw0 bw1 bw2 bw3)
           (Mat4 ax0 ax1 ax2 ax3 
                 ay0 ay1 ay2 ay3
                 az0 az1 az2 az3
                 aw0 aw1 aw2 aw3) = 
    Mat4 (bx0 * ax0 + by0 * ax1 + bz0 * ax2 + bw0 * ax3) 
         (bx1 * ax0 + by1 * ax1 + bz1 * ax2 + bw1 * ax3) 
         (bx2 * ax0 + by2 * ax1 + bz2 * ax2 + bw2 * ax3) 
         (bx3 * ax0 + by3 * ax1 + bz3 * ax2 + bw3 * ax3)
        
         (bx0 * ay0 + by0 * ay1 + bz0 * ay2 + bw0 * ay3) 
         (bx1 * ay0 + by1 * ay1 + bz1 * ay2 + bw1 * ay3) 
         (bx2 * ay0 + by2 * ay1 + bz2 * ay2 + bw2 * ay3) 
         (bx3 * ay0 + by3 * ay1 + bz3 * ay2 + bw3 * ay3)

         (bx0 * az0 + by0 * az1 + bz0 * az2 + bw0 * az3) 
         (bx1 * az0 + by1 * az1 + bz1 * az2 + bw1 * az3) 
         (bx2 * az0 + by2 * az1 + bz2 * az2 + bw2 * az3) 
         (bx3 * az0 + by3 * az1 + bz3 * az2 + bw3 * az3)

         (bx0 * aw0 + by0 * aw1 + bz0 * aw2 + bw0 * aw3) 
         (bx1 * aw0 + by1 * aw1 + bz1 * aw2 + bw1 * aw3) 
         (bx2 * aw0 + by2 * aw1 + bz2 * aw2 + bw2 * aw3) 
         (bx3 * aw0 + by3 * aw1 + bz3 * aw2 + bw3 * aw3)


mat4Identity :: Mat4
mat4Identity = 
    Mat4 1 0 0 0
         0 1 0 0
         0 0 1 0
         0 0 0 1

mat4Translation :: Float -> Float -> Float -> Float -> Mat4
mat4Translation x y z w =
    Mat4 1 0 0 0
         0 1 0 0
         0 0 1 0
         x y z w

mat4TranslationAnti :: Float -> Float -> Float -> Float -> Mat4
mat4TranslationAnti x y z w =
    Mat4 1 0 0 0
         0 1 0 0
         0 0 1 0
         (-x) (-y) (-z) w


mat4Perspective :: Float -> Float -> Float -> Float -> Mat4
mat4Perspective fovy aspect near far = 
    let ymax = near * tan (fovy * 0.5)
        ymin = -ymax
        xmax = ymax * aspect
        xmin = -xmax
    in  Mat4 ((2 * near) / (xmax - xmin)) 0 0 0
             0 ((2 * near) / (ymax - ymin)) 0 0 
             0 0 ((far + near) / (near - far)) (-1)
             0 0 ((2 * far * near) / (near - far)) 0

mat4Ortho :: Float -> Float -> Float -> Float -> Float -> Float -> Mat4
mat4Ortho left right bottom top near far =
    let tx = (right + left) / (left - right)
        ty = (top + bottom) / (bottom - top)
        tz = (far + near) / (near - far)

    in  Mat4 (2 / (right - left)) 0 0 0
             0 (2 / (top - bottom)) 0 0
             0 0 (2 / (near - far)) 0
             tx ty tz 1

mat4Ortho2D :: Float -> Float -> Float -> Float -> Mat4
mat4Ortho2D left right bottom top =
    mat4Ortho left right bottom top (-1) 1


-- | view the modelspace. 0 0 1 is the identity
mat4ViewABC :: Float -> Float -> Float -> Mat4
mat4ViewABC a b c = 
    let x0 = (c) * cos b * sin a
        x1 = (-c) * sin b
        x2 = (-c) * cos b * cos a
        z0 = (-x2)
        z1 = 0
        z2 = x0
        y0 = (-x1) * x0
        y1 = x0 * x0 + x2 * x2
        y2 = (-x1) * x2
    
        xscale = scale x0 x1 x2
        yscale = scale y0 y1 y2
        zscale = scale z0 z1 z2

        a0 = zscale * z0
        a1 = zscale * z1
        a2 = zscale * z2
        b0 = yscale * y0
        b1 = yscale * y1
        b2 = yscale * y2
        c0 = xscale * (-x0)
        c1 = xscale * (-x1)
        c2 = xscale * (-x2)

        -- translate. inverse is transpose
        tx = a0 * x0 + a1 * x1 + a2 * x2
        ty = b0 * x0 + b1 * x1 + b2 * x2
        tz = c0 * x0 + c1 * x1 + c2 * x2
    
    -- inverse is transpose
    in Mat4 a0 b0 c0 0 
            a1 b1 c1 0
            a2 b2 c2 0
            tx ty tz 1
           
    where
        scale x0 x1 x2 = 
            let abs = sqrt $ x0 * x0 + x1 * x1 + x2 * x2
            in  if abs == 0.0 then 0.0
                              else 1 / abs


mat4Translate2 :: Mat4 -> Float -> Float -> Mat4
mat4Translate2 mat x y =
    mat4Translate3 mat x y 0


mat4Translate3 :: Mat4 -> Float -> Float -> Float -> Mat4
mat4Translate3 mat x y z =
    case mat of
        Mat4 x0 x1 x2 x3
             y0 y1 y2 y3
             z0 z1 z2 z3
             w0 w1 w2 w3 -> Mat4 x0 x1 x2 x3
                                 y0 y1 y2 y3
                                 z0 z1 z2 z3
                                 (x0 * x + y0 * y + z0 * z + w0) 
                                 (x1 * x + y1 * y + z1 * z + w1) 
                                 (x2 * x + y2 * y + z2 * z + w2)
                                 (x3 * x + y3 * y + z3 * z + w3)


--------------------------------------------------------------------------------
--  Vec4

data Vec4 =
    Vec4 Float Float Float Float

infixl 7 *>
(*>) :: Float -> Vec4 -> Vec4
(*>) a (Vec4 x y z w) =
    Vec4 (a * x) (a * y) (a * z) (a * w)

infixl 6 <+>
(<+>) :: Vec4 -> Vec4 -> Vec4
(<+>) (Vec4 x0 y0 z0 w0) (Vec4 x1 y1 z1 w1) =
    Vec4 (x0 + x1) (y0 + y1) (z0 + z1) (w0 + w1)



normVec4 :: Vec4 -> Float
normVec4 (Vec4 x y z w ) =
    sqrt $ x*x + y*y + z*z + w*w


makeMat4 :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> Mat4
makeMat4 (Vec4 x0 x1 x2 x3) (Vec4 y0 y1 y2 y3) (Vec4 z0 z1 z2 z3) (Vec4 w0 w1 w2 w3) =
    Mat4  x0 x1 x2 x3
          y0 y1 y2 y3
          z0 z1 z2 z3
          w0 w1 w2 w3


