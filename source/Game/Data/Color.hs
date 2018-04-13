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
module Game.Data.Color
  (
    Color (..),

    uniformColor,
    smoothColor,

    colorNull,
    colorBlack,
    colorWhite,
    colorRed,
    colorGreen,
    colorBlue,
    colorYellow,

  ) where

import MyPrelude
import OpenGL
import OpenGL.Helpers


data Color =
    Color !GLfloat !GLfloat !GLfloat !GLfloat


uniformColor :: GLint -> Color -> IO ()
uniformColor uni (Color r g b a) = 
    glUniform4f uni r g b a


smoothColor :: Color -> Color -> Float -> Color
smoothColor (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) alpha = 
    Color (smooth r0 r1 alpha) (smooth g0 g1 alpha) 
          (smooth b0 b1 alpha) (smooth a0 a1 alpha)
    where
      smooth x x' alpha =
          (1.0 - rTF alpha) * x + (rTF alpha) * x'



--------------------------------------------------------------------------------
--  color palette

colorNull :: Color
colorNull =
    Color 0.0 0.0 0.0 0.0

colorBlack :: Color
colorBlack =
    Color 0.0 0.0 0.0 1.0

colorWhite :: Color
colorWhite =
    Color 1.0 1.0 1.0 1.0

colorRed :: Color
colorRed = 
    Color 1.0 0.0 0.0 1.0

colorGreen :: Color
colorGreen = 
    Color 0.0 1.0 0.0 1.0

colorBlue :: Color
colorBlue = 
    Color 0.0 0.0 1.0 1.0

colorYellow:: Color
colorYellow = 
    Color 1.0 1.0 0.0 1.0

