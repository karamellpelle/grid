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
module Game.Font.FontColor
  (
    FontColor (..),
    makeFontColorFloat,
    makeFontColorGLubyte,

    fontColorScaleAlpha,
    fontColorChangeAlpha,

  ) where

import MyPrelude

import OpenGL
import OpenGL.Helpers


-- | fixme/question: 
--   * representing colors with premultiplied alpha 4-tuple?
--   * use 4 GLubyte's, instead of GLfloat? (but I don't think GHC saves
--     this space, and instead represent GLubyte as Word32) 
data FontColor =
    FontColor
    {
        fontcolorR :: !GLfloat,
        fontcolorG :: !GLfloat,
        fontcolorB :: !GLfloat,
        fontcolorA :: !GLfloat
    }


makeFontColorFloat :: Float -> Float -> Float -> Float -> FontColor
makeFontColorFloat r g b a =
    FontColor (rTF r) (rTF g) (rTF b) (rTF a)


makeFontColorGLubyte :: GLubyte -> GLubyte -> GLubyte -> GLubyte -> FontColor
makeFontColorGLubyte r g b a = 
    FontColor (0.003921569 * fI r) (0.003921569 * fI g) 
              (0.003921569 * fI b) (0.003921569 * fI a)


-- | fixme: premultiplied?
fontColorScaleAlpha :: FontColor -> Float -> FontColor
fontColorScaleAlpha (FontColor r g b a) scale = 
    FontColor r g b (rTF scale * a)


-- | fixme: premultiplied?
fontColorChangeAlpha :: FontColor -> Float -> FontColor
fontColorChangeAlpha (FontColor r g b a) a' =
    FontColor r g b (rTF a')

