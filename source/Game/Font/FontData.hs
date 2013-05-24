-- grid is a game written in Haskell
-- Copyright (C) 2013 Carl Joachim Svenn
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
module Game.Font.FontData
  (
    FontData (..),

    loadFontData,
    makeFontData,
    destroyFontData,

    fontdataCharSize,
    fontdataCharWth,
    fontdataCharHth,


  ) where

import MyPrelude
import OpenGL
import OpenGL.Helpers


data FontData =
    FontData
    {
      fontdataStencil :: !GLuint,
      fontdataCharAspect :: !Float,
      fontdataCharsX :: !UInt,
      fontdataCharsY :: !UInt,
      fontdataCharX :: !GLushort,
      fontdataCharY :: !GLushort,
      fontdataCharPadX :: !GLushort,
      fontdataCharPadY :: !GLushort,
      fontdataOffset :: !UInt
    }


--------------------------------------------------------------------------------
--  



-- | (implement some file format. parsec + zlib? load that.)
loadFontData :: FilePath -> IO FontData
loadFontData path = do
    error "fixme loadFontData"


-- | (so this function is tmp)
--   note: stencil image is assumed to be flipped vertically!
makeFontData :: FilePath -> UInt -> UInt -> Float -> Float -> UInt -> IO FontData
makeFontData path xs ys occupyX occupyY offset = do
    -- stencil
    stencil <- bindNewTex gl_TEXTURE_2D
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR_MIPMAP_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    (wth, hth) <- loadTexPreMult gl_TEXTURE_2D gl_RGBA $ path ++ "/tex.png" -- fixme: intfmt
    glGenerateMipmap gl_TEXTURE_2D

    -- stencil properties
    let aspect = (fI wth * occupyX * fI ys) / 
                 (fI hth * occupyY * fI xs)

        charx = (0xffff `div` fI xs)
        chary = (0xffff `div` fI ys) 
        padx = truncate $ (0.5 * 0xffff * (1.0 - occupyX)) / fI xs
        pady = truncate $ (0.5 * 0xffff * (1.0 - occupyY)) / fI ys
    
    return $  FontData
              {
                  fontdataStencil = stencil,
                  fontdataCharAspect = aspect,
                  fontdataCharsX = xs,
                  fontdataCharsY = ys,
                  fontdataCharX = charx,
                  fontdataCharY = chary,
                  fontdataCharPadX = padx,
                  fontdataCharPadY = pady,
                  fontdataOffset = offset
              }


destroyFontData :: FontData -> IO ()
destroyFontData fd = 
    delTex $ fontdataStencil fd


--------------------------------------------------------------------------------
--  

fontdataCharSize :: FontData -> Float -> (Float, Float)
fontdataCharSize fd size =
    (fontdataCharAspect fd * size, size)

fontdataCharWth :: FontData -> Float -> Float
fontdataCharWth fd size =
    fontdataCharAspect fd * size

fontdataCharHth :: FontData -> Float -> Float
fontdataCharHth fd size =
    size


