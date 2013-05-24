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
module Game.Font
  (
    fontDrawDefault,
    fontDraw2D,
    fontDraw2DCentered,
    fontDraw3D,
    fontDraw3DCentered,

    fontDrawObject2D,
    fontDrawObject2DCentered,
    fontDrawObject3D,
    fontDrawObject3DCentered,

    module Game.Font.FontShade,
    module Game.Font.FontData,
    module Game.Font.FontObject,
    module Game.Font.FontColor,

  ) where

import MyPrelude

import Game.Font.FontShade 
import Game.Font.FontData
import Game.Font.FontObject
import Game.Font.FontColor
import Game.Font.Buffer

import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  Default


-- | begin default drawings (no FontObject)
fontDrawDefault :: FontShade -> FontData -> Float -> FontColor -> IO ()
fontDrawDefault sh fd size (FontColor r g b a) = do
    -- vao
    glBindVertexArrayOES $ fontShadeDefaultVAO sh

    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ fontdataStencil fd

    -- size
    let (wth, hth) = fontdataCharSize fd size
    glUniform2f (fontShadeUniCharSize sh) (rTF wth) (rTF hth)

    -- color
    glUniform4f (fontShadeUniColor sh) r g b a 

    

--------------------------------------------------------------------------------
--  Default: Draw2D

-- | note: undefined behaviour if (valueFontMaxCharacters < length str), probably crash!
fontDraw2D :: FontShade -> FontData -> Float -> Float -> String -> IO ()
fontDraw2D sh fd x y str = do

    -- translate
    glUniform2f (fontShadeUniTranslate sh) 0.0 0.0

    fontDraw2D' sh fd x y str


-- | note: undefined behaviour if (valueFontMaxCharacters < length str), probably crash!
fontDraw2DCentered :: FontShade -> FontData -> Float -> Float -> String -> IO ()
fontDraw2DCentered sh fd x y str = do

    -- translate
    let tx = (-0.5) * (fI $ length str)
        ty = (-0.5)
    glUniform2f (fontShadeUniTranslate sh) tx ty

    fontDraw2D' sh fd x y str


fontDraw2D' :: FontShade -> FontData -> Float -> Float -> String -> IO () 
fontDraw2D' sh fd x y str = do
    -- pos
    glUniform3f (fontShadeUniPos sh) (rTF x) (rTF y) 0.0

    glBindBuffer gl_ARRAY_BUFFER $ fontShadeDefaultVBOCoord sh
    let bytesize = valueFontMaxCharacters * 16
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_STATIC_DRAW
    len <- writeBuf gl_ARRAY_BUFFER $ \ptr -> 
        writeDefaultStencilCoord2D fd ptr str
     
    -- draw with ibo (4 + 2 vertices per character)
    glDrawElements gl_TRIANGLE_STRIP (fI $ len * 6) gl_UNSIGNED_SHORT nullPtr




--------------------------------------------------------------------------------
--  Default: Draw3D

-- | note: undefined behaviour if (valueFontMaxCharacters < length str), probably crash!
fontDraw3D :: FontShade -> FontData -> Float -> Float -> Float -> String -> IO ()
fontDraw3D sh fd x y z str = do

    -- translate
    glUniform2f (fontShadeUniTranslate sh) 0.0 0.0

    fontDraw3D' sh fd x y z str


-- | note: undefined behaviour if (valueFontMaxCharacters < length str), probably crash!
fontDraw3DCentered :: FontShade -> FontData -> Float -> Float -> Float -> String -> 
    IO ()
fontDraw3DCentered sh fd x y z str = do

    -- translate
    let tx = (-0.5) * (fI $ length str)
        ty = (-0.5)
    glUniform2f (fontShadeUniTranslate sh) tx ty

    fontDraw3D' sh fd x y z str


fontDraw3D' :: FontShade -> FontData -> Float -> Float -> Float -> String -> IO () 
fontDraw3D' sh fd x y z str = do
    -- pos
    glUniform3f (fontShadeUniPos sh) (rTF x) (rTF y) (rTF z)

    glBindBuffer gl_ARRAY_BUFFER $ fontShadeDefaultVBOCoord sh
    let bytesize = valueFontMaxCharacters * 16
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_STATIC_DRAW
    len <- writeBuf gl_ARRAY_BUFFER $ \ptr -> 
        writeDefaultStencilCoord3D fd ptr str
     
    -- draw with ibo (4 + 2 vertices per character)
    glDrawElements gl_TRIANGLE_STRIP (fI $ len * 6) gl_UNSIGNED_SHORT nullPtr






--------------------------------------------------------------------------------
--  FontObject


-- | draw FontObject at 2D-pos.
fontDrawObject2D :: FontShade -> FontObject -> Float -> Float -> IO ()
fontDrawObject2D sh fo x y =
    fontDrawObject3D sh fo x y 0.0

-- | draw FontObject at 2D-pos.
fontDrawObject2DCentered :: FontShade -> FontObject -> Float -> Float -> IO ()
fontDrawObject2DCentered sh fo x y =
    fontDrawObject3DCentered sh fo x y 0.0


-- | if num calls >= 2, and font objects are similar (i.e. only differ by text)
fontDrawObject2DCentered' :: FontShade -> FontObject -> Float -> Float -> IO ()
fontDrawObject2DCentered' sh fo x y =
    fontDrawObject3DCentered' sh fo x y 0.0


-- | if num calls >= 2, and font objects are similar (i.e. only differ by text)
fontDrawObject2D' :: FontShade -> FontObject -> Float -> Float -> IO ()
fontDrawObject2D' sh fo x y =
    fontDrawObject3D' sh fo x y 0.0


-- | draw FontObject at 3D-pos.
fontDrawObject3D :: FontShade -> FontObject -> Float -> Float -> Float -> IO ()
fontDrawObject3D sh fo x y z = do
    -- stencil
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ fontobjectStencil fo

    -- size
    glUniform2f (fontShadeUniCharSize sh) (rTF $ fontobjectCharSizeX fo)
                                          (rTF $ fontobjectCharSizeY fo)
    -- color
    case fontobjectColor fo of
        FontColor r g b a -> 
            glUniform4f (fontShadeUniCharSize sh) r g b a

    -- translate 
    glUniform2f (fontShadeUniTranslate sh) 0.0 0.0

    fontDrawObject3D' sh fo x y z


-- | draw FontObject at 3D-pos.
fontDrawObject3DCentered :: FontShade -> FontObject -> Float -> Float -> Float -> IO ()
fontDrawObject3DCentered sh fo x y z = do
    -- stencil
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ fontobjectStencil fo

    -- size
    glUniform2f (fontShadeUniCharSize sh) (rTF $ fontobjectCharSizeX fo)
                                          (rTF $ fontobjectCharSizeY fo)
    -- color
    case fontobjectColor fo of
        FontColor r g b a -> 
            glUniform4f (fontShadeUniCharSize sh) r g b a

    fontDrawObject3DCentered' sh fo x y z


-- | if num calls >= 2, and font objects are similar (i.e. only differ by text)
fontDrawObject3DCentered' :: FontShade -> FontObject -> Float -> Float -> Float -> 
                                 IO ()
fontDrawObject3DCentered' sh fo x y z = do
    -- translate
    let tx = (-0.5) * fI (fontobjectLen fo)
        ty = (-0.5)
    glUniform2f (fontShadeUniTranslate sh) tx ty

    fontDrawObject3D' sh fo x y z


-- | if num calls >= 2, and font objects are similar (i.e. only differ by text)
fontDrawObject3D' :: FontShade -> FontObject -> Float -> Float -> Float -> IO ()
fontDrawObject3D' sh fo x y z = do
    -- vao 
    glBindVertexArrayOES $ fontobjectVAO fo
    
    -- pos
    glUniform3f (fontShadeUniPos sh) (rTF x) (rTF y) (rTF z)

    -- draw with ibo (4 + 2 vertices per character)
    glDrawElements gl_TRIANGLE_STRIP (fI $ fontobjectLen fo * 6) gl_UNSIGNED_SHORT nullPtr



