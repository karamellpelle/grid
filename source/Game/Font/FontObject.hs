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
module Game.Font.FontObject
  (
    FontObject (..),
    makeFontObject,
    destroyFontObject,
    writeFontObject,
    -- changeFontObjectColor, ...

  ) where

import MyPrelude
import Game.Font.FontShade
import Game.Font.FontData
import Game.Font.FontColor
import Game.Font.Buffer

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade

-- note: since all fonts are drawn with one given ibo, the possible string length
--       is bounded

--------------------------------------------------------------------------------
--  


data FontObject =
    FontObject
    {
        fontobjectVAO :: !GLuint,
        fontobjectVBO :: !GLuint,
        fontobjectLen :: !UInt,
        -- changable properties
        fontobjectColor :: !FontColor,
        fontobjectCharSizeX :: !Float,
        fontobjectCharSizeY :: !Float,
        fontobjectStencil :: !GLuint
    }



--------------------------------------------------------------------------------
--  make / destroy


makeFontObject2D :: FontShade -> FontData -> Float -> FontColor -> String -> 
                    IO FontObject
makeFontObject2D =
    makeFontObject writePosStencilCoord2D


makeFontObject3D :: FontShade -> FontData -> Float -> FontColor -> String -> 
                    IO FontObject
makeFontObject3D =
    makeFontObject writePosStencilCoord3D



makeFontObject :: (FontData -> Ptr a -> String -> IO UInt) -> 
                  FontShade -> FontData -> Float -> FontColor -> String ->
                  IO FontObject
makeFontObject writePosStencilCoord sh fd size color str = do
    let len = fI $ length str

    -- vao 
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attStencilCoord

    -- vbo
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let bytesize = len * (4 * 8)
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_DYNAMIC_DRAW
    glVertexAttribPointer attPos 2 gl_UNSIGNED_SHORT gl_FALSE 8 $ mkPtrGLvoid 0
    glVertexAttribPointer attStencilCoord 2 gl_UNSIGNED_SHORT gl_FALSE 8 $ mkPtrGLvoid 4
    writeBuf gl_ARRAY_BUFFER $ \ptr -> 
        writePosLen ptr valueFontMaxCharacters

    return $  FontObject
              {
                  fontobjectVAO = vao,
                  fontobjectVBO = vbo,
                  fontobjectLen = len,
                  fontobjectColor = color,
                  fontobjectCharSizeX = size * fontdataCharAspect fd,
                  fontobjectCharSizeY = size,
                  fontobjectStencil = fontdataStencil fd

              }


destroyFontObject :: FontObject -> IO ()
destroyFontObject fo = do
    delBuf $ fontobjectVBO fo
    delBuf $ fontobjectVAO fo



--------------------------------------------------------------------------------
--  write


writeFontObject2D :: FontShade -> FontData -> FontObject -> String -> IO FontObject
writeFontObject2D =
    writeFontObject writeStencilCoord2D writePosStencilCoord2D


writeFontObject3D :: FontShade -> FontData -> FontObject -> String -> IO FontObject
writeFontObject3D =
    writeFontObject writeStencilCoord3D writePosStencilCoord3D


writeFontObject :: (FontData -> Ptr GLvoid -> String -> IO UInt) -> 
                   (FontData -> Ptr GLvoid -> String -> IO UInt) ->
                   FontShade -> FontData -> FontObject -> String -> IO FontObject
writeFontObject writeStencilCoord writePosStencilCoord sh fd fo str = do
    let len = fontobjectLen fo
        len' = fI $ length str

    -- create greater VBO, if necessary
    if len' <= len
      
      -- pos is written, write stencil coords
      then do
        glBindBuffer gl_ARRAY_BUFFER $ fontobjectVBO fo
        writeBuf gl_ARRAY_BUFFER $ \ptr -> 
            writeStencilCoord fd ptr str        

      -- create new buffer, write pos and stencil coords
      else do
        glBindBuffer gl_ARRAY_BUFFER $ fontobjectVBO fo
        let bytesize = len' * (4 * 8)
        glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_DYNAMIC_DRAW
        writeBuf gl_ARRAY_BUFFER $ \ptr -> 
            writePosStencilCoord fd ptr str     
    
    return $ fo { fontobjectLen = len' }


