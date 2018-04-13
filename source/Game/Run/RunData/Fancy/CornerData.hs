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
module Game.Run.RunData.Fancy.CornerData
  (
    CornerData (..),
   
    loadCornerData,
    unloadCornerData,

  ) where

import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade




data CornerData =
    CornerData
    {
        cornerdataTex :: !GLuint,
        cornerdataVAO3D :: !GLuint,
        cornerdataVAO2D :: !GLuint,
        cornerdataVBO3D :: !GLuint,
        cornerdataVBO2D :: !GLuint
    }



loadCornerData :: IO CornerData
loadCornerData = do
    -- vbo dynamic (pos)
    (vao3D, vbo3D) <- makeVAOVBO3D
    (vao2D, vbo2D) <- makeVAOVBO2D
    
    -- tex
    path <- fileStaticData "Run/Scene/corners_tex.png"
    tex <- loadTex path

    return  CornerData
            {
                cornerdataTex = tex,
                cornerdataVAO3D = vao3D,
                cornerdataVAO2D = vao2D,
                cornerdataVBO3D = vbo3D,
                cornerdataVBO2D = vbo2D

            }

    where
      loadTex path = do
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_NEAREST_MIPMAP_NEAREST
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
          glGenerateMipmap gl_TEXTURE_2D
          return tex
    
          

unloadCornerData :: CornerData -> IO ()
unloadCornerData sh = do
    return ()



--------------------------------------------------------------------------------
--  


makeVAOVBO3D :: IO (GLuint, GLuint)
makeVAOVBO3D = do
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord


    -- Pos
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glVertexAttribPointer attPos 3 gl_FLOAT gl_FALSE 12 $ mkPtrGLvoid 0
    glBufferData gl_ARRAY_BUFFER 288 nullPtr gl_STREAM_DRAW

    -- TexCoord
    _ <- bindNewBuf gl_ARRAY_BUFFER
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 4 $ mkPtrGLvoid 0
    glBufferData gl_ARRAY_BUFFER 96 nullPtr gl_STATIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        -- 0
        pokeByteOff ptr 0  (0x0000 :: GLushort)
        pokeByteOff ptr 2  (0xffff :: GLushort)
        
        pokeByteOff ptr 4  (0x0000 :: GLushort)
        pokeByteOff ptr 6  (0x8000 :: GLushort)
        
        pokeByteOff ptr 8  (0x8000 :: GLushort)
        pokeByteOff ptr 10 (0xffff :: GLushort)
        
        pokeByteOff ptr 12 (0x8000 :: GLushort)
        pokeByteOff ptr 14 (0x8000 :: GLushort)
       
        -- 1
        pokeByteOff ptr 24 (0x8000 :: GLushort)
        pokeByteOff ptr 26 (0xffff :: GLushort)
        
        pokeByteOff ptr 28 (0x8000 :: GLushort)
        pokeByteOff ptr 30 (0x8000 :: GLushort)
        
        pokeByteOff ptr 32 (0xffff :: GLushort)
        pokeByteOff ptr 34 (0xffff :: GLushort)
        
        pokeByteOff ptr 36 (0xffff :: GLushort)
        pokeByteOff ptr 38 (0x8000 :: GLushort)
        
        -- 2
        pokeByteOff ptr 48 (0x0000 :: GLushort)
        pokeByteOff ptr 50 (0x8000 :: GLushort)
        
        pokeByteOff ptr 52 (0x0000 :: GLushort)
        pokeByteOff ptr 54 (0x0000 :: GLushort)
        
        pokeByteOff ptr 56 (0x8000 :: GLushort)
        pokeByteOff ptr 58 (0x8000 :: GLushort)
        
        pokeByteOff ptr 60 (0x8000 :: GLushort)
        pokeByteOff ptr 62 (0x0000 :: GLushort)
        
        -- 3
        pokeByteOff ptr 72 (0x8000 :: GLushort)
        pokeByteOff ptr 74 (0x8000 :: GLushort)
        
        pokeByteOff ptr 76 (0x8000 :: GLushort)
        pokeByteOff ptr 78 (0x0000 :: GLushort)
        
        pokeByteOff ptr 80 (0xffff :: GLushort)
        pokeByteOff ptr 82 (0x8000 :: GLushort)
        
        pokeByteOff ptr 84 (0xffff :: GLushort)
        pokeByteOff ptr 86 (0x0000 :: GLushort)

   
    return (vao, vbo)



makeVAOVBO2D :: IO (GLuint, GLuint)
makeVAOVBO2D = do
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glVertexAttribPointer attPos 2 gl_FLOAT gl_FALSE 12 $ mkPtrGLvoid 0
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 12 $ mkPtrGLvoid 8
    glBufferData gl_ARRAY_BUFFER 288 nullPtr gl_DYNAMIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        -- 0
        pokeByteOff ptr 8  (0x0000 :: GLushort)
        pokeByteOff ptr 10 (0xffff :: GLushort)
        
        pokeByteOff ptr 20 (0x0000 :: GLushort)
        pokeByteOff ptr 22 (0x8000 :: GLushort)
        
        pokeByteOff ptr 32 (0x8000 :: GLushort)
        pokeByteOff ptr 34 (0xffff :: GLushort)
        
        pokeByteOff ptr 44 (0x8000 :: GLushort)
        pokeByteOff ptr 46 (0x8000 :: GLushort)
       
        -- 1
        pokeByteOff ptr 80 (0x8000 :: GLushort)
        pokeByteOff ptr 82 (0xffff :: GLushort)
        
        pokeByteOff ptr 92 (0x8000 :: GLushort)
        pokeByteOff ptr 94 (0x8000 :: GLushort)
        
        pokeByteOff ptr 104(0xffff :: GLushort)
        pokeByteOff ptr 106(0xffff :: GLushort)
        
        pokeByteOff ptr 116(0xffff :: GLushort)
        pokeByteOff ptr 118(0x8000 :: GLushort)
        
        -- 2
        pokeByteOff ptr 152(0x0000 :: GLushort)
        pokeByteOff ptr 154(0x8000 :: GLushort)
        
        pokeByteOff ptr 164(0x0000 :: GLushort)
        pokeByteOff ptr 166(0x0000 :: GLushort)
        
        pokeByteOff ptr 176(0x8000 :: GLushort)
        pokeByteOff ptr 178(0x8000 :: GLushort)
        
        pokeByteOff ptr 188(0x8000 :: GLushort)
        pokeByteOff ptr 190(0x0000 :: GLushort)
        
        -- 3
        pokeByteOff ptr 224(0x8000 :: GLushort)
        pokeByteOff ptr 226(0x8000 :: GLushort)
        
        pokeByteOff ptr 236(0x8000 :: GLushort)
        pokeByteOff ptr 238(0x0000 :: GLushort)
        
        pokeByteOff ptr 248(0xffff :: GLushort)
        pokeByteOff ptr 250(0x8000 :: GLushort)
        
        pokeByteOff ptr 260(0xffff :: GLushort)
        pokeByteOff ptr 262(0x0000 :: GLushort)
   
    return (vao, vbo)



