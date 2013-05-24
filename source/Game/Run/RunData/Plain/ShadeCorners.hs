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
module Game.Run.RunData.Plain.ShadeCorners
  (
    ShadeCorners (..),
   
    loadShadeCorners,
    unloadShadeCorners,

  ) where

import MyPrelude
import File


import OpenGL
import OpenGL.Helpers
import OpenGL.Shade




data ShadeCorners =
    ShadeCorners
    {
        shadeCornersPrg :: !GLuint,
        shadeCornersUniProjModvMatrix :: !GLint,
        shadeCornersUniAlpha :: !GLint,
        shadeCornersTex :: !GLuint,
        shadeCornersVAO3D :: !GLuint,
        shadeCornersVBOPos3D :: !GLuint,
        shadeCornersVAO2D :: !GLuint,
        shadeCornersVBO2D :: !GLuint
    }



loadShadeCorners :: IO ShadeCorners
loadShadeCorners = do
    vsh <- fileStaticData "shaders/RunCorners.vsh"
    fsh <- fileStaticData "shaders/RunCorners.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attTexCoord, "a_texcoord") ] [

                                (tex0, "u_tex")]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"


    -- vbo dynamic (pos)
    (vao3D, vbo3D) <- makeVAOVBOPos3D
    (vao2D, vbo2D) <- makeVAOVBO2D

    -- tex
    path <- fileStaticData "Run/Output/corners_tex.png"
    tex <- loadTex path

    return  ShadeCorners
            {
                shadeCornersPrg = prg,
                shadeCornersUniProjModvMatrix = uProjModvMatrix,
                shadeCornersUniAlpha = uAlpha,
                shadeCornersTex = tex,
                shadeCornersVAO3D = vao3D,
                shadeCornersVBOPos3D = vbo3D,
                shadeCornersVAO2D = vao2D,
                shadeCornersVBO2D = vbo2D

            }

    where
      loadTex path = do
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR_MIPMAP_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path
          glGenerateMipmap gl_TEXTURE_2D
          return tex
    
          

unloadShadeCorners :: ShadeCorners -> IO ()
unloadShadeCorners sh = do
    return ()



--------------------------------------------------------------------------------
--  


makeVAOVBOPos3D :: IO (GLuint, GLuint)
makeVAOVBOPos3D = do
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- Pos
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 168 nullPtr gl_STREAM_DRAW
    glVertexAttribPointer attPos 3 gl_FLOAT gl_FALSE 0 $ mkPtrGLvoid 0

    -- TexCoord
    _ <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 56 nullPtr gl_STATIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr 0  (0x0000 :: GLushort)
        pokeByteOff ptr 2  (0x8000 :: GLushort)
        
        pokeByteOff ptr 4  (0x0000 :: GLushort)
        pokeByteOff ptr 6  (0x0000 :: GLushort)
        
        pokeByteOff ptr 8  (0x8000 :: GLushort)
        pokeByteOff ptr 10 (0x0000 :: GLushort)
        
        pokeByteOff ptr 12 (0xffff :: GLushort)
        pokeByteOff ptr 14 (0x0000 :: GLushort)
        
        pokeByteOff ptr 16 (0x8000 :: GLushort)
        pokeByteOff ptr 18 (0x0000 :: GLushort)
        
        pokeByteOff ptr 20 (0xffff :: GLushort)
        pokeByteOff ptr 22 (0x8000 :: GLushort)
        
        pokeByteOff ptr 24 (0xffff :: GLushort)
        pokeByteOff ptr 26 (0x8000 :: GLushort)
        
        pokeByteOff ptr 28 (0xffff :: GLushort)
        pokeByteOff ptr 30 (0x8000 :: GLushort)
        
        pokeByteOff ptr 32 (0xffff :: GLushort)
        pokeByteOff ptr 34 (0x8000 :: GLushort)
        
        pokeByteOff ptr 36 (0xffff :: GLushort)
        pokeByteOff ptr 38 (0xffff :: GLushort)
        
        pokeByteOff ptr 40 (0x8000 :: GLushort)
        pokeByteOff ptr 42 (0xffff :: GLushort)
        
        pokeByteOff ptr 44 (0x0000 :: GLushort)
        pokeByteOff ptr 46 (0xffff :: GLushort)
        
        pokeByteOff ptr 48 (0x8000 :: GLushort)
        pokeByteOff ptr 50 (0xffff :: GLushort)
        
        pokeByteOff ptr 52 (0x0000 :: GLushort)
        pokeByteOff ptr 54 (0x8000 :: GLushort)
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 0 $ mkPtrGLvoid 0
   
    return (vao, vbo)



makeVAOVBO2D :: IO (GLuint, GLuint)
makeVAOVBO2D = do
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 224 nullPtr gl_DYNAMIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr (0  +  8) (0.0 :: GLfloat)
        pokeByteOff ptr (0  + 12) (0x0000 :: GLushort)
        pokeByteOff ptr (0  + 14) (0x8000 :: GLushort)
        
        pokeByteOff ptr (16 +  8) (0.0 :: GLfloat)
        pokeByteOff ptr (16 + 12) (0x0000 :: GLushort)
        pokeByteOff ptr (16 + 14) (0x0000 :: GLushort)
        
        pokeByteOff ptr (32 +  8) (0.0 :: GLfloat)
        pokeByteOff ptr (32 + 12) (0x8000 :: GLushort)
        pokeByteOff ptr (32 + 14) (0x0000 :: GLushort)
        
        pokeByteOff ptr (48 +  8) (0.0 :: GLfloat)
        pokeByteOff ptr (48 + 12) (0xffff :: GLushort)
        pokeByteOff ptr (48 + 14) (0x0000 :: GLushort)
        
        pokeByteOff ptr (64 +  8) (0.0 :: GLfloat)
        pokeByteOff ptr (64 + 12) (0x8000 :: GLushort)
        pokeByteOff ptr (64 + 14) (0x0000 :: GLushort)
        
        pokeByteOff ptr (80 +  8) (0.0 :: GLfloat)
        pokeByteOff ptr (80 + 12) (0xffff :: GLushort)
        pokeByteOff ptr (80 + 14) (0x8000 :: GLushort)
        
        pokeByteOff ptr (96 +  8) (0.0 :: GLfloat)
        pokeByteOff ptr (96 + 12) (0xffff :: GLushort)
        pokeByteOff ptr (96 + 14) (0x8000 :: GLushort)
        
        pokeByteOff ptr (112+  8) (0.0 :: GLfloat)
        pokeByteOff ptr (112+ 12) (0xffff :: GLushort)
        pokeByteOff ptr (112+ 14) (0x8000 :: GLushort)
        
        pokeByteOff ptr (128+  8) (0.0 :: GLfloat)
        pokeByteOff ptr (128+ 12) (0xffff :: GLushort)
        pokeByteOff ptr (128+ 14) (0x8000 :: GLushort)
        
        pokeByteOff ptr (144+  8) (0.0 :: GLfloat)
        pokeByteOff ptr (144+ 12) (0xffff :: GLushort)
        pokeByteOff ptr (144+ 14) (0xffff :: GLushort)
        
        pokeByteOff ptr (160+  8) (0.0 :: GLfloat)
        pokeByteOff ptr (160+ 12) (0x8000 :: GLushort)
        pokeByteOff ptr (160+ 14) (0xffff :: GLushort)
        
        pokeByteOff ptr (176+  8) (0.0 :: GLfloat)
        pokeByteOff ptr (176+ 12) (0x0000 :: GLushort)
        pokeByteOff ptr (176+ 14) (0xffff :: GLushort)
        
        pokeByteOff ptr (192+  8) (0.0 :: GLfloat)
        pokeByteOff ptr (192+ 12) (0x8000 :: GLushort)
        pokeByteOff ptr (192+ 14) (0xffff :: GLushort)
        
        pokeByteOff ptr (208+  8) (0.0 :: GLfloat)
        pokeByteOff ptr (208+ 12) (0x0000 :: GLushort)
        pokeByteOff ptr (208+ 14) (0x8000 :: GLushort)
        
   
    glVertexAttribPointer attPos 3 gl_FLOAT gl_FALSE 16 $ mkPtrGLvoid 0
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 16 $ mkPtrGLvoid 12
    return (vao, vbo)


