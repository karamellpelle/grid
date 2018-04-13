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
module Game.Grid.GridData.Plain.ShadePath
  (
    ShadePath (..),
    
    loadShadePath,
    unloadShadePath,

    attPos,
    attAntiPos,
    attDxDy,

  ) where

import MyPrelude
import File

import Game.Values

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data ShadePath = 
    ShadePath
    {
        shadePathPrg :: !GLuint,
        shadePathVAO :: !GLuint,
        shadePathUniProjModvMatrix :: !GLint,
        shadePathUniAlpha :: !GLint,
        shadePathUniColor :: !GLint,
        shadePathUniRadius :: !GLint,
        shadePathPath1VBO :: !GLuint,
        shadePathTex :: !GLuint,
        shadePathTexFat :: !GLuint
    }


loadShadePath :: IO ShadePath
loadShadePath = do
    vsh <- fileStaticData "shaders/Path.vsh"
    fsh <- fileStaticData "shaders/Path.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos"),
                                (attAntiPos, "a_antipos"),
                                (attDxDy, "a_dxdy"),
                                (attTexCoord, "a_texcoord") ]

                                [ (tex0, "u_tex") ]
    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uColor <- getUniformLocation prg "u_color"
    uRadius <- getUniformLocation prg "u_radius"

    -- create ibo
    vao <- bindNewVAO
    ibo <- makeGroupIBO 8 (valueGridMaxPathSize + 1)
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER ibo
    
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attAntiPos
    glEnableVertexAttribArray attTexCoord
    glEnableVertexAttribArray attDxDy

    -- attributes texcoord, dxdy
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let attribssize = 4
        elemsize = 8 * attribssize
        arraysize = (valueGridMaxPathSize + 1) * elemsize
    allocaBytes (fI arraysize) $ \ptr -> do
        writeShadePathVBO ptr 0 (valueGridMaxPathSize + 1)
        glBufferData gl_ARRAY_BUFFER (fI arraysize) (ptr :: Ptr GLvoid) gl_STATIC_DRAW

    -- fixme: 4-byte alignment for each attribute!
    glVertexAttribPointer attDxDy 2 gl_BYTE gl_TRUE (fI attribssize) 
                          (mkPtrGLvoid 0)
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_BYTE gl_TRUE (fI attribssize)
                          (mkPtrGLvoid 2)

    -- create vbo for path1
    vbo' <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER (1 * 8 * 24) nullPtr gl_STREAM_DRAW

    -- tex
    tex <- makeTex "Grid/Output/path.png"
    texFat <- makeTex "Grid/Output/path_fat.png"

    return  ShadePath
            {
                shadePathPrg = prg,
                shadePathVAO = vao,
                shadePathUniProjModvMatrix = uProjModvMatrix,
                shadePathUniAlpha = uAlpha,
                shadePathUniColor = uColor,
                shadePathUniRadius = uRadius,
                shadePathPath1VBO = vbo',
                shadePathTex = tex,
                shadePathTexFat = texFat
            }
    
    where
      makeTex path = do
          -- use mipmapping?
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ 
                          fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ 
                          fI gl_LINEAR_MIPMAP_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
          path <- fileStaticData path
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path
          glGenerateMipmap gl_TEXTURE_2D -- ?
          return tex


unloadShadePath :: ShadePath -> IO ()
unloadShadePath sh = do
    return ()


writeShadePathVBO :: Ptr GLvoid -> UInt -> UInt -> IO ()
writeShadePathVBO ptr i len = do
    if i == len 
      then return ()
      else do
        let size = 8 * 4
        
        -- pos
        pokeByteOffGLbyte  ptr (i * size +  0) (0x80)
        pokeByteOffGLbyte  ptr (i * size +  1) (0x7f)
        pokeByteOffGLubyte ptr (i * size +  2) (0x00)
        pokeByteOffGLubyte ptr (i * size +  3) (0xff)
        
        pokeByteOffGLbyte  ptr (i * size +  4) (0x80)
        pokeByteOffGLbyte  ptr (i * size +  5) (0x80)
        pokeByteOffGLubyte ptr (i * size +  6) (0x00)
        pokeByteOffGLubyte ptr (i * size +  7) (0x00)
        
        pokeByteOffGLbyte  ptr (i * size +  8) (0x00)
        pokeByteOffGLbyte  ptr (i * size +  9) (0x7f)
        pokeByteOffGLubyte ptr (i * size + 10) (0x80)
        pokeByteOffGLubyte ptr (i * size + 11) (0xff)
        
        pokeByteOffGLbyte  ptr(i * size + 12) (0x00)
        pokeByteOffGLbyte  ptr (i * size + 13) (0x80)
        pokeByteOffGLubyte ptr (i * size + 14) (0x80)
        pokeByteOffGLubyte ptr (i * size + 15) (0x00)
        
        -- pos'
        pokeByteOffGLbyte  ptr (i * size + 16) (0x00)
        pokeByteOffGLbyte  ptr (i * size + 17) (0x80)
        pokeByteOffGLubyte ptr (i * size + 18) (0x80)
        pokeByteOffGLubyte ptr (i * size + 19) (0xff)
        
        pokeByteOffGLbyte  ptr (i * size + 20) (0x00)
        pokeByteOffGLbyte  ptr (i * size + 21) (0x7f)
        pokeByteOffGLubyte ptr (i * size + 22) (0x80)
        pokeByteOffGLubyte ptr (i * size + 23) (0x00)
        
        pokeByteOffGLbyte  ptr (i * size + 24) (0x80)
        pokeByteOffGLbyte  ptr (i * size + 25) (0x80)
        pokeByteOffGLubyte ptr (i * size + 26) (0xff)
        pokeByteOffGLubyte ptr (i * size + 27) (0xff)
        
        pokeByteOffGLbyte  ptr (i * size + 28) (0x80)
        pokeByteOffGLbyte  ptr (i * size + 29) (0x7f)
        pokeByteOffGLubyte ptr (i * size + 30) (0xff)
        pokeByteOffGLubyte ptr (i * size + 31) (0x00)
      
        writeShadePathVBO ptr (i + 1) len

    where
      pokeByteOffGLbyte ptr ix a =
          pokeByteOff (castPtr ptr) (fI ix) (a :: GLbyte)
      pokeByteOffGLubyte ptr ix a =
          pokeByteOff (castPtr ptr) (fI ix) (a :: GLubyte)



attAntiPos :: GLuint
attAntiPos =
    3

attDxDy :: GLuint
attDxDy =
    attCoord1

