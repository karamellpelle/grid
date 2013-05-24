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
module Game.Grid.GridData.Fancy.ShadePath
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
        shadePathTex :: !GLuint
    }


loadShadePath :: IO ShadePath
loadShadePath = do
    vsh <- fileStaticData "shaders/Path.vsh"
    fsh <- fileStaticData "shaders/Path.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos"),
                                (attAntiPos, "a_antipos"),
                                (attTexCoord, "a_tex_coord"), 
                                (attDxDy, "a_dxdy") ] [
                                (tex0, "u_tex") ]

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
    
    -- VBO for attTexCoord and attDxDy
    vbo <- makeVBOTexCoordDxDy
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_FALSE 8 $ mkPtrGLvoid 0
    glVertexAttribPointer attDxDy 2 gl_SHORT gl_FALSE 8 $ mkPtrGLvoid 4
    
    -- create VBO for path1
    vbo' <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER (1 * 8 * 24) nullPtr gl_STREAM_DRAW

    -- tex
    tex <- makeTex "Grid/Output/path.png"

    return  ShadePath
            {
                shadePathPrg = prg,
                shadePathVAO = vao,
                shadePathUniProjModvMatrix = uProjModvMatrix,
                shadePathUniAlpha = uAlpha,
                shadePathUniColor = uColor,
                shadePathUniRadius = uRadius,
                shadePathPath1VBO = vbo',
                shadePathTex = tex
            }
    
    where
      makeTex path = do
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR_MIPMAP_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
          path <- fileStaticData path
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
          glGenerateMipmap gl_TEXTURE_2D
          return tex


unloadShadePath :: ShadePath -> IO ()
unloadShadePath sh = do
    return ()


makeVBOTexCoordDxDy :: IO GLuint
makeVBOTexCoordDxDy = do
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER (fI $ (valueGridMaxPathSize + 1) * 8 * 8) nullPtr gl_STATIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ helper 0 (valueGridMaxPathSize + 1)
    return vbo

    where
      helper i len = \ptr -> do
          if i == len then return ptr
            else do
              -- Pos
              pokeByteOff ptr (0  +  0) ( 0 :: GLushort)  --
              pokeByteOff ptr (0  +  2) ( 1 :: GLushort)  --
              pokeByteOff ptr (0  +  4) (-1 :: GLshort)   
              pokeByteOff ptr (0  +  6) ( 1 :: GLshort)
              pokeByteOff ptr (0  +  8) ( 0 :: GLushort)  --
              pokeByteOff ptr (0  + 10) ( 0 :: GLushort)  --
              pokeByteOff ptr (0  + 12) (-1 :: GLshort)
              pokeByteOff ptr (0  + 14) (-1 :: GLshort)
              pokeByteOff ptr (0  + 16) ( 1 :: GLushort)  --
              pokeByteOff ptr (0  + 18) ( 1 :: GLushort)  --
              pokeByteOff ptr (0  + 20) ( 0 :: GLshort)
              pokeByteOff ptr (0  + 22) ( 1 :: GLshort)
              pokeByteOff ptr (0  + 24) ( 1 :: GLushort)  --
              pokeByteOff ptr (0  + 26) ( 0 :: GLushort)  --
              pokeByteOff ptr (0  + 28) ( 0 :: GLshort)
              pokeByteOff ptr (0  + 30) (-1 :: GLshort)

              -- AntiPos
              pokeByteOff ptr (32 +  0) ( 1 :: GLushort)  --
              pokeByteOff ptr (32 +  2) ( 1 :: GLushort)  --
              pokeByteOff ptr (32 +  4) ( 0 :: GLshort)
              pokeByteOff ptr (32 +  6) (-1 :: GLshort)
              pokeByteOff ptr (32 +  8) ( 1 :: GLushort)  --
              pokeByteOff ptr (32 + 10) ( 0 :: GLushort)  --
              pokeByteOff ptr (32 + 12) ( 0 :: GLshort)
              pokeByteOff ptr (32 + 14) ( 1 :: GLshort)
              pokeByteOff ptr (32 + 16) ( 0 :: GLushort)  --
              pokeByteOff ptr (32 + 18) ( 1 :: GLushort)  --
              pokeByteOff ptr (32 + 20) (-1 :: GLshort)
              pokeByteOff ptr (32 + 22) (-1 :: GLshort)
              pokeByteOff ptr (32 + 24) ( 0 :: GLushort)  --
              pokeByteOff ptr (32 + 26) ( 0 :: GLushort)  --
              pokeByteOff ptr (32 + 28) (-1 :: GLshort)
              pokeByteOff ptr (32 + 30) ( 1 :: GLshort)


              helper (i + 1) len $ plusPtr ptr 64



attAntiPos :: GLuint
attAntiPos =
    attVec1

attDxDy :: GLuint
attDxDy =
    attCoord1

