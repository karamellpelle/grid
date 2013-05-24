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
module Game.Grid.GridData.Fancy.ShadeSpace
  (
    ShadeSpace (..),

    loadShadeSpace,
    unloadShadeSpace,

  ) where

import MyPrelude
import File
import Game.Values

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data ShadeSpace =
    ShadeSpace
    {
        shadeSpacePrg :: !GLuint,
        shadeSpaceUniModvInvMatrix :: !GLint,
        shadeSpaceUniAlpha :: !GLint,
        shadeSpaceUniScale :: !GLint,
        shadeSpaceUniScalePlus :: !GLint,
        shadeSpaceUniColor :: !GLint,
        shadeSpaceUniDot :: !GLint,
        shadeSpaceUniIntensity :: !GLint,
        shadeSpaceUniColorfy :: !GLint,
        shadeSpaceVAO :: !GLuint,
        shadeSpaceTex :: !GLuint

    }



loadShadeSpace :: IO ShadeSpace
loadShadeSpace = do
    -- program
    vsh <- fileStaticData "shaders/Space.vsh"
    fsh <- fileStaticData "shaders/Space.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos") ]
                              [ (tex0, "u_tex") ] 

    uModvInvMatrix <- getUniformLocation prg "u_modv_inv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uColor <- getUniformLocation prg "u_color"
    uScale <- getUniformLocation prg "u_scale"
    uScalePlus <- getUniformLocation prg "u_scale_plus"
    uDot <- getUniformLocation prg "u_dot"
    uIntensity <- getUniformLocation prg "u_intensity"
    uColorfy <- getUniformLocation prg "u_colorfy"

    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    vbo <- makeVBO
    glVertexAttribPointer attPos 3 gl_BYTE gl_TRUE 4 $ mkPtrGLvoid 0

    -- tex
    tex <- bindNewTex gl_TEXTURE_CUBE_MAP
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_MIN_FILTER $ fI gl_NEAREST
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE

    path <- fileStaticData "Grid/Output"
    forM_ [ (gl_TEXTURE_CUBE_MAP_NEGATIVE_X, "space_neg_x.png"),
            (gl_TEXTURE_CUBE_MAP_POSITIVE_X, "space_pos_x.png"),
            (gl_TEXTURE_CUBE_MAP_NEGATIVE_Y, "space_neg_y.png"),
            (gl_TEXTURE_CUBE_MAP_POSITIVE_Y, "space_pos_y.png"),
            (gl_TEXTURE_CUBE_MAP_NEGATIVE_Z, "space_neg_z.png"),
            (gl_TEXTURE_CUBE_MAP_POSITIVE_Z, "space_pos_z.png") ] $ \(tgt, file) -> do

        loadTexPreMult tgt {-gl_RGB565-} gl_RGBA $ path ++ "/" ++ file

    -- set non-empty scale
    glProgramUniform2fEXT prg uScale 1.0 1.0
    glProgramUniform2fEXT prg uScalePlus 1.0 1.0
   
    -- tmp: set const u_dot 
    glUniform4f uDot 0.1 0.15 0.1 0.0

    return $  ShadeSpace
              {
                  shadeSpacePrg = prg,
                  shadeSpaceUniModvInvMatrix = uModvInvMatrix,
                  shadeSpaceUniAlpha = uAlpha,
                  shadeSpaceUniColor = uColor,
                  shadeSpaceUniScale = uScale,
                  shadeSpaceUniScalePlus = uScalePlus,
                  shadeSpaceUniDot = uDot,
                  shadeSpaceUniIntensity = uIntensity,
                  shadeSpaceUniColorfy = uColorfy,
                  shadeSpaceTex = tex,
                  shadeSpaceVAO = vao
              }


unloadShadeSpace :: ShadeSpace -> IO ()
unloadShadeSpace sh = do
    return ()



--------------------------------------------------------------------------------
--  VBO

makeVBO :: IO GLuint
makeVBO = do
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 16 nullPtr gl_STATIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr (0  + 0)   (0x80 :: GLubyte) --(-1 :: GLbyte)
        pokeByteOff ptr (0  + 1)   (0x7f :: GLubyte) --(1 :: GLbyte)
        pokeByteOff ptr (0  + 2)   (0x80 :: GLubyte) --(-1 :: GLbyte)
        
        pokeByteOff ptr (4  + 0)   (0x80 :: GLubyte) --(-1 :: GLbyte)
        pokeByteOff ptr (4  + 1)   (0x80 :: GLubyte) --(-1 :: GLbyte)
        pokeByteOff ptr (4  + 2)   (0x80 :: GLubyte) --(-1 :: GLbyte)
        
        pokeByteOff ptr (8  + 0)   (0x7f :: GLubyte) --(1 :: GLbyte)
        pokeByteOff ptr (8  + 1)   (0x7f :: GLubyte) --(1 :: GLbyte)
        pokeByteOff ptr (8  + 2)   (0x80 :: GLubyte) --(-1 :: GLbyte)
        
        pokeByteOff ptr (12 + 0)   (0x7f :: GLubyte) --(1 :: GLbyte)
        pokeByteOff ptr (12 + 1)   (0x80 :: GLubyte) --(-1 :: GLbyte)
        pokeByteOff ptr (12 + 2)   (0x80 :: GLubyte) --(-1 :: GLbyte)
    
    return vbo
