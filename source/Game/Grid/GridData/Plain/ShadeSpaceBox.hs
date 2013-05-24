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
module Game.Grid.GridData.Plain.ShadeSpaceBox
  (
    ShadeSpaceBox (..),

    loadShadeSpaceBox,
    unloadShadeSpaceBox,

  ) where

import MyPrelude
import File

import Game.Values

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade






data ShadeSpaceBox =
    ShadeSpaceBox
    {
        shadeSpaceBoxPrg :: !GLuint,
        shadeSpaceBoxUniProjModvMatrix :: !GLint,
        shadeSpaceBoxUniAlpha :: !GLint,
        shadeSpaceBoxUniPW :: !GLint,
        shadeSpaceBoxUniRadius :: !GLint,
        shadeSpaceBoxUniColor :: !GLint,
        shadeSpaceBoxVAO :: !GLuint,
        shadeSpaceBoxTex :: !GLuint

    }



loadShadeSpaceBox :: IO ShadeSpaceBox
loadShadeSpaceBox = do
    -- program
    vsh <- fileStaticData "shaders/SpaceBox.vsh"
    fsh <- fileStaticData "shaders/SpaceBox.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos") ] 
                              [ (tex0, "u_cubetex") ] 

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uRadius <- getUniformLocation prg "u_radius"
    uColor <- getUniformLocation prg "u_color"
    uPW <- getUniformLocation prg "u_pw"

    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
   
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let bytesize = (8 + 2 + 8) * 4
    allocaBytes bytesize $ \ptr -> do
        let r = 1 :: GLbyte 
        pokeByteOff ptr (0  +  0) (-r)
        pokeByteOff ptr (0  +  1) (-r)
        pokeByteOff ptr (0  +  2) (-r)
        pokeByteOff ptr (0  +  3) (0 :: GLbyte)
        
        pokeByteOff ptr (4  +  0) (-r)
        pokeByteOff ptr (4  +  1) (r)
        pokeByteOff ptr (4  +  2) (-r)
        pokeByteOff ptr (4  +  3) (0 :: GLbyte)

        pokeByteOff ptr (8  +  0) (-r)
        pokeByteOff ptr (8  +  1) (-r)
        pokeByteOff ptr (8  +  2) (r)
        pokeByteOff ptr (8  +  3) (0 :: GLbyte)

        pokeByteOff ptr (12 +  0) (-r)
        pokeByteOff ptr (12 +  1) (r)
        pokeByteOff ptr (12 +  2) (r)
        pokeByteOff ptr (12 +  3) (0 :: GLbyte)

        pokeByteOff ptr (16 +  0) (r)
        pokeByteOff ptr (16 +  1) (-r)
        pokeByteOff ptr (16 +  2) (r)
        pokeByteOff ptr (16 +  3) (0 :: GLbyte)

        pokeByteOff ptr (20 +  0) (r)
        pokeByteOff ptr (20 +  1) (r)
        pokeByteOff ptr (20 +  2) (r)
        pokeByteOff ptr (20 +  3) (0 :: GLbyte)

        pokeByteOff ptr (24 +  0) (r)
        pokeByteOff ptr (24 +  1) (-r)
        pokeByteOff ptr (24 +  2) (-r)
        pokeByteOff ptr (24 +  3) (0 :: GLbyte)

        pokeByteOff ptr (28 +  0) (r)
        pokeByteOff ptr (28 +  1) (r)
        pokeByteOff ptr (28 +  2) (-r)
        pokeByteOff ptr (28 +  3) (0 :: GLbyte)

        -- begin strip end
        pokeByteOff ptr (32 +  0) (r)
        pokeByteOff ptr (32 +  1) (r)
        pokeByteOff ptr (32 +  2) (-r)
        pokeByteOff ptr (32 +  3) (0 :: GLbyte)

        pokeByteOff ptr (36 +  0) (r)
        pokeByteOff ptr (36 +  1) (r)
        pokeByteOff ptr (36 +  2) (r)
        pokeByteOff ptr (36 +  3) (0 :: GLbyte)
        -- end strip end

        pokeByteOff ptr (40 +  0) (r)
        pokeByteOff ptr (40 +  1) (r)
        pokeByteOff ptr (40 +  2) (r)
        pokeByteOff ptr (40 +  3) (0 :: GLbyte)
        
        pokeByteOff ptr (44 +  0) (-r)
        pokeByteOff ptr (44 +  1) (r)
        pokeByteOff ptr (44 +  2) (r)
        pokeByteOff ptr (44 +  3) (0 :: GLbyte)

        pokeByteOff ptr (48 +  0) (r)
        pokeByteOff ptr (48 +  1) (r)
        pokeByteOff ptr (48 +  2) (-r)
        pokeByteOff ptr (48 +  3) (0 :: GLbyte)

        pokeByteOff ptr (52 +  0) (-r)
        pokeByteOff ptr (52 +  1) (r)
        pokeByteOff ptr (52 +  2) (-r)
        pokeByteOff ptr (52 +  3) (0 :: GLbyte)

        pokeByteOff ptr (56 +  0) (r)
        pokeByteOff ptr (56 +  1) (-r)
        pokeByteOff ptr (56 +  2) (-r)
        pokeByteOff ptr (56 +  3) (0 :: GLbyte)

        pokeByteOff ptr (60 +  0) (-r)
        pokeByteOff ptr (60 +  1) (-r)
        pokeByteOff ptr (60 +  2) (-r)
        pokeByteOff ptr (60 +  3) (0 :: GLbyte)

        pokeByteOff ptr (64 +  0) (r)
        pokeByteOff ptr (64 +  1) (-r)
        pokeByteOff ptr (64 +  2) (r)
        pokeByteOff ptr (64 +  3) (0 :: GLbyte)

        pokeByteOff ptr (68 +  0) (-r)
        pokeByteOff ptr (68 +  1) (-r)
        pokeByteOff ptr (68 +  2) (r)
        pokeByteOff ptr (68 +  3) (0 :: GLbyte)

        glBufferData gl_ARRAY_BUFFER (fI bytesize) ptr gl_STATIC_DRAW

    glVertexAttribPointer attPos 4 gl_BYTE gl_FALSE 0 nullPtr
    
    -- tex
    -- fixme: mipmapping?
    tex <- bindNewTex gl_TEXTURE_CUBE_MAP
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_MAG_FILTER $ 
                                        fI gl_LINEAR
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_MIN_FILTER $ 
                                        fI gl_LINEAR --_MIPMAP_LINEAR
    -- fixme: clamp to edge??
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_WRAP_S $ 
                                        fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_CUBE_MAP gl_TEXTURE_WRAP_T $ 
                                        fI gl_CLAMP_TO_EDGE

    path <- fileStaticData "Grid/Output/"
    forM_ [(gl_TEXTURE_CUBE_MAP_NEGATIVE_X, "spacebox_neg_x.png"),
           (gl_TEXTURE_CUBE_MAP_POSITIVE_X, "spacebox_pos_x.png"),
           (gl_TEXTURE_CUBE_MAP_NEGATIVE_Y, "spacebox_neg_y.png"),
           (gl_TEXTURE_CUBE_MAP_POSITIVE_Y, "spacebox_pos_y.png"),
           (gl_TEXTURE_CUBE_MAP_NEGATIVE_Z, "spacebox_neg_z.png"),
           (gl_TEXTURE_CUBE_MAP_POSITIVE_Z, "spacebox_pos_z.png") ] $ \(tgt, name) -> do
        loadTexPreMult tgt gl_RGBA $ path ++ "/" ++ name

    return $  ShadeSpaceBox
              {
                  shadeSpaceBoxPrg = prg,
                  shadeSpaceBoxUniProjModvMatrix = uProjModvMatrix,
                  shadeSpaceBoxUniAlpha = uAlpha,
                  shadeSpaceBoxUniRadius = uRadius,
                  shadeSpaceBoxUniPW = uPW,
                  shadeSpaceBoxUniColor = uColor,
                  shadeSpaceBoxTex = tex,
                  shadeSpaceBoxVAO = vao
              }


unloadShadeSpaceBox :: ShadeSpaceBox -> IO ()
unloadShadeSpaceBox sh = do
    return ()
