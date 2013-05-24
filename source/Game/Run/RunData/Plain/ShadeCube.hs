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
module Game.Run.RunData.Plain.ShadeCube
  (
    ShadeCube (..),
   
    loadShadeCube,
    unloadShadeCube,

  ) where

import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade





data ShadeCube =
    ShadeCube
    {
        shadeCubePrg :: !GLuint,
        shadeCubeUniProjModvMatrix :: !GLint,
        shadeCubeUniAlpha :: !GLint,
        shadeCubeUniNormalMatrix :: !GLint,
        shadeCubeUniRefDir :: !GLint,
        shadeCubeVAO :: !GLuint,
        shadeCubeVBODynamic :: !GLuint
    }


loadShadeCube :: IO ShadeCube
loadShadeCube = do
    vsh <- fileStaticData "shaders/RunCube.vsh"
    fsh <- fileStaticData "shaders/RunCube.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attNormal, "a_normal"), 
                                (attTexCoord, "a_texcoord") ] [

                                (tex0, "u_tex") ]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uNormalMatrix <- getUniformLocation prg "u_normal_matrix"
    uRefDir <- getUniformLocation prg "u_ref_dir"

    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attNormal
    glEnableVertexAttribArray attTexCoord

    vboDynamic <- makeVBODynamic

    -- tmp
    glProgramUniform3fEXT prg uRefDir 0.0 0.707 0.707

    return $  ShadeCube
              {
                shadeCubePrg = prg,
                shadeCubeUniProjModvMatrix = uProjModvMatrix,
                shadeCubeUniAlpha = uAlpha,
                shadeCubeUniNormalMatrix = uNormalMatrix,
                shadeCubeUniRefDir = uRefDir,
                shadeCubeVAO = vao,
                shadeCubeVBODynamic = vboDynamic
              }


unloadShadeCube :: ShadeCube -> IO ()
unloadShadeCube sh = do
    return ()


makeVBODynamic :: IO GLuint
makeVBODynamic = do

    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let bytesize = 4 * 4 * 20
    glBufferData gl_ARRAY_BUFFER bytesize nullPtr gl_DYNAMIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        -- 0x80 0x00 0x00 0x00
        pokeByteOff ptr (0  +  12) (0x80 :: GLbyte)
        pokeByteOff ptr (0  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (0  +  14) (0x00 :: GLbyte )
        pokeByteOff ptr (0  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (20 +  12) (0x80 :: GLbyte)
        pokeByteOff ptr (20 +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (20 +  14) (0x00  :: GLbyte)
        pokeByteOff ptr (20 +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (40 +  12) (0x80 :: GLbyte)
        pokeByteOff ptr (40 +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (40 +  14) (0x00  :: GLbyte)
        pokeByteOff ptr (40 +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (60 +  12) (0x80 :: GLbyte)
        pokeByteOff ptr (60 +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (60 +  14) (0x00  :: GLbyte)
        pokeByteOff ptr (60 +  15) (0x00 :: GLbyte)

        -- 0x00 0x00 0x7f 0x00
        pokeByteOff ptr (80 +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (80 +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (80 +  14) (0x7f  :: GLbyte)
        pokeByteOff ptr (80 +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (100  +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (100  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (100  +  14) (0x7f  :: GLbyte)
        pokeByteOff ptr (100  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (120  +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (120  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (120  +  14) (0x7f  :: GLbyte)
        pokeByteOff ptr (120  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (140  +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (140  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (140  +  14) (0x7f  :: GLbyte)
        pokeByteOff ptr (140  +  15) (0x00 :: GLbyte)


        -- 0x7f 0x00 0x00 0x00
        pokeByteOff ptr (160  +  12) (0x7f :: GLbyte)
        pokeByteOff ptr (160  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (160  +  14) (0x00  :: GLbyte)
        pokeByteOff ptr (160  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (180  +  12) (0x7f :: GLbyte)
        pokeByteOff ptr (180  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (180  +  14) (0x00  :: GLbyte)
        pokeByteOff ptr (180  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (200  +  12) (0x7f :: GLbyte)
        pokeByteOff ptr (200  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (200  +  14) (0x00  :: GLbyte)
        pokeByteOff ptr (200  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (220  +  12) (0x7f :: GLbyte)
        pokeByteOff ptr (220  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (220  +  14) (0x00  :: GLbyte)
        pokeByteOff ptr (220  +  15) (0x00 :: GLbyte)

        -- 0x00 0x00 0x80 0x00
        pokeByteOff ptr (240  +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (240  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (240  +  14) (0x80  :: GLbyte)
        pokeByteOff ptr (240  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (260  +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (260  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (260  +  14) (0x80  :: GLbyte)
        pokeByteOff ptr (260  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (280  +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (280  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (280  +  14) (0x80  :: GLbyte)
        pokeByteOff ptr (280  +  15) (0x00 :: GLbyte)

        pokeByteOff ptr (300  +  12) (0x00 :: GLbyte)
        pokeByteOff ptr (300  +  13) (0x00 :: GLbyte)
        pokeByteOff ptr (300  +  14) (0x80  :: GLbyte)
        pokeByteOff ptr (300  +  15) (0x00 :: GLbyte)

        
    glVertexAttribPointer attPos 
                          3 gl_FLOAT gl_FALSE (20) (mkPtrGLvoid 0)
    glVertexAttribPointer attNormal 
                          4 gl_BYTE gl_TRUE (20) (mkPtrGLvoid 12)
    glVertexAttribPointer attTexCoord
                          2 gl_UNSIGNED_SHORT gl_TRUE (20) (mkPtrGLvoid 16) 

    return vbo
