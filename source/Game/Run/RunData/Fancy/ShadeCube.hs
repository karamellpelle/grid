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
module Game.Run.RunData.Fancy.ShadeCube
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
        shadeCubeUniAlpha :: !GLint,
        shadeCubeUniProjModvMatrix :: !GLint,
        shadeCubeUniNormalMatrix :: !GLint,
        shadeCubeUniRefDir :: !GLint,
        shadeCubeVAO :: !GLuint,
        shadeCubeVBODynamic :: !GLuint
    }


loadShadeCube :: IO ShadeCube
loadShadeCube = do
    vsh <- fileStaticData "shaders/Cube.vsh"
    fsh <- fileStaticData "shaders/Cube.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attNormal, "a_normal"), 
                                (attTexCoord, "a_texcoord") ]
                             [  (tex0, "u_tex") ]

    uAlpha <- getUniformLocation prg "u_alpha"
    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uNormalMatrix <- getUniformLocation prg "u_normal_matrix"
    uRefDir <- getUniformLocation prg "u_ref_dir"

    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attNormal
    glEnableVertexAttribArray attTexCoord

    vboDynamic <- makeVBODynamic

    -- tmp, bind RefDir
    glProgramUniform3fEXT prg uRefDir (0.0) (0.2425) (0.9701)

    return  ShadeCube
            {
                shadeCubePrg = prg,
                shadeCubeUniAlpha = uAlpha,
                shadeCubeUniProjModvMatrix = uProjModvMatrix,
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
    glBufferData gl_ARRAY_BUFFER (4 * 4 * 20) nullPtr gl_DYNAMIC_DRAW

    -- fill with Normal
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do

        -- (-1 0 0)
        pokeByteOff ptr (0  +  12) (-1 :: GLbyte)
        pokeByteOff ptr (0  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (0  +  14) ( 0 :: GLbyte )

        pokeByteOff ptr (20 +  12) (-1 :: GLbyte)
        pokeByteOff ptr (20 +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (20 +  14) ( 0 :: GLbyte)

        pokeByteOff ptr (40 +  12) (-1 :: GLbyte)
        pokeByteOff ptr (40 +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (40 +  14) ( 0 :: GLbyte)

        pokeByteOff ptr (60 +  12) (-1 :: GLbyte)
        pokeByteOff ptr (60 +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (60 +  14) ( 0 :: GLbyte)

        -- (0 0 1)
        pokeByteOff ptr (80 +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (80 +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (80 +  14) ( 1 :: GLbyte)

        pokeByteOff ptr (100  +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (100  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (100  +  14) ( 1 :: GLbyte)

        pokeByteOff ptr (120  +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (120  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (120  +  14) ( 1 :: GLbyte)

        pokeByteOff ptr (140  +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (140  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (140  +  14) ( 1 :: GLbyte)


        -- (1 0 0)
        pokeByteOff ptr (160  +  12) ( 1 :: GLbyte)
        pokeByteOff ptr (160  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (160  +  14) ( 0 :: GLbyte)

        pokeByteOff ptr (180  +  12) ( 1 :: GLbyte)
        pokeByteOff ptr (180  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (180  +  14) ( 0 :: GLbyte)

        pokeByteOff ptr (200  +  12) ( 1 :: GLbyte)
        pokeByteOff ptr (200  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (200  +  14) ( 0 :: GLbyte)

        pokeByteOff ptr (220  +  12) ( 1 :: GLbyte)
        pokeByteOff ptr (220  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (220  +  14) ( 0 :: GLbyte)

        -- (0 0 -1)
        pokeByteOff ptr (240  +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (240  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (240  +  14) (-1 :: GLbyte)

        pokeByteOff ptr (260  +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (260  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (260  +  14) (-1 :: GLbyte)

        pokeByteOff ptr (280  +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (280  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (280  +  14) (-1 :: GLbyte)

        pokeByteOff ptr (300  +  12) ( 0 :: GLbyte)
        pokeByteOff ptr (300  +  13) ( 0 :: GLbyte)
        pokeByteOff ptr (300  +  14) (-1 :: GLbyte)

        
    glVertexAttribPointer attPos 3 gl_FLOAT gl_FALSE (20) $ mkPtrGLvoid 0
    glVertexAttribPointer attNormal 3 gl_BYTE gl_FALSE (20) $ mkPtrGLvoid 12
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE (20) $ mkPtrGLvoid 16

    return vbo


