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
module Game.Run.RunData.Plain.ShadeScene
  (
    ShadeScene (..),
   
    loadShadeScene,
    unloadShadeScene,

  ) where


import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade





data ShadeScene =
    ShadeScene
    {
        shadeScenePrg :: !GLuint,
        shadeSceneUniProjModvMatrix :: !GLint,
        shadeSceneUniAlpha :: !GLint,
        shadeSceneVAO :: !GLuint,
        shadeSceneVBODynamic :: !GLuint -- ^ Static?
    }



loadShadeScene :: IO ShadeScene
loadShadeScene = do
    vsh <- fileStaticData "shaders/Scene.vsh"
    fsh <- fileStaticData "shaders/Scene.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attTexCoord, "a_texcoord") ] [

                                (tex0, "u_tex") ]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"

    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- vbo dynamic
    vboDynamic <- makeVBODynamic

    return  ShadeScene
            {
              shadeScenePrg = prg,
              shadeSceneUniProjModvMatrix = uProjModvMatrix,
              shadeSceneUniAlpha = uAlpha,
              shadeSceneVAO = vao,
              shadeSceneVBODynamic = vboDynamic
            }


unloadShadeScene :: ShadeScene -> IO ()
unloadShadeScene sh = do
    return ()

makeVBODynamic :: IO GLuint
makeVBODynamic = do
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let elemsize = 4 * 1 + 2 * 2
        bytesize = 4 * elemsize
    allocaBytes bytesize $ \ptr -> do
        pokeByteOff ptr (0  + 0)  (-1 :: GLbyte)
        pokeByteOff ptr (0  + 1)  (1 :: GLbyte)
        pokeByteOff ptr (0  + 2)  (0 :: GLbyte)
        pokeByteOff ptr (0  + 3)  (1 :: GLbyte)
        pokeByteOff ptr (0  + 4)  (0 :: GLushort)
        pokeByteOff ptr (0  + 6)  (1 :: GLushort)
        
        pokeByteOff ptr (8  + 0)  (-1 :: GLbyte)
        pokeByteOff ptr (8  + 1)  (-1 :: GLbyte)
        pokeByteOff ptr (8  + 2)  (0 :: GLbyte)
        pokeByteOff ptr (8  + 3)  (1 :: GLbyte)
        pokeByteOff ptr (8  + 4)  (0 :: GLushort)
        pokeByteOff ptr (8  + 6)  (0 :: GLushort)

        pokeByteOff ptr (16 + 0)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 1)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 2)  (0 :: GLbyte)
        pokeByteOff ptr (16 + 3)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 4)  (1 :: GLushort)
        pokeByteOff ptr (16 + 6)  (1 :: GLushort)

        pokeByteOff ptr (24 + 0)  (1 :: GLbyte)
        pokeByteOff ptr (24 + 1)  (-1 :: GLbyte)
        pokeByteOff ptr (24 + 2)  (0 :: GLbyte)
        pokeByteOff ptr (24 + 3)  (1 :: GLbyte)
        pokeByteOff ptr (24 + 4)  (1 :: GLushort)
        pokeByteOff ptr (24 + 6)  (0 :: GLushort)

        glBufferData gl_ARRAY_BUFFER (fI bytesize) ptr gl_DYNAMIC_DRAW
   
    glVertexAttribPointer attPos 3 gl_BYTE gl_FALSE 8 (mkPtrGLvoid 0)
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_FALSE
                          (8) (mkPtrGLvoid 4)
    return vbo

