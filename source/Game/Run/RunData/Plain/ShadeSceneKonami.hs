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
module Game.Run.RunData.Plain.ShadeSceneKonami
  (
    ShadeSceneKonami (..),
   
    loadShadeSceneKonami,
    unloadShadeSceneKonami,
    beginShadeSceneKonami,
    endShadeSceneKonami,

  ) where


import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade





data ShadeSceneKonami =
    ShadeSceneKonami
    {
        shadeSceneKonamiPrg :: !GLuint,
        shadeSceneKonamiUniProjModvMatrix :: !GLint,
        shadeSceneKonamiUniAlpha :: !GLint,
        shadeSceneKonamiUniTweak :: !GLint,
        shadeSceneKonamiVAO :: !GLuint,
        shadeSceneKonamiVBO :: !GLuint,
        shadeSceneKonamiTex :: !GLuint

    }



loadShadeSceneKonami :: IO ShadeSceneKonami
loadShadeSceneKonami = do
    vsh <- fileStaticData "shaders/SceneKonami.vsh"
    fsh <- fileStaticData "shaders/SceneKonami.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attTexCoord, "a_texcoord") ] [

                                (tex0, "u_tex_scene"),
                                (tex1, "u_tex_image")]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uTweak <- getUniformLocation prg "u_tweak"

    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- vbo. content created by beginShadeScenePause
    vbo <- bindNewBuf gl_ARRAY_BUFFER
   
    -- tex. content created by endShadeScenePause
    tex <- bindNewTex gl_TEXTURE_2D

    return  ShadeSceneKonami
            {
              shadeSceneKonamiPrg = prg,
              shadeSceneKonamiUniProjModvMatrix = uProjModvMatrix,
              shadeSceneKonamiUniAlpha = uAlpha,
              shadeSceneKonamiUniTweak = uTweak,
              shadeSceneKonamiVAO = vao,
              shadeSceneKonamiVBO = vbo,
              shadeSceneKonamiTex = tex
            }


unloadShadeSceneKonami :: ShadeSceneKonami -> IO ()
unloadShadeSceneKonami sh = do
    return ()


--------------------------------------------------------------------------------
--  using ShadeSceneKonami
--  question: ShadeSceneKonami -> IO ShadeSceneKonami?
--            if so, then we need to modify GameData in real time.

beginShadeSceneKonami :: ShadeSceneKonami -> IO ()
beginShadeSceneKonami sh = do
    -- vbo
    glBindVertexArrayOES $ shadeSceneKonamiVAO sh
    glBindBuffer gl_ARRAY_BUFFER $ shadeSceneKonamiVBO sh
    allocaBytes 32 $ \ptr -> do
        pokeByteOff ptr (0  + 0)  (-1 :: GLbyte)
        pokeByteOff ptr (0  + 1)  (1 :: GLbyte)
        pokeByteOff ptr (0  + 2)  (0 :: GLbyte)
        pokeByteOff ptr (0  + 4)  (0 :: GLushort)
        pokeByteOff ptr (0  + 6)  (1 :: GLushort)
        
        pokeByteOff ptr (8  + 0)  (-1 :: GLbyte)
        pokeByteOff ptr (8  + 1)  (-1 :: GLbyte)
        pokeByteOff ptr (8  + 2)  (0 :: GLbyte)
        pokeByteOff ptr (8  + 4)  (0 :: GLushort)
        pokeByteOff ptr (8  + 6)  (0 :: GLushort)

        pokeByteOff ptr (16 + 0)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 1)  (1 :: GLbyte)
        pokeByteOff ptr (16 + 2)  (0 :: GLbyte)
        pokeByteOff ptr (16 + 4)  (1 :: GLushort)
        pokeByteOff ptr (16 + 6)  (1 :: GLushort)

        pokeByteOff ptr (24 + 0)  (1 :: GLbyte)
        pokeByteOff ptr (24 + 1)  (-1 :: GLbyte)
        pokeByteOff ptr (24 + 2)  (0 :: GLbyte)
        pokeByteOff ptr (24 + 4)  (1 :: GLushort)
        pokeByteOff ptr (24 + 6)  (0 :: GLushort)

        glBufferData gl_ARRAY_BUFFER 32 ptr gl_STATIC_DRAW
   
    glVertexAttribPointer attPos 3 gl_BYTE gl_FALSE 8 (mkPtrGLvoid 0)
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_FALSE 8 $ mkPtrGLvoid 4

    -- tex
    glBindTexture gl_TEXTURE_2D $ shadeSceneKonamiTex sh
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR
    path <- fileStaticData "Run/Scene/konami.png"
    loadTexPreMult gl_TEXTURE_2D gl_RGBA path
  
    return ()

-- | question: will this save memory?
endShadeSceneKonami :: ShadeSceneKonami -> IO ()
endShadeSceneKonami sh = do
    glBindBuffer gl_ARRAY_BUFFER $ shadeSceneKonamiVBO sh
    glBufferData gl_ARRAY_BUFFER 0 nullPtr gl_STATIC_DRAW
    glBindTexture gl_TEXTURE_2D $ shadeSceneKonamiTex sh
    glTexImage2D gl_TEXTURE_2D 0 (fI gl_RGBA) 0 0 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr



