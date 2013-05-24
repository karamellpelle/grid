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
module Game.Run.RunData.Plain.ShadeScreenshot
  (
    ShadeScreenshot (..),
   
    loadShadeScreenshot,
    unloadShadeScreenshot,

  ) where


import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade





data ShadeScreenshot =
    ShadeScreenshot
    {
        shadeScreenshotPrg :: !GLuint,
        shadeScreenshotFBO :: !GLuint,
        shadeScreenshotUniAlpha :: !GLint,
        shadeScreenshotVAO :: !GLuint,
        shadeScreenshotVBO :: !GLuint
    }



loadShadeScreenshot:: IO ShadeScreenshot
loadShadeScreenshot = do
    vsh <- fileStaticData "shaders/RunScreenshot.vsh"
    fsh <- fileStaticData "shaders/RunScreenshot.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attTexCoord, "a_tex_coord") ] [

                                (tex0, "u_tex") ]

    uAlpha <- getUniformLocation prg "u_alpha"

    -- fbo
    fbo <- bindNewFBO

    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord

    -- vbo: Pos
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 32 nullPtr gl_STREAM_DRAW
    glVertexAttribPointer attPos 2 gl_FLOAT gl_FALSE 0 $ mkPtrGLvoid 0

    -- vbo: TexCoord
    bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER 16 nullPtr gl_STATIC_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
        pokeByteOff ptr 0   (0 :: GLushort)
        pokeByteOff ptr 2   (1 :: GLushort)

        pokeByteOff ptr 4   (0 :: GLushort)
        pokeByteOff ptr 6   (0 :: GLushort)

        pokeByteOff ptr 8   (1 :: GLushort)
        pokeByteOff ptr 10  (1 :: GLushort)

        pokeByteOff ptr 12  (1 :: GLushort)
        pokeByteOff ptr 14  (0 :: GLushort)
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_FALSE 0 $ mkPtrGLvoid 0

    return  ShadeScreenshot
            {
              shadeScreenshotPrg = prg,
              shadeScreenshotUniAlpha = uAlpha,
              shadeScreenshotFBO = fbo,
              shadeScreenshotVAO = vao,
              shadeScreenshotVBO = vbo
            }


unloadShadeScreenshot :: ShadeScreenshot -> IO ()
unloadShadeScreenshot sh = do
    putStrLn "unloadShadeScreenshot: no GL release"





