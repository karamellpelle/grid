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
module Game.Run.Scene.Plain.ShadeScreenshot
  (
    shadeScreenshot,

  ) where

import MyPrelude
import Game
import Game.Run.RunWorld
import Game.Run.RunData

import OpenGL
import OpenGL.Helpers



-- | draw tex0 with given shape, into tex1
shadeScreenshot :: ShadeScreenshot -> Float -> 
                   GLuint -> Shape -> 
                   GLenum -> GLuint -> UInt -> UInt -> IO ()
shadeScreenshot sh alpha tex0 (Shape wth0 hth0) target1 tex1 wth1 hth1 =  do
   
    -- bind fbo, render to tex1 
    glBindFramebuffer gl_FRAMEBUFFER $ shadeScreenshotFBO sh
    glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 target1 tex1 0
    glClear gl_COLOR_BUFFER_BIT -- fixme: check performance with/without
    glViewport 0 0 (fI wth1) (fI hth1)


    glUseProgram $ shadeScreenshotPrg sh

    glUniform1f (shadeScreenshotUniAlpha sh) $ rTF alpha

    -- write vbo
    glBindBuffer gl_ARRAY_BUFFER $ shadeScreenshotVBO sh
    glBufferData gl_ARRAY_BUFFER 32 nullPtr gl_STREAM_DRAW
    writeBuf gl_ARRAY_BUFFER $ \ptr -> do
       pokeByteOff ptr (00 + 0) (rTF (-wth0) :: GLfloat)
       pokeByteOff ptr (00 + 4) (rTF ( hth0) :: GLfloat)
       pokeByteOff ptr (08 + 0) (rTF (-wth0) :: GLfloat)
       pokeByteOff ptr (08 + 4) (rTF (-hth0) :: GLfloat)
       pokeByteOff ptr (16 + 0) (rTF ( wth0) :: GLfloat)
       pokeByteOff ptr (16 + 4) (rTF ( hth0) :: GLfloat)
       pokeByteOff ptr (24 + 0) (rTF ( wth0) :: GLfloat)
       pokeByteOff ptr (24 + 4) (rTF (-hth0) :: GLfloat)

    glBindVertexArrayOES $ shadeScreenshotVAO sh
    glDisable gl_DEPTH_TEST
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D tex0
    
    glDrawArrays gl_TRIANGLE_STRIP 0 4
  
    -- enable depth. (no, since this is Scene?)



