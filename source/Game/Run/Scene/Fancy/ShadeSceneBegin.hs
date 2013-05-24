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
module Game.Run.Scene.Fancy.ShadeSceneBegin
  (
    shadeSceneBeginTweak,

  ) where

import MyPrelude
import Game
import Game.Run

import OpenGL
import OpenGL.Helpers





-- | draw scene to framebuffer
shadeSceneBeginTweak :: SceneData -> ShadeSceneBegin -> Tweak -> Scene -> Tick -> IO ()
shadeSceneBeginTweak sdata sh tweak scene tick = do
    
    glUseProgram $ shadeSceneBeginPrg sh

    -- set Tweak uniforms --
    let (sceneA, texA)  | tick <= t1  = (0.0, diffAlpha t0 t1 tick)
                        | tick <= t2  = (diffAlpha t1 t2 tick, 1.0 - diffAlpha t1 t2 tick)
                        | tick <= t3  = (1.0, 0.0)
                      -- | tick <= t2  = (diffAlpha t1 t2 tick, 1.0)
                      -- | tick <= t3  = (1.0, 1.0 - diffAlpha t2 t3 tick)
                        | tick <= t4  = (1.0, 0.0)
                        | otherwise   = (1.0, 0.0)

    -- interpolate scene and tex
    glUniform1f (shadeSceneBeginUniSceneAlpha sh) (rTF sceneA)
    glUniform1f (shadeSceneBeginUniTexBeginAlpha sh) (rTF texA)

    -- scale with respect to screen shape
    let Shape wth hth = sceneShape scene
    glUniform2f (shadeSceneBeginUniTexBeginScale sh) (rTF $ 1.0 / hth) (rTF $ 1.0 / wth)

    -- textures --
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ sceneTex scene
    glActiveTexture gl_TEXTURE1
    glBindTexture gl_TEXTURE_2D $ shadeSceneBeginTexBegin sh

    -- draw --
    glBindVertexArrayOES $ scenedataVAO sdata
    glDrawArrays gl_TRIANGLE_STRIP 0 4
    
    where
      diffAlpha t0 t1 t = rTF (t - t0) / rTF (t1 - t0) :: Float
      t0 = (0.0       )   
      t1 = (t0 + valueRunIterationBeginT0)
      t2 = (t1 + valueRunIterationBeginT1)   
      t3 = (t2 + valueRunIterationBeginT2)
      t4 = (t3 + valueRunIterationBeginT3)
      t5 = (t4 + valueRunIterationBeginT4)

