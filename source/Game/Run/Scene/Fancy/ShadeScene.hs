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
module Game.Run.Scene.Fancy.ShadeScene
  (
    shadeSceneTweak,

  ) where

import MyPrelude
import Game
import Game.Run.RunWorld
import Game.Run.RunData

import OpenGL
import OpenGL.Helpers





shadeSceneTweak :: SceneData -> ShadeScene -> Tweak -> Scene -> IO ()
shadeSceneTweak sdata sh tweak scene = do

    glUseProgram $ shadeScenePrg sh

    -- set Tweak uniforms --
    let Vec4 rx ry rz rw = tweakRotPos tweak
        (x, y) = cossin (2.0 * rz)
        tx0 = x
        tx1 = y
        ty0 = -y
        ty1 = x
        ts = 0.2 + 0.6 * (tweakForPosAlpha tweak)
        --deltas = (1.0 + tweakPulse0Pos tweak) * 0.1 * tweakForPosAlpha tweak 
        deltas = 0.18 * tweakForPosAlpha tweak 
    glUniform2f (shadeSceneUniTexATX sh) (rTF $ ts * tx0) (rTF $ ts * tx1)
    glUniform2f (shadeSceneUniTexATY sh) (rTF $ ts * ty0) (rTF $ ts * ty1)
    glUniform2f (shadeSceneUniTexAPos sh) (rTF $ 1.0 * rx) (rTF $ 1.0 * ry)
    glUniform1f (shadeSceneUniDeltaScale sh) (rTF deltas)

    -- colorfy 
    let colorfy = 1.0 + tweakPulse0Pos tweak
    glUniform4f (shadeSceneUniColorfy sh) (1.0) (1.0) (1.0) (1.0)
    glUniform1f (shadeSceneUniColorfyAlpha sh) (rTF colorfy)

    -- textures --
    glDisable gl_DEPTH_TEST
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ sceneTex scene 
    glActiveTexture gl_TEXTURE1
    glBindTexture gl_TEXTURE_2D $ scenedataTexA sdata


    -- draw --
    glBindVertexArrayOES $ scenedataVAO sdata
    glDrawArrays gl_TRIANGLE_STRIP 0 4
   
    glEnable gl_DEPTH_TEST
