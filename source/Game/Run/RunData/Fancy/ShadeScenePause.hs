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
module Game.Run.RunData.Fancy.ShadeScenePause
  (
    ShadeScenePause (..),
   
    loadShadeScenePause,
    unloadShadeScenePause,

  ) where


import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade





data ShadeScenePause =
    ShadeScenePause
    {
        shadeScenePausePrg :: !GLuint,

        shadeScenePauseUniTX :: !GLint,
        shadeScenePauseUniTY :: !GLint,
        shadeScenePauseUniPos :: !GLint

    }



loadShadeScenePause :: IO ShadeScenePause
loadShadeScenePause = do
    vsh <- fileStaticData "shaders/ScenePause.vsh"
    fsh <- fileStaticData "shaders/ScenePause.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos") ]
                              [ (tex0, "u_scene"),
                                (tex1, "u_texa") ]


    uTX <- getUniformLocation prg "u_tx"
    uTY <- getUniformLocation prg "u_ty"
    uPos <- getUniformLocation prg "u_pos"

    return  ShadeScenePause
            {
              shadeScenePausePrg = prg,
              shadeScenePauseUniTX = uTX,
              shadeScenePauseUniTY = uTY,
              shadeScenePauseUniPos = uPos
            }


unloadShadeScenePause :: ShadeScenePause -> IO ()
unloadShadeScenePause sh = do
    return ()

