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
module Game.Run.RunData.Fancy.ShadeScene
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

        shadeSceneUniTexATX :: !GLint,
        shadeSceneUniTexATY :: !GLint,
        shadeSceneUniTexAPos :: !GLint,
        shadeSceneUniDeltaScale :: !GLint,
        shadeSceneUniColorfy :: !GLint,
        shadeSceneUniColorfyAlpha :: !GLint
    }



loadShadeScene :: IO ShadeScene
loadShadeScene = do
    vsh <- fileStaticData "shaders/Scene.vsh"
    fsh <- fileStaticData "shaders/Scene.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos") ]

                              [ (tex0, "u_scene"),
                                (tex1, "u_texa")]

    uTexATX <- getUniformLocation prg "u_texa_tx"
    uTexATY <- getUniformLocation prg "u_texa_ty"
    uTexAPos <- getUniformLocation prg "u_texa_pos"
    uDeltaScale <- getUniformLocation prg "u_delta_scale"
    uColorfy <- getUniformLocation prg "u_colorfy"
    uColorfyAlpha <- getUniformLocation prg "u_colorfy_alpha"


    return  ShadeScene
            {
                shadeScenePrg = prg,

                shadeSceneUniTexATX = uTexATX,
                shadeSceneUniTexATY = uTexATY,
                shadeSceneUniTexAPos = uTexAPos,
                shadeSceneUniDeltaScale = uDeltaScale,
                shadeSceneUniColorfy = uColorfy,
                shadeSceneUniColorfyAlpha = uColorfyAlpha

            }


unloadShadeScene :: ShadeScene -> IO ()
unloadShadeScene sh = do
    return ()

