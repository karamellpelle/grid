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
module Game.Grid.GridData.Fancy.ShadeGeneral
  (
    ShadeGeneral (..),
   
    loadShadeGeneral,
    unloadShadeGeneral,

  ) where

import MyPrelude
import File


import OpenGL
import OpenGL.Helpers
import OpenGL.Shade



-- | shader for general drawing. to be continued...
data ShadeGeneral =
    ShadeGeneral
    {
        shadeGeneralPrg :: !GLuint,
        shadeGeneralUniAlpha :: !GLint,
        shadeGeneralUniProjModvMatrix :: !GLint

    }



loadShadeGeneral :: IO ShadeGeneral
loadShadeGeneral = do
    vsh <- fileStaticData "shaders/General.vsh"
    fsh <- fileStaticData "shaders/General.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos"),
                                (attTexCoord, "a_texcoord") ]
                              [ (tex0, "u_tex") ]

    uAlpha <- getUniformLocation prg "u_alpha"
    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"


    return  ShadeGeneral
            {
                shadeGeneralPrg = prg,
                shadeGeneralUniAlpha = uAlpha,
                shadeGeneralUniProjModvMatrix = uProjModvMatrix

            }


unloadShadeGeneral :: ShadeGeneral -> IO ()
unloadShadeGeneral sh = do
    return ()



