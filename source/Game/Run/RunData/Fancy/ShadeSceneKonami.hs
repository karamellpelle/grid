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
module Game.Run.RunData.Fancy.ShadeSceneKonami
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
        shadeSceneKonamiUniFillColor :: !GLint,
        shadeSceneKonamiUniXColor :: !GLint,
        shadeSceneKonamiUniYColor :: !GLint,
        shadeSceneKonamiUniSceneAlpha :: !GLint,
        shadeSceneKonamiUniXAlpha :: !GLint,
        shadeSceneKonamiUniYAlpha :: !GLint,
        shadeSceneKonamiUniStencilAlpha :: !GLint,
        shadeSceneKonamiUniStencilScale :: !GLint,

        shadeSceneKonamiStencil :: !GLuint

    }



loadShadeSceneKonami :: IO ShadeSceneKonami
loadShadeSceneKonami = do
    vsh <- fileStaticData "shaders/SceneKonami.vsh"
    fsh <- fileStaticData "shaders/SceneKonami.fsh"
    prg <- createPrg vsh fsh  [ (attPos, "a_pos") ]

                              [ (tex0, "u_scene"),
                                (tex1, "u_stencil") ]

    uFillColor <- getUniformLocation prg "u_fill_color"
    uXColor <- getUniformLocation prg "u_x_color"
    uYColor <- getUniformLocation prg "u_y_color"
    uSceneAlpha <- getUniformLocation prg "u_scene_alpha"
    uXAlpha <- getUniformLocation prg "u_x_alpha"
    uYAlpha <- getUniformLocation prg "u_y_alpha"
    uStencilAlpha <- getUniformLocation prg "u_stencil_alpha"
    uStencilScale <- getUniformLocation prg "u_stencil_scale"

    -- content created by endShadeScenePause:
    stencil <- bindNewTex gl_TEXTURE_2D

    return  ShadeSceneKonami
            {
                shadeSceneKonamiPrg = prg,
                shadeSceneKonamiUniFillColor = uFillColor,
                shadeSceneKonamiUniXColor = uXColor,
                shadeSceneKonamiUniYColor = uYColor,
                shadeSceneKonamiUniSceneAlpha = uSceneAlpha,
                shadeSceneKonamiUniXAlpha = uXAlpha,
                shadeSceneKonamiUniYAlpha = uYAlpha,
                shadeSceneKonamiUniStencilAlpha = uStencilAlpha,
                shadeSceneKonamiUniStencilScale = uStencilScale,

                shadeSceneKonamiStencil = stencil
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
    -- tex
    glBindTexture gl_TEXTURE_2D $ shadeSceneKonamiStencil sh
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR
    path <- fileStaticData "Run/Scene/konami.png"
    loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
  
    return ()


-- | question: will this save memory?
endShadeSceneKonami :: ShadeSceneKonami -> IO ()
endShadeSceneKonami sh = do
    glBindTexture gl_TEXTURE_2D $ shadeSceneKonamiStencil sh
    glTexImage2D gl_TEXTURE_2D 0 (fI gl_RGBA) 0 0 0 gl_RGBA gl_UNSIGNED_BYTE nullPtr



