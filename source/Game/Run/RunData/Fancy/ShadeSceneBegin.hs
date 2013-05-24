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
module Game.Run.RunData.Fancy.ShadeSceneBegin
  (
    ShadeSceneBegin (..),
   
    loadShadeSceneBegin,
    unloadShadeSceneBegin,
    
  ) where


import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade





data ShadeSceneBegin =
    ShadeSceneBegin
    {
        shadeSceneBeginPrg :: !GLuint,

        shadeSceneBeginUniSceneAlpha :: !GLint,
        shadeSceneBeginUniTexBeginScale :: !GLint,
        shadeSceneBeginUniTexBeginAlpha :: !GLint,

        shadeSceneBeginTexBegin :: !GLuint
    }



loadShadeSceneBegin :: IO ShadeSceneBegin
loadShadeSceneBegin = do
    vsh <- fileStaticData "shaders/SceneBegin.vsh"
    fsh <- fileStaticData "shaders/SceneBegin.fsh"
    prg <- createPrg vsh fsh  [  (attPos, "a_pos"),
                                 (attTexCoord, "a_texcoord") ] 

                              [  (tex0, "u_scene"),
                                 (tex1, "u_texbegin") ]


    uSceneAlpha <- getUniformLocation prg "u_scene_alpha"
    uTexBeginScale <- getUniformLocation prg "u_texbegin_scale"
    uTexBeginAlpha <- getUniformLocation prg "u_texbegin_alpha"

    texBegin <- loadTexBegin

    return  ShadeSceneBegin
            {
              shadeSceneBeginPrg = prg,
              shadeSceneBeginUniSceneAlpha = uSceneAlpha,
              shadeSceneBeginUniTexBeginScale = uTexBeginScale,
              shadeSceneBeginUniTexBeginAlpha = uTexBeginAlpha,
              shadeSceneBeginTexBegin = texBegin
            }
    
    where
      loadTexBegin = do 
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_NEAREST
          path <- fileStaticData "Run/Scene/begin.png"
          loadTexPreMult gl_TEXTURE_2D gl_RGBA path -- fixme: intfmt
          return tex



unloadShadeSceneBegin :: ShadeSceneBegin -> IO ()
unloadShadeSceneBegin sh = do
    delTex $ shadeSceneBeginTexBegin sh
    glDeleteProgram $ shadeSceneBeginPrg sh
    return ()



