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
module Game.LevelPuzzle.LevelPuzzleData.Plain.ShadeWall
  (
    ShadeWall (..),

    loadShadeWall,
    unloadShadeWall,

  ) where

import MyPrelude
import File

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data ShadeWall =
    ShadeWall
    {
        shadeWallPrg :: !GLuint,
        shadeWallUniAlpha :: !GLint,
        shadeWallUniProjModvMatrix :: !GLint,
        shadeWallUniNormalMatrix :: !GLint,
        shadeWallUniRefDir :: !GLint,
        shadeWallTex :: !GLuint
    }





loadShadeWall :: IO ShadeWall
loadShadeWall = do
    vsh <- fileStaticData "shaders/LevelWall.vsh"
    fsh <- fileStaticData "shaders/LevelWall.fsh"
    prg <- createPrg vsh fsh [  (attPos, "a_pos"),
                                (attNormal, "a_normal"),
                                (attTexCoord, "a_texcoord") ] [

                                (tex0, "u_tex") ]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uNormalMatrix <- getUniformLocation prg "u_normal_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uRefDir <- getUniformLocation prg "u_ref_dir"

    -- tex
    tex <- makeTex "LevelPuzzle/Output/wall_tex.png"

    -- tmp, set RefDir
    glProgramUniform3fEXT prg uRefDir 1.0 0.0 0.0

    return  ShadeWall
            {
                shadeWallPrg = prg,
                shadeWallUniAlpha = uAlpha,
                shadeWallUniProjModvMatrix = uProjModvMatrix,
                shadeWallUniNormalMatrix = uNormalMatrix,
                shadeWallUniRefDir = uRefDir,
                shadeWallTex = tex

            }
    where
      makeTex path = do
          tex <- bindNewTex gl_TEXTURE_2D
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ 
                          fI gl_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ 
                          fI gl_LINEAR_MIPMAP_LINEAR
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ 
                          fI gl_CLAMP_TO_EDGE
          glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ 
                          fI gl_CLAMP_TO_EDGE
          path <- fileStaticData path
          loadTexPreMult gl_TEXTURE_2D path
          glGenerateMipmap gl_TEXTURE_2D 
          
          return tex


unloadShadeWall :: ShadeWall -> IO ()
unloadShadeWall sh = do
    return ()
    -- fixme

