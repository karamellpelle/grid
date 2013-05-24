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
module Game.LevelPuzzle.Output.Plain.ShadeWall
  (
    shadeWall,

  ) where


import MyPrelude
import Game
import Game.LevelPuzzle

import OpenGL
import OpenGL.Helpers



shadeWall :: ShadeWall -> Float -> Mat4 -> Mat4 -> IO ()
shadeWall sh alpha projmodv normal = do
    glUseProgram $ shadeWallPrg sh
    
    -- alpha
    glUniform1f (shadeWallUniAlpha sh) $ rTF alpha
    
    -- projmodv
    uniformMat4 (shadeWallUniProjModvMatrix sh) projmodv

    -- normal 
    uniformMat4AsMat3 (shadeWallUniNormalMatrix sh) normal
    
    glActiveTexture gl_TEXTURE0
    glBindTexture gl_TEXTURE_2D $ shadeWallTex sh
