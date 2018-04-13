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
module Game.LevelPuzzleMode.Output.Fancy.ShadeDot
  (
    shadeDot,
    shadeDotNode,
    shadeDotColor,
    shadeDotUseTexPlain,
    shadeDotUseTexBonus,
    shadeDotUseTexTele0,
    shadeDotUseTexTele1,
    shadeDotUseTexFinish,

  ) where


import MyPrelude
import Game
import Game.Data.Color

import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.LevelPuzzleData

import OpenGL
import OpenGL.Helpers



shadeDot :: ShadeDot -> Float -> Mat4 -> Mat4 -> IO ()
shadeDot sh alpha projmodv normal = do
    glUseProgram $ shadeDotPrg sh
    
    -- alpha
    glUniform1f (shadeDotUniAlpha sh) $ rTF alpha
    
    -- projmodv
    uniformMat4 (shadeDotUniProjModvMatrix sh) projmodv

    -- normal 
    uniformMat4AsMat3 (shadeDotUniNormalMatrix sh) normal

    -- vao
    glBindVertexArrayOES $ shadeDotVAO sh

    glActiveTexture gl_TEXTURE0



shadeDotNode :: ShadeDot -> Node -> IO ()
shadeDotNode sh (Node x y z) = do
    glUniform3f (shadeDotUniPos sh) (fI x) (fI y) (fI z)
    glDrawArrays gl_TRIANGLE_STRIP 0 $ fI $ valueLevelPuzzleDotStacks * 
                                            (2 * (valueLevelPuzzleDotSlices + 1) + 2)


shadeDotColor :: ShadeDot -> Color -> IO ()
shadeDotColor sh color =
    uniformColor (shadeDotUniColor sh) color

shadeDotUseTexPlain :: ShadeDot -> IO ()
shadeDotUseTexPlain sh = 
    glBindTexture gl_TEXTURE_2D $ shadeDotTexPlain sh

shadeDotUseTexBonus :: ShadeDot -> IO ()
shadeDotUseTexBonus sh = 
    glBindTexture gl_TEXTURE_2D $ shadeDotTexBonus sh

shadeDotUseTexTele0 :: ShadeDot -> IO ()
shadeDotUseTexTele0 sh = 
    glBindTexture gl_TEXTURE_2D $ shadeDotTexTele0 sh

shadeDotUseTexTele1 :: ShadeDot -> IO ()
shadeDotUseTexTele1 sh = 
    glBindTexture gl_TEXTURE_2D $ shadeDotTexTele1 sh

shadeDotUseTexFinish  :: ShadeDot -> IO ()
shadeDotUseTexFinish sh = 
    glBindTexture gl_TEXTURE_2D $ shadeDotTexFinish sh



