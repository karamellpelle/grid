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
module Game.LevelPuzzleMode.Do
  (
    doPlay,
    doComplete,
    --doSpecialComplete,
    doFailure,

  ) where

import MyPrelude
import Game

import Game.Grid.Helpers
import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.Iteration.State
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.Do.Grid
import Game.LevelPuzzleMode.Do.Input

import Game.Run.RunWorld
import Graphics.UI.GLFW


--------------------------------------------------------------------------------
--  

beginLevelPuzzleWorld lvl = do
    let lvl' = levelpuzzleClearEvents $ levelpuzzleModifyCamera lvl $ \cam -> 
                                        cameraToPath cam (levelpuzzlePath lvl)
    keysKeyOnce (CharKey ' ') >>= \bool -> case bool of
        False   -> return lvl'
        True    -> return $ if levelpuzzleIsPuzzle lvl'
                            then levelpuzzleModifyGrid (lvl { levelpuzzleIsPuzzle = False }) gridPathStart
                            else lvl' { levelpuzzleIsPuzzle = True } 


doPlay :: s -> LevelPuzzleWorld -> b -> MEnv' (s, LevelPuzzleWorld, b)
doPlay s lvl b = do

    lvl' <- beginLevelPuzzleWorld lvl
    
    -- Grid
    (s', grid', lvl'') <- doGridPlay s (levelpuzzleGrid lvl') lvl'
    let lvl''' = levelpuzzleModifyContent lvl'' $ \cnt -> cnt { contentGrid = grid' }

    return (s', lvl''', b)




doFailure :: s -> LevelPuzzleWorld -> b -> MEnv' (s, LevelPuzzleWorld, b)
doFailure s lvl b = do
   
    lvl' <- beginLevelPuzzleWorld lvl
    
    -- Grid
    (s', grid', lvl'') <- doGridFailure s (levelpuzzleGrid lvl') lvl'
    let lvl''' = levelpuzzleModifyContent lvl'' $ \cnt -> cnt { contentGrid = grid' }

    return (s', lvl''', b)


doComplete :: s -> LevelPuzzleWorld -> b -> MEnv' (s, LevelPuzzleWorld, b)
doComplete s lvl b = do

    lvl' <- beginLevelPuzzleWorld lvl
    
    -- do Grid
    (s', grid', lvl'') <- doGridComplete s (levelpuzzleGrid lvl') lvl'
    let lvl''' = levelpuzzleModifyContent lvl'' $ \cnt -> cnt { contentGrid = grid' }

    return (s', lvl''', b)









