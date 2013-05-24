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
module Game.LevelPuzzle.Do
  (
    doPlay,
    doComplete,
    doFailure,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.LevelPuzzle
import Game.LevelPuzzle.Do.Grid
import Game.LevelPuzzle.Iteration.State

import Game.Run
import Game.Run.RunWorld
import Game.Run.Do


doPlay :: s -> LevelPuzzleWorld -> b -> MEnv' (s, LevelPuzzleWorld, b)
doPlay = \s lvl b -> do
    let lvl' = levelpuzzleClearEvents $ levelpuzzleModifyCamera lvl $ \cam -> 
                                        cameraToPath cam (levelpuzzlePath lvl)
    
    -- Grid
    (s', grid', lvl'') <- doGridPlay s (levelpuzzleGrid lvl') lvl'
    let lvl''' = levelpuzzleModifyContent lvl'' $ \cnt -> cnt { contentGrid = grid' }

    return (s', lvl''', b)



doFailure :: s -> LevelPuzzleWorld -> b -> MEnv' (s, LevelPuzzleWorld, b)
doFailure = \s lvl b -> do
    let lvl' = levelpuzzleClearEvents lvl
    
    -- Grid
    (s', grid', lvl'') <- doGridFailure s (levelpuzzleGrid lvl') lvl'
    let lvl''' = levelpuzzleModifyContent lvl'' $ \cnt -> cnt { contentGrid = grid' }

    return (s', lvl''', b)





doComplete :: CompleteS -> LevelPuzzleWorld -> RunWorld -> 
              MEnv' (CompleteS, LevelPuzzleWorld, RunWorld)
doComplete = \s lvl run -> do
    
    let lvl' = levelpuzzleClearEvents lvl

    -- doGrid 
    (s', grid', lvl'') <- doGridComplete s (levelpuzzleGrid lvl') lvl'
    let lvl''' = levelpuzzleModifyContent lvl'' $ \cnt -> cnt { contentGrid = grid' }

    -- do RunWorld
    (s'', run', b') <- doLevelPuzzleComplete 
                        s' (run { runLevelPuzzleWorld = lvl''' }) ()

    return (s'', runLevelPuzzleWorld run', run')


