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
module Game.LevelPuzzle.LevelPuzzleData.Plain
  (
    LevelPuzzleData (..),
    
    loadLevelPuzzleData,
    unloadLevelPuzzleData,

    module Game.LevelPuzzle.LevelPuzzleData.Plain.ShadeWall,
    module Game.LevelPuzzle.LevelPuzzleData.Plain.ShadeDot,
    module Game.LevelPuzzle.LevelPuzzleData.Plain.SoundLevelPuzzle,

  ) where


import Game.LevelPuzzle.LevelPuzzleData.Plain.ShadeWall
import Game.LevelPuzzle.LevelPuzzleData.Plain.ShadeDot
import Game.LevelPuzzle.LevelPuzzleData.Plain.SoundLevelPuzzle



data LevelPuzzleData =
    LevelPuzzleData
    {
        levelpuzzledataShadeWall :: !ShadeWall,
        levelpuzzledataShadeDot :: !ShadeDot,
        levelpuzzledataSoundLevelPuzzle :: !SoundLevelPuzzle
    }


loadLevelPuzzleData :: IO LevelPuzzleData
loadLevelPuzzleData = do
    shWall <- loadShadeWall 
    shDot <- loadShadeDot
    snd <- loadSoundLevelPuzzle

    return  LevelPuzzleData
            {
                levelpuzzledataShadeWall = shWall,
                levelpuzzledataShadeDot = shDot,
                levelpuzzledataSoundLevelPuzzle = snd
            }


unloadLevelPuzzleData :: LevelPuzzleData -> IO ()
unloadLevelPuzzleData levelpuzzledata = do
    unloadSoundLevelPuzzle $ levelpuzzledataSoundLevelPuzzle levelpuzzledata
    unloadShadeWall $ levelpuzzledataShadeWall levelpuzzledata
    unloadShadeDot $ levelpuzzledataShadeDot levelpuzzledata
