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
module Game.Run.File.Write
  (
    wRunWorld,

  ) where

import MyPrelude
import File.Binary

import Game.Run.RunWorld
import Game.Run.File.Field
import Game.Run.File.Version


wRunWorld :: [(String, UInt)] -> RunWorld -> Writer
wRunWorld namepeak run = do
    -- header
    wWord8s version

    -- LevelPuzzleFileName
    wField fieldLevelPuzzleName
    wCStringAlign 4 $ runLevelPuzzleFileName run

    -- map FileName -> Peak, updating peak in RunWorld
    wField fieldLevelPuzzleNamePeakS
    forM_ namepeak $ \(name, peak) -> do
        wField fieldLevelPuzzleNamePeak
        wCStringAlign 4 name
        wUInt32 $ if name == runLevelPuzzleFileName run
                  then max peak $ runLevelPuzzlePeak run
                  else peak

    -- MemoryIx peak
    wField fieldMemoryPeak 
    wUInt32 $ runMemoryPeak run
   
    -- special LevelPuzzleWorld
    wField fieldSpecialIsCompleted
    wUInt32 $ if runSpecialIsCompleted run then 1 else 0

    -- intensity
    wField fieldIntensity 
    wUInt32 $ truncate $ runIntensity run * 65535.0
    


