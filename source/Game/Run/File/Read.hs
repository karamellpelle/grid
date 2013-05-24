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
module Game.Run.File.Read
  (
    rRunWorld,
    rLevelPuzzleNamePeakS,

  ) where

import MyPrelude
import File.Binary

import Game.Run.RunWorld
import Game.Run.File.Field
import Game.Run.File.Version


rCompatibleHeader :: Reader ()
rCompatibleHeader = do
    ws <- replicateM 4 rAnyWord8
    unless (ws == version) $ fail $ "RunWorld-file of version " ++ show ws ++
                                    " is not compatible with version " ++ show version



rRunWorld :: Reader (String, UInt, UInt, Bool, Float)
rRunWorld = do
    rCompatibleHeader

    -- LevelPuzzleName
    rThisField fieldLevelPuzzleName
    lpname <- rCString 
    rAlign 4

    -- LevelPuzzlePeak
    rThisField fieldLevelPuzzleNamePeakS
    lpix <- rPeek lpname

    -- MemoryPeak
    rSeekField fieldMemoryPeak
    mix <- rUInt32

    -- Special LevelPuzzleWorld
    rThisField fieldSpecialIsCompleted
    spec <- (/= 0) `fmap` rUInt32

    -- intensity [0, 1] "with 16 bits precision"
    rThisField fieldIntensity
    intensity <- (\u16 -> 0.000015258789 * fI u16) `fmap` rUInt32

    return (lpname, lpix, mix, spec, intensity)



-- | find Peak for LevelPuzzleName. assuming such name exist in list
rPeek :: String -> Reader UInt
rPeek name = do
    rThisField fieldLevelPuzzleNamePeak <?> ("no peak for LevelPuzzleName " ++ show name)
    name' <- rCString
    rAlign 4
    peak' <- rUInt32
    if name' == name then return peak'
                     else rPeek name


-- | read the map (LevelPuzzleName -> LevelPuzzlePeak)
rLevelPuzzleNamePeakS :: Reader [(String, UInt)]
rLevelPuzzleNamePeakS = do
    rCompatibleHeader 
    
    -- LevelPuzzleName
    rField_
    rCString_
    rAlign 4
    
    -- LevelPuzzlePeak
    rThisField fieldLevelPuzzleNamePeakS
    rHelper

    where
      rHelper = 
          rCons <|> rNil

      rCons = do
          rThisField fieldLevelPuzzleNamePeak
          name <- rCString
          rAlign 4
          peak <- rUInt32
          ((name, peak):) `fmap` rHelper

      rNil =
          return [] 

--------------------------------------------------------------------------------
--  skips


rField_ :: Reader ()
rField_ = do
    rAnyWord8
    rAnyWord8
    rAnyWord8
    rAnyWord8
    return ()


rCString_ :: Reader ()
rCString_ = 
    many rNonNUL >> rNUL


