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
module Game.Values.Fancy
  (
    valueSceneCornerSize,

    valueRunCubeFontSize,
    valueRunPathColor,
    valueColorMapSize, 
    valueSpaceBoxDefaultColor,

    valueLevelPuzzleEatTicksInv,
    valueLevelPuzzleTextLevelTicksInv,
    valueLevelPuzzleView,
    valueLevelPuzzleDotRadius,
    valueLevelPuzzleDotSlices,
    valueLevelPuzzleDotStacks,
    valueLevelPuzzleDotBonusColor,
    valueLevelPuzzleDotTeleColor,
    valueLevelPuzzleDotFinishColor,
    valueLevelPuzzleFailureTicksInv,

  ) where

import MyPrelude
import Data.Int
import OpenGL
import Game.Data.Color 
import Game.Data.View
   

--------------------------------------------------------------------------------
--  

valueSceneCornerSize :: Float 
valueSceneCornerSize =
    0.12

--------------------------------------------------------------------------------
--  Run

valueRunCubeFontSize :: Float
valueRunCubeFontSize = 
    5.5

valueRunPathColor :: Color
valueRunPathColor = 
    Color 0.0 0.04 0.8 1.0

--------------------------------------------------------------------------------
--  SoundPath

valueSoundSceneNoiseSize :: UInt
valueSoundSceneNoiseSize = 4


valueSoundSceneNoiseNodeRadius :: Int16
valueSoundSceneNoiseNodeRadius = 1024

valueSoundSceneNoiseTickMin :: Float
valueSoundSceneNoiseTickMin = 11.0

valueSoundSceneNoiseTickMax :: Float
valueSoundSceneNoiseTickMax = 20.0

valueSoundSceneNoisePitchMin :: Float
valueSoundSceneNoisePitchMin = 0.3

valueSoundSceneNoisePitchMax :: Float
valueSoundSceneNoisePitchMax = 2.0


--------------------------------------------------------------------------------
--  SoundLevelPuzzle
--------------------------------------------------------------------------------
--  SpaceBox

valueSpaceBoxDefaultColor :: Color
valueSpaceBoxDefaultColor = 
    Color 0.3 0.9 0.0 1.0


--------------------------------------------------------------------------------
--  LevelPuzzle


valueLevelPuzzleEatTicks :: Float 
valueLevelPuzzleEatTicks = 8.0

valueLevelPuzzleEatTicksInv :: Float 
valueLevelPuzzleEatTicksInv = 1.0 / valueLevelPuzzleEatTicks



valueLevelPuzzleTextLevelTicks :: Float
valueLevelPuzzleTextLevelTicks = 8.0

valueLevelPuzzleTextLevelTicksInv :: Float
valueLevelPuzzleTextLevelTicksInv = 1.0 / valueLevelPuzzleTextLevelTicks


valueLevelPuzzleView :: View
valueLevelPuzzleView = 
    View 0.0 0.3 10.0

valueLevelPuzzleFailureTicksInv :: Float
valueLevelPuzzleFailureTicksInv = 
    0.25

--------------------------------------------------------------------------------
--  ColorMap

valueColorMapSize :: UInt
valueColorMapSize =
    22


--------------------------------------------------------------------------------
--  Dots

valueLevelPuzzleDotRadius :: Float
valueLevelPuzzleDotRadius =
    0.16

valueLevelPuzzleDotSlices :: UInt
valueLevelPuzzleDotSlices =
    16

valueLevelPuzzleDotStacks :: UInt
valueLevelPuzzleDotStacks =
    16


valueLevelPuzzleDotTeleColor :: Color
valueLevelPuzzleDotTeleColor = 
    Color 0.0 0.5 0.9 1.0

valueLevelPuzzleDotBonusColor :: Color
valueLevelPuzzleDotBonusColor = 
    Color 0.3 0.0 0.9 1.0

valueLevelPuzzleDotFinishColor :: Color
valueLevelPuzzleDotFinishColor = 
    Color 0.3 1.0 0.9 1.0

