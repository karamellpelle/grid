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
module Game.Values.Fancy where

import MyPrelude
import Data.Int
import OpenGL
import OpenAL
import Game.Font
import Game.Data.Color 
import Game.Data.View
   



--------------------------------------------------------------------------------
--  Perspective

valuePerspectiveFOVY :: Float
valuePerspectiveFOVY =
    1.047

valuePerspectiveNear :: Float
valuePerspectiveNear = 
    0.1

valuePerspectiveFar :: Float 
valuePerspectiveFar =
    512.0

-- | call this value r. let mx my mz be the x, y, z colums of the current
--   modelview matrix. then 
--   r :  r * abs(mx + my + mz) < projection_far 
--   but we assume that modelview(3x3) is orthonormal, so 
--   r :  r < (projection_far / sqrt 3)
valuePerspectiveRadius :: Float
valuePerspectiveRadius =
    valuePerspectiveFar * 0.57735




--------------------------------------------------------------------------------
--  Scene

valueSceneCornerSize :: Float 
valueSceneCornerSize =
    0.20


--------------------------------------------------------------------------------
--  Grid

valueGridPathRadius :: Float
valueGridPathRadius =
    0.3


--------------------------------------------------------------------------------
--  Memory

valueMemoryPathRadius :: Float
valueMemoryPathRadius = 
    valueGridPathRadius 


--------------------------------------------------------------------------------
--  LevelPuzzle

valueLevelPuzzleEatTicks :: Float 
valueLevelPuzzleEatTicks = 1.0

valueLevelPuzzleEatTicksInv :: Float 
valueLevelPuzzleEatTicksInv = 1.0 / valueLevelPuzzleEatTicks

valueLevelPuzzleDotSlices :: UInt
valueLevelPuzzleDotSlices =
    16

valueLevelPuzzleDotStacks :: UInt
valueLevelPuzzleDotStacks =
    16

valueLevelPuzzleDotTele0Color :: Color
valueLevelPuzzleDotTele0Color = 
    Color 1.0 0.45882 0.0 1.0

valueLevelPuzzleDotTele1Color :: Color
valueLevelPuzzleDotTele1Color = 
    Color 0.0 0.57647 1.0 1.0

valueLevelPuzzleDotBonusColor :: Color
valueLevelPuzzleDotBonusColor = 
    Color 0.75294 0.09411 0.47843 1.0

valueLevelPuzzleDotFinishColor :: Color
valueLevelPuzzleDotFinishColor = 
    Color 1.0 1.0 1.0 1.0

valueFadeContentTicks :: Float
valueFadeContentTicks = 
    4.0

valueFadeContentTicksInv :: Float
valueFadeContentTicksInv = 
    1.0 / valueFadeContentTicks

valueFadeRoomTicks :: Float
valueFadeRoomTicks = 
    2.0

valueFadeRoomTicksInv :: Float
valueFadeRoomTicksInv = 
    1.0 / valueFadeRoomTicks

valueFadeFailureTicks :: Float
valueFadeFailureTicks = 
    8.0

valueFadeFailureTicksInv :: Float
valueFadeFailureTicksInv = 
    1.0 / valueFadeRoomTicks

valueLevelPuzzlePathRadius :: Float
valueLevelPuzzlePathRadius = 
    valueGridPathRadius 


--------------------------------------------------------------------------------
--  Run

valueRunCubeFontSize :: Float
valueRunCubeFontSize = 
    5.2

valueRunCubeFontColor :: FontColor
valueRunCubeFontColor = 
    FontColor 1.0 1.0 0.2 1.0

valueRunPathColor :: Color
valueRunPathColor = 
    Color 0.0 0.7 0.8 1.0

valueRunPathRadius :: Float
valueRunPathRadius =
    1.0

--------------------------------------------------------------------------------
--  SoundScene. fixme: into module!

valueSoundSceneNoiseNodeRadius :: Int16
valueSoundSceneNoiseNodeRadius = 256

valueSoundSceneNoiseTickMin :: Float
valueSoundSceneNoiseTickMin = 11.0

valueSoundSceneNoiseTickMax :: Float
valueSoundSceneNoiseTickMax = 20.0

valueSoundSceneNoisePitchMin :: Float
valueSoundSceneNoisePitchMin = 0.6

valueSoundSceneNoisePitchMax :: Float
valueSoundSceneNoisePitchMax = 1.8


--------------------------------------------------------------------------------
--  SoundLevelPuzzle. fixme: into module!

valueSoundLevelPuzzleNodeConeInnerAngle :: ALfloat
valueSoundLevelPuzzleNodeConeInnerAngle = 45.0

valueSoundLevelPuzzleNodeConeOuterAngle :: ALfloat
valueSoundLevelPuzzleNodeConeOuterAngle = 360.0

valueSoundLevelPuzzleNodeConeOuterGain :: ALfloat
valueSoundLevelPuzzleNodeConeOuterGain = 0.4

valueSoundLevelPuzzleNodeReferenceDistance :: ALfloat
valueSoundLevelPuzzleNodeReferenceDistance = 8.0

valueSoundLevelPuzzleNodeMaxDistance :: ALfloat
valueSoundLevelPuzzleNodeMaxDistance = 32.0

valueSoundLevelPuzzleNodeRolloffFactor :: ALfloat 
valueSoundLevelPuzzleNodeRolloffFactor = 1.0


--------------------------------------------------------------------------------
--  SpaceBox. fixme: into module!!

valueSpaceBoxDefaultColor :: Color
valueSpaceBoxDefaultColor = 
    Color 0.3 0.9 0.0 1.0


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--  Text


-- | (relative to Scene hth)
valueTextFontASize :: Float
valueTextFontASize =
    0.1

-- | (relative to Scene hth)
valueTextFontAY :: Float
valueTextFontAY =
    0.4

valueTextFontAColor :: FontColor
valueTextFontAColor =
    FontColor 1.0 0.0 0.0 1.0

-- | (relative to Scene hth)
valueTextFontBSize :: Float
valueTextFontBSize =
    0.06

-- | (relative to Scene hth)
valueTextFontBY :: Float
valueTextFontBY =
    0.3

valueTextFontBColor :: FontColor
valueTextFontBColor =
    FontColor 0.0 1.0 0.0 1.0

-- | (relative to Scene hth)
valueTextFontCSize :: Float
valueTextFontCSize =
    0.05

-- | (relative to Scene hth)
valueTextFontCY :: Float
valueTextFontCY =
    0.08

valueTextFontCColor :: FontColor
valueTextFontCColor =
    FontColor 0.0 0.0 1.0 1.0




