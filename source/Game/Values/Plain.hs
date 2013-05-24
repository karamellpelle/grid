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
module Game.Values.Plain where

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
    0.12


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

valueLevelPuzzleView :: View
valueLevelPuzzleView = 
    View 0.0 0.3 10.0

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

valueFadeContentTicks :: Float
valueFadeContentTicks = 
    4.0

valueFadeContentTicksInv :: Float
valueFadeContentTicksInv = 
    1.0 / valueFadeContentTicks

valueFadeRoomTicks :: Float
valueFadeRoomTicks = 
    1.0

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
    5.5

valueRunCubeFontColor :: FontColor
valueRunCubeFontColor = 
    FontColor 0.2 1.0 0.5 1.0

valueRunPathColor :: Color
valueRunPathColor = 
    Color 0.0 0.04 0.8 1.0

valueRunPathRadius :: Float
valueRunPathRadius =
    1.0

--------------------------------------------------------------------------------
--  SoundPath. fixme: into module!!

valueSoundPathConeInnerAngle :: ALfloat
valueSoundPathConeInnerAngle = 360.0

valueSoundPathConeOuterAngle :: ALfloat
valueSoundPathConeOuterAngle = 360.0

valueSoundPathConeOuterGain :: ALfloat
valueSoundPathConeOuterGain = 1.0

valueSoundPathReferenceDistance :: ALfloat
valueSoundPathReferenceDistance = 32.0 

valueSoundPathMaxDistance :: ALfloat
valueSoundPathMaxDistance = 128.0

valueSoundPathRolloffFactor :: ALfloat
valueSoundPathRolloffFactor = 1.0


--------------------------------------------------------------------------------
--  SoundScene. fixme: into module!

valueSoundSceneNoiseSize :: UInt
valueSoundSceneNoiseSize = 4

valueSoundSceneNoiseConeInnerAngle :: ALfloat
valueSoundSceneNoiseConeInnerAngle = 360.0

valueSoundSceneNoiseConeOuterAngle :: ALfloat
valueSoundSceneNoiseConeOuterAngle = 360.0

valueSoundSceneNoiseConeOuterGain :: ALfloat
valueSoundSceneNoiseConeOuterGain = 1.0

valueSoundSceneNoiseReferenceDistance :: ALfloat
valueSoundSceneNoiseReferenceDistance = 50.0 

valueSoundSceneNoiseMaxDistance :: ALfloat
valueSoundSceneNoiseMaxDistance = 1024.0

valueSoundSceneNoiseRolloffFactor :: ALfloat
valueSoundSceneNoiseRolloffFactor = 3.0


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
--  ColorMap. fixme: into module!

valueColorMapSize :: UInt
valueColorMapSize =
    23


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
    FontColor 0.0 1.0 0.6 1.0

-- | (relative to Scene hth)
valueTextFontBSize :: Float
valueTextFontBSize =
    0.05

-- | (relative to Scene hth)
valueTextFontBY :: Float
valueTextFontBY =
    0.3

valueTextFontBColor :: FontColor
valueTextFontBColor =
    FontColor 0.6 1.0 0.0 1.0

-- | (relative to Scene hth)
valueTextFontCSize :: Float
valueTextFontCSize =
    0.02

-- | (relative to Scene hth)
valueTextFontCY :: Float
valueTextFontCY =
    0.05

valueTextFontCColor :: FontColor
valueTextFontCColor =
    FontColor 0.6 0.0 1.0 1.0

