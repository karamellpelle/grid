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
module Game.Values
  (
    valueDTUnit,
    valueMaxElaps,
    valuePerspectiveFOVY,
    valuePerspectiveNear,
    valuePerspectiveFar,

    valueRunBeginTicks,
    valueRunBeginTicksInv,
    valueRunGridView,
    valueRunGridViewABound,
    valueRunGridViewBBound,
    valueRunGridViewCBound,
    valueRunGridCameraViewCMin,
    valueRunGridCameraViewCMax,
    valueRunGridCameraViewCSens,
    valueRunGridScale,
    valueRunBoundingRadius,
    valueRunMessageSize,
    valueRunMessageSpeed,
    valueRunMessageRadius,
    valueRunMessageCharWth,
    valueRunFaceDrag,
    valueRunFaceBeginEndSpeed,
    valueRunFaceSpeed,
    valueRunCubeRadius,
    valueRunPathRadius,
    valueRunAboutSettingsFadeTicksInv,
    valueRunPauseTicks, 
    valueRunPauseTicksInv, 
    valueRunAModeToBModeView,
    valueRunAModeToBModeTicks,

    valueModePathTurnSpeed,
    valueModePathRadius,



    -- fixme XXX -> XXXTicks
    valueGridPathTurnSens,
    valueGridCameraViewASens,
    valueGridCameraViewBSens,
    valueGridCameraViewCSens,
    valueGridCameraViewASpeed,
    valueGridCameraViewBSpeed,
    valueGridCameraViewCSpeed,
    valueGridCameraViewCMin,
    valueGridCameraViewCMax,
    valueGridSpaceBoxRadius,
    valueGridMaxPathSize,
    valueGridRollSize,
    valueGridPathSpeed,
    valueGridClockSpeedMin,
    valueGridClockSpeedMax,

    valueLevelModeGridView,
    valueLevelModeOldPhysicalTicks,
    valueLevelModeLevelInfoTicks,
    valueLevelModeFailureTicks,
    valueLevelModeFailureTicksInv,
    valueLevelPuzzleCompleteGridSize,

    valueMemoryModeGridShowView,
    valueMemoryModeGridPlayView,
    valueMemoryModeGridFailureViewCPlus,
    valueMemoryModeGridFailureViewTicks,
    valueMemoryModeTextTicks,
    valueMemoryModeTextTicksInv,

    valueGUIBoardWth,
    valueTextFontASize,
    valueTextFontAY,
    valueTextFontBSize,
    valueTextFontBY,
    valueTextFontCSize,
    valueTextFontCY,
    
    valueEggKonamiTicks,
    valueEggKonamiTicksInv,
    
    valueFileNameSpecialLevelPuzzleWorld,

    valuePlayersAchievementEggKonami,
    valuePlayersSpecialLevelPuzzleWorldCompleted,
    valuePlayersScoreMemoryMode,

#ifdef GRID_FANCY
    module Game.Values.Fancy,
#else
    module Game.Values.Plain,
#endif

  ) where


import MyPrelude
import MEnv.Tick
import Game.Data.View

-- fixme: put values into Fancy/Plain
#ifdef GRID_FANCY
import Game.Values.Fancy
#else
import Game.Values.Plain
#endif

--------------------------------------------------------------------------------
--  

valueDTUnit :: TickT
valueDTUnit =
    0.02

valueMaxElaps :: TickT
valueMaxElaps = 
    2.0

valuePerspectiveFOVY :: Float
valuePerspectiveFOVY =
    1.047

valuePerspectiveNear :: Float
valuePerspectiveNear = 
    0.1

valuePerspectiveFar :: Float 
valuePerspectiveFar =
    512.0

--------------------------------------------------------------------------------
--  Run


valueRunBeginNEnd :: UInt
valueRunBeginNEnd  =
    12

valueRunBeginTicks :: Float
valueRunBeginTicks  =
    4.0

valueRunBeginTicksInv :: Float 
valueRunBeginTicksInv = 
    1.0 / valueRunBeginTicks


valueRunGridView :: View
valueRunGridView =
    View 0.0 0.0 128.0

valueRunGridViewABound :: Float
valueRunGridViewABound =
    0.32

valueRunGridViewBBound :: Float 
valueRunGridViewBBound =
    0.32

valueRunGridViewCBound :: Float
valueRunGridViewCBound =
    0.0

valueRunBoundingRadius :: Float
valueRunBoundingRadius = 
    1.4142135 * fI (valueRunMessageRadius + valueRunMessageCharHth)

valueRunGridScale :: Float
valueRunGridScale =
    fI valueRunCubeRadius

valueRunGridCameraViewCMin :: Float 
valueRunGridCameraViewCMin =
    1.7320508 * fI valueRunCubeRadius

valueRunGridCameraViewCMax :: Float 
valueRunGridCameraViewCMax =
    valueGridSpaceBoxRadius


valueRunGridCameraViewCSens :: Float
valueRunGridCameraViewCSens =
    valueGridSpaceBoxRadius  - valueRunBoundingRadius


valueRunMessageSize :: UInt
valueRunMessageSize = 
    255

valueRunMessageSpeed :: Float 
valueRunMessageSpeed = 
    32.0

valueRunMessageRadius :: UInt
valueRunMessageRadius =
    10 * valueRunMessageCharWth

-- assert: non-empty!
valueRunMessageCharWth :: UInt
valueRunMessageCharWth =
    4

valueRunMessageCharHth :: UInt
valueRunMessageCharHth = 
    6

valueRunCubeRadius :: UInt
valueRunCubeRadius =
    20

valueRunPathRadius :: Float
valueRunPathRadius =
    0.5

valueRunFaceDrag :: Float
valueRunFaceDrag = 
    0.05

valueRunFaceBeginEndSpeed :: Float
valueRunFaceBeginEndSpeed =
    1.2

valueRunFaceSpeed:: Float
valueRunFaceSpeed = 
    1.2

valueRunAboutSettingsFadeTicksInv :: Float
valueRunAboutSettingsFadeTicksInv  = 
    1.0

valueRunPauseTicks :: Float
valueRunPauseTicks = 
    1.0

valueRunPauseTicksInv :: Float
valueRunPauseTicksInv = 
    1.0 / valueRunPauseTicks


valueRunAModeToBModeView :: View
valueRunAModeToBModeView = 
    View 0.0 0.0 20.0

valueRunAModeToBModeTicks :: Float
valueRunAModeToBModeTicks =
    4.0


--------------------------------------------------------------------------------
--  Mode

-- | typically >= 1.0
valueModePathTurnSpeed :: Float
valueModePathTurnSpeed =
    1.2

valueModePathRadius :: Float
valueModePathRadius =
    0.3


--------------------------------------------------------------------------------
--  Grid

valueGridPathTurnSens :: Float 
valueGridPathTurnSens =
    0.05

valueGridCameraViewASens :: Float 
valueGridCameraViewASens =
    4.0

valueGridCameraViewBSens :: Float 
valueGridCameraViewBSens =
    4.0

valueGridCameraViewCSens :: Float 
valueGridCameraViewCSens =
    16.0

valueGridCameraViewCMin :: Float 
valueGridCameraViewCMin =
    1.0

valueGridCameraViewCMax :: Float 
valueGridCameraViewCMax =
    valueGridSpaceBoxRadius


valueGridCameraViewASpeed :: Float 
valueGridCameraViewASpeed =
    8.0

valueGridCameraViewBSpeed :: Float 
valueGridCameraViewBSpeed =
    8.0

valueGridCameraViewCSpeed :: Float 
valueGridCameraViewCSpeed =
    4.0

valueGridPathSpeed :: Float 
valueGridPathSpeed =
    1.6

valueGridClockSpeedMin :: Float
valueGridClockSpeedMin =
    1.0

valueGridClockSpeedMax :: Float
valueGridClockSpeedMax =
    3.0


-- | call this value r. let mx my mz be the x, y, z colums of the current
--   modelview matrix. then 
--   r :  r * abs(mx + my + mz) < projection_far 
--   but we assume that modelview(3x3) is orthonormal, so 
--   r :  r < (projection_far / sqrt 3)
valueGridSpaceBoxRadius :: Float
valueGridSpaceBoxRadius =
    valuePerspectiveFar * 0.57735


valueGridMaxPathSize :: UInt
valueGridMaxPathSize =
    255


valueGridRollSize :: Float
valueGridRollSize =
    0.0625


--------------------------------------------------------------------------------
--  LevelMode

valueLevelModeGridView :: View
valueLevelModeGridView =
    View 0.1 0.4 4.5

valueLevelModeFailureTicks :: TickT
valueLevelModeFailureTicks =
    2.0

valueLevelModeFailureTicksInv :: TickT
valueLevelModeFailureTicksInv =
    1.0 / valueLevelModeFailureTicks

valueLevelModeOldPhysicalTicks :: TickT
valueLevelModeOldPhysicalTicks =
    2.0

valueLevelModeLevelInfoTicks :: TickT
valueLevelModeLevelInfoTicks =
    8.0

valueLevelPuzzleCompleteGridSize :: UInt
valueLevelPuzzleCompleteGridSize =
    128





--------------------------------------------------------------------------------
--  MemoryMode

valueMemoryModeGridShowView :: View
valueMemoryModeGridShowView =
    View 0.1 0.4 8.0

valueMemoryModeGridPlayView :: View
valueMemoryModeGridPlayView =
    View 0.1 0.4 4

valueMemoryModeGridFailureViewCPlus :: Float
valueMemoryModeGridFailureViewCPlus =
    4.0


valueMemoryModeGridFailureViewTicks :: TickT
valueMemoryModeGridFailureViewTicks =
    1.0


valueMemoryModeTextTicks :: Float
valueMemoryModeTextTicks =
    8.0

valueMemoryModeTextTicksInv :: Float
valueMemoryModeTextTicksInv =
    1.0 / valueMemoryModeTextTicks




--------------------------------------------------------------------------------
--  GUI

valueGUIBoardWth :: Float
valueGUIBoardWth =
    0.7



--------------------------------------------------------------------------------
--  Text


valueTextFontASize :: Float
valueTextFontASize =
    0.10

valueTextFontAY :: Float
valueTextFontAY =
    0.375


valueTextFontBSize :: Float
valueTextFontBSize =
    0.05

valueTextFontBY :: Float
valueTextFontBY =
    0.03


valueTextFontCSize :: Float
valueTextFontCSize =
    0.02

valueTextFontCY :: Float
valueTextFontCY =
    0.08





--------------------------------------------------------------------------------
--  Eggs

valueEggKonamiTicks :: Float
valueEggKonamiTicks =   
    20.0

valueEggKonamiTicksInv :: Float 
valueEggKonamiTicksInv =
    1.0 / valueEggKonamiTicks


--------------------------------------------------------------------------------
--  File

valueFileNameSpecialLevelPuzzleWorld :: FilePath
valueFileNameSpecialLevelPuzzleWorld =
    "world"


--------------------------------------------------------------------------------
--  Players

valuePlayersAchievementEggKonami :: String
valuePlayersAchievementEggKonami =
    "achievement_EggKonami"

valuePlayersSpecialLevelPuzzleWorldCompleted:: String
valuePlayersSpecialLevelPuzzleWorldCompleted =
    "achievement_SpecialLevelPuzzleWorldCompleted"

valuePlayersScoreMemoryMode :: String
valuePlayersScoreMemoryMode = 
    "score_MemoryMode"



