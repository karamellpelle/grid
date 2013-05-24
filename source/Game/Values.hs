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
    valueRunBeginTicks,
    valueRunBeginTicksInv,
    valueRunGridView,
    valueRunGridViewABound,
    valueRunGridViewBBound,
    valueRunGridViewCBound,
    valueRunGridCameraViewCMin,
    valueRunGridCameraViewCMax,
    valueRunGridCameraViewCSens,
    valueRunGridCameraViewCScale,
    valueRunBoundingRadius,
    valueRunMessageCharWth,
    valueRunMessageRadius,
    valueRunMessageSegsPerWait,
    valueRunMessageSegsPerTick,
    valueRunMessageSpeed,
    valueRunMessageSize,
    valueRunFaceSpeed,
    valueRunTurnSpeed,
    valueRunCubeRadius,
    valueRunAboutSettingsTicksInv,
    valueRunPauseTicks, 
    valueRunPauseTicksInv, 
    valueRunIterationBeginT0,
    valueRunIterationBeginS0,
    valueRunIterationBeginT1,
    valueRunIterationBeginS1,
    valueRunIterationBeginT2,
    valueRunIterationBeginS2,
    valueRunIterationBeginT3,
    valueRunIterationBeginS3,
    valueRunIterationBeginT4,
    valueRunIterationBeginS4,

    valueGridPathTurnSpeed,
    valueGridCameraViewASens,
    valueGridCameraViewBSens,
    valueGridCameraViewCSens,
    valueGridCameraViewASpeed,
    valueGridCameraViewBSpeed,
    valueGridCameraViewCSpeed,
    valueGridCameraViewCMin,
    valueGridCameraViewCMax,
    valueGridMaxPathSize,
    valueGridRollSize,
    valueGridClockSpeedMin,
    valueGridClockSpeedMax,
    valueGridControlWaitSens,
    valueGridControlContinueSens0,
    valueGridControlContinueSens1,

    valueLevelPuzzleGridView,
    valueLevelPuzzlePathSpeedLevel,
    valueLevelPuzzlePathSpeedPuzzle,
    valueMemoryGridView,
    valueMemoryPathSpeed,

    valueGUIWth,
    valueGUIHth,
    valueGUIBorderSize,
    valueGUIContourSize,
    valueEggKonamiTicks,
    valueEggKonamiTicksInv,
   
    valueFileNameSpecialLevelPuzzleWorld,

    valuePlayersAchievementEggKonami,
    valuePlayersAchievementSpecialLevelPuzzleWorldCompleted,
    valuePlayersScoreMemoryMode,

#ifdef GRID_STYLE_FANCY
    module Game.Values.Fancy,
#endif
#ifdef GRID_STYLE_PLAIN
    module Game.Values.Plain,
#endif

  ) where


import MyPrelude
import MEnv.Tick
import Game.Font
import Game.Data.View

#ifdef GRID_STYLE_FANCY
import Game.Values.Fancy
#endif
#ifdef GRID_STYLE_PLAIN
import Game.Values.Plain
#endif

--   * if only one module needs a value, it may be better to define the variable there, 
--     to prevent too much recompilation. however, it is nice that only one module
--     controls all the game settings. 
--   * fixme: some of these values depends on 'valuePerspectiveRadius' which is inside
--            Plain/Fancy, and this module should not know about these modules. 
--


--------------------------------------------------------------------------------
--  Run

valueRunBeginTicks :: Float
valueRunBeginTicks  =
    4.0

valueRunBeginTicksInv :: Float 
valueRunBeginTicksInv = 
    1.0 / valueRunBeginTicks

valueRunGridView :: View
valueRunGridView =
    View 0.0 0.0 70.0

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

valueRunGridCameraViewCScale :: Float
valueRunGridCameraViewCScale =
    valueRunCubeRadius

valueRunGridCameraViewCMin :: Float 
valueRunGridCameraViewCMin =
    1.7320508 * valueRunCubeRadius

valueRunGridCameraViewCMax :: Float 
valueRunGridCameraViewCMax =
    120.0 

valueRunGridCameraViewCSens :: Float
valueRunGridCameraViewCSens =
    valuePerspectiveRadius  - valueRunBoundingRadius

valueRunMessageSize :: UInt
valueRunMessageSize = 
    255

valueRunMessageRadius :: UInt
valueRunMessageRadius =
    10 * valueRunMessageCharWth

-- | assert: non-empty!
valueRunMessageCharWth :: UInt
valueRunMessageCharWth =
    4

valueRunMessageCharHth :: UInt
valueRunMessageCharHth = 
    6

-- | number of segments per wait character ('\0')
valueRunMessageSegsPerWait :: UInt
valueRunMessageSegsPerWait = 
    8

-- | number of segments per 1.0 Tick
valueRunMessageSegsPerTick :: UInt
valueRunMessageSegsPerTick  = 
    32

valueRunMessageSpeed :: Float 
valueRunMessageSpeed = 
    fI valueRunMessageSegsPerTick 


valueRunCubeRadius :: Float
valueRunCubeRadius =
    20.0

valueRunFaceSpeed :: Float
valueRunFaceSpeed =
    2.0

valueRunTurnSpeed:: Float
valueRunTurnSpeed = 
    3.0

valueRunAboutSettingsTicksInv :: Float
valueRunAboutSettingsTicksInv  = 
    1.0

valueRunPauseTicks :: Float
valueRunPauseTicks = 
    2.0

valueRunPauseTicksInv :: Float
valueRunPauseTicksInv = 
    1.0 / valueRunPauseTicks


valueRunIterationBeginT0 :: Tick
valueRunIterationBeginT0 = 50.0

valueRunIterationBeginS0 :: Float
valueRunIterationBeginS0 = 1.0 / rTF valueRunIterationBeginT0
    
valueRunIterationBeginT1 :: Tick
valueRunIterationBeginT1 = 20.0

valueRunIterationBeginS1 :: Float
valueRunIterationBeginS1 = 1.0 / rTF valueRunIterationBeginT1
    
valueRunIterationBeginT2 :: Tick
valueRunIterationBeginT2 = 8.0

valueRunIterationBeginS2 :: Float
valueRunIterationBeginS2 = 1.0 / rTF valueRunIterationBeginT2
    
valueRunIterationBeginT3 :: Tick
valueRunIterationBeginT3 = 1.0 / rTF valueRunIterationBeginS3

valueRunIterationBeginS3 :: Float
valueRunIterationBeginS3 = 0.00172 * valueRunMessageSpeed
    
valueRunIterationBeginT4 :: Tick
valueRunIterationBeginT4 = 1.0

valueRunIterationBeginS4 :: Float
valueRunIterationBeginS4 = 1.0 / rTF valueRunIterationBeginT4
    
    
    
--------------------------------------------------------------------------------
--  Grid

-- | typically >= 1.0
valueGridPathTurnSpeed :: Float
valueGridPathTurnSpeed =
    1.1

valueGridCameraViewASens :: Float 
valueGridCameraViewASens =
    4.0

valueGridCameraViewBSens :: Float 
valueGridCameraViewBSens =
    4.0

valueGridCameraViewCSens :: Float 
valueGridCameraViewCSens =
    48.0

valueGridCameraViewCMin :: Float 
valueGridCameraViewCMin =
    1.0

valueGridCameraViewCMax :: Float 
valueGridCameraViewCMax =
    valuePerspectiveRadius

valueGridCameraViewASpeed :: Float 
valueGridCameraViewASpeed =
    8.0

valueGridCameraViewBSpeed :: Float 
valueGridCameraViewBSpeed =
    8.0

valueGridCameraViewCSpeed :: Float 
valueGridCameraViewCSpeed =
    6.0

-- | 
valueGridClockSpeedMin :: Float
valueGridClockSpeedMin =
    1.0

valueGridClockSpeedMax :: Float
valueGridClockSpeedMax =
    4.0

valueGridMaxPathSize :: UInt
valueGridMaxPathSize =
    255

valueGridRollSize :: Float
valueGridRollSize =
    0.0625

valueGridControlWaitSens :: Float
valueGridControlWaitSens =
    0.05

valueGridControlContinueSens0 :: Float
valueGridControlContinueSens0 =
    0.01

valueGridControlContinueSens1 :: Float
valueGridControlContinueSens1 =
    0.05

--------------------------------------------------------------------------------
--  LevelMode

valueLevelPuzzleGridView :: View
valueLevelPuzzleGridView =
    View 0.05 0.3 10.0

valueLevelPuzzlePathSpeedLevel :: Float
valueLevelPuzzlePathSpeedLevel = 
    1.5

valueLevelPuzzlePathSpeedPuzzle :: Float
valueLevelPuzzlePathSpeedPuzzle = 
    5.0
--------------------------------------------------------------------------------
--  MemoryMode

valueMemoryGridView :: View
valueMemoryGridView =
    View 0.16 0.4 8.0

valueMemoryPathSpeed :: Float
valueMemoryPathSpeed =
    1.5

--------------------------------------------------------------------------------
--  GUI

valueGUIWth :: Float
valueGUIWth =
    0.65

valueGUIHth :: Float
valueGUIHth =
    0.4

valueGUIBorderSize :: Float
valueGUIBorderSize = 
    0.02

valueGUIContourSize :: Float
valueGUIContourSize = 
    0.02



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

valuePlayersAchievementSpecialLevelPuzzleWorldCompleted :: String
valuePlayersAchievementSpecialLevelPuzzleWorldCompleted =
    "achievement_SpecialLevelPuzzleWorldCompleted"

valuePlayersScoreMemoryMode :: String
valuePlayersScoreMemoryMode = 
    "score_MemoryMode"

