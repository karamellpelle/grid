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
module Game.LevelPuzzle.Output.Fancy
  (
    outputBeginPlay,
    outputPlay,
    outputPlay',
    outputFailure,
    outputFailure',
    outputComplete,
    outputComplete',

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.Grid.Output
import Game.LevelPuzzle
import Game.LevelPuzzle.Iteration.State
import Game.LevelPuzzle.Output.Fancy.Sound
import Game.LevelPuzzle.Output.Fancy.Screen
import Game.LevelPuzzle.Output.Fancy.State
import Game.Run

import Linear

-- fixme: remove outputBeginPlay, use iterationPlay

--------------------------------------------------------------------------------
--  outputBeginPlay

outputBeginPlay :: LevelPuzzleWorld -> RunWorld -> MEnv' (LevelPuzzleWorld, RunWorld)
outputBeginPlay = \lvl run -> do
    gamedata <- resourceGameData

    -- Tweak Scene
    let run' = handleAllEvents lvl run $ \run event -> case event of
               EventPathEatDotFinish dot -> runTweakPulse0Push run valuePulseFinish
               _                         -> run

    
    io $ do
      putStrLn "LevelPuzzle.iterationBeginPlay"
      (lvl', run'') <- ostateBeginPlaySet lvl run'
      outputSoundBeginPlay gamedata lvl' run''
      outputScreenBeginPlay gamedata lvl' run''
      return (lvl', run'')
    
    where
      valuePulseFinish = 1.0

--------------------------------------------------------------------------------
--  outputPlay

outputPlay :: LevelPuzzleWorld -> RunWorld -> MEnv' (LevelPuzzleWorld, RunWorld)
outputPlay = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationPlay"
      outputSoundPlay gamedata lvl run
      outputScreenPlay gamedata lvl run
      return (lvl, run)


outputPlay' :: s -> LevelPuzzleWorld -> RunWorld -> MEnv' (s, LevelPuzzleWorld, RunWorld) 
outputPlay' = \s lvl run -> do
    gamedata <- resourceGameData
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ levelpuzzleCamera lvl) `mappend`
                 (cameraModelMat4 $ levelpuzzleCamera lvl)

    -- Tweak Scene
    let run' = handleAllEvents lvl run $ \run event -> case event of
               --EventPathEatDotBonus dot -> runTweakXXX run valuePulseFinish
               _                         -> run
    io $ do
      (s', lvl', run'') <- ostatePlayBegin gamedata s lvl run'
      outputSoundPlay' gamedata proj2D proj3D modv3D s' lvl' run''
      outputScreenPlay' gamedata proj2D proj3D modv3D s' lvl' run''
      return (s', lvl', run'')  


--------------------------------------------------------------------------------
--  outputFailure

outputFailure :: LevelPuzzleWorld -> RunWorld -> 
                 MEnv' (LevelPuzzleWorld, RunWorld)
outputFailure = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationFailure"
      outputSoundFailure gamedata lvl run
      outputScreenFailure gamedata lvl run

      return (lvl, run)


outputFailure' :: FailureS -> LevelPuzzleWorld -> RunWorld -> 
                  MEnv' (FailureS, LevelPuzzleWorld, RunWorld)
outputFailure' = \s lvl run -> do
    gamedata <- resourceGameData
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ levelpuzzleCamera lvl) `mappend`
                 (cameraModelMat4 $ levelpuzzleCamera lvl)
    io $ do
      outputSoundFailure' gamedata proj2D proj3D modv3D s lvl run
      outputScreenFailure' gamedata proj2D proj3D modv3D s lvl run
      return (s, lvl, run)


--------------------------------------------------------------------------------
--  outputComplete


outputComplete :: LevelPuzzleWorld -> RunWorld -> MEnv' (LevelPuzzleWorld, RunWorld)
outputComplete = \lvl run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationComplete"
      (lvl', run') <- ostateCompleteSet lvl run
      outputSoundComplete gamedata lvl' run'
      outputScreenComplete gamedata lvl' run'
      return (lvl', run')


outputComplete' :: CompleteS -> LevelPuzzleWorld -> RunWorld -> 
                   MEnv' (CompleteS, LevelPuzzleWorld, RunWorld)
outputComplete' = \s lvl run -> do 
    gamedata <- resourceGameData
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ levelpuzzleCamera lvl) `mappend`
                 (cameraModelMat4 $ levelpuzzleCamera lvl)
    io $ do
      outputSoundComplete' gamedata proj2D proj3D modv3D s lvl run
      outputScreenComplete' gamedata proj2D proj3D modv3D s lvl run
      ostateCompleteEnd s lvl run


