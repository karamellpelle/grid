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
module Game.LevelPuzzleMode.Output.Fancy
  (
    outputBeginPlay,
    outputPlay,
    outputPlay',
    outputFailure,
    outputFailure',
    outputComplete,
    outputComplete',
    outputSpecialComplete,
    outputSpecialComplete',

  ) where


import MyPrelude
import Game
import Linear

import Game.Grid.Output
import Game.Grid.Output.Fancy
import Game.Grid.GridWorld
import Game.Grid.Helpers

import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.LevelPuzzleData
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.Iteration.State
import Game.LevelPuzzleMode.Output.Fancy.Screen

import Game.Run.RunWorld 



--------------------------------------------------------------------------------
--  outputBeginPlay

outputBeginPlay :: LevelPuzzleWorld -> RunWorld -> 
                   MEnv' (LevelPuzzleWorld, RunWorld)
outputBeginPlay lvl run = do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationBeginPlay"
      outputScreenBeginPlay gamedata lvl run
    
    return (lvl, run)



--------------------------------------------------------------------------------
--  outputPlay

outputPlay :: LevelPuzzleWorld -> RunWorld -> 
              MEnv' (LevelPuzzleWorld, RunWorld)
outputPlay lvl run = do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationPlay"
      outputScreenPlay gamedata lvl run

      return (lvl, run)


outputPlay' :: s -> LevelPuzzleWorld -> RunWorld -> 
               MEnv' (s, LevelPuzzleWorld, RunWorld) 
outputPlay' s lvl run = do
    gamedata <- resourceGameData
    (wth, hth) <- screenShape
    let proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    outputScreenPlay' gamedata proj modv normal s lvl run

    return (s, lvl, run)


--------------------------------------------------------------------------------
--  outputFailure

outputFailure :: LevelPuzzleWorld -> RunWorld -> 
                 MEnv' (LevelPuzzleWorld, RunWorld)
outputFailure lvl run = do
    gamedata <- resourceGameData
    io $ do
      putStrLn "LevelPuzzle.iterationFailure"
      outputScreenFailure gamedata lvl run

      return (lvl, run)


outputFailure' :: FailureS -> LevelPuzzleWorld -> RunWorld -> 
                  MEnv' (FailureS, LevelPuzzleWorld, RunWorld)
outputFailure' s lvl run = do
    gamedata <- resourceGameData
    (wth, hth) <- screenShape
    let proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    outputScreenFailure' gamedata proj modv normal s lvl run

    return (s, lvl, run)


--------------------------------------------------------------------------------
--  outputComplete


outputComplete :: LevelPuzzleWorld -> RunWorld -> 
                  MEnv' (LevelPuzzleWorld, RunWorld)
outputComplete lvl run = do
    gamedata <- resourceGameData
    io $ do
      (lvl', run') <- return (lvl, run)

      putStrLn "LevelPuzzle.iterationComplete"
      outputScreenComplete gamedata lvl' run'

      return (lvl', run')


outputComplete' :: s -> LevelPuzzleWorld -> RunWorld -> 
                   MEnv' (s, LevelPuzzleWorld, RunWorld)
outputComplete' s lvl run = do 
    gamedata <- resourceGameData
    (wth, hth) <- screenShape
    let proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    outputScreenComplete' gamedata proj modv normal s lvl run
    return (s, lvl, run) 



--------------------------------------------------------------------------------
--  outputSpecialComplete


outputSpecialComplete :: LevelPuzzleWorld -> RunWorld -> 
                         MEnv' (LevelPuzzleWorld, RunWorld)
outputSpecialComplete lvl run = do
    gamedata <- resourceGameData
    io $ do
      (lvl', run') <- return (lvl, run)

      putStrLn "LevelPuzzle.iterationSpecialComplete"
      outputScreenSpecialComplete gamedata lvl' run'

      return (lvl', run')


outputSpecialComplete' :: s -> LevelPuzzleWorld -> RunWorld -> 
                          MEnv' (s, LevelPuzzleWorld, RunWorld)
outputSpecialComplete' s lvl run = do 
    gamedata <- resourceGameData
    (wth, hth) <- screenShape
    let proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear
                               valuePerspectiveFar
        modv = mat4SceneCamera $ levelpuzzleGrid lvl
        normal = modv
    outputScreenSpecialComplete' gamedata proj modv normal s lvl run

    return (s, lvl, run)


