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
module Game.Memory.Output.Plain
  (
    outputBeginShow,
    outputShow,
    outputShow',
    outputBeginPlay,
    outputPlay,
    outputPlay',
    outputFailure,
    outputFailure',

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Grid.Output
import Game.Memory
import Game.Run.RunWorld

import Game.Memory.Output.Plain.Sound
import Game.Memory.Output.Plain.Screen
import Game.Memory.Output.Plain.State

import OpenGL
import OpenGL.Helpers




--------------------------------------------------------------------------------
--  Show

outputBeginShow :: MemoryWorld -> RunWorld -> MEnv' (MemoryWorld, RunWorld)
outputBeginShow = \mem run -> do
    gamedata <- resourceGameData

    io $ do
      (mem', run') <- stateSetBeginShow mem run
      
      putStrLn "Memory.iterationBeginShow"
      outputSoundBeginShow gamedata mem' run'
      outputScreenBeginShow gamedata mem' run'
      
      return (mem', run')


outputShow :: MemoryWorld -> RunWorld -> MEnv' (MemoryWorld, RunWorld)
outputShow = \mem run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "Memory.iterationShow"
      outputSoundShow gamedata mem run
      outputScreenShow gamedata mem run

      return (mem, run)


outputShow' :: s -> MemoryWorld -> RunWorld -> MEnv' (s, MemoryWorld, RunWorld)
outputShow' = \s mem run -> do
    gamedata <- resourceGameData 
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear 
                               valuePerspectiveFar
        modv = mat4SceneCamera $ memoryGrid mem
        normal = modv

    io $ do
      outputSoundShow' gamedata proj modv normal s mem run
      outputScreenShow' gamedata proj modv normal s mem run
      stateStep s mem run


--------------------------------------------------------------------------------
--  Play

outputBeginPlay :: MemoryWorld -> RunWorld -> MEnv' (MemoryWorld, RunWorld)
outputBeginPlay = \mem run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "Memory.iterationBeginPlay"
      outputSoundBeginPlay gamedata mem run
      outputScreenBeginPlay gamedata mem run

      return (mem, run)


outputPlay :: MemoryWorld -> RunWorld -> MEnv' (MemoryWorld, RunWorld)
outputPlay = \mem run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "Memory.iterationPlay"
      outputSoundPlay gamedata mem run
      outputScreenPlay gamedata mem run

      return (mem, run)


outputPlay' :: s -> MemoryWorld -> RunWorld -> MEnv' (s, MemoryWorld, RunWorld)
outputPlay' = \s mem run -> do
    gamedata <- resourceGameData 
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear 
                               valuePerspectiveFar
        modv = mat4SceneCamera $ memoryGrid mem
        normal = modv

    io $ do
      outputSoundPlay' gamedata proj modv normal s mem run
      outputScreenPlay' gamedata proj modv normal s mem run
    
      return (s, mem, run)


--------------------------------------------------------------------------------
--  Failure

outputFailure :: MemoryWorld -> RunWorld -> MEnv' (MemoryWorld, RunWorld)
outputFailure = \mem run -> do
    gamedata <- resourceGameData
    io $ do
      putStrLn "Memory.iterationFailure"
      outputSoundFailure gamedata mem run
      outputScreenFailure gamedata mem run

      return (mem, run)


outputFailure' :: s -> MemoryWorld -> RunWorld -> MEnv' (s, MemoryWorld, RunWorld)
outputFailure' = \s mem run -> do
    gamedata <- resourceGameData
    let Shape wth hth = sceneShape $ runScene run
        proj = mat4Perspective valuePerspectiveFOVY
                               (wth / hth)
                               valuePerspectiveNear 
                               valuePerspectiveFar
        modv = mat4SceneCamera $ memoryGrid mem
        normal = modv

    io $ do
      outputSoundFailure' gamedata proj modv normal s mem run
      outputScreenFailure' gamedata proj modv normal s mem run

      return (s, mem, run)



