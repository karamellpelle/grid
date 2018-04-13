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
module Game.Memory.Output.Fancy
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

import Game.Memory.Output.Fancy.Sound
import Game.Memory.Output.Fancy.Screen
import Game.Memory.Output.Fancy.State

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
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ memoryCamera mem) `mappend`
                 (cameraModelMat4 $ memoryCamera mem)

    io $ do
      outputSoundShow' gamedata proj2D proj3D modv3D s mem run
      outputScreenShow' gamedata proj2D proj3D modv3D s mem run
      return (s, mem, run)    

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
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ memoryCamera mem) `mappend`
                 (cameraModelMat4 $ memoryCamera mem)
    io $ do
      outputSoundPlay' gamedata proj2D proj3D modv3D s mem run
      outputScreenPlay' gamedata proj2D proj3D modv3D s mem run
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
    let scene = runScene run
        proj2D = sceneProj2D scene
        proj3D = sceneProj3D scene
        modv3D = (cameraViewMat4 $ memoryCamera mem) `mappend`
                 (cameraModelMat4 $ memoryCamera mem)
    io $ do
      outputSoundFailure' gamedata proj2D proj3D modv3D s mem run
      outputScreenFailure' gamedata proj2D proj3D modv3D s mem run
      return (s, mem, run)



