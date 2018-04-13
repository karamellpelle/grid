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
module Game.Memory.Output.Fancy.Sound
  (
    outputSoundBeginShow,
    outputSoundShow,
    outputSoundShow',
    outputSoundBeginPlay,
    outputSoundPlay,
    outputSoundPlay',
    outputSoundFailure,
    outputSoundFailure',

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Grid.Helpers
import Game.Grid.Output
import Game.Memory
import Game.Memory.Output.Fancy.SoundMemory
import Game.Run.RunWorld

import OpenAL
import OpenAL.Helpers




--------------------------------------------------------------------------------
--  Show

outputSoundBeginShow :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputSoundBeginShow gamedata mem run = do
    return ()

outputSoundShow :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputSoundShow gamedata mem run = do
    return ()

outputSoundShow' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                    s -> MemoryWorld -> RunWorld -> IO ()
outputSoundShow' gamedata proj2D proj3D modv3D = \s mem run -> do
    -- set listener
    listenerMat4 modv3D



--------------------------------------------------------------------------------
--  Play

outputSoundBeginPlay :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputSoundBeginPlay gamedata mem run = do
    return ()


outputSoundPlay :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputSoundPlay gamedata mem run = do
    return ()


outputSoundPlay' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                    s -> MemoryWorld -> RunWorld -> IO ()
outputSoundPlay' gamedata proj2D proj3D modv3D = \s mem run -> do
    -- set listener
    listenerMat4 modv3D






--------------------------------------------------------------------------------
--  Failure

outputSoundFailure :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputSoundFailure gamedata mem run = do
    soundMemoryIterationFailure $ memorydataSoundMemory 
                                $ gamedataMemoryData gamedata


outputSoundFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                       s -> MemoryWorld -> RunWorld -> IO ()
outputSoundFailure' gamedata proj2D proj3D modv3D = \s mem run -> do
    -- set listener
    listenerMat4 modv3D



