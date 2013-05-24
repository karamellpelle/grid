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
module Game.Memory.Output.Plain.Sound
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
import Game.Memory.Output.Plain.SoundMemory
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
outputSoundShow' gamedata proj modv normal s mem run = do
    -- set listener
    listenerMat4 modv

    -- path sound
    let path = gridPath $ memoryGrid mem
    playPath gamedata path 


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
outputSoundPlay' gamedata proj modv normal s mem run = do
    -- set listener
    listenerMat4 modv

    -- path sound
    let path = gridPath $ memoryGrid mem
    playPath gamedata path





--------------------------------------------------------------------------------
--  Failure

outputSoundFailure :: GameData -> MemoryWorld -> RunWorld -> IO ()
outputSoundFailure gamedata mem run = do
    soundMemoryIterationFailure $ memorydataSoundMemory 
                                $ gamedataMemoryData gamedata


outputSoundFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                       s -> MemoryWorld -> RunWorld -> IO ()
outputSoundFailure' gamedata proj modv normal tick mem run = do
    return ()



--------------------------------------------------------------------------------
--  

playPath :: GameData -> Path -> IO ()
playPath gamedata path = do
    when (pathHasEventNewSegment path) $ do
        soundPathNewSegment (griddataSoundPath $ gamedataGridData gamedata)
                            (pathCurrent path)

