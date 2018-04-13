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
module Game.LevelPuzzleMode.Output.Fancy.Sound
  (
    outputSoundBeginPlay,
    outputSoundPlay,
    outputSoundPlay',
    outputSoundComplete,
    outputSoundComplete',
    outputSoundSpecialComplete,
    outputSoundSpecialComplete',
    outputSoundFailure,
    outputSoundFailure',

  ) where

import MyPrelude
import Game

import Game.Grid.GridData
import Game.Grid.Helpers
import Game.Grid.Output

import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.LevelPuzzleData
import Game.LevelPuzzleMode.Helpers
import Game.LevelPuzzleMode.Iteration.State

import Game.Run.RunWorld




--------------------------------------------------------------------------------
--  BeginPlay

outputSoundBeginPlay :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundBeginPlay gamedata lvl run = do
    return ()
--------------------------------------------------------------------------------
--  Play

outputSoundPlay :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundPlay gamedata lvl run = do
    return ()


outputSoundPlay' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                    s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundPlay' gamedata proj modv normal s lvl run = do
    return ()
--------------------------------------------------------------------------------
--  Failure

outputSoundFailure :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundFailure gamedata lvl run = do
    return ()


outputSoundFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                       FailureS -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundFailure' gamedata proj modv normal s lvl run = do
    return ()
--------------------------------------------------------------------------------
--  Complete

outputSoundComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundComplete gamedata lvl run = do
    return ()


outputSoundComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundComplete' gamedata proj modv normal s lvl run = do
    return ()


--------------------------------------------------------------------------------
--  SpecialComplete

outputSoundSpecialComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundSpecialComplete gamedata lvl run = do
    return ()

outputSoundSpecialComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                               s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundSpecialComplete' gamedata proj modv normal s lvl run = do
    return ()

--------------------------------------------------------------------------------
--  


-- | play sound if new segment
playPath :: GameData -> Path -> IO ()
playPath gamedata path = do
    return ()

