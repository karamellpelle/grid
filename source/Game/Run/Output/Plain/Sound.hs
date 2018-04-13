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
module Game.Run.Output.Plain.Sound
  (
    outputSoundBegin,
    outputSoundBegin',
    outputSoundMain,
    outputSoundMain',
    outputSoundEnd,
    outputSoundEnd',

    outputSoundLevelMode,
    outputSoundPuzzleMode,
    outputSoundMemoryMode,
    outputSoundForeign,

    outputSoundAtFace',
    outputSoundAtFace''',
    outputSoundAToB,

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.Grid.Output
import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.Iteration.State
import Game.Run.Output.Plain.SoundRun

import OpenAL
import OpenAL.Helpers



--------------------------------------------------------------------------------
--  Begin

outputSoundBegin :: GameData -> RunWorld -> IO ()
outputSoundBegin gamedata run = do
    -- play intro sound
    soundRunIterationBegin $ rundataSoundRun $ gamedataRunData gamedata


outputSoundBegin' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                     s -> RunWorld -> b -> IO ()
outputSoundBegin' gamedata proj modv normal s run b = do
    -- set listener
    listenerMat4 modv



--------------------------------------------------------------------------------
--  Main

outputSoundMain :: GameData -> RunWorld -> IO ()
outputSoundMain gamedata run = do
    return ()


outputSoundMain' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                    s -> RunWorld -> b -> IO ()
outputSoundMain' gamedata proj modv normal s run b = do
    -- set listener
    listenerMat4 modv



--------------------------------------------------------------------------------
--  End

outputSoundEnd :: GameData -> RunWorld -> IO ()
outputSoundEnd gamedata run = do
    return ()

outputSoundEnd' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                   s -> RunWorld -> b -> IO ()
outputSoundEnd' gamedata proj modv normal s run b = do
    return ()


--------------------------------------------------------------------------------
--  Faces

outputSoundLevelMode :: GameData -> RunWorld -> IO ()
outputSoundLevelMode gamedata run = do
    return ()

outputSoundPuzzleMode :: GameData -> RunWorld -> IO ()
outputSoundPuzzleMode gamedata run = do
    return ()

outputSoundMemoryMode :: GameData -> RunWorld -> IO ()
outputSoundMemoryMode gamedata run = do
    return ()

outputSoundForeign :: GameData -> RunWorld -> IO ()
outputSoundForeign gamedata run = do
    return ()

--------------------------------------------------------------------------------
--  AtFace

outputSoundAtFace' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                      AtFaceState -> RunWorld -> b -> IO ()
outputSoundAtFace' gamedata proj modv normal s run b = do
    return ()

outputSoundAtFace''' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        s -> RunWorld -> b -> IO ()
outputSoundAtFace''' gamedata proj modv normal s run b = do
    return ()



--------------------------------------------------------------------------------
--  

outputSoundAToB :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                   s -> RunWorld -> b -> IO ()
outputSoundAToB gamedata proj modv normal s run b = do
    return ()



