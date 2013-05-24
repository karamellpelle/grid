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
module Game.LevelPuzzle.Output.Fancy.Sound
  (
    outputSoundBeginPlay,
    outputSoundPlay,
    outputSoundPlay',
    outputSoundComplete,
    outputSoundComplete',
    outputSoundFailure,
    outputSoundFailure',

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Grid.Output
import Game.LevelPuzzle
import Game.LevelPuzzle.Iteration.State
import Game.LevelPuzzle.Output.Fancy.SoundLevelPuzzle
import Game.Run

import OpenAL
import OpenAL.Helpers




--------------------------------------------------------------------------------
--  BeginPlay

outputSoundBeginPlay :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundBeginPlay gamedata lvl run = do

    -- eat sounds
    handleAllEventsM_ lvl $ \event -> case event of
        
        EventPathEatDotFinish dot   -> playSoundPathEatDotFinish gamedata lvl

        _       -> return ()



--------------------------------------------------------------------------------
--  Play

outputSoundPlay :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundPlay gamedata lvl run = do
    return ()


outputSoundPlay' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                    s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundPlay' gamedata proj2D proj3D modv3D = \s lvl run -> do
    -- set listener
    listenerMat4 modv3D
    
    -- eat sounds
    handleAllEventsM_ lvl $ \event -> case event of
        --EventPathEatDotBonus dot   -> playSoundPathEatDotBonus gamedata lvl
        _       -> return ()     

    -- path sound
    let path = gridPath $ levelpuzzleGrid lvl
    playSoundPath gamedata lvl


--------------------------------------------------------------------------------
--  Failure

outputSoundFailure :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundFailure gamedata lvl run = do
    return ()


outputSoundFailure' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                       FailureS -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundFailure' gamedata proj2D proj3D modv3D = \s lvl run -> do
    -- set listener
    listenerMat4 modv3D

    -- path sound
    let path = gridPath $ levelpuzzleGrid lvl
    playSoundPath gamedata lvl



--------------------------------------------------------------------------------
--  Complete

outputSoundComplete :: GameData -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundComplete gamedata lvl run = do
    soundLevelPuzzleIterationComplete $ levelpuzzledataSoundLevelPuzzle
                                      $ gamedataLevelPuzzleData gamedata


outputSoundComplete' :: GameData -> Mat4 -> Mat4 -> Mat4 -> 
                        s -> LevelPuzzleWorld -> RunWorld -> IO ()
outputSoundComplete' gamedata proj2D proj3D modv3D = \s lvl run -> do
    -- set listener
    listenerMat4 modv3D



--------------------------------------------------------------------------------
--  

playSoundPathEatDotFinish :: GameData -> LevelPuzzleWorld -> IO ()
playSoundPathEatDotFinish gamedata lvl = do
    let snd = levelpuzzledataSoundLevelPuzzle $ gamedataLevelPuzzleData gamedata
    soundLevelPuzzleIterationBeginPlay snd


playSoundPath :: GameData -> LevelPuzzleWorld -> IO ()
playSoundPath gamedata lvl = do
    when (pathHasEventNewSegment $ levelpuzzlePath lvl) $ 
        case levelpuzzleSegmentsCount lvl of
            1   -> play 3.0
            2   -> play 2.0
            3   -> play 1.0
            n   -> return ()

    where
      play pitch = do
        let snd = griddataSoundPath $ gamedataGridData gamedata
        soundPathNewSegment snd (pathCurrent $ levelpuzzlePath lvl) pitch

