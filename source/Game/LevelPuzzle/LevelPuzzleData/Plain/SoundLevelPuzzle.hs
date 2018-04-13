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
module Game.LevelPuzzle.LevelPuzzleData.Plain.SoundLevelPuzzle
  (
    SoundLevelPuzzle (..),

    loadSoundLevelPuzzle,
    unloadSoundLevelPuzzle,
    beginSoundLevelPuzzleIterationComplete,
    endSoundLevelPuzzleIterationComplete,

  ) where

import MyPrelude
import Game.Values
import File

import OpenAL
import OpenAL.Helpers



data SoundLevelPuzzle =
    SoundLevelPuzzle
    {
        soundLevelPuzzleIterationBeginPlayBuf :: !ALuint,
        soundLevelPuzzleIterationCompleteBuf :: !ALuint,
        -- eat ...

        soundLevelPuzzleIterationSrc :: !ALuint,
        soundLevelPuzzleNodeSrc :: !ALuint
    }



loadSoundLevelPuzzle :: IO SoundLevelPuzzle
loadSoundLevelPuzzle = do
    -- buffer
    bufIterationBeginPlay <- makeBuf "LevelPuzzle/Output/iteration_beginplay.caf"
    bufIterationComplete <- genBuf

    -- non-3D source for Iterations
    srcIteration <- genSrc
    alSourcei  srcIteration al_SOURCE_RELATIVE $ fI al_TRUE
    alSource3f srcIteration al_POSITION 0.0 0.0 0.0
    alSource3f srcIteration al_VELOCITY 0.0 0.0 0.0

    -- 3D source for Content
    srcNode <- genSrc

    -- cone
    alSourcef srcNode al_CONE_INNER_ANGLE valueSoundLevelPuzzleNodeConeInnerAngle
    alSourcef srcNode al_CONE_OUTER_ANGLE valueSoundLevelPuzzleNodeConeOuterAngle
    alSourcef srcNode al_CONE_OUTER_GAIN  valueSoundLevelPuzzleNodeConeOuterGain

    -- distance properties
    alSourcef srcNode al_REFERENCE_DISTANCE valueSoundLevelPuzzleNodeReferenceDistance
    alSourcef srcNode al_MAX_DISTANCE valueSoundLevelPuzzleNodeMaxDistance
    alSourcef srcNode al_ROLLOFF_FACTOR valueSoundLevelPuzzleNodeRolloffFactor

    return SoundLevelPuzzle
           {
              soundLevelPuzzleIterationBeginPlayBuf = bufIterationBeginPlay,
              soundLevelPuzzleIterationCompleteBuf = bufIterationComplete,
              soundLevelPuzzleIterationSrc = srcIteration,
              soundLevelPuzzleNodeSrc = srcNode
           }

    where
      makeBuf path = do
          buf <- genBuf
          path' <- fileStaticData path
          loadBuf buf path'
          return buf


unloadSoundLevelPuzzle :: SoundLevelPuzzle -> IO ()
unloadSoundLevelPuzzle snd = do
    -- alStopSource?
    delSrc $ soundLevelPuzzleNodeSrc snd
    delSrc $ soundLevelPuzzleIterationSrc snd
    delBuf $ soundLevelPuzzleIterationBeginPlayBuf snd
    delBuf $ soundLevelPuzzleIterationCompleteBuf snd



--------------------------------------------------------------------------------
--  using IterationComplete

beginSoundLevelPuzzleIterationComplete :: SoundLevelPuzzle -> IO ()
beginSoundLevelPuzzleIterationComplete snd = do
    path <- fileStaticData "LevelPuzzle/Output/iteration_complete.caf"
    loadBuf (soundLevelPuzzleIterationCompleteBuf snd) path

endSoundLevelPuzzleIterationComplete :: SoundLevelPuzzle -> IO ()
endSoundLevelPuzzleIterationComplete snd = do
    alBufferData (soundLevelPuzzleIterationCompleteBuf snd) al_FORMAT_MONO16 nullPtr 0 0

