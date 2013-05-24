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
module Game.Run.RunData.Plain.SoundRun
  (
    SoundRun (..),

    loadSoundRun,
    unloadSoundRun,
    beginSoundRunIterationBegin,
    endSoundRunIterationBegin,
  ) where

import MyPrelude
import Game.Values
import File

import OpenAL
import OpenAL.Helpers



data SoundRun =
    SoundRun
    {
        soundRunIterationBeginBuf :: !ALuint,
        soundRunSrc :: !ALuint
    }



loadSoundRun :: IO SoundRun
loadSoundRun = do
    -- buffer
    buf <- genBuf
    
    -- src, non-3D. typically for Iteration-sounds
    src <- genSrc
    alSourcei  src al_SOURCE_RELATIVE $ fI al_TRUE
    alSource3f src al_POSITION 0.0 0.0 0.0
    alSource3f src al_VELOCITY 0.0 0.0 0.0

    return SoundRun
           {
              soundRunIterationBeginBuf = buf,
              soundRunSrc = src
           }


unloadSoundRun :: SoundRun -> IO ()
unloadSoundRun sound = do
    -- alStopSource?
    delBuf $ soundRunIterationBeginBuf sound


-- | begin usege of sounds for iterationBegin
beginSoundRunIterationBegin :: SoundRun -> IO ()
beginSoundRunIterationBegin sound = do
    path <- fileStaticData "Run/Output/iteration_begin.caf"
    loadBuf (soundRunIterationBeginBuf sound) path



-- | end usage of sounds for iterationBegin
endSoundRunIterationBegin :: SoundRun -> IO ()
endSoundRunIterationBegin sound = do
    -- clear buffer data
    alBufferData (soundRunIterationBeginBuf sound) al_FORMAT_MONO16 nullPtr 0 0

