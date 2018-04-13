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
module Game.Run.RunData.Fancy.SoundRun
  (
    SoundRun (..),

    loadSoundRun,
    unloadSoundRun,
    unloadSoundRunIterationBegin,
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
    -- src, non-3D. typically for Iteration-sounds
    src <- genSrc
    alSourcei  src al_SOURCE_RELATIVE $ fI al_TRUE
    alSource3f src al_POSITION 0.0 0.0 0.0
    alSource3f src al_VELOCITY 0.0 0.0 0.0

    -- buffer for iterationBegin 
    bufBegin <- genBuf
    fileStaticData "Run/Output/iteration_begin.caf" >>= loadBuf bufBegin
    
    return SoundRun
           {
              soundRunSrc = src,
              soundRunIterationBeginBuf = bufBegin
           }
    

unloadSoundRun :: SoundRun -> IO ()
unloadSoundRun sound = do
    alSourceStop $ soundRunSrc sound
    -- fixme!!: unload buffer iff IterationBegin part is not unloaded.
    --          but I don't think OpenAL consider 0 to be a special buffer name.
    delBuf $ soundRunIterationBeginBuf sound
    delSrc $ soundRunSrc sound


-- | unload the IterationBegin part of resource
unloadSoundRunIterationBegin :: SoundRun -> IO ()
unloadSoundRunIterationBegin sound = do
    alSourceStop $ soundRunSrc sound
    alBufferData (soundRunIterationBeginBuf sound) al_FORMAT_MONO16 nullPtr 0 0

