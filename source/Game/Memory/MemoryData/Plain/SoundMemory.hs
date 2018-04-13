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
module Game.Memory.MemoryData.Plain.SoundMemory
  (
    SoundMemory (..),

    loadSoundMemory,
    unloadSoundMemory,
  ) where

import MyPrelude
import Game.Values
import File

import OpenAL
import OpenAL.Helpers



data SoundMemory =
    SoundMemory
    {
        soundMemoryIterationFailureBuf :: !ALuint,
        soundMemorySrc :: !ALuint
    }



loadSoundMemory :: IO SoundMemory
loadSoundMemory = do
    -- buffer
    buf <- genBuf
    path <- fileStaticData "Memory/Output/iteration_failure.caf"
    loadBuf buf path
    
    -- src
    src <- genSrc
    
    -- make source non-3D
    alSourcei  src al_SOURCE_RELATIVE $ fI al_TRUE
    alSource3f src al_POSITION 0.0 0.0 0.0
    alSource3f src al_VELOCITY 0.0 0.0 0.0

    -- set default buffer
    alSourcei  src al_BUFFER (fI buf)
  
    return SoundMemory
           {
              soundMemoryIterationFailureBuf = buf,
              soundMemorySrc = src
           }


unloadSoundMemory :: SoundMemory -> IO ()
unloadSoundMemory sound = do
    -- alStopSource?
    delSrc $ soundMemorySrc sound
    delBuf $ soundMemoryIterationFailureBuf sound


