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
module Game.Grid.GridData.Plain.SoundPath
  (
    SoundPath (..),

    loadSoundPath,
    unloadSoundPath,

  ) where

import MyPrelude
import Game.Values
import File

import OpenAL
import OpenAL.Helpers



data SoundPath =
    SoundPath
    {
        soundPathBuf :: !ALuint,
        soundPathSrc :: !ALuint
    }



loadSoundPath :: IO SoundPath
loadSoundPath = do
    -- buffer
    buf <- genBuf
    fileStaticData "Grid/Output/path_newsegment.mp3" >>= loadBuf buf
    
    -- src
    src <- genSrc

    -- cone
    alSourcef src al_CONE_INNER_ANGLE valueSoundPathConeInnerAngle
    alSourcef src al_CONE_OUTER_ANGLE valueSoundPathConeOuterAngle
    alSourcef src al_CONE_OUTER_GAIN  valueSoundPathConeOuterGain

    -- distance properties
    alSourcef src al_REFERENCE_DISTANCE valueSoundPathReferenceDistance
    alSourcef src al_MAX_DISTANCE valueSoundPathMaxDistance
    alSourcef src al_ROLLOFF_FACTOR valueSoundPathRolloffFactor

    -- set default buffer
    alSourcei src al_BUFFER $ fI buf

    return SoundPath
           {
              soundPathBuf = buf,
              soundPathSrc = src
           }



unloadSoundPath :: SoundPath -> IO ()
unloadSoundPath sound = do
    -- alStopSource?
    delSrc $ soundPathSrc sound
    delBuf $ soundPathBuf sound
