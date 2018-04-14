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
module Game.Run.RunData.Plain.SoundScene
  (
    SoundScene (..),

    loadSoundScene,
    unloadSoundScene,

    NoiseArray,
    noisearrayAt,
    makeNoiseArray,

  ) where

import MyPrelude
import Game.Values
import File

import Data.Array.Base

import OpenAL
import OpenAL.Helpers



data SoundScene =
    SoundScene
    {
        soundSceneNoiseBufs :: !NoiseArray,
        soundSceneNoiseSrc :: !ALuint
    }



loadSoundScene :: IO SoundScene
loadSoundScene = do
   
    bufs <- forM names $ \name -> do
        buf <- genBuf
        fileStaticData ("Run/Scene/" ++ name) >>= loadBuf buf
        return buf

    -- src
    src <- genSrc
    -- cone
    alSourcef src al_CONE_INNER_ANGLE valueSoundSceneNoiseConeInnerAngle
    alSourcef src al_CONE_OUTER_ANGLE valueSoundSceneNoiseConeOuterAngle
    alSourcef src al_CONE_OUTER_GAIN valueSoundSceneNoiseConeOuterGain
    -- distance properties
    alSourcef src al_REFERENCE_DISTANCE valueSoundSceneNoiseReferenceDistance
    alSourcef src al_MAX_DISTANCE valueSoundSceneNoiseMaxDistance
    alSourcef src al_ROLLOFF_FACTOR valueSoundSceneNoiseRolloffFactor

    return SoundScene
           {
              soundSceneNoiseBufs = makeNoiseArray bufs,
              soundSceneNoiseSrc = src
           }
    where
      -- note: remember to update valueNoisesSize!!
      names = [ "noise0.mp3", "noise1.mp3", "noise2.mp3", "noise3.mp3" ]

      valueSoundSceneNoiseConeInnerAngle :: ALfloat
      valueSoundSceneNoiseConeInnerAngle = 360.0

      valueSoundSceneNoiseConeOuterAngle :: ALfloat
      valueSoundSceneNoiseConeOuterAngle = 360.0

      valueSoundSceneNoiseConeOuterGain :: ALfloat
      valueSoundSceneNoiseConeOuterGain = 1.0

      valueSoundSceneNoiseReferenceDistance :: ALfloat
      valueSoundSceneNoiseReferenceDistance = 50.0 

      valueSoundSceneNoiseMaxDistance :: ALfloat
      valueSoundSceneNoiseMaxDistance = 1024.0

      valueSoundSceneNoiseRolloffFactor :: ALfloat
      valueSoundSceneNoiseRolloffFactor = 3.0

      
unloadSoundScene :: SoundScene -> IO ()
unloadSoundScene sound = do
    -- alStopSource?
    delSrc $ soundSceneNoiseSrc sound
    
    forM_ (range 0 valueSoundSceneNoiseSize) $ \ix -> 
        delBuf $ noisearrayAt (soundSceneNoiseBufs sound) ix


--------------------------------------------------------------------------------
--  Noises

valueSoundSceneNoises :: [ String ]
valueSoundSceneNoises =
      [ "noise0.mp3", "noise1.mp3", "noise2.mp3", "noise3.mp3" ]
      
valueSoundSceneNoiseSize :: UInt
valueSoundSceneNoiseSize = 
    length' valueSoundSceneNoises


--------------------------------------------------------------------------------
--  NoiseArray 

type NoiseArray =
    UArray Int Word

noisearrayAt :: NoiseArray -> UInt -> ALuint
noisearrayAt array ix = 
    fI $ unsafeAt array (fI ix)

makeNoiseArray :: [ALuint] -> NoiseArray
makeNoiseArray es = 
    listArray (0, length es - 1) $ map fI es 




