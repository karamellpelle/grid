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
module MEnv.IOS.Init
  (
    Init (..),

    ScreenOrientation (..),

  ) where

import MyPrelude
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C.Types
import Data.Bits
import Data.List


data Init =
    Init
    {
        initScreenMultisample :: !UInt,                 -- ^ number of multisamples
        initScreenOrientations :: [ScreenOrientation],  -- ^ orientations
        initScreenRate :: !UInt,                        -- ^ skip frames
        initSoundSampleRate :: !UInt,                   -- ^ sound rate per second
        initKeysAcclGyroRate :: !Float                  -- ^ update interval in seconds.
                                                        --   0.0 disables
    }


data ScreenOrientation =
    OrientationPortrait         |
    OrientationPortraitFlipped  |
    OrientationLandscapeLeft    |
    OrientationLandscapeRight 



instance Storable Init where
    sizeOf _    = 4 + 4 + 4 + 4 + 4
    alignment _ = 4
    poke ptr init = do
        pokeByteOff ptr 0  (fI $ initScreenMultisample init :: CUInt)
        pokeByteOff ptr 4  (pokeOrientations $ initScreenOrientations init)
        pokeByteOff ptr 8  (fI$ initScreenRate init :: CUInt)
        pokeByteOff ptr 12 (fI $ initSoundSampleRate init :: CUInt)
        pokeByteOff ptr 16 (rTF $ initKeysAcclGyroRate init :: CFloat)
    peek ptr = do
        mult <- peekByteOff ptr 0 :: IO CUInt 
        orin <- peekByteOff ptr 4 :: IO CUInt
        rateScreen <- peekByteOff ptr 8 :: IO CUInt
        rateSound <- peekByteOff ptr 12 :: IO CUInt
        rateKeys <- peekByteOff ptr 16 :: IO CFloat

        return Init
               {
                  initScreenMultisample = fI mult,
                  initScreenOrientations = peekOrientations orin,
                  initScreenRate = fI rateScreen,
                  initSoundSampleRate = fI rateSound,
                  initKeysAcclGyroRate = rTF rateKeys
               }

pokeOrientations :: [ScreenOrientation] -> CUInt
pokeOrientations os = 
    foldl' step 0x00000000 os
    where
      step a o = case o of
          OrientationPortrait         -> a .|. 0x01
          OrientationPortraitFlipped  -> a .|. 0x02
          OrientationLandscapeLeft    -> a .|. 0x04
          OrientationLandscapeRight   -> a .|. 0x08

peekOrientations :: CUInt -> [ScreenOrientation]
peekOrientations a = 
    testA ++ testB ++ testC ++ testD
    where
      testA = if testBit a 0 then [OrientationPortrait]         else []
      testB = if testBit a 1 then [OrientationPortraitFlipped]  else []
      testC = if testBit a 2 then [OrientationLandscapeLeft]    else []
      testD = if testBit a 3 then [OrientationLandscapeRight]   else []

    

