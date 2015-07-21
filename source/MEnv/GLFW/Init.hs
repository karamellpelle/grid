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
module MEnv.GLFW.Init
  (
    Init (..),


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
        initScreenMultisample :: !UInt,         -- ^ number of multisamples
        initScreenFullscreen :: !Bool           -- ^ fullscreen?
    }


instance Storable Init where
    sizeOf _    = 4 + 4
    alignment _ = 4
    poke ptr init = do
        pokeByteOff ptr 0  (fI $ initScreenMultisample init :: CUInt)
        pokeByteOff ptr 4  (if initScreenFullscreen init then 1 else 0 :: CUInt)
    peek ptr = do
        mult <- peekByteOff ptr 0 :: IO CUInt 
        full <- peekByteOff ptr 4 :: IO CUInt

        return Init
               {
                  initScreenMultisample = fI mult,
                  initScreenFullscreen = (full /= 0)
               }

