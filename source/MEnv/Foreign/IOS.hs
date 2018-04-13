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
{-# LANGUAGE ForeignFunctionInterface #-}
module MEnv.Foreign.IOS
  (
    foreignBeginForeign,
    foreignHandleForeignEnd,

  ) where

import MyPrelude
import MEnv
import Foreign.C



-- | start Foreign
foreignBeginForeign :: MEnv res ()
foreignBeginForeign = 
    io $ ios_foreignBeginForeign

foreign import ccall unsafe "ios_foreignBeginForeign" ios_foreignBeginForeign
    :: IO ()



-- | handle end of Foreign
foreignHandleForeignEnd :: a -> a -> MEnv res a
foreignHandleForeignEnd a a' =
    io $ ios_foreignHandleForeignEnd >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_foreignHandleForeignEnd" ios_foreignHandleForeignEnd
    :: IO CUInt

