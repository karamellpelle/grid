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
{-# LANGUAGE ForeignFunctionInterface #-}
module MEnv.System.IOS
  (
    systemHandleFrontBegin,
    systemHandleFrontEnd,
    systemHandleBackBegin,
    systemHandleBackEnd,

  ) where

import MyPrelude
import MEnv
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array



-- | application: begin frontground
systemHandleFrontBegin :: a -> a -> MEnv res a
systemHandleFrontBegin a a' = io $ 
    ios_systemHandleFrontBegin >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleFrontBegin" ios_systemHandleFrontBegin
    :: IO CUInt



-- | application: end frontground
systemHandleFrontEnd :: a -> a -> MEnv res a
systemHandleFrontEnd a a' = io $ 
    ios_systemHandleFrontEnd >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleFrontEnd" ios_systemHandleFrontEnd
    :: IO CUInt



-- | application: begin background
systemHandleBackBegin :: a -> a -> MEnv res a
systemHandleBackBegin a a' = io $ 
    ios_systemHandleBackBegin >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleBackBegin" ios_systemHandleBackBegin
    :: IO CUInt



-- | application: end background
systemHandleBackEnd :: a -> a -> MEnv res a
systemHandleBackEnd a a' = io $ 
    ios_systemHandleBackEnd >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleBackEnd" ios_systemHandleBackEnd
    :: IO CUInt



