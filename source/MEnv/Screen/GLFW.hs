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
module MEnv.Screen.GLFW
  (
    screenFBO,
    screenSize,
    screenShape,
    screenSetRate,

  ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc

import MyPrelude
import MEnv

import OpenGL


--------------------------------------------------------------------------------
--  FBO

foreign import ccall unsafe "glfw_screenFBO" glfw_screenFBO
    :: IO CUInt

-- | screen framebuffer
screenFBO :: MEnv res GLuint
screenFBO = io $ do
    fmap fI glfw_screenFBO


--------------------------------------------------------------------------------
--  Size

foreign import ccall unsafe "glfw_screenSize" glfw_screenSize
    :: Ptr CUInt -> Ptr CUInt -> IO ()

-- | current screen size
screenSize :: MEnv res (UInt, UInt)
screenSize = io $ do
    alloca $ \ptrWth ->
      alloca $ \ptrHth -> do
          glfw_screenSize ptrWth ptrHth
          wth <- peek ptrWth
          hth <- peek ptrHth
          return (fromIntegral wth, fromIntegral hth)


-- | current normalized screen size
screenShape :: MEnv res (Float, Float)
screenShape =
    screenSize >>= \(wth, hth) ->
        let scale  = 1 / (fromIntegral $ max wth hth)
        in return (scale * fromIntegral wth, scale * fromIntegral hth)



--------------------------------------------------------------------------------
--  Screen rate


foreign import ccall unsafe "glfw_screenSetRate" glfw_screenSetRate
    :: CUInt -> IO ()


-- | set screen update interval. 0 sets the value used when initializing screen.
screenSetRate :: UInt -> MEnv res ()
screenSetRate r = io $ 
    glfw_screenSetRate (fI r)

     

