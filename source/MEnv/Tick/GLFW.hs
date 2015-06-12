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
module MEnv.Tick.GLFW
  (
    Tick,
    tickGet,
    tickSet,
   
    tickClockAGet,
    tickClockASet,
    tickClockAGetSpeed,
    tickClockASetSpeed,

    tickClockBGet,
    tickClockBSet,
    tickClockBGetSpeed,
    tickClockBSetSpeed,

    tickClockCGet,
    tickClockCSet,
    tickClockCGetSpeed,
    tickClockCSetSpeed,

    tickClockDGet,
    tickClockDSet,
    tickClockDGetSpeed,
    tickClockDSetSpeed,
    
    tickClockEGet,
    tickClockESet,
    tickClockEGetSpeed,
    tickClockESetSpeed,

    tickClockFGet,
    tickClockFSet,
    tickClockFGetSpeed,
    tickClockFSetSpeed,
    
  ) where


import Foreign.C.Types
import MyPrelude
import MEnv



-- | time-type
type Tick =
    Double





--------------------------------------------------------------------------------
--  


foreign import ccall unsafe "glfw_tickGet" glfw_tickGet
    :: IO CDouble

-- | get current tick
tickGet :: MEnv res Tick
tickGet = io $ do
    fmap realToFrac glfw_tickGet



foreign import ccall unsafe "glfw_tickSet" glfw_tickSet
    :: CDouble -> IO ()
    
-- | set current tick
tickSet :: Tick -> MEnv res ()
tickSet t = io $ do
    glfw_tickSet (realToFrac t)
    


--------------------------------------------------------------------------------
-- clockA


foreign import ccall unsafe "glfw_tickClockAGet" glfw_tickClockAGet
    :: IO CDouble

-- | get tick for ClockA
tickClockAGet :: MEnv res Tick
tickClockAGet = io $ do
    fmap realToFrac glfw_tickClockAGet


foreign import ccall unsafe "glfw_tickClockASet" glfw_tickClockASet
    :: CDouble -> IO ()

-- | set tick for ClockA
tickClockASet :: Tick -> MEnv res ()
tickClockASet t = io $ do
    glfw_tickClockASet (realToFrac t)


foreign import ccall unsafe "glfw_tickClockASetSpeed" glfw_tickClockASetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockA. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockASetSpeed :: Double -> MEnv res ()
tickClockASetSpeed a =  io $ do
    glfw_tickClockASetSpeed (realToFrac a)


foreign import ccall unsafe "glfw_tickClockAGetSpeed" glfw_tickClockAGetSpeed
    :: IO CDouble

-- | get speed of ClockA
tickClockAGetSpeed :: MEnv res Double
tickClockAGetSpeed = io $ do
    fmap realToFrac glfw_tickClockAGetSpeed


--------------------------------------------------------------------------------
-- clock B

foreign import ccall unsafe "glfw_tickClockBGet" glfw_tickClockBGet
    :: IO CDouble

-- | get tick for ClockB
tickClockBGet :: MEnv res Tick
tickClockBGet = io $ do
    fmap realToFrac glfw_tickClockBGet


foreign import ccall unsafe "glfw_tickClockBSet" glfw_tickClockBSet
    :: CDouble -> IO ()

-- | set tick for ClockB
tickClockBSet :: Tick -> MEnv res ()
tickClockBSet t = io $ do
    glfw_tickClockBSet (realToFrac t)


foreign import ccall unsafe "glfw_tickClockBSetSpeed" glfw_tickClockBSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockB. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockBSetSpeed :: Double -> MEnv res ()
tickClockBSetSpeed a =  io $ do
    glfw_tickClockBSetSpeed (realToFrac a)


foreign import ccall unsafe "glfw_tickClockBGetSpeed" glfw_tickClockBGetSpeed
    :: IO CDouble

-- | get speed of ClockB
tickClockBGetSpeed :: MEnv res Double
tickClockBGetSpeed = io $ do
    fmap realToFrac glfw_tickClockCGetSpeed


--------------------------------------------------------------------------------
-- clock C


foreign import ccall unsafe "glfw_tickClockCGet" glfw_tickClockCGet
    :: IO CDouble

-- | get tick for ClockC
tickClockCGet :: MEnv res Tick
tickClockCGet = io $ do
    fmap realToFrac glfw_tickClockCGet


foreign import ccall unsafe "glfw_tickClockCSet" glfw_tickClockCSet
    :: CDouble -> IO ()

-- | set tick for ClockC
tickClockCSet :: Tick -> MEnv res ()
tickClockCSet t = io $ do
    glfw_tickClockCSet (realToFrac t)


foreign import ccall unsafe "glfw_tickClockCSetSpeed" glfw_tickClockCSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockC. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockCSetSpeed :: Double -> MEnv res ()
tickClockCSetSpeed a =  io $ do
    glfw_tickClockCSetSpeed (realToFrac a)


foreign import ccall unsafe "glfw_tickClockCGetSpeed" glfw_tickClockCGetSpeed
    :: IO CDouble

-- | get speed of ClockC
tickClockCGetSpeed :: MEnv res Double
tickClockCGetSpeed = io $ do
    fmap realToFrac glfw_tickClockCGetSpeed


--------------------------------------------------------------------------------
-- clock D

foreign import ccall unsafe "glfw_tickClockDGet" glfw_tickClockDGet
    :: IO CDouble

-- | get tick for ClockD
tickClockDGet :: MEnv res Tick
tickClockDGet = io $ do
    fmap realToFrac glfw_tickClockDGet


foreign import ccall unsafe "glfw_tickClockDSet" glfw_tickClockDSet
    :: CDouble -> IO ()

-- | set tick for ClockD
tickClockDSet :: Tick -> MEnv res ()
tickClockDSet t = io $ do
    glfw_tickClockDSet (realToFrac t)


foreign import ccall unsafe "glfw_tickClockDSetSpeed" glfw_tickClockDSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockD. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockDSetSpeed :: Double -> MEnv res ()
tickClockDSetSpeed a =  io $ do
    glfw_tickClockDSetSpeed (realToFrac a)


foreign import ccall unsafe "glfw_tickClockDGetSpeed" glfw_tickClockDGetSpeed
    :: IO CDouble

-- | get speed of ClockD
tickClockDGetSpeed :: MEnv res Double
tickClockDGetSpeed = io $ do
    fmap realToFrac glfw_tickClockDGetSpeed
    
    
--------------------------------------------------------------------------------
-- clock E

foreign import ccall unsafe "glfw_tickClockEGet" glfw_tickClockEGet
    :: IO CDouble

-- | get tick for ClockE
tickClockEGet :: MEnv res Tick
tickClockEGet = io $ do
    fmap realToFrac glfw_tickClockEGet


foreign import ccall unsafe "glfw_tickClockESet" glfw_tickClockESet
    :: CDouble -> IO ()

-- | set tick for ClockE
tickClockESet :: Tick -> MEnv res ()
tickClockESet t = io $ do
    glfw_tickClockESet (realToFrac t)


foreign import ccall unsafe "glfw_tickClockESetSpeed" glfw_tickClockESetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockE. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockESetSpeed :: Double -> MEnv res ()
tickClockESetSpeed a =  io $ do
    glfw_tickClockESetSpeed (realToFrac a)


foreign import ccall unsafe "glfw_tickClockEGetSpeed" glfw_tickClockEGetSpeed
    :: IO CDouble

-- | get speed of ClockE
tickClockEGetSpeed :: MEnv res Double
tickClockEGetSpeed = io $ do
    fmap realToFrac glfw_tickClockEGetSpeed


--------------------------------------------------------------------------------
-- clock F

foreign import ccall unsafe "glfw_tickClockFGet" glfw_tickClockFGet
    :: IO CDouble

-- | get tick for ClockF
tickClockFGet :: MEnv res Tick
tickClockFGet = io $ do
    fmap realToFrac glfw_tickClockFGet


foreign import ccall unsafe "glfw_tickClockFSet" glfw_tickClockFSet
    :: CDouble -> IO ()

-- | set tick for ClockF
tickClockFSet :: Tick -> MEnv res ()
tickClockFSet t = io $ do
    glfw_tickClockFSet (realToFrac t)


foreign import ccall unsafe "glfw_tickClockFSetSpeed" glfw_tickClockFSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockF. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockFSetSpeed :: Double -> MEnv res ()
tickClockFSetSpeed a =  io $ do
    glfw_tickClockFSetSpeed (realToFrac a)


foreign import ccall unsafe "glfw_tickClockFGetSpeed" glfw_tickClockFGetSpeed
    :: IO CDouble

-- | get speed of ClockF
tickClockFGetSpeed :: MEnv res Double
tickClockFGetSpeed = io $ do
    fmap realToFrac glfw_tickClockFGetSpeed
    








