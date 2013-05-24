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
module MEnv.Env.TickObject.GLFW
  (
    TickInit (..),
    TickObject (..),
    Clock (..),
    Sampling (..),

    withLoadedTick,

  ) where

import LoadM



data TickInit =
    TickInit


data TickObject =
    TickObject
    {
        tickobjSampling :: !Sampling,
        tickobjClockTickPrev :: !TickT,
        tickobjClockTick :: !TickT,
        tickobjClockA :: !Clock,
        tickobjClockB :: !Clock,
        tickobjClockC :: !Clock,
        tickobjClockD :: !Clock,
        tickobjClockE :: !Clock,
        tickobjClockF :: !Clock
    }


type TickT = Double



-- | Clocks are timers which are updated when used
data Clock =
    Clock
    {
        clockTime :: !TickT,
        clockSpeed :: !Double,
        clockUsed :: !Bool

    }


data Sampling = 
    Sampling
    {
        samplingTick :: !TickT,
        samplingSampleTick :: !TickT,
        samplingSample :: !(Maybe TickT), -- !?
        samplingUnit :: !TickT
    }



--------------------------------------------------------------------------------
--  


withLoadedTick :: TickInit -> (TickObject -> LoadM a) -> LoadM a
withLoadedTick init handler =
    handler $ TickObject  { tickobjSampling = emptySampling,
                            tickobjClockTickPrev = 0.0,
                            tickobjClockTick = 0.0,
                            tickobjClockA = emptyClock,
                            tickobjClockB = emptyClock,
                            tickobjClockC = emptyClock,
                            tickobjClockD = emptyClock,
                            tickobjClockE = emptyClock,
                            tickobjClockF = emptyClock
                          }
    

emptySampling =
    Sampling {  samplingTick = 0.0,
                samplingSampleTick = 0.0,
                samplingSample = Nothing,
                samplingUnit = 0.02 }

emptyClock = 
    Clock { clockTime = 0.0,
            clockSpeed = 1.0,
            clockUsed = False }
