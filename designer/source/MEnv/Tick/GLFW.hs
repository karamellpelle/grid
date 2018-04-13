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
module MEnv.Tick.GLFW
  (
    TickT,

    tickBegin,
    tickEnd,
    tickGet,
   
    tickHandleSample,
    tickSampleModifyUnit,
    tickSampleGetUnit,

    tickClockAGet,
    tickClockASet,
    tickClockASetSpeed,
    tickClockAGetSpeed,
    tickClockAModifySpeed,

    tickClockBGet,
    tickClockBSet,
    tickClockBSetSpeed,
    tickClockBGetSpeed,
    tickClockBModifySpeed,

    tickClockCGet,
    tickClockCSet,
    tickClockCSetSpeed,
    tickClockCGetSpeed,
    tickClockCModifySpeed,

    tickClockDGet,
    tickClockDSet,
    tickClockDSetSpeed,
    tickClockDGetSpeed,
    tickClockDModifySpeed,
    
    tickClockEGet,
    tickClockESet,
    tickClockESetSpeed,
    tickClockEGetSpeed,
    tickClockEModifySpeed,

    tickClockFGet,
    tickClockFSet,
    tickClockFSetSpeed,
    tickClockFGetSpeed,
    tickClockFModifySpeed,
    
  ) where

import MEnv
import MEnv.Helpers

import qualified Control.Monad.State as MState

-- tmp:
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL




type TickT =
    Double



-- | begin iteration
tickBegin :: MEnv res ()
tickBegin = do
    tick' <- tickGet
    modifyTickObject $ \obj ->
        let samp' = beginSampling tick' $ tickobjSampling obj
            dt = tick' - tickobjClockTickPrev obj
        in case clockUpdateDT <= dt of
           False  -> obj { tickobjSampling = samp',
                           tickobjClockTick = tick' }
           True   -> let clockA' = updateClock dt $ tickobjClockA obj
                         clockB' = updateClock dt $ tickobjClockB obj
                         clockC' = updateClock dt $ tickobjClockC obj
                         clockD' = updateClock dt $ tickobjClockD obj
                         clockE' = updateClock dt $ tickobjClockE obj
                         clockF' = updateClock dt $ tickobjClockF obj
                         
                     in obj { tickobjSampling = samp',
                              tickobjClockA = clockA',
                              tickobjClockB = clockB',
                              tickobjClockC = clockC',
                              tickobjClockD = clockD',
                              tickobjClockE = clockE',
                              tickobjClockF = clockF',
                              tickobjClockTickPrev = tick',
                              tickobjClockTick = tick' }


-- | update clock when 'clockUpdateDT' ticks has elapsed. a value of 0.0 implies update each iteration.
clockUpdateDT = 
    --0.02
    0.0   -- (is this not as smooth as, say, 2.0?). the idea with updateDT was to fix lag when speed < 1,
          -- but turned out to not be affected


beginSampling tick samp =
    if samplingSampleTick samp + samplingUnit samp <= tick
      then samp { samplingSample = Just $ samplingSampleTick samp ,
                  samplingSampleTick = tick,
                  samplingTick = tick }

      else samp { samplingSample = Nothing,
                  samplingTick = tick }


updateClock dt clock =
    case clockUsed clock of
        False   -> clock
        True    -> clock { clockTime = clockTime clock +  (clockSpeed clock) * dt, 
                           clockUsed = False }



-- | end iteration
tickEnd :: MEnv res ()
tickEnd = do
    return ()



--------------------------------------------------------------------------------
--  


-- | get current tick
tickGet :: MEnv res TickT
tickGet =
    liftIO $ GL.get GLFW.time




--------------------------------------------------------------------------------
--  tickSample



-- | sampling dt in iteration
tickHandleSample :: a -> (TickT -> a) -> MEnv res a
tickHandleSample a f =
    getTickObject >>= \obj -> 
        let samp = tickobjSampling obj
        in case samplingSample samp of
            Nothing   -> return a
            Just tick -> return $ f (samplingSampleTick samp - tick)
        


-- | modify unit used in sampling
tickSampleModifyUnit :: (TickT -> TickT) -> MEnv res ()
tickSampleModifyUnit f = 
    modifyTickObject $ \obj -> 
        let samp = tickobjSampling obj
        in obj { tickobjSampling = samp { samplingUnit = f $ samplingUnit samp } }


-- | get unit used in sampling
tickSampleGetUnit :: MEnv res TickT
tickSampleGetUnit =
    getTickObject >>= \obj -> return $ samplingUnit (tickobjSampling obj)



--------------------------------------------------------------------------------
-- clockA


-- | get tick for ClockA
tickClockAGet :: MEnv res TickT
tickClockAGet =
    modifyClockA' fGet


-- | set tick for ClockA
tickClockASet :: TickT -> MEnv res ()
tickClockASet t =
    modifyClockA' $ fSet t


-- | set speed of ClockA. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockAModifySpeed :: (TickT -> TickT) -> MEnv res ()
tickClockAModifySpeed f = 
    modifyClockA' $ fModifySpeed f

tickClockASetSpeed speed =
    tickClockAModifySpeed (const speed)

-- | get speed of ClockA
tickClockAGetSpeed :: MEnv res Double
tickClockAGetSpeed =
    modifyClockA' $ fGetSpeed


--------------------------------------------------------------------------------
-- clock B

-- | get tick for ClockB
tickClockBGet :: MEnv res TickT
tickClockBGet =
    modifyClockB' fGet


-- | set tick for ClockB
tickClockBSet :: TickT -> MEnv res ()
tickClockBSet t =
    modifyClockB' $ fSet t


-- | set speed of ClockB. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockBModifySpeed :: (TickT -> TickT) -> MEnv res ()
tickClockBModifySpeed f = 
    modifyClockB' $ fModifySpeed f

tickClockBSetSpeed speed =
    tickClockBModifySpeed (const speed)



-- | get speed of ClockB
tickClockBGetSpeed :: MEnv res Double
tickClockBGetSpeed =
    modifyClockB' $ fGetSpeed


--------------------------------------------------------------------------------
-- clock B

-- | get tick for ClockC
tickClockCGet :: MEnv res TickT
tickClockCGet =
    modifyClockC' fGet


-- | set tick for ClockC
tickClockCSet :: TickT -> MEnv res ()
tickClockCSet t =
    modifyClockC' $ fSet t


-- | set speed of ClockC. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockCModifySpeed :: (TickT -> TickT) -> MEnv res ()
tickClockCModifySpeed f = 
    modifyClockC' $ fModifySpeed f

tickClockCSetSpeed speed =
    tickClockCModifySpeed (const speed)


-- | get speed of ClockC
tickClockCGetSpeed :: MEnv res Double
tickClockCGetSpeed =
    modifyClockC' $ fGetSpeed


--------------------------------------------------------------------------------
-- clock B

-- | get tick for ClockD
tickClockDGet :: MEnv res TickT
tickClockDGet =
    modifyClockD' fGet


-- | set tick for ClockD
tickClockDSet :: TickT -> MEnv res ()
tickClockDSet t =
    modifyClockD' $ fSet t


-- | set speed of ClockD. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockDModifySpeed :: (TickT -> TickT) -> MEnv res ()
tickClockDModifySpeed f = 
    modifyClockD' $ fModifySpeed f

tickClockDSetSpeed speed =
    tickClockDModifySpeed (const speed)


-- | get speed of ClockD
tickClockDGetSpeed :: MEnv res Double
tickClockDGetSpeed =
    modifyClockD' $ fGetSpeed

--------------------------------------------------------------------------------
-- clock E

-- | get tick for ClockE
tickClockEGet :: MEnv res TickT
tickClockEGet =
    modifyClockE' fGet


-- | set tick for ClockE
tickClockESet :: TickT -> MEnv res ()
tickClockESet t =
    modifyClockE' $ fSet t


-- | set speed of ClockE. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockEModifySpeed :: (TickT -> TickT) -> MEnv res ()
tickClockEModifySpeed f = 
    modifyClockE' $ fModifySpeed f

tickClockESetSpeed speed =
    tickClockEModifySpeed (const speed)


-- | get speed of ClockE
tickClockEGetSpeed :: MEnv res Double
tickClockEGetSpeed =
    modifyClockE' $ fGetSpeed


--------------------------------------------------------------------------------
-- clock F

-- | get tick for ClockF
tickClockFGet :: MEnv res TickT
tickClockFGet =
    modifyClockF' fGet


-- | set tick for ClockE
tickClockFSet :: TickT -> MEnv res ()
tickClockFSet t =
    modifyClockF' $ fSet t


-- | set speed of ClockE. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockFModifySpeed :: (TickT -> TickT) -> MEnv res ()
tickClockFModifySpeed f = 
    modifyClockF' $ fModifySpeed f

tickClockFSetSpeed speed =
    tickClockFModifySpeed (const speed)


-- | get speed of ClockE
tickClockFGetSpeed :: MEnv res Double
tickClockFGetSpeed =
    modifyClockF' $ fGetSpeed


--------------------------------------------------------------------------------
--  

modifyClockA' :: (TickObject -> Clock -> (Clock, a)) -> MEnv res a
modifyClockA' f = do
    env <- MState.get
    let obj = envTick env
        clock = tickobjClockA obj
        (clock', a) = f obj clock
    MState.put $ env { envTick = obj { tickobjClockA = clock' } }
    return a


modifyClockB' :: (TickObject -> Clock -> (Clock, a)) -> MEnv res a
modifyClockB' f = do
    env <- MState.get
    let obj = envTick env
        clock = tickobjClockB obj
        (clock', a) = f obj clock
    MState.put $ env { envTick = obj { tickobjClockB = clock' } }
    return a
    
modifyClockC' :: (TickObject -> Clock -> (Clock, a)) -> MEnv res a
modifyClockC' f = do
    env <- MState.get
    let obj = envTick env
        clock = tickobjClockC obj
        (clock', a) = f obj clock
    MState.put $ env { envTick = obj { tickobjClockC = clock' } }
    return a
    
modifyClockD' :: (TickObject -> Clock -> (Clock, a)) -> MEnv res a
modifyClockD' f = do
    env <- MState.get
    let obj = envTick env
        clock = tickobjClockD obj
        (clock', a) = f obj clock
    MState.put $ env { envTick = obj { tickobjClockD = clock' } }
    return a
    
modifyClockE' :: (TickObject -> Clock -> (Clock, a)) -> MEnv res a
modifyClockE' f = do
    env <- MState.get
    let obj = envTick env
        clock = tickobjClockE obj
        (clock', a) = f obj clock
    MState.put $ env { envTick = obj { tickobjClockE = clock' } }
    return a

modifyClockF' :: (TickObject -> Clock -> (Clock, a)) -> MEnv res a
modifyClockF' f = do
    env <- MState.get
    let obj = envTick env
        clock = tickobjClockF obj
        (clock', a) = f obj clock
    MState.put $ env { envTick = obj { tickobjClockF = clock' } }
    return a
    

fGet obj clock = 
    let tick = tickobjClockTickPrev obj
        tick' = tickobjClockTick obj
        time = clockTime clock + (clockSpeed clock) * (tick' - tick)
        clock' = clock { clockUsed = True }

    in (clock', time)

fSet t obj clock = 
    let tick = tickobjClockTickPrev obj
        tick' = tickobjClockTick obj
        time = t - (clockSpeed clock) * (tick' - tick)
        clock' = clock { clockTime = time }

    in (clock', ())

fModifySpeed f obj clock =
    let clock' = clock { clockSpeed = f $ clockSpeed clock }
    in (clock', ())


fGetSpeed obj clock = 
    let speed = clockSpeed clock
    in (clock, speed)







