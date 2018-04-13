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
module Game.LevelPuzzle.Output.Plain.State
  (
    stateSetComplete,
    stateStepComplete',
    stateSetSpecialComplete,
    stateStepSpecialComplete',

  ) where


import MyPrelude
import Game

import Game.LevelPuzzle
import Game.LevelPuzzle.LevelPuzzleWorld.OutputState
import Game.Run.RunWorld
import qualified Game.Run.RunWorld.OutputState as R

import System.Random

--------------------------------------------------------------------------------
--  


stateSetComplete :: LevelPuzzleWorld -> RunWorld -> IO (LevelPuzzleWorld, RunWorld)
stateSetComplete lvl run = do
    lvl' <- modifyLvlIO lvl $ \state -> do
            let ix = if contentIsEmpty (levelpuzzleOldContent lvl)
                     then 0 
                     else contentRoom $ levelpuzzleOldContent lvl
            ix' <- randomRIO (0, (valueColorMapSize - 1))
            return state
                   {
                       outputstateCompleteColorIx = ix,
                       outputstateCompleteColorIx' = ix',
                       outputstateCompleteAlpha = 0.0,
                       outputstateCompleteTick = worldTick lvl
                   }

    return (lvl', run)



stateStepComplete' :: s -> LevelPuzzleWorld -> RunWorld -> 
                     IO (s, LevelPuzzleWorld, RunWorld)
stateStepComplete' s lvl run = do
    lvl' <- modifyLvlIO lvl $ step (worldTick lvl) 
    return (s, lvl', run)



stateSetSpecialComplete :: LevelPuzzleWorld -> RunWorld -> IO (LevelPuzzleWorld, RunWorld)
stateSetSpecialComplete lvl run = do
    lvl' <- modifyLvlIO lvl $ \state -> do
            let ix = if contentIsEmpty (levelpuzzleOldContent lvl)
                     then 0 
                     else contentRoom $ levelpuzzleOldContent lvl
            ix' <- randomRIO (0, (valueColorMapSize - 1))
            return state
                   {
                       outputstateCompleteColorIx = ix,
                       outputstateCompleteColorIx' = ix',
                       outputstateCompleteAlpha = 0.0,
                       outputstateCompleteTick = worldTick run,
                       outputstateSpecialCompleteTick = worldTick run
                   }

    return (lvl', run)



stateStepSpecialComplete' :: s -> LevelPuzzleWorld -> RunWorld -> 
                            IO (s, LevelPuzzleWorld, RunWorld)
stateStepSpecialComplete' s lvl run = do
    lvl' <- modifyLvlIO lvl $ step (worldTick run)
    run' <- modifyRunIO run $ \state -> 
            let s = levelpuzzleOutputState lvl'
            in  return state
                       {
                          R.outputstateColorIx = outputstateCompleteColorIx s,
                          R.outputstateColorIx' = outputstateCompleteColorIx' s,
                          R.outputstateAlpha = outputstateCompleteAlpha s
                       }
    return (s, lvl', run')



--------------------------------------------------------------------------------
--  

step :: Tick -> OutputState -> IO OutputState
step tick s = do
    let dt = rTF (tick - outputstateCompleteTick s)
    if unit <= dt
      then let alpha' = outputstateCompleteAlpha s + dt * scale
           in  if 1.0 <= alpha'
               then do
                  ix' <- randomRIO (0, (valueColorMapSize - 1))
                  return s
                         {
                            outputstateCompleteAlpha = 0.0,
                            outputstateCompleteColorIx = outputstateCompleteColorIx' s,
                            outputstateCompleteColorIx' = ix',
                            outputstateCompleteTick = tick 
                         }
               else do
                  return s
                         {
                            outputstateCompleteAlpha = alpha',
                            outputstateCompleteTick = tick
                         }

      else return s

    where
      scale = 0.4
      unit = 0.08



modifyLvlIO :: LevelPuzzleWorld -> (OutputState -> IO OutputState) -> IO LevelPuzzleWorld
modifyLvlIO lvl f = do
   s' <- f $ levelpuzzleOutputState lvl
   return lvl { levelpuzzleOutputState = s' }


modifyRunIO :: RunWorld -> (R.OutputState -> IO R.OutputState) -> IO RunWorld
modifyRunIO run f = do
   s' <- f $ runOutputState run
   return run { runOutputState = s' }

