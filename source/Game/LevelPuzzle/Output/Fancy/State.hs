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
module Game.LevelPuzzle.Output.Fancy.State
  (
    ostateBeginPlaySet,
    ostatePlayBegin,
    ostateCompleteSet,
    ostateCompleteEnd,

  ) where


import MyPrelude
import Game

import Game.Grid.Output
import Game.LevelPuzzle
import Game.LevelPuzzle.LevelPuzzleWorld.OutputState
import Game.Run.RunWorld

import System.Random
import Linear


--------------------------------------------------------------------------------
--  

ostateBeginPlaySet :: LevelPuzzleWorld -> RunWorld -> IO (LevelPuzzleWorld, RunWorld)
ostateBeginPlaySet lvl run = do
    let lvl' = modifyOState lvl $ \ostate -> 
               ostate
               {
                  ostateBonusTick = 0.0,
                  ostateBonusAdd = 0
               }

    return (lvl', run)


--------------------------------------------------------------------------------
--  

ostatePlayBegin :: GameData -> s -> LevelPuzzleWorld -> RunWorld -> 
                   IO (s, LevelPuzzleWorld, RunWorld)
ostatePlayBegin gamedata = \s lvl run -> 
    
    handleAllEventsM lvl (s, lvl, run) $ \slr@(s, lvl, run) event -> case event of

        EventPathEatDotBonus dot  -> do
            -- we assume bonus node at path current
            color <- randomColor
            let lvl' = modifyOState lvl $ \ostate -> 
                       ostate
                       {
                          ostateBonusTick = worldTick lvl,
                          ostateBonusAdd = dotbonusAdd dot,
                          ostateBonusRef = pathCurrent $ gridPath $ levelpuzzleGrid lvl,
                          ostateBonusColor = color
                       }

            return (s, lvl', run)

        _                         ->
            return slr
    where
      randomColor = do
          ix <- randomRIO (0, valueColorMapSize - 3) 
          return $ colormapAt2 (griddataColorMap $ gamedataGridData gamedata) ix

--------------------------------------------------------------------------------
--  




ostateCompleteSet :: LevelPuzzleWorld -> RunWorld -> IO (LevelPuzzleWorld, RunWorld)
ostateCompleteSet lvl run = do
    lvl' <- modifyOStateIO lvl $ \state -> do
            let ix = contentRoom $ levelpuzzleOldContent lvl
            ix' <- randomRIO (0, (valueColorMapSize - 1))
            return state
                   {
                       ostateColorIx0 = ix,
                       ostateColorIx1 = ix',
                       ostateAlpha = 0.0,
                       ostateTick = worldTick run
                   }

    return (lvl', run)



ostateCompleteEnd :: s -> LevelPuzzleWorld -> RunWorld -> 
                      IO (s, LevelPuzzleWorld, RunWorld)
ostateCompleteEnd s lvl run = do
    lvl' <- modifyOStateIO lvl $ step (worldTick run)
    return (s, lvl', run)



--------------------------------------------------------------------------------
--  

step :: Tick -> OutputState -> IO OutputState
step tick s = do
    let dt = rTF (tick - ostateTick s)
    if unit <= dt
      then let alpha' = ostateAlpha s + dt * scale
           in  if 1.0 <= alpha'
               then do
                  ix' <- randomRIO (0, (valueColorMapSize - 1))
                  return s
                         {
                            ostateAlpha = 0.0,
                            ostateColorIx0 = ostateColorIx1 s,
                            ostateColorIx1 = ix',
                            ostateTick = tick 
                         }
               else do
                  return s
                         {
                            ostateAlpha = alpha',
                            ostateTick = tick
                         }

      else return s

    where
      scale = 0.4
      unit = 0.08


modifyOState :: LevelPuzzleWorld -> (OutputState -> OutputState) -> LevelPuzzleWorld
modifyOState lvl f =
    lvl { levelpuzzleOutputState = f (levelpuzzleOutputState lvl) }


modifyOStateIO :: LevelPuzzleWorld -> (OutputState -> IO OutputState) -> IO LevelPuzzleWorld
modifyOStateIO lvl f = do
   s' <- f $ levelpuzzleOutputState lvl
   return lvl { levelpuzzleOutputState = s' }


