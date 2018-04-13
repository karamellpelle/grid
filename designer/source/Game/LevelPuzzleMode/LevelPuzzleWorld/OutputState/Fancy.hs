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
module Game.LevelPuzzleMode.LevelPuzzleWorld.OutputState.Fancy
  (
    OutputState (..),
    
    makeOutputState,

  ) where

import MyPrelude
import Game.MEnv


data OutputState =
    OutputState
    {
        outputstateCompleteColorIx :: !UInt,
        outputstateCompleteColorIx' :: !UInt,
        outputstateCompleteAlpha :: !Float,
        outputstateCompleteTick :: !TickT,
        outputstateSpecialCompleteTick :: !TickT
        --outputstate
    }


makeOutputState :: MEnv' OutputState
makeOutputState = do
    return OutputState
           {
              outputstateCompleteColorIx = 0,
              outputstateCompleteColorIx' = 0,
              outputstateCompleteAlpha = 0.0,
              outputstateCompleteTick = 0.0,
              outputstateSpecialCompleteTick = 0.0 
           }
