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
module Game.LevelPuzzle.LevelPuzzleWorld.OutputState.Fancy
  (
    OutputState (..),
    
    makeOutputState,

  ) where

import MyPrelude
import Game

import Game.Data.Color
import Game.Grid

import Linear



data OutputState =
    OutputState
    {
        ostateBonusTick :: !Tick,
        ostateBonusAdd :: !UInt,
        ostateBonusRef :: !Segment,
        ostateBonusColor :: !Color,

        ostateColorIx0 :: !UInt,
        ostateColorIx1 :: !UInt,
        ostateAlpha :: !Float,
        ostateTick :: !Tick
    }


makeOutputState :: MEnv' OutputState
makeOutputState = do
    return OutputState
           {
              ostateBonusTick = 0.0,
              ostateBonusAdd = 0,
              ostateBonusRef = mempty,
              ostateBonusColor = colorNull,
              ostateColorIx0 = 0,
              ostateColorIx1 = 0,
              ostateAlpha = 0.0,
              ostateTick = 0.0
           }
