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
module Game.LevelPuzzle.LevelPuzzleWorld.Dot
  (
    DotPlain (..),
    DotBonus (..),
    DotTele (..),
    DotFinish (..),

    makeDotPlain,
    makeDotBonus,
    makeDotTele,
    makeDotFinish,

  ) where

import MyPrelude
import Game.Grid
import Game.LevelPuzzle.LevelPuzzleWorld.RoomIx



--------------------------------------------------------------------------------
--  DotPlain

data DotPlain =
    DotPlain
    {
        dotplainNode :: !Node,
        dotplainSize :: !UInt,
        dotplainCount :: !UInt,
        dotplainRoom :: !RoomIx
    }

makeDotPlain :: Node -> UInt -> UInt -> RoomIx -> DotPlain
makeDotPlain node size count room = 
    DotPlain
    {
        dotplainNode = node,
        dotplainSize = size,
        dotplainCount = count,
        dotplainRoom = room
    }

--------------------------------------------------------------------------------
--  DotBonus

data DotBonus =
    DotBonus
    {
        dotbonusNode :: !Node,
        dotbonusSize :: !UInt,
        dotbonusCount :: !UInt,
        dotbonusAdd :: !UInt
    }

makeDotBonus :: Node -> UInt -> UInt -> UInt -> DotBonus
makeDotBonus node size count add = 
    DotBonus
    {
        dotbonusNode = node,
        dotbonusSize = size,
        dotbonusCount = count,
        dotbonusAdd = add
    }

--------------------------------------------------------------------------------
--  DotTele

data DotTele =
    DotTele
    {
        dotteleNode :: !Node,
        dotteleSize :: !UInt,
        dotteleCount :: !UInt,
        dotteleNode' :: !Node
        -- RoomIx 
    }

makeDotTele :: Node -> UInt -> UInt -> Node -> DotTele
makeDotTele node size count node' = 
    DotTele
    {
        dotteleNode = node,
        dotteleSize = size,
        dotteleCount = count,
        dotteleNode' = node'
    }

--------------------------------------------------------------------------------
--  DotFinish

data DotFinish =
    DotFinish
    {
        dotfinishNode :: !Node
    }

makeDotFinish :: Node -> DotFinish
makeDotFinish node = 
    DotFinish
    {
        dotfinishNode = node
    }


