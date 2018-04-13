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
module LevelTools.SemiRoom
  (
    SemiRoom (..),
    makeSemiRoom,
    makeRoomIxRoom,

    module Game.LevelPuzzleMode.LevelPuzzleWorld.Wall,
    module Game.LevelPuzzleMode.LevelPuzzleWorld.Dot,

  ) where


import MyPrelude
import Game.LevelPuzzleMode.LevelPuzzleWorld.Wall
import Game.LevelPuzzleMode.LevelPuzzleWorld.Dot
import Game.LevelPuzzleMode.LevelPuzzleWorld.RoomIx
import Game.LevelPuzzleMode.LevelPuzzleWorld.Room



data SemiRoom =
    SemiRoom
    {
        sroomRoomIx :: !RoomIx,
        sroomWall :: [Wall],
        sroomDotPlain :: [DotPlain],
        sroomDotBonus :: [DotBonus],
        sroomDotTele :: [DotTele],
        sroomDotFinish :: [DotFinish]

    }



makeSemiRoom :: RoomIx -> [Wall] -> [DotPlain] -> [DotBonus] -> [DotTele] -> [DotFinish] -> SemiRoom
makeSemiRoom ix wall dotplain dotbonus dottele dotfinish =
    SemiRoom
    {
       sroomRoomIx = ix,
       sroomWall = wall,
       sroomDotPlain = dotplain,
       sroomDotBonus = dotbonus,
       sroomDotTele = dottele,
       sroomDotFinish = dotfinish

    }


makeRoomIxRoom :: SemiRoom -> IO (RoomIx, Room)
makeRoomIxRoom sroom = do
    let roomix = sroomRoomIx sroom
    room <- makeRoom (sroomWall sroom) (sroomDotPlain sroom) 
                     (sroomDotBonus sroom) (sroomDotTele sroom)
                     (sroomDotFinish sroom)
    return (roomix, room)

