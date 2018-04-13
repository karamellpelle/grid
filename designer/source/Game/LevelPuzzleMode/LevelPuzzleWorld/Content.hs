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
module Game.LevelPuzzleMode.LevelPuzzleWorld.Content
  (
    Content (..),
    makeContentEmpty,
    makeContent,
    makeContentCamera,
    destroyContent,

    RoomArray,
    roomarrayList,
    roomarrayAt,
    roomarrayModifyAt,
    roomarrayUpdate,
    roomarrayUpdateIO,

    module Game.Grid.GridWorld,
    module Game.LevelPuzzleMode.LevelPuzzleWorld.Room,
    module Game.LevelPuzzleMode.LevelPuzzleWorld.RoomIx,
  
  ) where

import MyPrelude
import Game.MEnv
import Data.Array.IArray hiding (range)
import Data.Array.MArray hiding (range)
import Data.Array.IO     hiding (range)
import Data.Array.Base

import Game.Grid.GridWorld
import Game.Grid.GridWorld.Make
import Game.LevelPuzzleMode.LevelPuzzleWorld.Room
import Game.LevelPuzzleMode.LevelPuzzleWorld.RoomIx


data Content =
    Content
    {
        contentGrid :: !GridWorld,
        contentRooms :: !RoomArray,
        contentRoomsSize :: !UInt,

        -- current
        contentRoom :: !RoomIx,

        -- previous 
        contentEatRoom :: !RoomIx,
        contentEatTick :: !TickT,
        contentEatPathBegin :: !UInt

    }



--------------------------------------------------------------------------------
--  

makeContentEmpty :: MEnv' Content
makeContentEmpty = do
    grid <- makeGridWorldEmpty
    rooms <- io $ roomarrayList 0 []
    return Content
           {
               contentGrid = grid,
               contentRooms = rooms,
               contentRoomsSize = 0,
               contentRoom = 0,
               contentEatPathBegin = 0,
               contentEatRoom = 0,
               contentEatTick = 0
           }


makeContent :: UInt -> [(RoomIx, Room)] -> MEnv' Content
makeContent size rooms = do
    makeContentCamera size rooms makeCamera


makeContentCamera :: UInt -> [(RoomIx, Room)] -> Camera -> MEnv' Content
makeContentCamera size ixrooms camera = do
    roomslist <- renameIxs ixrooms
    let roomssize = length' roomslist

    grid <- makeGridWorldWithCamera size camera
    rooms <- io $ roomarrayList roomssize roomslist 

    return Content
           {
               contentGrid = grid,
               contentRooms = rooms,
               contentRoomsSize = roomssize,
               contentRoom = 0,
               contentEatPathBegin = 0,
               contentEatRoom = 0,
               contentEatTick = 0.0
           }

destroyContent :: Content -> MEnv' ()
destroyContent cnt = do
    destroyGridWorld $ contentGrid cnt



--------------------------------------------------------------------------------
--  

-- | 
renameIxs :: [(RoomIx, Room)] -> MEnv' [Room]
renameIxs ixrooms = io $ do
    let (ixs, rooms) = unzip ixrooms
    mapM (mapRoomIx ixs) rooms
    

mapRoomIx :: [RoomIx] -> Room -> IO Room
mapRoomIx ixs = \room -> do
    -- DotPlain is the only one with reference to a Room
    dotplainarrayUpdate (roomDotPlainSize room) 
                        (roomDotPlain room) $ \dot -> 
                            dot { dotplainRoom = mapIx (dotplainRoom dot) }
                 
    return room

    where
      mapIx ix = 
          mapIx' ixs 0 ix
      mapIx' (ix:ixs) jx ix' =
          if ix == ix' then jx else mapIx' ixs (jx + 1) ix'
      mapIx' [] jx ix' = 
          error "mapRoomIx: logic error"
          --jx  


--------------------------------------------------------------------------------
--  RoomArray

type RoomArray =
    IOArray Int Room

roomarrayList :: UInt -> [Room] -> IO RoomArray
roomarrayList size rooms = 
    newListArray (0, fI size - 1) rooms

roomarrayAt :: RoomArray -> UInt -> IO Room
roomarrayAt array ix =
    unsafeRead array (fI ix)

roomarrayWrite :: RoomArray -> UInt -> Room  -> IO ()
roomarrayWrite array ix room' = 
    unsafeWrite array (fI ix) room'

roomarrayModifyAt :: RoomArray -> UInt -> (Room -> Room) -> IO ()
roomarrayModifyAt array ix f = 
    case fI ix of
        ix  -> unsafeRead array ix >>= \room -> unsafeWrite array ix (f room)

roomarrayUpdate :: UInt -> RoomArray -> (Room -> Room) -> IO ()
roomarrayUpdate size array f =
    forM_ (range 0 size) $ \ix -> case fI ix of
        ix  -> unsafeRead array ix >>= \room -> unsafeWrite array ix(f room)

roomarrayUpdateIO :: UInt -> RoomArray -> (Room -> IO Room) -> IO ()
roomarrayUpdateIO size array f = 
    forM_ (range 0 size) $ \ix -> case fI ix of
        ix  -> unsafeRead array ix >>= \room -> (f room >>= unsafeWrite array ix)

