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
module Game.LevelPuzzle.LevelPuzzleWorld.Content
  (
    Content (..),
    makeContent,
    makeContentEmpty,
    makeContentCamera,
    destroyContent,

    RoomArray,
    roomarrayList,
    roomarrayAt,
    roomarrayModifyAt,
    roomarrayUpdate,
    roomarrayUpdateIO,

#ifdef GRID_STYLE_FANCY
    module Game.LevelPuzzle.LevelPuzzleWorld.Content.Fancy,
#endif
#ifdef GRID_STYLE_PLAIN
    module Game.LevelPuzzle.LevelPuzzleWorld.Content.Plain,
#endif

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.LevelPuzzle.LevelPuzzleWorld.Room
import Game.LevelPuzzle.LevelPuzzleWorld.RoomIx
import Game.LevelPuzzle.LevelPuzzleWorld.Dot

import Data.Array.IArray hiding (range)
import Data.Array.MArray hiding (range)
import Data.Array.IO     hiding (range)
import Data.Array.Base

#ifdef GRID_STYLE_FANCY
import Game.LevelPuzzle.LevelPuzzleWorld.Content.Fancy
#endif
#ifdef GRID_STYLE_PLAIN
import Game.LevelPuzzle.LevelPuzzleWorld.Content.Plain
#endif

#ifdef GRID_SAFE
import Control.Exception as C
#endif

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
        contentEatTick :: !Tick,
        contentEatPathBegin :: !UInt,

        contentData :: !ContentData
    }



--------------------------------------------------------------------------------
--  


makeContent :: UInt -> [(RoomIx, Room)] -> MEnv' Content
makeContent size rooms = do
    makeContentCamera size rooms makeCamera


-- | "Content without content"
makeContentEmpty :: MEnv' Content
makeContentEmpty = do
    grid <- makeGridWorldEmpty
    room <- io $ makeRoomEmpty
    cntdata <- makeContentData 1 [room]
    rooms <- io $ roomarrayList 1 [room]
    return Content
           {
               contentGrid = grid,
               contentRooms = rooms,
               contentRoomsSize = 0,
               contentRoom = 0,
               contentEatPathBegin = 0,
               contentEatRoom = 0,
               contentEatTick = 0,
               contentData = cntdata
           }


makeContentCamera :: UInt -> [(RoomIx, Room)] -> Camera -> MEnv' Content
makeContentCamera size ixrooms camera = do
    roomslist <- renameRoomIx ixrooms
    let roomssize = length' roomslist

    cntdata <- makeContentData roomssize roomslist
    grid <- makeGridWorldCamera size camera
    rooms <- io $ roomarrayList roomssize roomslist 
    return Content
           {
               contentGrid = grid,
               contentRooms = rooms,
               contentRoomsSize = roomssize,
               contentRoom = 0,
               contentEatPathBegin = 0,
               contentEatRoom = 0,
               contentEatTick = 0.0,
               contentData = cntdata
           }


destroyContent :: Content -> MEnv' ()
destroyContent cnt = do
    destroyContentData $ contentData cnt
    destroyGridWorld $ contentGrid cnt



--------------------------------------------------------------------------------
--  

-- | 
renameRoomIx :: [(RoomIx, Room)] -> MEnv' [Room]
renameRoomIx ixrooms = io $ 
    let (ixs, rooms) = unzip ixrooms
    in  mapM (mapRoomIx ixs) rooms
    

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
#ifdef GRID_SAFE
    C.catch (readArray array (fI ix)) 
            (\e -> error ("roomarrayAt: " ++ show (e :: SomeException)))
#else
    unsafeRead array (fI ix)
#endif


roomarrayWrite :: RoomArray -> UInt -> Room  -> IO ()
roomarrayWrite array ix room' = 
#ifdef GRID_SAFE
    writeArray array (fI ix) room'
#else
    unsafeWrite array (fI ix) room'
#endif

roomarrayModifyAt :: RoomArray -> UInt -> (Room -> Room) -> IO ()
roomarrayModifyAt array ix f = 
    case fI ix of
#ifdef GRID_SAFE
        ix  -> readArray array ix >>= \room -> writeArray array ix (f room)
#else
        ix  -> unsafeRead array ix >>= \room -> unsafeWrite array ix (f room)
#endif

roomarrayUpdate :: UInt -> RoomArray -> (Room -> Room) -> IO ()
roomarrayUpdate size array f =
    forM_ (range 0 size) $ \ix -> case fI ix of
#ifdef GRID_SAFE
        ix  -> readArray array ix >>= \room -> writeArray array ix(f room)
#else
        ix  -> unsafeRead array ix >>= \room -> unsafeWrite array ix(f room)
#endif

roomarrayUpdateIO :: UInt -> RoomArray -> (Room -> IO Room) -> IO ()
roomarrayUpdateIO size array f = 
    forM_ (range 0 size) $ \ix -> case fI ix of
#ifdef GRID_SAFE
        ix  -> readArray array ix >>= \room -> (f room >>= writeArray array ix)
#else
        ix  -> unsafeRead array ix >>= \room -> (f room >>= unsafeWrite array ix)
#endif

