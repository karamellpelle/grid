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
module Game.LevelPuzzle.LevelPuzzleWorld.Room
  (
    Room (..),
    makeRoom,
    makeRoomEmpty,

    WallArray,
    wallarrayList,
    wallarrayAt,
    
    DotPlainArray,
    dotplainarrayList,
    dotplainarrayAt,
    dotplainarrayWrite,
    dotplainarrayUpdate,
    dotplainarrayForIO_,

    DotBonusArray,
    dotbonusarrayList,
    dotbonusarrayAt,
    dotbonusarrayWrite,
    dotbonusarrayUpdate,
    dotbonusarrayForIO_,
    
    DotTeleArray,
    dottelearrayList,
    dottelearrayAt,
    dottelearrayWrite,
    dottelearrayUpdate,
    dottelearrayForIO_,

    DotFinishArray,
    dotfinisharrayList,
    dotfinisharrayAt,
    dotfinisharrayWrite,
    dotfinisharrayUpdate,
    dotfinisharrayForIO_,
  
  ) where


import MyPrelude

import Game.LevelPuzzle.LevelPuzzleWorld.Wall
import Game.LevelPuzzle.LevelPuzzleWorld.Dot

import Data.Array.IArray hiding (range)
import Data.Array.MArray hiding (range)
import Data.Array.IO     hiding (range)
import Data.Array.Base

#ifdef GRID_SAFE
import Control.Exception as C
#endif


data Room =
    Room
    {
        -- walls --
        roomWall :: !WallArray,
        roomWallSize :: !UInt,

        -- dots --
        roomDotPlain :: !DotPlainArray,
        roomDotPlainSize :: !UInt,

        roomDotBonus :: !DotBonusArray,
        roomDotBonusSize :: !UInt,

        roomDotTele :: !DotTeleArray,
        roomDotTeleSize :: !UInt,

        roomDotFinish :: !DotFinishArray,
        roomDotFinishSize :: !UInt,

        -- path --
        roomPathBeginEnd :: [(UInt, UInt)]
    }



makeRoom :: [Wall] -> [DotPlain] -> [DotBonus] -> [DotTele] -> [DotFinish] -> IO Room
makeRoom wall dotplain dotbonus dottele dotfinish = do
    let wallSize = length' wall

    let dotplainSize = length' dotplain
    dotplainArray <- dotplainarrayList dotplainSize dotplain
    let dotbonusSize = length' dotbonus
    dotbonusArray <- dotbonusarrayList dotbonusSize dotbonus
    let dotteleSize = length' dottele
    dotteleArray <- dottelearrayList dotteleSize dottele
    let dotfinishSize = length' dotfinish
    dotfinishArray <- dotfinisharrayList dotfinishSize dotfinish

    return Room
           {
              roomWall = wallarrayList wallSize wall,
              roomWallSize = wallSize,
              roomDotPlain = dotplainArray,
              roomDotPlainSize = dotplainSize,
              roomDotBonus = dotbonusArray,
              roomDotBonusSize = dotbonusSize,
              roomDotTele = dotteleArray,
              roomDotTeleSize = dotteleSize,
              roomDotFinish = dotfinishArray,
              roomDotFinishSize = dotfinishSize,
              roomPathBeginEnd = []
           }


makeRoomEmpty :: IO Room
makeRoomEmpty = 
    makeRoom [] [] [] [] [] 

--------------------------------------------------------------------------------
--  WallArray

type WallArray =
    Array Int Wall

wallarrayList :: UInt -> [Wall] -> WallArray
wallarrayList size walls = 
    listArray (0, fI size - 1) walls

wallarrayAt :: WallArray -> UInt -> Wall
wallarrayAt array ix =
#ifdef GRID_SAFE
    (!) array (fI ix)
#else
    unsafeAt array (fI ix)
#endif


--------------------------------------------------------------------------------
--  DotPlainArray

type DotPlainArray =
    IOArray Int DotPlain


dotplainarrayList :: UInt -> [DotPlain] -> IO DotPlainArray
dotplainarrayList size dots = 
    newListArray (0, fI size - 1) dots

dotplainarrayAt :: DotPlainArray -> UInt -> IO DotPlain
dotplainarrayAt array ix = 
#ifdef GRID_SAFE
    C.catch (readArray array (fI ix))
            (\e -> error ("dotplainarrayAt: " ++ show (e :: SomeException)))
#else
    unsafeRead array (fI ix)
#endif

dotplainarrayWrite :: DotPlainArray -> UInt -> DotPlain -> IO ()
dotplainarrayWrite array ix dot = 
#ifdef GRID_SAFE
    writeArray array (fI ix) dot
#else
    unsafeWrite array (fI ix) dot
#endif

dotplainarrayUpdate :: UInt -> DotPlainArray -> (DotPlain -> DotPlain) -> 
                       IO ()
dotplainarrayUpdate size array f = 
    forM_ (range 0 size) $ \ix -> 
      case fI ix of
#ifdef GRID_SAFE
        ix -> C.catch (readArray array ix >>= \dot -> writeArray array ix (f dot))
                      (\e -> error ("dotplainarrayUpdate: " ++ show (e :: SomeException)))
#else
        ix -> unsafeRead array ix >>= \dot -> unsafeWrite array ix (f dot)
#endif


dotplainarrayForIO_ :: UInt -> DotPlainArray -> (DotPlain -> IO ()) -> IO ()
dotplainarrayForIO_ size array f = 
#ifdef GRID_SAFE
    C.catch (forM_ (range 0 size) $ \ix -> readArray array (fI ix) >>= f)
            (\e -> error ("dotplainarrayForIO_: " ++ show (e :: SomeException)))
#else
    forM_ (range 0 size) $ \ix -> unsafeRead array (fI ix) >>= f
#endif

--------------------------------------------------------------------------------
--  DotBonusArray

type DotBonusArray =
    IOArray Int DotBonus


dotbonusarrayList :: UInt -> [DotBonus] -> IO DotBonusArray
dotbonusarrayList size dots = 
    newListArray (0, fI size - 1) dots

dotbonusarrayAt :: DotBonusArray -> UInt -> IO DotBonus
dotbonusarrayAt array ix = 
#ifdef GRID_SAFE
    C.catch (readArray array (fI ix))
            (\e -> error ("dotbonusarrayAt: " ++ show (e :: SomeException)))
#else
    unsafeRead array (fI ix)
#endif

dotbonusarrayWrite :: DotBonusArray -> UInt -> DotBonus -> IO ()
dotbonusarrayWrite array ix dot = 
#ifdef GRID_SAFE
    writeArray array (fI ix) dot
#else
    unsafeWrite array (fI ix) dot
#endif

dotbonusarrayUpdate :: UInt -> DotBonusArray -> (DotBonus -> DotBonus) -> IO ()
dotbonusarrayUpdate size array f = 
    forM_ (range 0 size) $ \ix -> 
      case fI ix of
#ifdef GRID_SAFE
        ix -> C.catch (readArray array ix >>= \dot -> writeArray array ix (f dot))
                      (\e -> error ("dotbonusarrayUpdate: " ++ show (e :: SomeException)))
#else
        ix -> unsafeRead array ix >>= \dot -> unsafeWrite array ix (f dot)
#endif

dotbonusarrayForIO_ :: UInt -> DotBonusArray -> (DotBonus -> IO ()) -> IO ()
dotbonusarrayForIO_ size array f = 
#ifdef GRID_SAFE
    C.catch (forM_ (range 0 size) $ \ix -> readArray array (fI ix) >>= f)
            (\e -> error ("dotbonusarrayForIO_: " ++ show (e :: SomeException)))
#else
    forM_ (range 0 size) $ \ix -> unsafeRead array (fI ix) >>= f
#endif



--------------------------------------------------------------------------------
--  DotTeleArray

type DotTeleArray =
    IOArray Int DotTele


dottelearrayList :: UInt -> [DotTele] -> IO DotTeleArray
dottelearrayList size dots = 
    newListArray (0, fI size - 1) dots

dottelearrayAt :: DotTeleArray -> UInt -> IO DotTele
dottelearrayAt array ix = 
#ifdef GRID_SAFE
    C.catch (readArray array (fI ix))
            (\e -> error ("dottelearrayAt: " ++ show (e :: SomeException)))
#else
    unsafeRead array (fI ix)
#endif

dottelearrayWrite :: DotTeleArray -> UInt -> DotTele -> IO ()
dottelearrayWrite array ix dot = 
#ifdef GRID_SAFE
    writeArray array (fI ix) dot
#else
    unsafeWrite array (fI ix) dot
#endif

dottelearrayUpdate :: UInt -> DotTeleArray -> (DotTele -> DotTele) -> IO ()
dottelearrayUpdate size array f = 
    forM_ (range 0 size) $ \ix -> 
      case fI ix of
#ifdef GRID_SAFE
        ix -> C.catch (readArray array ix >>= \dot -> writeArray array ix (f dot))
                      (\e -> error ("dottelarrayUpdate: " ++ show (e :: SomeException)))
#else
        ix -> unsafeRead array ix >>= \dot -> unsafeWrite array ix (f dot)
#endif

dottelearrayForIO_ :: UInt -> DotTeleArray -> (DotTele -> IO ()) -> IO ()
dottelearrayForIO_ size array f = 
#ifdef GRID_SAFE
    C.catch (forM_ (range 0 size) $ \ix -> readArray array (fI ix) >>= f)
            (\e -> error ("dottelearrayForIO_: " ++ show (e :: SomeException)))
#else
    forM_ (range 0 size) $ \ix -> unsafeRead array (fI ix) >>= f
#endif



--------------------------------------------------------------------------------
--  DotFinishArray

type DotFinishArray =
    IOArray Int DotFinish


dotfinisharrayList :: UInt -> [DotFinish] -> IO DotFinishArray
dotfinisharrayList size dots = 
    newListArray (0, fI size - 1) dots

dotfinisharrayAt :: DotFinishArray -> UInt -> IO DotFinish
dotfinisharrayAt array ix = 
#ifdef GRID_SAFE
    C.catch (readArray array (fI ix))
            (\e -> error ("dotfinisharrayAt: " ++ show (e :: SomeException)))
#else
    unsafeRead array (fI ix)
#endif

dotfinisharrayWrite :: DotFinishArray -> UInt -> DotFinish -> IO ()
dotfinisharrayWrite array ix dot = 
#ifdef GRID_SAFE
    writeArray array (fI ix) dot
#else
    unsafeWrite array (fI ix) dot
#endif

dotfinisharrayUpdate :: UInt -> DotFinishArray -> (DotFinish -> DotFinish) -> IO ()
dotfinisharrayUpdate size array f = 
    forM_ (range 0 size) $ \ix -> 
      case fI ix of
#ifdef GRID_SAFE
        ix -> C.catch (readArray array ix >>= \dot -> writeArray array ix (f dot))
                      (\e -> error ("dotfinisharrayUpdate: " ++ show (e :: SomeException)))
#else
        ix -> unsafeRead array ix >>= \dot -> unsafeWrite array ix (f dot)
#endif

dotfinisharrayForIO_ :: UInt -> DotFinishArray -> (DotFinish -> IO ()) -> IO ()
dotfinisharrayForIO_ size array f = 
#ifdef GRID_SAFE
    C.catch (forM_ (range 0 size) $ \ix -> readArray array (fI ix) >>= f)
            (\e -> error ("dotfinisharrayForIO_: " ++ show (e :: SomeException)))
#else
    forM_ (range 0 size) $ \ix -> unsafeRead array (fI ix) >>= f
#endif


