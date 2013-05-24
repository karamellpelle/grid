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
module Game.LevelPuzzleMode.File.Read
  (
    rLevelPuzzleWorld,
    rLevel,
    rLevelOffset,
    rWall,
    rDotPlain,
    rDotBonus,
    rDotTele,
    rDotFinish,
    rCompatibleHeader,

  ) where

import MyPrelude
import File.Binary

import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.File.Field
import Game.LevelPuzzleMode.File.Version


rCompatibleHeader :: Reader ()
rCompatibleHeader = do
    ws <- replicateM 4 rAnyWord8
    unless (ws == version) $ fail $ "LevelPuzzleWorld-file of version " ++ show ws ++
                                    " is not compatible with version " ++ show version


rLevelPuzzleWorld :: Reader (String, String)
rLevelPuzzleWorld = do

    rCompatibleHeader 

    -- creator
    rThisField fieldCreator
    creator <- rCString
    rAlign 4

    -- name
    rThisField fieldName
    name <- rCString
    rAlign 4

    -- offset for level definitions (level 0)
    --rThisField FieldLevelS
    --offset <- rOffset

    return (creator, name)


-- | assuming existence
rLevelOffset :: UInt -> Reader UInt
rLevelOffset ix = do
    rCompatibleHeader

    rSeekEatField fieldLevelS

    replicateM_ (fI ix) rLevel_
    rOffset


-- | assuming Level
rLevel :: Reader (Level, [(RoomIx, Room)], UInt)
rLevel = do

    -- Level --
    rThisField fieldLevel

    rThisField fieldLevelName  
    name <- rCString
    rAlign 4
    
    rThisField fieldLevelPuzzleTag
    puzzletag <- (/= 0) `fmap` rUInt32

    rThisField fieldLevelSegments
    segs <- rUInt32

    let level = makeLevel name puzzletag segs

    -- [ (RoomIx, Room) ] --
    rThisField fieldLevelRoomS
    ixrooms <- many rIxRoom
   
    -- end offset --
    off <- rOffset
    
    return (level, ixrooms, off)


-- | skip level (assuming level is ok)
rLevel_ :: Reader ()
rLevel_ = do
    rField_
    rField_
    rCString_ 
    rAlign 4
    rField_
    rField_
    rField_
    rField_
    rField_
    many_ rIxRoom_
    where


rCString_ :: Reader ()
rCString_ = 
    many_ rNonNUL >> rNUL

rField_ :: Reader ()
rField_ = do
    rAnyWord8
    rAnyWord8
    rAnyWord8
    rAnyWord8
    return ()

many_ :: Reader a -> Reader ()
many_ r = 
    many r >> return () 


rIxRoom :: Reader (RoomIx, Room)
rIxRoom = do
    rThisField fieldLevelRoom

    -- ix
    rThisField fieldLevelRoomIx 
    ix <- rUInt32

    -- walls
    rThisField fieldLevelRoomWallS
    walls <- many rWall

    -- dot plain
    rThisField fieldLevelRoomDotPlainS
    dotplains <- many rDotPlain

    -- dot bonus
    rThisField fieldLevelRoomDotBonusS
    dotbonuss <- many rDotBonus

    -- dot tele
    rThisField fieldLevelRoomDotTeleS
    dotteles <- many rDotTele

    -- dot finish
    rThisField fieldLevelRoomDotFinishS
    dotfinish <- many rDotFinish
    room <- io $ makeRoom walls dotplains dotbonuss dotteles dotfinish
    return (ix, room)

rIxRoom_ :: Reader ()
rIxRoom_ = do
    rThisField fieldLevelRoom
    rField_
    rField_
    rField_
    many_ rWall_
    rField_
    many_ rDotPlain_
    rField_
    many_ rDotBonus_
    rField_
    many_ rDotTele_
    rField_
    many_ rDotFinish_
    where
      rFields_ n = 
          replicateM_ n rField_
      rWall_ = 
          rThisField fieldLevelRoomWall >> rFields_ 10
      rDotPlain_ = 
          rThisField fieldLevelRoomDotPlain >> rFields_ 5
      rDotBonus_ = 
          rThisField fieldLevelRoomDotBonus >> rFields_ 5
      rDotTele_ = 
          rThisField fieldLevelRoomDotTele >> rFields_ 7
      rDotFinish_ = 
          rThisField fieldLevelRoomDotFinish >> rFields_ 3


rWall :: Reader Wall
rWall = do
    rThisField fieldLevelRoomWall
    isdouble <- (/= 0) `fmap` rUInt32
    n0 <- rInt32
    n1 <- rInt32
    n2 <- rInt32
    x0 <- rInt32
    x1 <- rInt32
    x2 <- rInt32
    y0 <- rInt32
    y1 <- rInt32
    y2 <- rInt32
    return $ makeWall isdouble (Node (fI n0) (fI n1) (fI n2))
                               (Node (fI x0) (fI x1) (fI x2))
                               (Node (fI y0) (fI y1) (fI y2))

rDotPlain :: Reader DotPlain
rDotPlain = do
    rThisField fieldLevelRoomDotPlain
    n0 <- rInt32
    n1 <- rInt32
    n2 <- rInt32
    size <- rUInt32
    ix <- rUInt32
    return $ makeDotPlain (Node (fI n0) (fI n1) (fI n2)) (fI size) (fI size) (fI ix)


rDotBonus :: Reader DotBonus
rDotBonus = do
    rThisField fieldLevelRoomDotBonus
    n0 <- rInt32
    n1 <- rInt32
    n2 <- rInt32
    size <- rUInt32
    add <- rUInt32
    return $ makeDotBonus (Node (fI n0) (fI n1) (fI n2)) (fI size) (fI size) (fI add)


rDotTele :: Reader DotTele
rDotTele = do
    rThisField fieldLevelRoomDotTele
    n0 <- rInt32
    n1 <- rInt32
    n2 <- rInt32
    size <- rUInt32
    n0' <- rInt32
    n1' <- rInt32
    n2' <- rInt32
    return $ makeDotTele (Node (fI n0) (fI n1) (fI n2)) (fI size) (fI size) 
                         (Node (fI n0') (fI n1') (fI n2'))

rDotFinish :: Reader DotFinish
rDotFinish = do
    rThisField fieldLevelRoomDotFinish
    n0 <- rInt32
    n1 <- rInt32
    n2 <- rInt32
    return $ makeDotFinish (Node (fI n0) (fI n1) (fI n2)) 


