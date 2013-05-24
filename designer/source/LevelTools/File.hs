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
module LevelTools.File
  (
    wLevel,
    wWorldHeader,
    findSimilarFileName,
    rLevelFile,
    rCompatibleHeaderLevel,

  ) where

import MyPrelude
import Game
import Game.LevelPuzzleMode.File.Field
import qualified Game.LevelPuzzleMode.File.Version as LP
import Game.LevelPuzzleMode.File.Read
import Game.LevelPuzzleMode.File
import File.Binary
import LevelTools.EditWorld
import System.IO
import System.Directory
import System.FilePath


findSimilarFileName base ext = do
    let path = base <.> ext
    boolA <- doesFileExist path
    boolB <- doesDirectoryExist path
    if boolA || boolB then findSimilarFileName (base ++ "~") ext
                      else return path


version :: [Word8]
version = 
    [0x6c, 0x30, 0x30, 0x30]


wWorldHeader :: String -> String -> Writer
wWorldHeader creator name = do
    wWord8s LP.version

    -- creator
    wField fieldCreator
    wCStringAlign 4 $ creator

    -- name
    wField fieldName
    wCStringAlign 4 $ name

    wField fieldLevelS


wLevel :: EditWorld -> Writer
wLevel edit = do

    -- header
    wWord8s version

    wField fieldLevel

    let level = editLevel edit

    -- name
    wField fieldLevelName
    wCStringAlign 4 $ levelName level

    -- pussle tag
    wField fieldLevelPuzzleTag
    case levelPuzzleTag level of
        False   -> wUInt32  0
        True    -> wUInt32  1

    -- segments
    wField fieldLevelSegments
    wUInt32 $ levelSegments level

    -- [(RoomIx, Room)]
    let scnt = editSemiContent edit
    wField fieldLevelRoomS
    forM_ (scontentRooms scnt) $ wSemiRoom
    -- assert: align == 4

wSemiRoom sroom = do
    wField fieldLevelRoom

    -- ix
    wField fieldLevelRoomIx
    wUInt32 $ sroomRoomIx sroom

    -- walls
    wField fieldLevelRoomWallS
    forM_ (sroomWall sroom) wWall

    -- dot plain
    wField fieldLevelRoomDotPlainS
    forM_ (sroomDotPlain sroom) wDotPlain

    -- dot bonus
    wField fieldLevelRoomDotBonusS
    forM_ (sroomDotBonus sroom) wDotBonus

    -- dot tele
    wField fieldLevelRoomDotTeleS
    forM_ (sroomDotTele sroom) wDotTele

    -- dot finish
    wField fieldLevelRoomDotFinishS
    forM_ (sroomDotFinish sroom) wDotFinish


wWall wall = do
    wField fieldLevelRoomWall
    case wallIsDouble wall of
        False   -> wUInt32 0
        True    -> wUInt32 1

    let Node n0 n1 n2 = wallNode wall
        Node x0 x1 x2 = wallX wall
        Node y0 y1 y2 = wallY wall

    wInt32 $ fI n0
    wInt32 $ fI n1
    wInt32 $ fI n2
    wInt32 $ fI x0
    wInt32 $ fI x1
    wInt32 $ fI x2
    wInt32 $ fI y0
    wInt32 $ fI y1
    wInt32 $ fI y2


wDotPlain dot = do
    wField fieldLevelRoomDotPlain
    let Node n0 n1 n2 = dotplainNode dot
    wInt32 $ fI n0
    wInt32 $ fI n1
    wInt32 $ fI n2
    wUInt32 $ dotplainSize dot
    wUInt32 $ dotplainRoom dot


wDotBonus dot = do
    wField fieldLevelRoomDotBonus
    let Node n0 n1 n2 = dotbonusNode dot
    wInt32 $ fI n0
    wInt32 $ fI n1
    wInt32 $ fI n2
    wUInt32 $ dotbonusSize dot
    wUInt32 $ dotbonusAdd dot


wDotTele dot = do
    wField fieldLevelRoomDotTele
    let Node n0 n1 n2 = dotteleNode dot
        Node n0' n1' n2' = dotteleNode dot
    wInt32 $ fI n0
    wInt32 $ fI n1
    wInt32 $ fI n2
    wUInt32 $ dotteleSize dot
    wInt32 $ fI n0'
    wInt32 $ fI n1'
    wInt32 $ fI n2'

wDotFinish dot = do
    wField fieldLevelRoomDotFinish
    let Node n0 n1 n2 = dotfinishNode dot
    wInt32 $ fI n0
    wInt32 $ fI n1
    wInt32 $ fI n2

        

    
wField :: Field -> Writer
wField field = do
    case word32AsWord8s field of
        (w0, w1, w2, w3)  -> wWord8s [w0, w1, w2, w3]



--------------------------------------------------------------------------------
--  Reader

rCompatibleHeaderLevel :: Reader ()
rCompatibleHeaderLevel = do
    ws <- replicateM 4 rAnyWord8
    unless (ws == version) $ fail $ "can not read .ldef file with header " ++ show ws



-- | assuming Level
rLevelFile :: Reader (Level, [SemiRoom])
rLevelFile = do
    rCompatibleHeaderLevel

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

    -- [ SemiRoom ] --
    rThisField fieldLevelRoomS
    srooms <- many rSemiRoom
   
    return (level, srooms)



rSemiRoom :: Reader SemiRoom
rSemiRoom = do
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
    dotfinishs <- many rDotFinish

    return $ makeSemiRoom ix walls dotplains dotbonuss dotteles dotfinishs

