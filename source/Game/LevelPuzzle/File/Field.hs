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
module Game.LevelPuzzle.File.Field where

import MyPrelude
import Numeric
import Data.Word
import File.Binary

type Field = 
    Word32


--------------------------------------------------------------------------------
--  Field

fieldCreator :: Field
fieldCreator =  0x00000001

fieldName :: Field
fieldName =     0x00000002

fieldLevelS :: Field
fieldLevelS =   0x00000003



--------------------------------------------------------------------------------
--  FieldLevel

fieldLevel :: Field
fieldLevel = 0x00000004

fieldLevelName :: Field         
fieldLevelName = 0x00000005

fieldLevelPuzzleTag :: Field    
fieldLevelPuzzleTag = 0x00000006

fieldLevelSegments :: Field     
fieldLevelSegments = 0x00000007

fieldLevelRoomS :: Field        
fieldLevelRoomS = 0x00000008

fieldLevelRoom :: Field         
fieldLevelRoom = 0x00000009

fieldLevelRoomIx :: Field       
fieldLevelRoomIx = 0x0000000a

fieldLevelRoomWallS :: Field    
fieldLevelRoomWallS = 0x0000000b

fieldLevelRoomDotPlainS :: Field
fieldLevelRoomDotPlainS = 0x0000000c

fieldLevelRoomDotBonusS :: Field
fieldLevelRoomDotBonusS = 0x0000000d

fieldLevelRoomDotTeleS :: Field 
fieldLevelRoomDotTeleS = 0x0000000e

fieldLevelRoomDotFinishS :: Field 
fieldLevelRoomDotFinishS = 0x0000000f

fieldLevelRoomWall :: Field     
fieldLevelRoomWall = 0x00000010

fieldLevelRoomDotPlain :: Field 
fieldLevelRoomDotPlain = 0x00000011

fieldLevelRoomDotBonus :: Field 
fieldLevelRoomDotBonus = 0x00000012

fieldLevelRoomDotTele :: Field  
fieldLevelRoomDotTele = 0x00000013

fieldLevelRoomDotFinish :: Field
fieldLevelRoomDotFinish = 0x00000014



--------------------------------------------------------------------------------
--  

rThisField :: Field -> Reader ()
rThisField field = 
    case word32AsWord8s field of
        (w0, w1, w2, w3) -> try (rWord8s [w0, w1, w2, w3] <?> showField field)
    


rField :: Reader Field
rField = do
    w0 <- rAnyWord8
    w1 <- rAnyWord8
    w2 <- rAnyWord8
    w3 <- rAnyWord8
    case word8sAsWord32 w0 w1 w2 w3 of
        w32 -> return w32


-- | seek field. assuming 4 bytes aligment
--   fixme: fix this!
--   3 possibilities: eof instead of 4 bytes (error "no field [field]", 
--                    4 bytes equal to 'field' (revert to beginning, ok), 
--                    4 bytes not equal to 'field' (continue recursion)
rSeekField :: Field -> Reader ()
rSeekField field = do
    --lookAhead (rThisField field) <|> rSeekField field
    error "fixme: rSeekField"


-- | seek and eat field. assuming 4 bytes aligment 
--   fixme: handle eof
rSeekEatField :: Field -> Reader ()
rSeekEatField field = do
    field' <- rField <?> ("eof while searching for field " ++ showField field)
    if field' == field then return ()
                       else rSeekEatField field



showField :: Field -> String
showField field = 
    "Field " ++ showHex field ""
