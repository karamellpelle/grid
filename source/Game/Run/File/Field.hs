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
module Game.Run.File.Field
  (
    Field,
  
    fieldLevelPuzzleName,
    fieldLevelPuzzleNamePeakS,
    fieldLevelPuzzleNamePeak,
    fieldMemoryPeak,
    fieldSpecialIsCompleted,
    fieldIntensity,
    
    rThisField,
    rField,
    rSeekField,
    wField,

  ) where


import MyPrelude
import Numeric
import Data.Word
import File.Binary


type Field = 
    Word32


--------------------------------------------------------------------------------
--  Field

fieldLevelPuzzleName :: Field
fieldLevelPuzzleName =  0x00000001

fieldLevelPuzzleNamePeakS :: Field
fieldLevelPuzzleNamePeakS =  0x00000002

fieldLevelPuzzleNamePeak :: Field
fieldLevelPuzzleNamePeak =  0x00000003

fieldMemoryPeak :: Field
fieldMemoryPeak = 0x00000004

fieldSpecialIsCompleted :: Field
fieldSpecialIsCompleted = 0x00000005

fieldIntensity :: Field
fieldIntensity = 0x00000006



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


-- | seek after field. assuming 4 bytes aligment 
rSeekField :: Field -> Reader ()
rSeekField field = 
    (rField <?> ("could not seek to field " ++ show field)) >>= \field' -> 
        if field' == field then return () else rSeekField field


wField :: Field -> Writer
wField field = do
    case word32AsWord8s field of
        (w0, w1, w2, w3) -> wWord8s [w0, w1, w2, w3]


showField :: Field -> String
showField field = 
    "Field " ++ showHex field ""





