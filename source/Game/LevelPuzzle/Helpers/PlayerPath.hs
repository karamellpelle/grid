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
module Game.LevelPuzzle.Helpers.PlayerPath
  (
    savePlayerPathEmpty,
    savePlayerPath,

  ) where

import MyPrelude
import File.Binary

import Game
import Game.LevelPuzzle.LevelPuzzleWorld



savePlayerPathEmpty :: LevelPuzzleWorld -> MEnv' ()
savePlayerPathEmpty lvl = do
    path <- filePlayerPathEmpty  $ takeFileName (levelpuzzleFile lvl) ++
                                   show (levelpuzzleLevelIx lvl) ++ ".plp"
    
    writeBinary' (wPlayerPath player lvl) path
   

savePlayerPath :: Player -> LevelPuzzleWorld -> MEnv' ()
savePlayerPath player lvl = io $ do
    path <- filePlayerPath player $ takeFileName (levelpuzzleFile lvl) ++
                                    show (levelpuzzleLevelIx lvl) ++ ".plp"
    
    writeBinary' (wPlayerPath player lvl) path



wPlayerPath :: Player -> LevelPuzzleWorld -> Writer
wPlayerPath player lvl = do
    wWord8s version

    wField fieldWorld
    wCStringAlign 4 $ takeFileName (levelpuzzleFile lvl)

    wField fieldLevel
    wUInt32 $ levelpuzzleLevelIx lvl

    wField fieldPlayerID
    wCStringAlign 4 $ playerID player

    wField fieldPlayerAlias
    wCStringAlign 4 $ playerAlias player

    wField fieldSegmentS
    let path = gridPath $ levelpuzzleGrid lvl
    forM_ (rangeMod (pathArraySize path) (pathArrayBegin path) (pathArrayEnd path))
         $ \ix -> case segmentarrayRead (pathArray path) ix of
                  seg -> wSegment seg


    where
      rangeMod size b e =
          if b == e then [] else b : rangeMod ((b + 1) `mod` size) e

      wSegment (Segment (Turn a0 a1 a2 _ _ _ _ _ _) (Node n0 n1 n2)) = do
          wWord8s [255, fI a0, fI a1, fI a2] -- fixme: assert bit conversion!
          wInt32 $ fI n0
          wInt32 $ fI n1
          wInt32 $ fI n2



--------------------------------------------------------------------------------
--  

-- | s000
version :: [Word8]
version =
    [0x73, 0x30, 0x30, 0x30]


fieldWorld :: Word32
fieldWorld = 0x00000001

fieldLevel :: Word32
fieldLevel = 0x00000002

fieldPlayerID :: Word32
fieldPlayerID = 0x00000003

fieldPlayerAlias :: Word32
fieldPlayerAlias = 0x00000004

fieldSegments :: Word32
fieldSegmentS = 0x00000005

wField :: Word32 -> Writer
wField field =
    case word32asWord8s field of
        (w0, w1, w2, w3)  -> wWord8s [w0, w1, w2, w3]

    
