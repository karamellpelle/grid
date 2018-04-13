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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Game.Grid.GridWorld.SegmentArray
  (
      SegmentArray,
      makeSegmentArray,
      segmentarrayRead,
      segmentarrayWrite,


  ) where


import MyPrelude
import Game.Grid.GridWorld.Segment
import Data.Int

-- making unportable
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Int
import System.IO.Unsafe

--------------------------------------------------------------------------------
-- SegmentArray
-- we need some type of sequential container for the segments of path. since the 
-- tail of the segments syncs our GL data, we want fast access to the tail. also,
-- we want the path to be able to grow modulo its size. so I decided to use a 
-- custom array type.
-- fixme: verify that this type is fast (enough)
-- note: in GHC, the datatypes Int8/Int16/... are implemented by the word size
--       of the architecture (which is 32 bits on ARMv7), so using them does not
--       save memory. but in this implementation of SegmentArray, we use the cor-
--       responding bits and so save memory.

data SegmentArray =
    SegmentArray (MutableByteArray# RealWorld)


unsafeState# :: (State# RealWorld -> (# State# RealWorld, a #)) -> a
unsafeState# =
    unsafePerformIO . IO



makeSegmentArray :: UInt -> SegmentArray
makeSegmentArray (W# size#) = unsafeState# $ \s# ->
  case newAlignedPinnedByteArray# (word2Int# (size# `timesWord#` 16##)) 4# s# of
        (# s'#, mba# #) -> (# s'#, SegmentArray mba# #)


segmentarrayWrite :: SegmentArray -> UInt -> Segment -> SegmentArray
segmentarrayWrite sa@(SegmentArray mba#) 
                  (W# ix#) 
                  (Segment (Node (I16# x#) (I16# y#) (I16# z#))
                           (Turn (I8# x0#) (I8# x1#) (I8# x2#)
                                 (I8# y0#) (I8# y1#) (I8# y2#)
                                 (I8# z0#) (I8# z1#) (I8# z2#))) = unsafeState# $ \s# -> 
    case writeInt16Array# mba# (ix2Bytes# ix# 0##)  x# s# of { s# ->
    case writeInt16Array# mba# (ix2Bytes# ix# 1##)  y# s# of { s# ->
    case writeInt16Array# mba# (ix2Bytes# ix# 2##)  z# s# of { s# ->
    case writeInt8Array# mba# (ix4Bytes# ix# 6##) x0#  s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 7##) x1#  s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 8##) x2#  s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 9##) y0#  s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 10##) y1# s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 11##) y2# s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 12##) z0# s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 13##) z1# s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 14##) z2# s# of { s# -> 
    case writeInt8Array# mba# (ix4Bytes# ix# 15##) 0#  s# of { s# -> 
        (# s#, sa #) }}}}}}}}}}}}}

    where
      ix2Bytes# ix# a# = 
          word2Int# ((8## `timesWord#` ix#) `plusWord#` a#)
      ix4Bytes# ix# a# =
          word2Int# ((16## `timesWord#` ix#) `plusWord#` a#)


segmentarrayRead :: SegmentArray -> UInt -> Segment
segmentarrayRead (SegmentArray mba#) (W# ix#) = unsafeState# $ \s# ->
    case readInt16Array# mba# (ix2Bytes# ix# 0##) s# of { (# s#, x# #) -> 
    case readInt16Array# mba# (ix2Bytes# ix# 1##) s# of { (# s#, y# #) -> 
    case readInt16Array# mba# (ix2Bytes# ix# 2##) s# of { (# s#, z# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 6##)  s# of { (# s#, x0# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 7##)  s# of { (# s#, x1# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 8##)  s# of { (# s#, x2# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 9##)  s# of { (# s#, y0# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 10##) s# of { (# s#, y1# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 11##) s# of { (# s#, y2# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 12##) s# of { (# s#, z0# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 13##) s# of { (# s#, z1# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 14##) s# of { (# s#, z2# #) -> 
    case readInt8Array# mba# (ix4Bytes# ix# 15##) s# of { (# s#, _ #) -> 
        (# s#, Segment (Node (I16# x#) (I16# y#) (I16# z#))
                       (Turn (I8# x0#) (I8# x1#) (I8# x2#)
                             (I8# y0#) (I8# y1#) (I8# y2#)
                             (I8# z0#) (I8# z1#) (I8# z2#)) #) }}}}}}}}}}}}}

    where
      ix2Bytes# ix# a# = 
          word2Int# ((8## `timesWord#` ix#) `plusWord#` a#)
      ix4Bytes# ix# a# =
          word2Int# ((16## `timesWord#` ix#) `plusWord#` a#)



{-
-- | we create a newtype so that we later can just store the direction if we want
--   (strip Segment for structure)
newtype StorableSegment =
    StorableSegment Segment 


wrapStorableSegment :: Segment -> StorableSegment
wrapStorableSegment seg =
    StorableSegment seg


instance Storable StorableSegment where
    sizeOf _    = 16
    alignment _ = 4   -- ^ 4 byte align on ARM (?)
    peek ptr = do
        -- node
        x <- peekByteOff ptr 0 :: IO Int16
        y <- peekByteOff ptr 2 :: IO Int16
        z <- peekByteOff ptr 4 :: IO Int16
        -- turn
        x0 <- peekByteOff ptr 6 :: IO Int8
        x1 <- peekByteOff ptr 7 :: IO Int8
        x2 <- peekByteOff ptr 8 :: IO Int8
        y0 <- peekByteOff ptr 9 :: IO Int8
        y1 <- peekByteOff ptr 10 :: IO Int8
        y2 <- peekByteOff ptr 11 :: IO Int8
        z0 <- peekByteOff ptr 12 :: IO Int8
        z1 <- peekByteOff ptr 13 :: IO Int8
        z2 <- peekByteOff ptr 14 :: IO Int8
        -- 15 is empty!
        return $ StorableSegment $ Segment (Node x y z) (Turn x0 x1 x2
                                                              y0 y1 y2
                                                              z0 z1 z2)

    poke ptr (StorableSegment (Segment (Node x y z) (Turn x0 x1 x2
                                                          y0 y1 y2
                                                          z0 z1 z2))) = do
        -- node
        pokeByteOff ptr 0  x
        pokeByteOff ptr 2  y
        pokeByteOff ptr 4  z
        -- turn
        pokeByteOff ptr 6  x0
        pokeByteOff ptr 7  x1
        pokeByteOff ptr 8  x2
        pokeByteOff ptr 9  y0
        pokeByteOff ptr 10 y1
        pokeByteOff ptr 11 y2
        pokeByteOff ptr 12 z0
        pokeByteOff ptr 13 z1
        pokeByteOff ptr 14 z2
        -- 15: empty!
--------------------------------------------------------------------------------
--  tmp:

instance Show Path where
    show path = "Path:" ++
                "\npathCurrent:   " ++ show (pathCurrent path) ++
                "\npathAlpha:     " ++ show (pathAlpha path) ++ 
                "\npathSegments:  " ++ show (pathSegments path) ++
                "\npathSpeed:     " ++ show (pathSpeed path) ++
                "\npathTurnState: " ++ maybe "(not defined)" (const "(defined)") (pathTurnState path)

-}
