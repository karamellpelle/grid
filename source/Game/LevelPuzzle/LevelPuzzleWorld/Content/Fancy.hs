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
module Game.LevelPuzzle.LevelPuzzleWorld.Content.Fancy
  (
    ContentData (..),
    makeContentData,
    destroyContentData,
    offsetarrayAt,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.LevelPuzzle.LevelPuzzleWorld.Room
import Game.LevelPuzzle.LevelPuzzleWorld.Wall

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade

import Data.Array.Unboxed
import Data.Array.Base


data ContentData =
    ContentData
    {
        contentdataWallVAO :: !GLuint,
        contentdataWallVBO :: !GLuint,

        contentdataWallOffsets :: !OffsetArray

        -- map any two connected rooms into distinct colors:
        -- contentdataColorMapMap :: !ColorMapMapArray -- RoomIx -> ColorMapIx
    }



destroyContentData :: ContentData -> MEnv' ()
destroyContentData cntdata = io $ do
    delBuf $ contentdataWallVBO cntdata
    delVAO $ contentdataWallVAO cntdata



makeContentData :: UInt -> [Room] -> MEnv' ContentData
makeContentData roomssize rooms = io $ do

    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord
    glEnableVertexAttribArray attNormal
    
    -- vbo
    (vbo, offs) <- makeWallVBOOff rooms
    glVertexAttribPointer attPos 3 gl_SHORT gl_FALSE 16 $ mkPtrGLvoid 0
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 16 $ mkPtrGLvoid 8
    glVertexAttribPointer attNormal 3 gl_BYTE gl_FALSE 16 $ mkPtrGLvoid 12

    return ContentData
           {
              contentdataWallVAO = vao,
              contentdataWallVBO = vbo,
              contentdataWallOffsets = offsetarrayList (length' offs) offs
           }
    

makeWallVBOOff :: [Room] -> IO (GLuint, [UInt])
makeWallVBOOff rooms = do
    vbo <- bindNewBuf gl_ARRAY_BUFFER

    let size = foldl' (\n room -> n + roomWallSize room) 0 rooms
    glBufferData gl_ARRAY_BUFFER (fI $ 160 * size) nullPtr gl_STATIC_DRAW
    -- wtf?? empty data not allowed in glMapBufferOES!!!
    offs <- if size == 0 
            then return [0, 0]  -- prevent index out of bounds
            else writeBuf gl_ARRAY_BUFFER $ writeRooms rooms size

    return (vbo, offs)


writeRooms :: [Room] -> UInt -> Ptr a -> IO [UInt]
writeRooms rooms size = \ptr -> 
    helper rooms 0 ptr
    where
      -- write walls in content
      helper rooms off = \ptr -> 
          case rooms of
              []            -> return [off]
              (room:rooms)  -> do
                  let size = roomWallSize room
                      walls = roomWall room
                      verts = size * 10
                  ptr' <- helper' walls size 0 ptr 
                  (off:) `fmap` helper rooms (off + verts) ptr'
     
      -- write walls in room
      helper' wallarray wallarraysize ix = \ptr ->
          if ix == wallarraysize
            then return ptr
            else do
              let wall = wallarrayAt wallarray ix
              helper'' wall ptr
              helper' wallarray wallarraysize (ix + 1) (plusPtr ptr 160)

      -- write wall
      helper'' wall ptr = do
          let Node tmpn0 tmpn1 tmpn2 = wallNode wall
              n0 = fI tmpn0 :: GLshort
              n1 = fI tmpn1 :: GLshort
              n2 = fI tmpn2 :: GLshort
              Node tmpx0 tmpx1 tmpx2 = wallX wall
              x0 = fI tmpx0 :: GLshort
              x1 = fI tmpx1 :: GLshort
              x2 = fI tmpx2 :: GLshort
              Node tmpy0 tmpy1 tmpy2 = wallY wall
              y0 = fI tmpy0 :: GLshort
              y1 = fI tmpy1 :: GLshort
              y2 = fI tmpy2 :: GLshort
              s0 = 0x0000 :: GLushort
              s1 = 0x7fff :: GLushort
              s2 = if wallIsDouble wall then 0x8000 else 0x7fff :: GLushort
              s3 = if wallIsDouble wall then 0x0000 else 0xffff :: GLushort
              t0 = 0x0000 :: GLushort
              t1 = 0xffff :: GLushort
              Node tmpx tmpy tmpz = nodeCross (wallX wall) (wallY wall)
              norx = fI $ signum tmpx :: GLbyte
              nory = fI $ signum tmpy :: GLbyte
              norz = fI $ signum tmpz :: GLbyte
          pokeByteOff ptr (0  +  0)  (n0)
          pokeByteOff ptr (0  +  2)  (n1)
          pokeByteOff ptr (0  +  4)  (n2)
          pokeByteOff ptr (0  +  8)  (s0) 
          pokeByteOff ptr (0  +  9)  (t1)
          pokeByteOff ptr (0  + 10)  (norx)
          pokeByteOff ptr (0  + 11)  (nory)
          pokeByteOff ptr (0  + 12)  (norz)

          pokeByteOff ptr (16 +  0)  (n0)
          pokeByteOff ptr (16 +  2)  (n1)
          pokeByteOff ptr (16 +  4)  (n2)
          pokeByteOff ptr (16 +  8)  (s0)
          pokeByteOff ptr (16 + 10)  (t0)
          pokeByteOff ptr (16 + 12)  (norx)
          pokeByteOff ptr (16 + 13)  (nory)
          pokeByteOff ptr (16 + 14)  (norz)

          pokeByteOff ptr (32 +  0)  (n0 + y0)
          pokeByteOff ptr (32 +  2)  (n1 + y1)
          pokeByteOff ptr (32 +  4)  (n2 + y2)
          pokeByteOff ptr (32 +  8)  (s0)
          pokeByteOff ptr (32 + 10)  (t1)
          pokeByteOff ptr (32 + 12)  (norx)
          pokeByteOff ptr (32 + 13)  (nory)
          pokeByteOff ptr (32 + 14)  (norz)

          pokeByteOff ptr (48 +  0)  (n0 + x0)
          pokeByteOff ptr (48 +  2)  (n1 + x1)
          pokeByteOff ptr (48 +  4)  (n2 + x2)
          pokeByteOff ptr (48 +  8)  (s1)
          pokeByteOff ptr (48 + 10)  (t0)
          pokeByteOff ptr (48 + 12)  (norx)
          pokeByteOff ptr (48 + 13)  (nory)
          pokeByteOff ptr (48 + 14)  (norz)
          
          pokeByteOff ptr (64 +  0)  (n0 + x0 + y0) 
          pokeByteOff ptr (64 +  2)  (n1 + x1 + y1)
          pokeByteOff ptr (64 +  4)  (n2 + x2 + y2)
          pokeByteOff ptr (64 +  8)  (s1)
          pokeByteOff ptr (64 + 10)  (t1)
          pokeByteOff ptr (64 + 12)  (norx)
          pokeByteOff ptr (64 + 13)  (nory)
          pokeByteOff ptr (64 + 14)  (norz)
          
          pokeByteOff ptr (80 +  0)  (n0 + x0)
          pokeByteOff ptr (80 +  2)  (n1 + x1)
          pokeByteOff ptr (80 +  4)  (n2 + x2)
          pokeByteOff ptr (80 +  8)  (s2)
          pokeByteOff ptr (80 + 10)  (t0)
          pokeByteOff ptr (80 + 12)  (-norx)
          pokeByteOff ptr (80 + 13)  (-nory)
          pokeByteOff ptr (80 + 14)  (-norz)
          
          pokeByteOff ptr (96 +  0)  (n0 + x0 + y0)
          pokeByteOff ptr (96 +  2)  (n1 + x1 + y1)
          pokeByteOff ptr (96 +  4)  (n2 + x2 + y2)
          pokeByteOff ptr (96 +  8)  (s2)
          pokeByteOff ptr (96 + 10)  (t1)
          pokeByteOff ptr (96 + 12)  (-norx)
          pokeByteOff ptr (96 + 13)  (-nory)
          pokeByteOff ptr (96 + 14)  (-norz)
          
          pokeByteOff ptr (112+  0)  (n0)
          pokeByteOff ptr (112+  2)  (n1)
          pokeByteOff ptr (112+  4)  (n2)
          pokeByteOff ptr (112+  8)  (s3)
          pokeByteOff ptr (112+ 10)  (t0)
          pokeByteOff ptr (112+ 12)  (-norx)
          pokeByteOff ptr (112+ 13)  (-nory)
          pokeByteOff ptr (112+ 14)  (-norz)
          
          pokeByteOff ptr (128+  0)  (n0 + y0)
          pokeByteOff ptr (128+  2)  (n1 + y1)
          pokeByteOff ptr (128+  4)  (n2 + y2)
          pokeByteOff ptr (128+  8)  (s3)
          pokeByteOff ptr (128+ 10)  (t1)
          pokeByteOff ptr (128+ 12)  (-norx)
          pokeByteOff ptr (128+ 13)  (-nory)
          pokeByteOff ptr (128+ 14)  (-norz)
          
          pokeByteOff ptr (144+  0)  (n0 + y0)
          pokeByteOff ptr (144+  2)  (n1 + y1)
          pokeByteOff ptr (144+  4)  (n2 + y2)
          --pokeByteOff ptr (144+  8)  ()
          --pokeByteOff ptr (144+  9)  ()
          --pokeByteOff ptr (144+ 10)  ()
          --pokeByteOff ptr (144+ 11)  ()
          --pokeByteOff ptr (144+ 12)  ()
          --pokeByteOff ptr (144+ 14)  ()
          


--------------------------------------------------------------------------------
--  OffsetArray

type OffsetArray = 
    UArray Int UInt

offsetarrayList :: UInt -> [UInt] -> OffsetArray
offsetarrayList size offs = 
    listArray (0, fI size) offs

offsetarrayAt :: OffsetArray -> UInt -> UInt
offsetarrayAt array ix = 
#ifdef GRID_SAFE
    (!) array (fI ix)
#else
    unsafeAt array (fI ix)
#endif


