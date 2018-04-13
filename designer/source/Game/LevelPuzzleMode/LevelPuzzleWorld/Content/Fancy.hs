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
module Game.LevelPuzzleMode.LevelPuzzleWorld.Content.Fancy
  (
    ContentData (..),
    makeContentData,
    destroyContentData,

    OffsetArray,
    offsetarrayAt,

  ) where

import MyPrelude
import Game.MEnv
import Data.Array.Unboxed
import Data.Array.Base

import Game.Shade
import Game.Grid.GridWorld.Node
import Game.LevelPuzzleMode.LevelPuzzleWorld.Room

import OpenGL
import OpenGL.Helpers


data ContentData =
    ContentData
    {
        contentdataWallVAO :: !GLuint,
        contentdataWallVBO :: !GLuint,
        contentdataWallIBO :: !GLuint,

        contentdataWallOffsets :: !OffsetArray
        -- map any two connected rooms into distinct colors:
        -- contentdataColorMapMap :: !ColorMapMapArray -- RoomIx -> ColorMapIx
    }





makeContentData :: UInt -> [Room] -> MEnv' ContentData
makeContentData roomssize rooms = io $ do
    let numWalls = foldl' (\ix room -> ix + roomWallSize room) 0 rooms :: UInt

    -- vao
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attTexCoord
    
    -- ibo
    ibo <- makeGroupIBO 6 numWalls

    -- vbo
    (vbo, offs) <- makeWallVBOOffsets rooms numWalls
    
    return ContentData
           {
              contentdataWallVAO = vao,
              contentdataWallVBO = vbo,
              contentdataWallIBO = ibo,

              contentdataWallOffsets = offsetarrayList (length' offs) offs
           }


makeWallVBOOffsets :: [Room] -> UInt -> IO (GLuint, [UInt])
makeWallVBOOffsets rooms numWalls = do
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    glBufferData gl_ARRAY_BUFFER (fI $ numWalls * 72) nullPtr gl_STATIC_DRAW

    -- wtf?? empty data not allowed in glMapBufferOES!!!
    offs <- if numWalls == 0 then return [0] else do
            writeBuf gl_ARRAY_BUFFER $ helper rooms 0
    glVertexAttribPointer attPos 3 gl_SHORT gl_FALSE 12 $ mkPtrGLvoid 0
    glVertexAttribPointer attTexCoord 2 gl_UNSIGNED_SHORT gl_TRUE 12 $ mkPtrGLvoid 8

    return (vbo, offs)

    where
      -- write walls in content
      helper rooms off ptr = 
          case rooms of
              []      -> return [off]
              (r:rs)  -> do
                  helper' (roomWall r) (roomWallSize r) 0 ptr 
                  (off:) `fmap` helper rs (off + roomWallSize r * 8)
                                       (plusPtr ptr (fI $ roomWallSize r * 72))
     
      -- write walls in room
      helper' wallarray wallarraysize ix ptr = 
          if ix == wallarraysize
            then return ()
            else do
              helper'' (wallarrayAt wallarray ix) ptr
              helper' wallarray wallarraysize (ix + 1) (plusPtr ptr 72)

      -- write wall
      helper'' wall ptr = do
          let Node n0 n1 n2 = wallNode wall
              Node x0 x1 x2 = wallX wall
              Node y0 y1 y2 = wallY wall
              s0 = 0x0000
              s1 = 0x8000
              s2 = if wallIsDouble wall then 0x0000 else 0xffff
              t0 = 0x0000
              t1 = 0xffff
          pokeByteOff ptr (0 +  0)  (fI (n0 + y0) :: GLshort)
          pokeByteOff ptr (0 +  2)  (fI (n1 + y1) :: GLshort)
          pokeByteOff ptr (0 +  4)  (fI (n2 + y2) :: GLshort)
          pokeByteOff ptr (0 +  8)  (s0 :: GLushort)
          pokeByteOff ptr (0 + 10)  (t1 :: GLushort)
        
          pokeByteOff ptr (12+  0)  (fI n0 :: GLshort)
          pokeByteOff ptr (12+  2)  (fI n1 :: GLshort)
          pokeByteOff ptr (12+  4)  (fI n2 :: GLshort)
          pokeByteOff ptr (12+  8)  (s0 :: GLushort)
          pokeByteOff ptr (12+ 10)  (t0 :: GLushort)
        
          pokeByteOff ptr (24+  0)  (fI (n0 + x0 + y0) :: GLshort)
          pokeByteOff ptr (24+  2)  (fI (n1 + x1 + y1) :: GLshort)
          pokeByteOff ptr (24+  4)  (fI (n2 + x2 + y2) :: GLshort)
          pokeByteOff ptr (24+  8)  (s1 :: GLushort)
          pokeByteOff ptr (24+ 10)  (t1 :: GLushort)
        
          pokeByteOff ptr (36+  0)  (fI (n0 + x0) :: GLshort)
          pokeByteOff ptr (36+  2)  (fI (n1 + x1) :: GLshort)
          pokeByteOff ptr (36+  4)  (fI (n2 + x2) :: GLshort)
          pokeByteOff ptr (36+  8)  (s1 :: GLushort)
          pokeByteOff ptr (36+ 10)  (t0 :: GLushort)
        
          pokeByteOff ptr (48+  0)  (fI (n0 + y0) :: GLshort)
          pokeByteOff ptr (48+  2)  (fI (n1 + y1) :: GLshort)
          pokeByteOff ptr (48+  4)  (fI (n2 + y2) :: GLshort)
          pokeByteOff ptr (48+  8)  (s2 :: GLushort)
          pokeByteOff ptr (48+ 10)  (t1 :: GLushort)
        
          pokeByteOff ptr (60+  0)  (fI n0 :: GLshort)
          pokeByteOff ptr (60+  2)  (fI n1 :: GLshort)
          pokeByteOff ptr (60+  4)  (fI n2 :: GLshort)
          pokeByteOff ptr (60+  8)  (s2 :: GLushort)
          pokeByteOff ptr (60+ 10)  (t0 :: GLushort)
        
          


destroyContentData :: ContentData -> MEnv' ()
destroyContentData cntdata = io $ do
    delBuf $ contentdataWallVBO cntdata
    delBuf $ contentdataWallIBO cntdata
    delBuf $ contentdataWallVAO cntdata



--------------------------------------------------------------------------------
--  OffsetArray

type OffsetArray = 
    UArray Int UInt

offsetarrayList :: UInt -> [UInt] -> OffsetArray
offsetarrayList size offs = 
    listArray (0, fI size - 1) offs

offsetarrayAt :: OffsetArray -> UInt -> UInt
offsetarrayAt array ix = 
    unsafeAt array (fI ix)

