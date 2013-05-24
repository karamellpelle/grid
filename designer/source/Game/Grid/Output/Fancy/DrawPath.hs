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
module Game.Grid.Output.Fancy.DrawPath
  (
    drawPathBegin,
    drawPathEnd,

    drawPathColor,
    
    drawPathDraw,
    drawPathDrawBegin,
    drawPathDrawBeginEnd,

    drawPathDrawPath0,
    drawPathDrawPath0Begin,
    drawPathDrawPath0BeginEnd,

    drawPathDrawPath1,
    drawPathDrawSegment,

  ) where

import MyPrelude

import OpenGL
import OpenGL.Helpers

import Game
import Game.Data.Color
import Game.Grid.GridWorld
import Game.Grid.GridData
import Game.Grid.Helpers



drawPathBegin :: IO ()
drawPathBegin = 
    glLineWidth 4.0

drawPathEnd :: IO ()
drawPathEnd =
    glLineWidth 1.0


--------------------------------------------------------------------------------
--  parameters
drawPathColor :: Color -> IO ()
drawPathColor color = 
    glColor4f 1 0 0 1


--------------------------------------------------------------------------------
--  draw Path

drawPathDraw :: Path -> IO ()
drawPathDraw path =
    drawPathDrawBegin (pathArrayBegin path) path

-- | draw Path beginning from array ix 
drawPathDrawBegin :: UInt -> Path -> IO ()
drawPathDrawBegin begin path = do
    drawPathDrawPath0Begin begin path
    drawPathDrawPath1 path
 



-- | drawing path in range [ix, ix'), alpha interpolates against ix'
drawPathDrawBeginEnd :: UInt -> UInt -> Float -> Path -> IO ()
drawPathDrawBeginEnd begin end alpha path = do
    -- path0
    drawPathDrawPath0BeginEnd begin end path 

    -- path1
    let current = if end == pathArrayEnd path 
                  then pathCurrent path 
                  else segmentarrayRead (pathArray path) end
    drawPathDrawSegment current alpha 



--------------------------------------------------------------------------------
--  draw Path0


drawPathDrawPath0 :: Path -> IO ()
drawPathDrawPath0 path =
    drawPathDrawPath0Begin (pathArrayBegin path) path



drawPathDrawPath0Begin :: UInt -> Path -> IO ()
drawPathDrawPath0Begin begin path =
    drawPathDrawPath0BeginEnd begin (pathArrayEnd path) path


drawSegment (Segment (Node nx ny nz) turn) = do
    let x = fromIntegral nx 
        y = fromIntegral ny 
        z = fromIntegral nz
        Dir dx dy dz = direction turn
        x' = (fI nx) + (fI dx)
        y' = (fI ny) + (fI dy)
        z' = (fI nz) + (fI dz)
    io $ do
      glBegin gl_LINES 
      glVertex3f x y z
      glVertex3f x' y' z'
      glEnd


drawSegmentAlpha (Segment (Node nx ny nz) turn) alpha = do
    let x = fromIntegral nx 
        y = fromIntegral ny 
        z = fromIntegral nz
        Dir dx dy dz = direction turn
        x' = x + realToFrac alpha * fromIntegral dx
        z' = z + realToFrac alpha * fromIntegral dz
        y' = y + realToFrac alpha * fromIntegral dy
    io $ do
      glBegin gl_LINES 
      glVertex3f x y z
      glVertex3f x' y' z'
      glEnd
    



drawPathDrawPath0BeginEnd :: UInt -> UInt -> Path -> IO ()
drawPathDrawPath0BeginEnd begin end path = do
    let size = pathArraySize path
        end' = begin + ((end + (size - begin)) `mod` size)

        a0 = begin
        a1 = min end' size
        b0 = 0
        b1 = (max end' size) `mod` size

    -- [a0, a1)
    forM_ (range a0 a1) $ \ix -> drawSegment (segmentarrayRead (pathArray path) ix)
    forM_ (range b0 b1) $ \ix -> drawSegment (segmentarrayRead (pathArray path) ix)



--------------------------------------------------------------------------------
--  draw Path1

drawPathDrawPath1 :: Path -> IO ()
drawPathDrawPath1 path = do
    drawPathDrawSegment (pathCurrent path) (pathAlpha path)


drawPathDrawSegment :: Segment -> Float -> IO ()
drawPathDrawSegment segment alpha = do
    drawSegmentAlpha segment alpha
