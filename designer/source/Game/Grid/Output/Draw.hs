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
module Game.Grid.Output.Draw
  (
    drawGround,

{-
    drawPath,
    drawSegment,
    drawSegmentAlpha,
-}
  ) where


import MyPrelude
import Game
import Game.Grid.GridWorld
import Game.Grid.Helpers

import OpenGL





--------------------------------------------------------------------------------
--  drawings


-- draw ground centered at node
drawGround node = io $ do
    let Node x y z = node
        x0 = x - radius
        z0 = z - radius
        x1 = x + radius
        z1 = z + radius
-- tmp
    glColor4f 1 1 1 0.4
    glBegin gl_LINES
    forM_ [x0.. x1] $ \x -> do
        glVertex3f (fromIntegral x) 0 (fromIntegral z0)
        glVertex3f (fromIntegral x) 0 (fromIntegral z1)
    forM_ [z0..z1] $ \z -> do
        glVertex3f (fromIntegral x0) 0 (fromIntegral z)
        glVertex3f (fromIntegral x1) 0 (fromIntegral z)
    glEnd
--
    where
        radius = 5

{-
drawPath path = do
    -- draw segments
    io $ glColor4f 1 0 0 1
    forM_ (pathSegments path) $ drawSegment

    -- draw current
    io $ glColor4f 0 1 0 1
    drawSegmentAlpha (pathCurrent path) (pathAlpha path)

drawSegment (Segment (Node nx ny nz) turn) = do
    let x = fromIntegral nx 
        y = fromIntegral ny 
        z = fromIntegral nz
        (dx, dy, dz) = direction turn
        x' = fromIntegral $ nx + dx
        y' = fromIntegral $ ny + dy
        z' = fromIntegral $ nz + dz
    io $ do
      glBegin gl_LINES 
      glVertex3f x y z
      glVertex3f x' y' z'
      glEnd


drawSegmentAlpha (Segment (Node nx ny nz) turn) alpha = do
    let x = fromIntegral nx 
        y = fromIntegral ny 
        z = fromIntegral nz
        (dx, dy, dz) = direction turn
        x' = x + realToFrac alpha * fromIntegral dx
        z' = z + realToFrac alpha * fromIntegral dz
        y' = y + realToFrac alpha * fromIntegral dy
    io $ do
      glBegin gl_LINES 
      glVertex3f x y z
      glVertex3f x' y' z'
      glEnd
    
-}
