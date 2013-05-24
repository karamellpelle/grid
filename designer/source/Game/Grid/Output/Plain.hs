module Game.Grid.Output.Plain
  (
    drawGround,
    drawPath,
    drawSegment,
    drawSegmentAlpha,

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
    

