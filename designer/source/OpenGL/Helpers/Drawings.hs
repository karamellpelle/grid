--------------------------------------------------------------------------------
-- | 
-- Module      : Graphics.Rendering.OpenGLHelper.Drawings
-- 
-- Some figures to draw
-- 
--------------------------------------------------------------------------------


module OpenGL.Helpers.Drawings
  (
    drawCross,
    drawGroundTile,
    drawGroundGrid,
    drawCube,
    drawSphere,

  ) where


import Control.Monad.Trans
import OpenGL.Helpers
import OpenGL.Helpers.GL2D
import OpenGL.Helpers.GL3D
import Control.Monad


-- tmp:!!!
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU as GL

--------------------------------------------------------------------------------
--  2D figures


-- | Red and blue cross, inside '[0,1] x [0,1] x {0}'
drawCross :: MonadIO m => m ()
drawCross = liftIO $ GL.unsafeRenderPrimitive Lines $ do
    color4 1 0 0 1
    vertex2 0 0
    vertex2 1 1
    
    color4 0 0 1 1
    vertex2 0 1
    vertex2 1 0



--------------------------------------------------------------------------------
--  3D figures

-- | Draw set '[0,1] x {0} x [0,1]' using current color
drawGroundTile :: MonadIO m => m ()
drawGroundTile = liftIO $ GL.unsafeRenderPrimitive Quads $ do
    vertex3 0 0 0
    vertex3 0 0 1
    vertex3 1 0 1
    vertex3 1 0 0


-- | draw grid in xz-plane, with range [a, b)
drawGroundGrid :: MonadIO m => (Int, Int) -> (Int, Int) -> m ()
drawGroundGrid (x0, x1) (z0, z1) = liftIO $ do
    forM_ (range x0 x1) $ \x ->
        renderPrimitive Lines $ do
            vertex3 (fromIntegral x) 0  (fromIntegral z0)
            vertex3 (fromIntegral x) 0 (fromIntegral z1)
    forM_ (range z0 z1) $ \z ->
        renderPrimitive Lines $ do
            vertex3 (fromIntegral x0) 0 (fromIntegral z)
            vertex3 (fromIntegral x1) 0 (fromIntegral z)
    where
        range m n = if m == n then []
                              else m : range (succ m) n


-- | draw cube [-1,1]x[-1,1]x[-1,1], with faces outwards
drawCube :: MonadIO m => m ()
drawCube = liftIO $ GL.unsafeRenderPrimitive GL.Quads $ do
    -- -xy 
    vertex3 (-1) (-1) (-1)
    vertex3 (-1) (1) (-1)
    vertex3 (1) (1) (-1)
    vertex3 (1) (-1) (-1)
    -- -yz
    vertex3 (-1) (-1) (-1)
    vertex3 (-1) (-1) (1)
    vertex3 (-1) (1) (1)
    vertex3 (-1) (1) (-1)
    -- -zx
    vertex3 (-1) (-1) (-1)
    vertex3 (1) (-1) (-1)
    vertex3 (1) (-1) (1)
    vertex3 (-1) (-1) (1)
    -- xy
    vertex3 1 1 1
    vertex3 (-1) 1 1
    vertex3 (-1) (-1) 1
    vertex3 (1) (-1) 1
    -- yz
    vertex3 1 1 1
    vertex3 1 (-1) 1
    vertex3 1 (-1) (-1)
    vertex3 1 1 (-1)
    -- zx
    vertex3 1 1 1
    vertex3 1 1 (-1)
    vertex3 (-1) 1 (-1)
    vertex3 (-1) 1 1



-- | draw sphere with radius 1
drawSphere :: MonadIO m => m ()
drawSphere = liftIO $ do
    let style = QuadricStyle Nothing NoTextureCoordinates Outside FillStyle
        prim = Sphere 1 16 16
    GL.renderQuadric style prim
