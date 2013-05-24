--------------------------------------------------------------------------------
-- | 
-- Module      : Graphics.Rendering.OpenGLHelper.OpenGL2D
-- 
-- helper functions for 2D (xy-plane)
-- 
--------------------------------------------------------------------------------

module OpenGL.Helpers.GL1D
  (

    vertex1,
    translate1,
    scale1,
    
  ) where

import Control.Monad.Trans
import OpenGL as GL




-- | vertex at (x, y, 0)
vertex1 :: MonadIO m => Double -> m ()
vertex1 x = do
    liftIO $ glVertex3f (realToFrac x) 0 0


-- | translate (x, y, 0)
translate1 :: MonadIO m => Double -> m ()
translate1 x = do
    liftIO $ glTranslatef (realToFrac x) 0 0


-- | scaling (x, y, 1)
scale1 :: MonadIO m => Double -> m ()
scale1 x = do
    liftIO $ glScalef (realToFrac x) 1 1


