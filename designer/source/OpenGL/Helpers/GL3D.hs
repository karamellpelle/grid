--------------------------------------------------------------------------------
-- | 
-- Module      : Graphics.Rendering.OpenGLHelper.OpenGL3D
-- 
-- helper functions for 3D
-- 
--------------------------------------------------------------------------------

module OpenGL.Helpers.GL3D
  (
    lookAt9,
    vertex3,
    normal3,
    translate3,
    rotate3D,
    scale3,
  ) where


import OpenGL as GL
import Control.Monad.Trans


-- | look at point p from point e, with upward direction u
lookAt9 :: MonadIO m => Double -> Double -> Double -> 
                        Double -> Double -> Double ->
                        Double -> Double -> Double ->
                        m ()
lookAt9   ex ey ez    px py pz    ux uy uz = liftIO $ 
    gluLookAt (realToFrac ex) (realToFrac ey) (realToFrac ez) 
              (realToFrac px) (realToFrac py) (realToFrac pz) 
              (realToFrac ux) (realToFrac uy) (realToFrac uz)



-- todo: we should use OpenGLRaw to push/pop matrix, so that we are able to run MonadIO inside:
-- as3DView :: MonadIO m => Double -> Double -> Double -> Double -> Double -> m a -> m a
-- as3DView w h fovy near far = ...
    

-- | vertex at (x, y, z)
vertex3 :: MonadIO m => Double -> Double -> Double -> m ()
vertex3 x y z =
    liftIO $ glVertex3f (realToFrac x) (realToFrac y) (realToFrac z)


-- | normal in direction (x, y, z)
--   note: normals shall typically have length 1. arguments to this functions are scaled
--         by modelview, if this is a problem, set GL.rescaleNormal $= Enabled
normal3 :: MonadIO m => Double -> Double -> Double -> m ()
normal3 x y z =
    liftIO $ glNormal3f (realToFrac x) (realToFrac y) (realToFrac z)


-- | translate (x, y, z)
translate3 :: MonadIO m => Double -> Double -> Double -> m ()
translate3 x y z = do
    liftIO $ glTranslatef (realToFrac x) (realToFrac y) (realToFrac z)


-- | rotating radians around direction
rotate3D :: MonadIO m => Double -> Double -> Double -> Double -> m ()
rotate3D rad x y z = do
    liftIO $ glRotatef (realToFrac $ rad * 57.295780) (realToFrac x) (realToFrac y) (realToFrac z)


-- | scaling (x, y, z)
scale3 :: MonadIO m => Double -> Double -> Double -> m ()
scale3 x y z = do
    liftIO $ glScalef (realToFrac x) (realToFrac y) (realToFrac z)


