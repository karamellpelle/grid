--------------------------------------------------------------------------------
-- | 
-- Module      : Graphics.Rendering.OpenGLHelper.OpenGL2D
-- 
-- helper functions for 2D (xy-plane)
-- 
--------------------------------------------------------------------------------

module OpenGL.Helpers.GL2D
  (
    as2DView, -- fixme#
    as2DViewYDown, -- fixme!
    inside2DClip,

    inPlace,

    vertex2,
    translate2,
    rotate2D,
    scale2,
    
  ) where

import Control.Monad.Trans
import Graphics.Rendering.OpenGL.Raw
import Foreign.Marshal.Array

-- tmp:
import OpenGL as GL



-- | set up projection and modelview for 2D drawing,
--   with screen size as described by 'wth' and 'hth'
--as2DView :: MonadIO m => Double -> Double -> m a -> m a
as2DView wth hth ma = do
    -- begin
    liftIO $ glMatrixMode gl_PROJECTION
    liftIO $ glPushMatrix
    liftIO $ glLoadIdentity
    liftIO $ glOrtho 0 (realToFrac wth) 0 (realToFrac hth) (-1) 1
    liftIO $ glMatrixMode gl_MODELVIEW
    liftIO $ glPushMatrix
    liftIO $ glLoadIdentity
    liftIO $ glTranslatef 0 0 (-1)    -- 2D always on top of 3D

    liftIO $ glDisable gl_DEPTH_TEST
    liftIO $ glDisable gl_LIGHTING

    a <- ma

    -- end
    liftIO $ glMatrixMode gl_MODELVIEW
    liftIO $ glPopMatrix
    liftIO $ glMatrixMode gl_PROJECTION
    liftIO $ glPopMatrix
    liftIO $ glMatrixMode gl_MODELVIEW

    return a


-- | set up projection and modelview for 2D drawing,
--   with screen size as described by 'wth' and 'hth'
--as2DView :: MonadIO m => Double -> Double -> m a -> m a
as2DViewYDown wth hth ma = do



    -- begin
    liftIO $ glMatrixMode gl_PROJECTION
    liftIO $ glPushMatrix
    liftIO $ glLoadIdentity
    liftIO $ glOrtho 0 (realToFrac wth) (realToFrac hth) 0 (-1) 1
    liftIO $ glMatrixMode gl_MODELVIEW
    liftIO $ glPushMatrix
    liftIO $ glLoadIdentity
    liftIO $ glTranslatef 0 0 (-1) -- 2D always on top of 3D


    --face <- liftIO $ glGet gl_CULL_FACE_MODE -- no glGet?
    --liftIO $ glCullFace gl_FRONT      -- it seems that cull depends on projection matrix too, hence switch
    isCull <- liftIO $ glIsEnabled gl_CULL_FACE
    liftIO $ glDisable gl_CULL_FACE
    liftIO $ glDisable gl_DEPTH_TEST
    liftIO $ glDisable gl_LIGHTING

    a <- ma

    
    -- end
    --liftIO $ glCullFace face
    liftIO $ if True == (toEnum $ fromEnum isCull) then (glEnable gl_CULL_FACE) else return ()

    liftIO $ glMatrixMode gl_MODELVIEW
    liftIO $ glPopMatrix
    liftIO $ glMatrixMode gl_PROJECTION
    liftIO $ glPopMatrix
    liftIO $ glMatrixMode gl_MODELVIEW

    return a


inside2DClip :: MonadIO m => Double -> Double -> Double -> Double -> m a -> m a
inside2DClip x y wth hth ma = do
    let x'   = realToFrac x
        y'   = realToFrac y
        wth' = realToFrac wth
        hth' = realToFrac hth
    liftIO $ do -- fixme: do not allocate/deallocate in each iteration!
        withArray [1, 0, 0, x'] $ \ptr -> 
            glClipPlane gl_CLIP_PLANE0 ptr
        withArray [-1, 0, 0, x' + wth'] $ \ptr -> 
            glClipPlane gl_CLIP_PLANE1 ptr
        withArray [0, 1, 0, y'] $ \ptr -> 
            glClipPlane gl_CLIP_PLANE2 ptr
        withArray [0, -1, 0, y' + hth'] $ \ptr -> 
            glClipPlane gl_CLIP_PLANE3 ptr
        
        glEnable gl_CLIP_PLANE0
        glEnable gl_CLIP_PLANE1
        glEnable gl_CLIP_PLANE2
        glEnable gl_CLIP_PLANE3

    a <- ma

    liftIO $ do
        glDisable gl_CLIP_PLANE3
        glDisable gl_CLIP_PLANE2
        glDisable gl_CLIP_PLANE1
        glDisable gl_CLIP_PLANE0

    return a



-- | let 'ma' be performed in rectangle (x, y, w, h)
inPlace :: MonadIO m => Double -> Double -> Double -> Double -> m a -> m a
inPlace x y w h ma = do
    liftIO $ do
        glMatrixMode gl_MODELVIEW
        glPushMatrix 
        glTranslated (realToFrac x) (realToFrac y) 0 
        --glScaled (realToFrac $ 1/w) (realToFrac $ 1/h) 1

    a <- ma

    liftIO $ do 
        glMatrixMode gl_MODELVIEW
        glPopMatrix

    return a

{-
myPush :: MonadIO m => Double -> Double -> Double -> Double  -> m ()
myPush x y w h =
    liftIO $ do
        matrixMode $= Modelview 0
        translate2 x y
        scale2 w h

myPop :: MonadIO m => Double -> Double -> Double -> Double -> m ()
myPop x y w h =
    liftIO $ do
        matrixMode $= Modelview 0
        scale2 (1 / w) (1 / h)
        translate2 (-x) (-y)
-}

-- | vertex at (x, y, 0)
vertex2 :: MonadIO m => Double -> Double -> m ()
vertex2 x y = do
    liftIO $ glVertex3f (realToFrac x) (realToFrac y) 0


-- | translate (x, y, 0)
translate2 :: MonadIO m => Double -> Double -> m ()
translate2 x y = do
    liftIO $ glTranslatef (realToFrac x) (realToFrac y) 0


-- | rotating radians in positive direction
rotate2D :: MonadIO m => Double -> m ()
rotate2D rad = do
    liftIO $ glRotatef (realToFrac $ rad * 57.295780) 0 0 1


-- | scaling (x, y, 1)
scale2 :: MonadIO m => Double -> Double -> m ()
scale2 x y = do
    liftIO $ glScalef (realToFrac x) (realToFrac y) 1


