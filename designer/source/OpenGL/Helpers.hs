{-# LANGUAGE ForeignFunctionInterface #-}
module OpenGL.Helpers 
  (
    module Linear,
    localMatrix,
    statevarLocal,
    enableLocal,
    disableLocal,

    setProjModV,
    setModelView,
    setProjection,
    color4,
    multMat4,

    rotateX,
    rotateY,
    rotateZ,

    scaleXYZ,

    module OpenGL.Helpers.GL1D,
    module OpenGL.Helpers.GL2D,
    module OpenGL.Helpers.GL3D,


  ) where

import MyPrelude
import Linear

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw

import OpenGL.Helpers.GL1D
import OpenGL.Helpers.GL2D
import OpenGL.Helpers.GL3D

import Control.Monad.Trans


import Foreign.Marshal.Array

setProjModV :: Mat4 -> Mat4 -> IO ()
setProjModV proj modv = do
    glMatrixMode gl_PROJECTION
    glLoadIdentity
    multMat4 proj
    glMatrixMode gl_MODELVIEW
    glLoadIdentity
    multMat4 modv

localMatrix :: MonadIO m => m a -> m a
localMatrix ma = do
    liftIO $ glPushMatrix
    a <- ma
    liftIO $ glPopMatrix
    return a
    
    
-- todo: only rely on OpenGLRaw. create
-- localMatrix, localPrimitive

-- | set 'var' to 'v' in 'ma', else unchanged
statevarLocal :: MonadIO m => a -> GL.StateVar a -> m b -> m b
statevarLocal v' var mb = do
    v <- liftIO $ GL.get var
    liftIO $ var $= v
    b <- mb
    liftIO $ var $= v
    return b


-- | set enabled in 'ma', else unchanged
enableLocal :: MonadIO m => GL.StateVar GL.Capability -> m a -> m a
enableLocal var =
    statevarLocal GL.Enabled var


-- | set disabled in 'ma', else unchanged
disableLocal :: MonadIO m => GL.StateVar GL.Capability -> m a -> m a
disableLocal var =
    statevarLocal GL.Disabled var



-- | sets ModelView matrix
setModelView :: MonadIO m => m ()
setModelView = do
    liftIO $ do
        GL.matrixMode $= GL.Modelview 0


-- | sets Projection matrix
setProjection :: MonadIO m => m ()
setProjection = do
    liftIO $ do
        GL.matrixMode $= GL.Projection


-- | set current rgba color
color4 :: MonadIO m => GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> m ()
color4 r g b a = do
    liftIO $ GL.currentColor $= GL.Color4 r g b a 


-- | rotating radians around x-axis
rotateX :: MonadIO m => Double -> m ()
rotateX rad = liftIO $ 
    GL.rotate (rad * 57.295780) $ GL.Vector3 1 0 0


-- | rotating radians around y-axis
rotateY :: MonadIO m => Double -> m ()
rotateY rad = liftIO $ 
    GL.rotate (rad * 57.295780) $ GL.Vector3 0 1 0


-- | rotating radians around z-axis
rotateZ :: MonadIO m => Double -> m ()
rotateZ rad = liftIO $ 
    GL.rotate (rad * 57.295780) $ GL.Vector3 0 0 1


-- | scales x,y,z with value
scaleXYZ :: MonadIO m => Double -> m ()
scaleXYZ v = 
    liftIO $ GL.scale v v v

 

multMat4 :: Mat4 -> IO ()
multMat4 mat = 
    let Mat4 x0 x1 x2 x3
             y0 y1 y2 y3
             z0 z1 z2 z3
             w0 w1 w2 w3 = mat
    in withArray [c x0, c x1, c x2, c x3,
                  c y0, c y1, c y2, c y3,
                  c z0, c z1, c z2, c z3,
                  c w0, c w1, c w2, c w3] $ \ptr -> glMultMatrixd ptr
    where
        c = realToFrac


