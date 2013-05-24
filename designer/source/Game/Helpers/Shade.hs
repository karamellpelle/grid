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
module Game.Helpers.Shade
  (
    shade3D,
    shade3DModelview,
    shade3DAlpha,

    shade2D,
    shade2D',
    shade2DModelview,
    shade2DAlpha,

    shadeFont,
    shadeFontColor,
    shadeFontGlow,
    shadeFontGlowColor,

  ) where

import MyPrelude
import Game.MEnv
import OpenGL
import OpenGL.Helpers
import Linear



--------------------------------------------------------------------------------
--  shadePhyObj

-- | draw physical objects
shade3D :: Mat4 -> Mat4 -> MEnv' ()
shade3D projection modelview = do
    -- let normal = compute normal matrix
    -- fixme: set uniforms

-- tmp
    io $ do
      glMatrixMode gl_PROJECTION
      glLoadIdentity
      multMat4 projection
      glMatrixMode gl_MODELVIEW
      glLoadIdentity
      multMat4 modelview
      
      -- enable depth
      glEnable gl_DEPTH_TEST

      -- set front facing
      glFrontFace gl_CCW
--
    return ()


shade3DModelview :: Mat4 -> MEnv' ()
shade3DModelview mat = do
-- tmp
    io $ do
        glMatrixMode gl_MODELVIEW
        glLoadIdentity
        multMat4 mat

    return ()


shade3DAlpha :: Float -> MEnv' ()
shade3DAlpha alpha = 
    -- fragment factor = 1 1 1 alpha
    return ()



--------------------------------------------------------------------------------
--  shade2D


-- | draw 2D with units (wth, hth) from upper left
shade2D :: Float -> Float -> IO (Mat4, Mat4)
shade2D wth hth = do
    let projection = mat4Ortho 0 wth hth 0 (-1) 1
        modelview = mempty

    shade2D' projection modelview

    return (projection, modelview)


shade2D' :: Mat4 -> Mat4 -> IO ()
shade2D' projection modelview = do
    -- set uniforms

-- tmp
    io $ do
      glMatrixMode gl_PROJECTION
      glLoadIdentity
      multMat4 projection
      glMatrixMode gl_MODELVIEW
      glLoadIdentity
      multMat4 modelview
      
      -- disable depth
      glDisable gl_DEPTH_TEST

      -- set front facing
      glFrontFace gl_CW
--


shade2DModelview :: Mat4 -> IO ()
shade2DModelview mat = do
-- tmp
    io $ do
        glMatrixMode gl_MODELVIEW
        glLoadIdentity
        multMat4 mat

    return ()


shade2DAlpha :: Float -> MEnv' ()
shade2DAlpha alpha =
    -- set fragment factor = 1 1 1 alpha
    return ()


--------------------------------------------------------------------------------
--  shadeFont

shadeFont :: Mat4 -> Mat4 -> MEnv' ()
shadeFont projection modelview = do
    -- fixme: set uniforms
-- tmp
    io $ do
      glMatrixMode gl_PROJECTION
      glLoadIdentity
      multMat4 projection
      glMatrixMode gl_MODELVIEW
      glLoadIdentity
      multMat4 modelview 
--



shadeFontColor :: Float -> Float -> Float -> Float -> MEnv' ()
shadeFontColor r g b a = do
-- tmp
    io $ glColor4f (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
--


shadeFontGlow :: Bool -> MEnv' ()
shadeFontGlow glow =
    undefined


shadeFontGlowColor :: Float -> Float -> Float -> Float -> MEnv' ()
shadeFontGlowColor r g b a = do
-- tmp
    undefined
--


