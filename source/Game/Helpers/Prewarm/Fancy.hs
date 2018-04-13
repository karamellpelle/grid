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
module Game.Helpers.Prewarm.Fancy
  (
    prewarmShaders,

  ) where

import MyPrelude
import Game.MEnv

#ifdef GRID_PLATFORM_IOS

prewarmShaders :: MEnv' ()
prewarmShaders = 
    return ()


{-
prewarmGUIShade :: MEnv' ()
prewarmGUIShade = do
    glActiveTexture gl_TEXTURE0 -- fixme: necessary ?
    glBindTexture gl_TEXTURE_2D 0
    glBindFramebuffer gl_FRAMEBUFFER fbo
    glClear $ gl_COLOR_BUFFER_BIT .|. 
              gl_DEPTH_BUFFER_BIT .|. 
              gl_STENCIL_BUFFER_BIT
    (wth, hth) <- iosSize
    glViewport 0 0 (fI wth) (fI hth)
      
    --glEnable gl_DEPTH_TEST
    glDisable gl_CULL_FACE
    glUseProgram $ shadeGUIPrg sh
    uniformMat4 (shadeGUIUniProjModvMatrix sh) $ mat4Ortho2D 0 (fI wth) (fI hth) 0
    -- pos
    glUniform2f (shadeGUIUniPos sh) 0 0
    -- scale
    glUniform2f (shadeGUIUniScale sh) 1 1
    -- alpha
    glUniform1f (shadeGUIUniAlpha sh) 1.0
    -- depth 

-}
#endif

#ifdef GRID_PLATFORM_GLFW

prewarmShaders :: MEnv' ()
prewarmShaders = 
    return ()

#endif

