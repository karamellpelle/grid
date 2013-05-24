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
module MEnv.Env.ScreenObject.GLFW
  (
    ScreenInit (..),
    ScreenObject(..),

    withLoadedScreen,

  ) where

import LoadM
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL



data ScreenInit =
    ScreenInit


data ScreenObject =
    ScreenObject




--------------------------------------------------------------------------------
--  


withLoadedScreen :: ScreenInit -> (ScreenObject -> LoadM a) -> LoadM a
withLoadedScreen init handler = do

    logging "opening screen."

    init <- liftIO $ GLFW.initialize
    unless init $ loggingError "could not initialize GLFW"

    liftIO $ GLFW.openWindowHint GL.$= (GLFW.FSAASamples, 8)
    open <- liftIO $ GLFW.openWindow (GL.Size 800 600) [ GLFW.DisplayAlphaBits 32,
                                                         GLFW.DisplayDepthBits 32 ] 
                                                                        GLFW.Window 
                                                                        --GLFW.FullScreen
    unless open $ loggingError "could not open window"

    let screenobj = ScreenObject

    -- handle object
    a <- handler screenobj

    logging "closing screen."
    liftIO $ GLFW.terminate

    return a



