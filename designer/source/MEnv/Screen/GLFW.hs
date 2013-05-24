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
module MEnv.Screen.GLFW
  (
    screenBegin,
    screenEnd,
    screenSize,
    screenShape,


  ) where

import MEnv
import Control.Monad.Trans

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL



-- | before iteration
screenBegin :: MEnv res ()
screenBegin = do
    return ()


-- | after iteration
screenEnd :: MEnv res ()
screenEnd = do
    liftIO $ do
        GLFW.swapBuffers


-- | current screen size
screenSize :: MEnv res (Int, Int)
screenSize =
    liftIO $ GL.get GLFW.windowSize >>= \(GL.Size wth hth) -> 
             return (fromIntegral wth, fromIntegral hth)


screenShape :: MEnv res (Float, Float)
screenShape =
    screenSize >>= \(wth, hth) ->
        let scale  = 1 / (fromIntegral $ max wth hth)
        in return (scale * fromIntegral wth, scale * fromIntegral hth)
