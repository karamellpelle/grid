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
module Game.Run.Helpers.Plain
  (
    runBeginScreen,

    screenshotLevelPuzzle,
    screenshotMemory,
    screenshotForeign,

  ) where

import MyPrelude
import Game
import Game.Run.RunWorld
import Game.Run.RunData
import Game.Run.Output.Plain.ShadeCube
import Game.Run.Scene.Plain
import Game.Run.Scene.Plain.ShadeCorners

import OpenGL
import OpenGL.Helpers



-- | setup screen for iteration
runBeginScreen :: RunWorld -> MEnv' RunWorld
runBeginScreen run = do
    rundata <- resourceRunData

    -- handle screen size change
    (wth, hth) <- screenSize
    if (wth == sceneWth (runScene run) && hth == sceneHth (runScene run))
      then do
          return run
      else do

        -- change Scene
        scene' <- remakeScene (runScene run) wth hth
      
        case sceneShape scene' of
            Shape wth' hth' -> io $ do
                -- change Cube
                shadeCubeWrite (rundataShadeCube rundata) wth' hth'
                
                -- change Corners
                shadeCornersWrite2D (rundataShadeCorners rundata) wth' hth'

        return $ run { runScene = scene' }




--------------------------------------------------------------------------------
--  screenshot

screenshotLevelPuzzle :: RunWorld -> MEnv' RunWorld
screenshotLevelPuzzle run = do
    sceneScreenshot (runScene run) gl_TEXTURE_2D 
                    (screenshotTex $ runLevelPuzzleScreenshot run)
                    (screenshotWth $ runLevelPuzzleScreenshot run)
                    (screenshotHth $ runLevelPuzzleScreenshot run)
    --glGenerateMipmap gl_TEXTURE_2D
    return run


screenshotMemory :: RunWorld -> MEnv' RunWorld
screenshotMemory run = do
    sceneScreenshot (runScene run) gl_TEXTURE_2D
                    (screenshotTex $ runMemoryScreenshot run)
                    (screenshotWth $ runMemoryScreenshot run)
                    (screenshotHth $ runMemoryScreenshot run)
    --glGenerateMipmap gl_TEXTURE_2D
    return run


screenshotForeign :: RunWorld -> MEnv' RunWorld
screenshotForeign run = io $ do
    foreign_screenshot gl_TEXTURE_2D 
                       (screenshotTex $ runForeignScreenshot run)
                       (fI $ screenshotWth $ runForeignScreenshot run) 
                       (fI $ screenshotHth $ runForeignScreenshot run)
    --glGenerateMipmap gl_TEXTURE_2D
    return run                                              


foreign import ccall unsafe "foreign_screenshot" foreign_screenshot
    :: GLenum -> GLuint -> CUInt -> CUInt -> IO ()
