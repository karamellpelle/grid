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
module Game.Run.Helpers.Fancy
  (
    runBeginScreen,
    
    screenshotLevelPuzzle,
    screenshotMemory,
    screenshotForeign,

    runTweakClear,
    runTweakPulse0Push,

  ) where

import MyPrelude
import Game

import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.Scene.Fancy.ShadeScreenshot
import Game.Run.Output.Fancy.ShadeCube
import Game.Run.Output.Fancy.CornerData
import Game.Grid.Output.Fancy.ShadeSpace
import Game.Grid.Output.Fancy.ShadeGeneral

import OpenGL
import OpenGL.Helpers



-- | setup screen for iteration
runBeginScreen :: RunWorld -> MEnv' RunWorld
runBeginScreen run = do

    griddata <- resourceGridData
    rundata <- resourceRunData
  
    -- handle screen size change
    (wth, hth) <- screenSize
    if (wth == sceneWth (runScene run) && hth == sceneHth (runScene run))
      then do
          return run
      else do

        -- change Scene
        scene' <- remakeScene (runScene run) wth hth

        io $ do
            -- change GL
            glViewport 0 0 (fI wth) (fI hth)

            -- change Cube
            shadeCubeWrite (rundataShadeCube rundata) $ sceneShape scene'

            -- change Space
            shadeSpaceWrite (griddataShadeSpace griddata) $ sceneShape scene'

            -- change Corners
            cornerdataWrite2D (rundataCornerData rundata) $ sceneShape scene'
            
        return $ run { runScene = scene' }




--------------------------------------------------------------------------------
--  screenshot


-- | Scene to texture
sceneScreenshot :: Scene -> GLenum -> GLuint -> UInt -> UInt -> MEnv' ()
sceneScreenshot scene target tex wth hth = do
    rundata <- resourceRunData
    io $ do
        shadeScreenshot (rundataShadeScreenshot rundata)
                         1.0 (sceneTex scene) (sceneShape scene) target tex wth hth
        -- reset viewport
        glViewport 0 0 (fI $ sceneWth scene) (fI $ sceneHth scene)


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


--------------------------------------------------------------------------------
--  Tweak

runModifyTweak :: RunWorld -> (Tweak -> Tweak) -> RunWorld
runModifyTweak run f =
    let scene = runScene run
        tweak = sceneTweak scene
    in  run { runScene = scene { sceneTweak = f tweak } }


runTweakClear :: RunWorld -> RunWorld
runTweakClear run = 
    runModifyTweak run $ \tweak -> tweak
        {
            tweakForAcc = mempty,
            tweakForAccAlpha = 0.0,
            tweakForVel = mempty,
            tweakForVelAlpha = 0.0,
            tweakForPos = mempty,
            tweakForPosAlpha = 0.0,
            tweakRotAcc = mempty,
            tweakRotAccAlpha = 0.0,
            tweakRotVel = mempty,
            tweakRotVelAlpha = 0.0,
            tweakRotPos = mempty,
            tweakRotPosAlpha = 0.0,
            tweakOsc0Acc = 0.0,
            tweakOsc0Vel = 0.0,
            tweakOsc0Pos = 0.0,
            tweakPulse0Vel = 0.0,
            tweakPulse0Pos = 0.0

        }

-- | push a pulse, typically values [0, 1]
runTweakPulse0Push :: RunWorld -> Float -> RunWorld
runTweakPulse0Push run pulse = 
    runModifyTweak run $ \tweak -> tweak { tweakPulse0Vel = 5.0 * pulse }

