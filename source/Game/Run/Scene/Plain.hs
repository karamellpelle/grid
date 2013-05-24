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
module Game.Run.Scene.Plain
  (
    sceneBegin,
    scenePresent,
    scenePresentBegin,
    scenePresentPause,
    scenePresentKonami,

    sceneHandleEscape,
    sceneScreenshot,

  ) where

import MyPrelude
import Game

import Game.Font
import Game.Grid
import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.Scene.Plain.ShadeScene
import Game.Run.Scene.Plain.ShadeSceneBegin
import Game.Run.Scene.Plain.ShadeScenePause
import Game.Run.Scene.Plain.ShadeSceneKonami
import Game.Run.Scene.Plain.ShadeScreenshot
import Game.Run.Scene.Plain.ShadeCorners
import Game.Run.Scene.Plain.SoundScene
import System.Random

import OpenGL
import OpenGL.Helpers

--------------------------------------------------------------------------------
--  begin Scene


sceneBegin :: Scene -> MEnv' Scene
sceneBegin scene = io $ do

    -- todo: save touches

    -- render to sceneTex
    glBindTexture gl_TEXTURE_2D 0
    glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
    glClear $ gl_COLOR_BUFFER_BIT .|. 
              gl_DEPTH_BUFFER_BIT .|. 
              gl_STENCIL_BUFFER_BIT
    glViewport 0 0 (fI $ sceneWth scene) (fI $ sceneHth scene)

    return scene
                


--------------------------------------------------------------------------------
--  present Scene


-- | default Scene output
scenePresent :: Scene -> MEnv' Scene 
scenePresent scene = do
    gamedata <- resourceGameData
    fbo <- screenFBO
    tick <- tickGet

    io $ do
      -- output --
      -- set fbo
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|. -- note: on iOS, not clearing caused huge
                gl_DEPTH_BUFFER_BIT .|. --       performance penality
                gl_STENCIL_BUFFER_BIT
      glViewport 0 0 (fI $ sceneWth scene) (fI $ sceneHth scene)

      -- render
      let projmodv = mat4Ortho2D (-1) 1 (-1) 1 
      shadeScene (rundataShadeScene $ gamedataRunData gamedata) 
                 1.0 projmodv scene

      -- discard Scene FBO
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

      -- noise
      when (sceneNoiseTick scene <= tick) $ do
          soundSceneNoise  (rundataSoundScene $ gamedataRunData gamedata)
                           (sceneNoiseIx scene) (sceneNoisePitch scene) 
                           (sceneNoiseNode scene)
      
      -- step --
      stepScene gamedata tick scene




-- | Scene output for beginIteration
scenePresentBegin :: Scene -> Float -> MEnv' Scene
scenePresentBegin scene tweak = do
    gamedata <- resourceGameData
    fbo <- screenFBO
    tick <- tickGet

    io $ do
      -- output --
      -- bind fbo
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|. -- note: on iOS, not clearing caused huge
                gl_DEPTH_BUFFER_BIT .|. --       performance penality
                gl_STENCIL_BUFFER_BIT
      glViewport 0 0 (fI $ sceneWth scene) (fI $ sceneHth scene)

      -- render
      let projmodv = mat4Ortho (-1) 1 (-1) 1 (-1) 1
      shadeSceneBegin (rundataShadeSceneBegin $ gamedataRunData gamedata) 
                      1.0 projmodv tweak scene
      
      -- discard Scene FBO
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

      -- step --
      stepScene gamedata tick scene 



-- | Scene output for pauseIteration
scenePresentPause :: Scene -> Float -> MEnv' Scene
scenePresentPause scene tweak = do
    gamedata <- resourceGameData
    fbo <- screenFBO
    tick <- tickGet 


    io $ do
      -- output --
      -- bind fbo
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|. 
                gl_DEPTH_BUFFER_BIT .|.
                gl_STENCIL_BUFFER_BIT
      glViewport 0 0 (fI $ sceneWth scene) (fI $ sceneHth scene)

      -- render
      let projmodv = mat4Ortho (-1) 1 (-1) 1 (-1) 1
      shadeScenePause (rundataShadeScenePause $ gamedataRunData gamedata) 
                      1.0 projmodv tweak scene
      
      -- discard Scene FBO
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

      -- "pause"
      let fsh = gamedataFontShade gamedata
          ffd = gamedataFontData gamedata
          Shape wth hth = sceneShape scene
      fontShade fsh tweak $ mat4Ortho 0 wth hth 0 (-1) 1
      glDisable gl_CULL_FACE
      fontDrawDefault fsh ffd valueTextFontCSize valueTextFontCColor
      fontDraw2DCentered fsh ffd (wth * 0.5) (hth * 0.5) "pause"
      glEnable gl_CULL_FACE

      -- step --
      stepScene gamedata tick scene



-- | Scene output for konamiIteration
scenePresentKonami :: Scene -> Float -> MEnv' Scene
scenePresentKonami scene tweak = do
    gamedata <- resourceGameData
    fbo <- screenFBO
    tick <- tickGet

    io $ do
      -- output --
      -- bind fbo
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|. 
                gl_DEPTH_BUFFER_BIT .|.
                gl_STENCIL_BUFFER_BIT
      glViewport 0 0 (fI $ sceneWth scene) (fI $ sceneHth scene)

      -- render
      let projmodv = mat4Ortho (-1) 1 (-1) 1 (-1) 1
      shadeSceneKonami (rundataShadeSceneKonami $ gamedataRunData gamedata) 
                       1.0 projmodv tweak scene
      
      -- discard Scene FBO
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

      -- step --
      stepScene gamedata tick scene



--------------------------------------------------------------------------------
--  escaping from Scene

sceneHandleEscape :: Scene -> MEnv' a -> MEnv' a -> MEnv' a
sceneHandleEscape scene ma ma' = do
    join $ keysTouchHandlePointTouched 
        (case sceneShape scene of
            Shape wth hth -> do
                rundata <- resourceRunData
                io $ shadeCorners2D (rundataShadeCorners rundata) 
                                    1.0 (mat4Ortho2D 0 wth hth 0)
                ma  
        )
        (\(x, y) -> case sceneShape scene of
            Shape wth hth -> if inTriangle size size x y                  || 
                                inTriangle size size (wth - x) y          || 
                                inTriangle size size (wth - x) (hth - y)  ||
                                inTriangle size size x (hth - y)
                             then ma'
                             else do
                                rundata <- resourceRunData
                                io $ shadeCorners2D (rundataShadeCorners rundata) 
                                                    1.0 (mat4Ortho2D 0 wth hth 0)
                                ma
        )

    where
      size = valueSceneCornerSize
      inTriangle :: Float -> Float -> Float -> Float -> Bool
      inTriangle r s x y =
          r * y + s * x < s * r
        

--------------------------------------------------------------------------------
--  screenshot of Scene


-- | Scene to texture
sceneScreenshot :: Scene -> GLenum -> GLuint -> UInt -> UInt -> MEnv' ()
sceneScreenshot scene target tex wth hth = do
    rundata <- resourceRunData
    io $ shadeScreenshot (rundataShadeScreenshot rundata)
                         1.0 (sceneTex scene) (sceneShape scene) target tex wth hth
                        


--------------------------------------------------------------------------------
--  step Scene

-- | step scene
stepScene :: GameData -> Tick -> Scene -> IO Scene
stepScene gamedata tick scene = do

    -- noise
    stepNoise gamedata tick scene


stepNoise :: GameData -> Tick -> Scene -> IO Scene
stepNoise gamedata tick scene = 
    if sceneNoiseTick scene <= tick
      -- schedule next noise
      then do
        dtick <- randomIO >>= \uni -> return $ 
                 smooth valueSoundSceneNoiseTickMin valueSoundSceneNoiseTickMax uni
        x <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
        y <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
        z <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
        ix <- randomRIO (0, valueSoundSceneNoiseSize - 1)
        pitch <- randomIO >>= \uni -> return $ 
                 smooth valueSoundSceneNoisePitchMin valueSoundSceneNoisePitchMax uni
        return scene
               {
                  sceneNoiseTick = tick + rTF dtick,
                  sceneNoiseNode = Node x y z,
                  sceneNoiseIx = ix,
                  sceneNoisePitch = pitch
               }
      else do
        return scene

