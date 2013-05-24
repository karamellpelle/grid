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
module Game.Run.Scene.Fancy.Present
  (
    scenePresentBegin,
    scenePresent,
    scenePresentMode,
    scenePresentPause,
    scenePresentKonami,
  
  ) where

import MyPrelude
import Game

import Game.Font
import Game.Grid
import Game.Grid.Output
import Game.Run.RunData
import Game.Run.RunWorld
import Game.Run.Scene.Fancy.Step
import Game.Run.Scene.Fancy.ShadeScene
import Game.Run.Scene.Fancy.ShadeSceneBegin
import Game.Run.Scene.Fancy.ShadeScenePause
import Game.Run.Scene.Fancy.ShadeSceneKonami
import Game.Run.Scene.Fancy.ShadeScreenshot

import OpenGL
import OpenGL.Helpers


-- | Scene presentation for beginIteration
scenePresentBegin :: Scene -> Tweak -> Tick -> MEnv' Scene
scenePresentBegin scene tweak tick = do
    gamedata <- resourceGameData
    fbo <- screenFBO

    -- output --
    io $ do
      -- bind fbo
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|.  gl_DEPTH_BUFFER_BIT .|.  gl_STENCIL_BUFFER_BIT

      -- render, using world tick
      shadeSceneBeginTweak (rundataSceneData $ gamedataRunData gamedata)
                           (rundataShadeSceneBegin $ gamedataRunData gamedata) tweak scene 
                           tick
      
      -- discard Scene FBO
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

    -- step --
    tick <- tickGet
    sceneStepBegin gamedata tick scene 





-- | default Scene presentation
scenePresent :: Scene -> Tweak -> MEnv' Scene 
scenePresent scene tweak = do
    gamedata <- resourceGameData
    fbo <- screenFBO


    -- output --
    io $ do

      -- FBO --
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|.  gl_DEPTH_BUFFER_BIT .|.  gl_STENCIL_BUFFER_BIT

      -- render --
      shadeSceneTweak (rundataSceneData $ gamedataRunData gamedata)
                      (rundataShadeScene $ gamedataRunData gamedata) tweak scene

      -- discard FBO --
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

      
    -- step --
    tick <- tickGet
    sceneStep gamedata tick scene 


-- | default Scene presentation
scenePresentMode :: Scene -> Tweak -> MEnv' Scene 
scenePresentMode scene tweak = do
    gamedata <- resourceGameData
    fbo <- screenFBO


    -- output --
    io $ do
      -- FBO --
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|.  gl_DEPTH_BUFFER_BIT .|.  gl_STENCIL_BUFFER_BIT

      -- render --
      shadeSceneTweak (rundataSceneData $ gamedataRunData gamedata)
                      (rundataShadeScene $ gamedataRunData gamedata) tweak scene

      -- draw Corners
      let sh = griddataShadeGeneral $ gamedataGridData gamedata
          cornerdata = rundataCornerData $ gamedataRunData gamedata
      shadeGeneral sh 1.0 $ sceneProj2D scene
     
      glActiveTexture gl_TEXTURE0
      glBindTexture gl_TEXTURE_2D $ cornerdataTex cornerdata

      -- draw
      glDisable gl_CULL_FACE
      glBindVertexArrayOES $ cornerdataVAO2D cornerdata
      glDrawArrays gl_TRIANGLE_STRIP 0 24
      glEnable gl_CULL_FACE


      -- discard FBO --
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

      
    -- step --
    tick <- tickGet
    sceneStep gamedata tick scene 




-- | Scene presentation for pauseIteration
scenePresentPause :: Scene -> Tweak -> Float -> MEnv' Scene
scenePresentPause scene tweak fade = do
    gamedata <- resourceGameData
    fbo <- screenFBO

    -- output --
    io $ do
      -- bind fbo
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|.  gl_DEPTH_BUFFER_BIT .|.  gl_STENCIL_BUFFER_BIT

      -- render
      shadeScenePauseTweak (rundataSceneData $ gamedataRunData gamedata)
                           (rundataShadeScenePause $ gamedataRunData gamedata) tweak scene
      
      -- "pause"
      let fsh = gamedataFontShade gamedata
          ffd = gamedataFontData gamedata
          Shape wth hth = sceneShape scene
      fontShade fsh fade $ mat4Ortho 0 wth hth 0 (-1) 1
      glDisable gl_CULL_FACE
      fontDrawDefault fsh ffd (0.2 * hth) valueTextFontBColor
      fontDraw2DCentered fsh ffd (0.5 * wth) (0.5 * hth) "pause"
      glEnable gl_CULL_FACE

      -- discard Scene FBO
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

    -- step --
    tick <- tickGet 
    sceneStepPause gamedata tick scene 


-- | Scene presentation for konamiIteration
scenePresentKonami :: Scene -> Tweak -> RunWorld -> MEnv' Scene
scenePresentKonami scene tweak run = do
    gamedata <- resourceGameData
    fbo <- screenFBO

    -- output --
    io $ do
      -- bind fbo
      glBindFramebuffer gl_FRAMEBUFFER fbo
      glClear $ gl_COLOR_BUFFER_BIT .|.  gl_DEPTH_BUFFER_BIT .|.  gl_STENCIL_BUFFER_BIT

      -- render
      shadeSceneKonamiTweak (rundataSceneData $ gamedataRunData gamedata)
                            (rundataShadeSceneKonami $ gamedataRunData gamedata) tweak 
                            run scene
      
      -- discard Scene FBO
      glBindFramebuffer gl_FRAMEBUFFER $ sceneFBO scene
      discardFramebuffer gl_FRAMEBUFFER [ gl_COLOR_ATTACHMENT0,
                                          gl_DEPTH_ATTACHMENT,
                                          gl_STENCIL_ATTACHMENT ]

    -- step --
    tick <- tickGet
    sceneStepKonami gamedata tick scene 

