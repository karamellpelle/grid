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
module Game.Run.RunWorld.Scene.Fancy
  (
    Scene (..),

    makeScene,
    makeSceneEmpty,
    remakeScene,

    makeSceneProj3D,
    makeSceneProj2D,

    module Game.Data.Shape,
    module Game.Run.RunWorld.Scene.Fancy.Noise,
    module Game.Run.RunWorld.Scene.Fancy.Tweak,

  ) where

import MyPrelude
import File

import Game
import Game.Data.Shape
import Game.Grid.GridWorld.Node
import Game.Run.RunWorld.Scene.Fancy.Noise
import Game.Run.RunWorld.Scene.Fancy.Tweak
import System.Random

import OpenGL
import OpenGL.Helpers


data Scene =
    Scene
    {
        -- GL
        sceneFBO :: !GLuint,
        sceneTex :: !GLuint,
        sceneDepthStencil :: !GLuint,

        sceneWth :: !UInt,
        sceneHth :: !UInt,
        sceneShape :: !Shape,
        sceneProj3D :: !Mat4,
        sceneProj2D :: !Mat4,

        -- Noise
        sceneNoise :: !Noise,
        
        -- Tweak
        sceneTweak :: !Tweak

    }



makeSceneEmpty :: MEnv' Scene
makeSceneEmpty = 
    makeScene 1 1


-- | make a Scene from size
makeScene :: UInt -> UInt -> MEnv' Scene
makeScene wth hth = io $ do
    
    -- fbo
    fbo <- bindNewFBO 

    -- tex
    tex <- bindNewTex gl_TEXTURE_2D
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexImage2D gl_TEXTURE_2D 0 (fI gl_RGBA) (fI wth) (fI hth) 0 
                 gl_RGBA gl_UNSIGNED_BYTE nullPtr
    glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0

    -- depthstencil
    depthstencil <- bindNewRBO
    glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH24_STENCIL8_OES (fI wth) (fI hth)
    glFramebufferRenderbuffer gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER 
                              depthstencil
    glFramebufferRenderbuffer gl_FRAMEBUFFER gl_STENCIL_ATTACHMENT gl_RENDERBUFFER 
                              depthstencil

    -- check status
    status <- glCheckFramebufferStatus gl_FRAMEBUFFER
    unless (status == gl_FRAMEBUFFER_COMPLETE) $ 
        error "makeScene: could not create framebuffer!"

    -- Noise
    noise <- makeNoise

    let shape = shapeOfSize wth hth

    return Scene
           {
              sceneFBO = fbo,
              sceneTex = tex,
              sceneDepthStencil = depthstencil,

              sceneWth = wth,
              sceneHth = hth,
              sceneShape = shape,
              sceneProj3D = makeSceneProj3D (fI wth) (fI hth),
              sceneProj2D = makeSceneProj2D (shapeWth shape) (shapeHth shape),

              sceneNoise = noise,
              sceneTweak = makeTweak
           }



-- | remake Scene
remakeScene :: Scene -> UInt -> UInt -> MEnv' Scene
remakeScene scene wth hth = io $ do
    
    -- destroy
    delRBO $ sceneDepthStencil scene
    delTex $ sceneTex scene
    delFBO $ sceneFBO scene

    -- fbo
    fbo <- bindNewFBO

    -- tex
    tex <- bindNewTex gl_TEXTURE_2D
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexImage2D gl_TEXTURE_2D 0 (fI gl_RGBA) (fI wth) (fI hth) 0 
                 gl_RGBA gl_UNSIGNED_BYTE nullPtr
    glFramebufferTexture2D gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0
    
    -- depthstencil
    depthstencil <- bindNewRBO
    glRenderbufferStorage gl_RENDERBUFFER gl_DEPTH24_STENCIL8_OES (fI wth) (fI hth)
    glFramebufferRenderbuffer gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER
                              depthstencil
    glFramebufferRenderbuffer gl_FRAMEBUFFER gl_STENCIL_ATTACHMENT gl_RENDERBUFFER
                              depthstencil

    status <- glCheckFramebufferStatus gl_FRAMEBUFFER 
    unless (status == gl_FRAMEBUFFER_COMPLETE) $ 
        error "remakeScene: could not create framebuffer!"

    let shape = shapeOfSize wth hth

    return scene
           {
              sceneFBO = fbo,
              sceneTex = tex,
              sceneDepthStencil = depthstencil,

              sceneWth = wth,
              sceneHth = hth,
              sceneShape = shape,
              sceneProj3D = makeSceneProj3D (fI wth) (fI hth),
              sceneProj2D = makeSceneProj2D (shapeWth shape) (shapeHth shape)
           }



makeSceneProj3D :: Float -> Float -> Mat4 
makeSceneProj3D wth hth = 
    mat4Perspective valueFOVY (wth / hth) valueNear valueFar

    where
      valueFOVY = valuePerspectiveFOVY
      valueNear = valuePerspectiveNear
      valueFar = valuePerspectiveFar


makeSceneProj2D :: Float -> Float -> Mat4
makeSceneProj2D wth hth = 
    mat4Ortho2D 0 wth hth 0


