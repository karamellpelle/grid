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
module Game.Run.RunWorld.Scene.Plain
  (
    Scene (..),
    makeScene,
    makeSceneEmpty,
    remakeScene,

    module Game.Data.Shape,

  ) where

import MyPrelude
import File
import System.Random

import Game
import Game.Data.Shape
import Game.Grid.GridWorld.Node

import OpenGL
import OpenGL.Helpers


data Scene =
    Scene
    {
        -- GL
        sceneFBO :: !GLuint,
        sceneShape :: !Shape,
        sceneWth :: !UInt,
        sceneHth :: !UInt,
        sceneTex :: !GLuint,
        sceneDepthStencil :: !GLuint,

        -- Noise
        sceneNoiseTick :: !Tick,
        sceneNoiseNode :: !Node,
        sceneNoiseIx :: !UInt,
        sceneNoisePitch :: !Float

        -- tweak

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
    --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR
    --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_NEAREST 
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_NEAREST
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
    tick <- randomIO >>= \uni -> return $ 
            smooth valueSoundSceneNoiseTickMin valueSoundSceneNoiseTickMax uni
    x <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
    y <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
    z <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
    ix <- randomRIO (0, valueSoundSceneNoiseSize - 1)
    pitch <- randomIO >>= \uni -> return $ 
             smooth valueSoundSceneNoisePitchMin valueSoundSceneNoisePitchMax uni 

    return Scene
           {
              sceneFBO = fbo,
              sceneShape = shapeOfSize wth hth,
              sceneWth = wth,
              sceneHth = hth,
              sceneTex = tex,
              sceneDepthStencil = depthstencil,

              sceneNoiseTick = rTF tick,
              sceneNoiseNode = Node x y z,
              sceneNoiseIx = ix,
              sceneNoisePitch = pitch
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
    --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR
    --glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_NEAREST 
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_NEAREST
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


    return scene
           {
              sceneFBO = fbo,
              sceneShape = shapeOfSize wth hth,
              sceneWth = wth,
              sceneHth = hth,
              sceneTex = tex,
              sceneDepthStencil = depthstencil
           }

  
