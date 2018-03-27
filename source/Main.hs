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
module Main
  (
    main,

  ) where

import MyPrelude
import File
import Game

import Game
import Game.Run
import Game.Run.Helpers.Make
import Game.Run.Iteration

import OpenGL
import OpenGL.Helpers
import OpenAL
import OpenAL.Helpers


#ifdef DEBUG
import Foreign
#endif

main :: IO ()
main = do

#ifdef DEBUG
    -- we assume the following bitsizes in our code. 
    -- otherwise, the program will probably fail...
    assert (sizeOf (undefined :: GLubyte) == 1)   $ "sizeof GLubyte == 1"
    assert (sizeOf (undefined :: GLbyte) == 1)    $ "sizeof GLbyte == 1"
    assert (sizeOf (undefined :: GLushort) == 2)  $ "sizeof GLushort == 2"
    assert (sizeOf (undefined :: GLshort) == 2)   $ "sizeof GLshort == 2"
    assert (sizeOf (undefined :: GLfloat) == 4)   $ "sizeof GLfloat == 4"
#endif



    -- define MEnv
    let init = Init
               {
#ifdef GRID_PLATFORM_IOS
                  initScreenOrientations = [  OrientationLandscapeLeft, 
                                              OrientationLandscapeRight ],
                  initScreenMultisample = 4,
                  initScreenRate = 0,
                  initSoundSampleRate = 22050,
                  initKeysAcclGyroRate = 0.1
#endif
#ifdef GRID_PLATFORM_GLFW
                  initScreenMultisample = 0,  -- let GLFW choose best (??)
                  initScreenFullscreen = False
#endif
               }
   

    -- run MEnv!
    -- loadGameData creates the real RunWorld; not the emtpy below
    let a = ()
#ifdef GRID_PLATFORM_IOS
    c <- runMEnvIOS init loadGameData unloadGameData 
                    begin iterate end a
#endif
#ifdef GRID_PLATFORM_GLFW
    c <- runMEnvGLFW init loadGameData unloadGameData 
                     begin iterate end a
#endif
    
    return ()

    where
      begin _ = do
          
          -- setup OpenGL and OpenAL
          io $ do
              -- OpenGL --
              -- (see readme/invariants.txt for GL state)
              glClearColor 0 0 0 0
              glDisable gl_STENCIL_TEST
              glClearStencil 0
              
              -- lets use premultiplied colors to represent colors, as default
              glEnable gl_BLEND
              glBlendEquationSeparate gl_FUNC_ADD 
                                      gl_FUNC_ADD
              glBlendFuncSeparate gl_ONE gl_ONE_MINUS_SRC_ALPHA
                                  gl_ONE gl_ONE_MINUS_SRC_ALPHA
              
              glDepthMask gl_TRUE
              glDepthFunc gl_LEQUAL
              glEnable gl_DEPTH_TEST
              glDisable gl_DITHER -- ??

              -- OpenAL --
              alDistanceModel al_INVERSE_DISTANCE
              -- doppler, speed of sound, ...


          -- we want to play this game with a local player (if possible)
          playersAuthenticateLocalPlayer

          -- if first run, create folders and files for dynamic data
          createDynamicData

          -- load the RunWorld not assocciated with any local player ("empty")
          path <- fileRunWorldEmpty
          run <- loadRunWorld path
         
          -- play game
          return (run, (), [iterationBegin])


      iterate (a, b, stack) = do
          iterateABStack a b stack

      end (run, b, stack) = do
          saveRunWorld run



