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
module Game.Run.RunWorld.Scene.Fancy.Noise
  (
    Noise (..),
    makeNoise,

  ) where

import MyPrelude
import File
import System.Random

import Game
import Game.Grid
import Game.Run.RunData
import Game.Data.Shape

import OpenGL
import OpenGL.Helpers


data Noise =
    Noise 
    {
        noiseTick :: !Tick,
        noiseNode :: !Node,
        noiseIx :: !UInt,
        noisePitch :: !Float
    }




-- | create Noise 
makeNoise :: IO Noise
makeNoise = do
    
    tick <- randomIO >>= \uni -> return $ 
            smooth valueSoundSceneNoiseTickMin valueSoundSceneNoiseTickMax uni
    x <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
    y <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
    z <- randomRIO (-valueSoundSceneNoiseNodeRadius, valueSoundSceneNoiseNodeRadius)
    ix <- randomRIO (0, valueSoundSceneNoiseSize - 1)
    pitch <- randomIO >>= \uni -> return $ 
             smooth valueSoundSceneNoisePitchMin valueSoundSceneNoisePitchMax uni 

    return Noise
           {
              noiseTick = rTF tick,
              noiseNode = Node x y z,
              noiseIx = ix,
              noisePitch = pitch
           }



