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
module Main.GLFW.IsValid
  (
    mainIsValid,

  ) where

import MyPrelude
import File
import LoadM

import Game
import Game.GameData
import LevelTools.EditWorld
import LevelTools.Helpers
import LevelTools.EditWorld.Make
import LevelTools.Iteration
import LevelTools.Make
import LevelTools.File


import OpenGL
import OpenGL.Helpers
import Data.Maybe

import Graphics.UI.GLFW as GLFW
import System.IO


mainIsValid path = do
    
    let init = error "no EnvInit"
   
    edit <- makeEditWorldIO path

    case findInvalidObject edit of
        Nothing                 -> 
            putStrLn $ path ++ " is valid"
        Just (room, node, str)  -> do
            putStrLn $ path ++ " invalid in RoomIx: " ++ show room ++ " at " ++ show node ++
                       ":   " ++ str 

   
