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
module Main.GLFW
  (
    main',

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


import OpenGL
import OpenGL.Helpers
import Data.Maybe

import Graphics.UI.GLFW as GLFW
import System.IO
import Main.GLFW.New
import Main.GLFW.Load
import Main.GLFW.IsValid
import Main.GLFW.Info
import Main.GLFW.Merge
import System.IO
import System.Environment


cmdStrs = [
    "    --new                   edit new level",
    "    --load <path>           edit .ldef file",
    "    --is-valid <path>       check if .ldef file defines valid level",
    "    --info <path>           info about .ldef or .def file",
    "    --info-verbose <path>   verbose info about .ldef or .def file",
    "    --merge [<path>]        merge ordered list of .ldef files into .def file" ]


main' :: IO ()
main' = do
    args <- getArgs 
    case args of
      ("--new":[])                -> mainNew
      ("--load":path:[])          -> mainLoad path
      ("--is-valid":path:[])      -> mainIsValid path
      ("--info":path:[])          -> mainInfo path
      ("--info-verbose":path:[])  -> mainInfoVerbose path
      ("--merge":paths)           -> mainMerge paths
      _                           -> mapM_ putStrLn cmdStrs


