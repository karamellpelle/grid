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
module Main.GLFW.Info
  (
    mainInfo,
    mainInfoVerbose,

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


import Game.LevelPuzzleMode.File.Read as LP
import Game.LevelPuzzleMode.File.Field as LP
import LevelTools.File as LT
import File.Binary

mainInfo path = do
    readBinary rHeader path >>= \either -> case either of
        Left msg  -> putStrLn $ path ++ " is not a valid .def or .ldef file"
        Right ws     -> case ws of
            (0x6c, 0x30, 0x30, 0x30)  -> mainInfoLevel path False
            (0x77, 0x30, 0x30, 0x30)  -> mainInfoWorld path False

mainInfoVerbose path = do
    readBinary rHeader path >>= \either -> case either of
        Left msg  -> putStrLn $ path ++ " is not a valid .def or .ldef file"
        Right ws     -> case ws of
            (0x6c, 0x30, 0x30, 0x30)  -> mainInfoLevel path True
            (0x77, 0x30, 0x30, 0x30)  -> mainInfoWorld path True




mainInfoWorld path verbose = do
    readBinary rWorld path >>= \either -> case either of
        Left msg  -> putStrLn $ path ++ " is not a valid .def file"
        Right (creator, name, off) -> do

            putStrLn $ "creator       : " ++ creator
            putStrLn $ "name          : " ++ name 
            when verbose $ do
                putStrLn "(verbose output not implemented)"


mainInfoLevel path verbose = do
    readBinary rLevelFile path >>= \either -> case either of
        Left msg  -> putStrLn $ path ++ " is not a valid .ldef file"
        Right (level, srooms) -> do
            
            putStrLn $ "Name          : " ++ (levelName level)
            putStrLn $ "PuzzleTag     : " ++ show (levelPuzzleTag level) 
            putStrLn $ "Segments      : " ++ show (levelSegments level)

            when verbose $ do
                putStrLn "(verbose output not implemented)"


--------------------------------------------------------------------------------
--  


rHeader :: Reader (Word8, Word8, Word8, Word8)
rHeader = do
    w0 <- rAnyWord8
    w1 <- rAnyWord8
    w2 <- rAnyWord8
    w3 <- rAnyWord8
    return (w0, w1, w2, w3)


rWorld :: Reader (String, String, UInt)
rWorld = do
    LP.rCompatibleHeader

    rThisField fieldCreator
    creator <- rCString
    rAlign 4

    rThisField fieldName
    name  <- rCString
    rAlign 4

    off <- rOffset
    return (creator, name, off)

