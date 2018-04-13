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
module Main.GLFW.Merge
  (
    mainMerge,

  ) where

import MyPrelude
import File
import LoadM

import Game
import Game.GameData
import LevelTools.EditWorld
import LevelTools.Helpers
import LevelTools.EditWorld.Make
import LevelTools.Make
import LevelTools.File
import System.IO
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import File.Binary
import Data.List

mainMerge paths = do
    maybePath <- assertLevelPaths paths 
    case maybePath of
        Just path -> putStrLn $ "file is not a .ldef: " ++ path 
        Nothing   -> do
            creator <- inputLine  "creator        : " 
            name <- inputLine     "name           : "

            pathDir <- tmpDir
            pathHeader <- makeWorldHeader pathDir creator name
            pathLevels <- makeLevelData pathDir paths

            path <- findSimilarFileName (basenameFromString name) ".def"

            -- cat into path
            err <- system $ "cat " ++ pathHeader ++ " " 
                                   ++ intercalate " " pathLevels
                                   ++ " > " ++ path
            case err of
                ExitFailure n -> putStrLn $ "could not write file (exit code " ++ show n ++ ")"
                ExitSuccess   -> putStrLn $ "written to file: " ++ path

    
    where
      inputLine str =do
          putStr str
          hFlush stdout
          getLine

      tmpDir = do
          getTemporaryDirectory

      -- create header for "w000" file
      makeWorldHeader dir creator name = do
          path <- findSimilarFileName (dir </> "w000") ""
          writeWorldHeader creator name path
          return path

      -- convert "l000" files into data for "w000" file
      makeLevelData dir paths = do
          mapM helper paths
          where
            helper path = do
                path' <- findSimilarFileName (dir </> path) ""
                -- just strip the first 4 bytes
                err <- system $ "tail -c +5 " ++ path ++ " > " ++ path'
                case err of 
                    ExitSuccess -> return path'
                    _           -> error (show err)
                
      assertLevelPaths paths = 
          case paths of
              []      -> return Nothing
              (p:ps)  -> do
                  readBinary rCompatibleHeaderLevel p >>= \either -> 
                      case either of
                          Left  msg   -> do
                              putStrLn $ "error while reading: " ++ msg
                              return (Just p)
                          Right _   -> assertLevelPaths ps

