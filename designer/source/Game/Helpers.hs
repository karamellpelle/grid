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
module Game.Helpers
  (
    tickClockRunGet,
    tickClockRunSet,
    tickClockRunSetSpeed,
    tickClockLevelPuzzleModeGet,
    tickClockLevelPuzzleModeSet,
    tickClockLevelPuzzleModeSetSpeed,
    tickClockMemoryModeGet,
    tickClockMemoryModeSet,
    tickClockMemoryModeSetSpeed,

    createDynamicData,
    basenameFromString,
    resourceGameData,
  ) where


import MyPrelude
import File
import Data.Char

import Game.MEnv

import Game.GameData
import Game.Grid.GridData
import Game.LevelPuzzleMode.LevelPuzzleWorld
import Game.LevelPuzzleMode.LevelPuzzleData

import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  clocks for each mode

tickClockRunGet :: MEnv' TickT
tickClockRunGet = tickClockFGet

tickClockRunSet :: TickT -> MEnv' ()
tickClockRunSet = tickClockFSet

tickClockRunSetSpeed :: Double -> MEnv' ()
tickClockRunSetSpeed = tickClockFSetSpeed


tickClockLevelPuzzleModeGet :: MEnv' TickT
tickClockLevelPuzzleModeGet = tickClockAGet

tickClockLevelPuzzleModeSet :: TickT -> MEnv' ()
tickClockLevelPuzzleModeSet = tickClockASet

tickClockLevelPuzzleModeSetSpeed :: Double -> MEnv' ()
tickClockLevelPuzzleModeSetSpeed = tickClockASetSpeed


tickClockMemoryModeGet :: MEnv' TickT
tickClockMemoryModeGet = tickClockCGet

tickClockMemoryModeSet :: TickT -> MEnv' ()
tickClockMemoryModeSet = tickClockCSet

tickClockMemoryModeSetSpeed :: Double -> MEnv' ()
tickClockMemoryModeSetSpeed = tickClockCSetSpeed





--------------------------------------------------------------------------------
--  

-- | create dynamic data to be used by application, if not present
createDynamicData :: MEnv' ()
createDynamicData = io $ do

    -- directories
    createDirectory "players/empty/Run/"
    createDirectory "players/empty/LevelPuzzle/"
    createDirectory "LevelPuzzle/"

    -- files
    copyStaticFile "players/empty/Run/world" 
    copyStaticFile "LevelPuzzle/world"

    where
      createDirectory p = do
          fileDynamicData p >>= createDirectoryIfMissing True
      
      copyStaticFile p = do
          path <- fileDynamicData p
          exists <- doesFileExist path 
          --unless exists $ do
          do -- overwirte!!
              putStrLn "overwriting dynamic file!"
              path' <- fileStaticData p
              copyFile path' path


      

--------------------------------------------------------------------------------
--  




{-
filePlayerPathEmpty :: FilePath -> MEnv' FilePath
filePlayerPathEmptyplayer path = 
    (fileDynamicData "players/empty/LevelPuzzle/") ++ path

filePlayerPath :: Player -> FilePath -> MEnv' FilePath
filePlayerPath player path = 
    (fileDynamicData "players/") ++ filenameFromPlayer player ++ "/LevelPuzzle/" ++ path
-}


-- | path to LevelPuzzleWorld with name
fileLevelPuzzleWorld :: String -> MEnv' FilePath
fileLevelPuzzleWorld name = io $ 
    fileDynamicData $ "LevelPuzzle/" ++ name

basenameFromString :: String -> FilePath
basenameFromString str  =
    map helper str
    where
      -- substitute non-alphanum characters with '_'
      helper c =
          case isAlphaNum c of
              False   -> '_'
              True    -> c
--------------------------------------------------------------------------------
--  resources

resourceGameData :: MEnv' GameData
resourceGameData =
    resourceGet



--------------------------------------------------------------------------------
--  GUI 
