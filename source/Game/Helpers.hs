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
    tickClockLevelPuzzleGet,
    tickClockLevelPuzzleSet,
    tickClockLevelPuzzleSetSpeed,
    tickClockMemoryGet,
    tickClockMemorySet,
    tickClockMemorySetSpeed,

    createDynamicData,
    createLocalPlayerData,

    fileRunWorldEmpty,
    fileRunWorld,
    fileLevelPuzzleWorld,
    
    gameSetIntensity,

    resourceGameData,
    resourceFontShade,
    resourceFontData,
    resourceGUIShade,
    resourceGUIData,
    resourceGridData,
    resourceRunData,
    resourceLevelPuzzleData,
    resourceMemoryData,

    iterateGUI,
    iterateGUINoInput,

  ) where


import MyPrelude

import Game.MEnv
import Game.Font
import Game.GUI
import Game.Values

import Game.Grid.GridData
import Game.LevelPuzzle.LevelPuzzleData
import Game.Memory.MemoryData
import Game.Run.RunData

import File
import Data.Char

import OpenGL
import OpenGL.Helpers

--------------------------------------------------------------------------------
--  clocks for each mode

tickClockRunGet :: MEnv' Tick
tickClockRunGet = tickClockFGet

tickClockRunSet :: Tick -> MEnv' ()
tickClockRunSet = tickClockFSet

tickClockRunSetSpeed :: Double -> MEnv' ()
tickClockRunSetSpeed = tickClockFSetSpeed


tickClockLevelPuzzleGet :: MEnv' Tick
tickClockLevelPuzzleGet = tickClockAGet

tickClockLevelPuzzleSet :: Tick -> MEnv' ()
tickClockLevelPuzzleSet = tickClockASet

tickClockLevelPuzzleSetSpeed :: Double -> MEnv' ()
tickClockLevelPuzzleSetSpeed = tickClockASetSpeed


tickClockMemoryGet :: MEnv' Tick
tickClockMemoryGet = tickClockCGet

tickClockMemorySet :: Tick -> MEnv' ()
tickClockMemorySet = tickClockCSet

tickClockMemorySetSpeed :: Double -> MEnv' ()
tickClockMemorySetSpeed = tickClockCSetSpeed





--------------------------------------------------------------------------------
--  

-- | create dynamic data for application, if not present
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
          unless exists $ do
              path' <- fileStaticData p
              copyFile path' path



-- | create dynamic data for Player, if not present
createLocalPlayerData :: Player -> MEnv' ()
createLocalPlayerData player = do
    pathDir <- fileLocalPlayer player
    pathRunWorld <- fileRunWorldEmpty
    io $ do
        -- directories
        createDirectory $ pathDir ++ "Run/"
        createDirectory $ pathDir ++ "LevelPuzzle/"

        -- files
        copyFile pathRunWorld $ pathDir ++ "Run/world"

    where
      createDirectory p = do
          fileDynamicData p >>= createDirectoryIfMissing True
      
      copyStaticFile p = do
          path <- fileDynamicData p
          exists <- doesFileExist path 
          unless exists $ do
              path' <- fileStaticData p
              copyFile path' path

      

--------------------------------------------------------------------------------
--  

fileLocalPlayer :: Player -> MEnv' FilePath
fileLocalPlayer player = io $ do
    path <- fileDynamicData "players/"
    return $ path ++ filenameFromPlayer player ++ "/"


-- | map playerID to file basename 
--   fixme: make this function one-one
filenameFromPlayer :: Player -> FilePath
filenameFromPlayer player =
    map helper (playerID player) ++ ".player"
    where
      -- substitute non-alphanum characters with '_'
      helper c =
          case isAlphaNum c of
              False   -> '_'
              True    -> c


-- | path to RunWorld not associated with any local player
fileRunWorldEmpty :: MEnv' FilePath
fileRunWorldEmpty = io $ 
    fileDynamicData "players/empty/Run/world"


-- | path to RunWorld for player
fileRunWorld :: Player -> MEnv' FilePath
fileRunWorld player = do
    (++ "Run/world") `fmap` fileLocalPlayer player




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

{-
-- | map playerID to file basename (not one-one)
basenameFromPlayer :: Player -> FilePath
basenameFromPlayer player =
    map helper (playerID player)
    where
      -- substitute non-alphanum characters with '_'
      helper c =
          case isAlphaNum c of
              False   -> '_'
              True    -> c
-}

-- | set clocks
gameSetIntensity :: Float -> MEnv' ()
gameSetIntensity intensity = do
    let speed = rTF $ smooth valueGridClockSpeedMin valueGridClockSpeedMax intensity
    tickClockLevelPuzzleSetSpeed speed
    tickClockMemorySetSpeed speed
    tickClockRunSetSpeed speed
    


--------------------------------------------------------------------------------
--  resources

resourceGameData :: MEnv' GameData
resourceGameData =
    resourceGet

resourceFontShade :: MEnv' FontShade
resourceFontShade =
    fmap gamedataFontShade resourceGet

resourceFontData :: MEnv' FontData
resourceFontData =
    fmap gamedataFontData resourceGet

resourceGUIShade :: MEnv' GUIShade
resourceGUIShade = 
    fmap gamedataGUIShade resourceGet

resourceGUIData :: MEnv' GUIData
resourceGUIData = 
    fmap gamedataGUIData resourceGet

resourceGridData :: MEnv' GridData
resourceGridData =
    fmap gamedataGridData resourceGet

resourceRunData :: MEnv' RunData
resourceRunData =
    fmap gamedataRunData resourceGet

resourceLevelPuzzleData :: MEnv' LevelPuzzleData
resourceLevelPuzzleData =
    fmap gamedataLevelPuzzleData resourceGet

resourceMemoryData :: MEnv' MemoryData
resourceMemoryData =
    fmap gamedataMemoryData resourceGet



--------------------------------------------------------------------------------
--  GUI 


-- | use MEnv to perform GUI
iterateGUI :: Widget w => Float -> w a -> a -> MEnv' (w a, a)
iterateGUI alpha w a = do
    gamedata <- resourceGameData
    let sh = gamedataGUIShade gamedata
        gd = gamedataGUIData gamedata

    (wth, hth) <- screenShape

    -- query input
    input <- queryInput
    
    -- query tick
    tick <- tickGet

    io $ do
        -- shade
        projmodv <- guiShade sh 1.0 wth hth

        -- iterate. enbles gl_DEPTH_TEST, disables gl_CULL_FACE
        (w', a') <- guiIterate sh gd projmodv alpha wth hth input tick w a

        glEnable gl_CULL_FACE
        return (w', a')

    where
      -- fixme: 2 fingers touching causes no WIDrop input. is this wanted behaviour?
      --        (cf. onNewNumber/onNewValue/...). maybe create WICancelled?
      queryInput = do
          let input = []
          input' <- keysTouchHandlePointDrag input $ \ticks (x, y) (x', y') -> 
              let pos = GUIPos x y
                  pos' = GUIPos x' y'
              in  (WidgetInput pos pos' ticks WIDrag : input)
          input'' <- keysTouchHandlePointDrop input' $ \ticks (x, y) (x', y') ->
              let pos = GUIPos x y
                  pos' = GUIPos x' y'
              in  (WidgetInput pos pos' ticks WIDrop : input')
          return input''


-- | use MEnv to perform GUI, no input
iterateGUINoInput :: Widget w => Float -> w a -> a -> MEnv' (w a, a)
iterateGUINoInput alpha w a = do
    gamedata <- resourceGameData
    let sh = gamedataGUIShade gamedata
        gd = gamedataGUIData gamedata

    (wth, hth) <- screenShape

    -- query tick
    tick <- tickGet

    io $ do
        -- shade
        projmodv <- guiShade sh 1.0 wth hth

        -- iterate
        (w', a') <- guiIterate sh gd projmodv alpha wth hth [] tick w a
        
        glEnable gl_CULL_FACE
        return (w', a')


