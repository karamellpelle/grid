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
module Game.GameData
  (
    GameData (..),

    loadGameData,
    unloadGameData,

  ) where

import MyPrelude
import File

import Game.Values
import Game.Font
import Game.GUI

import Game.Grid.GridData
import Game.LevelPuzzle.LevelPuzzleData
import Game.Memory.MemoryData
import Game.Run.RunData


-- fixme: use GRID_STYLE_* macros
data GameData =
    GameData
    {
        gamedataFontShade :: !FontShade,              -- ^ shader for font rendering
        gamedataFontData :: !FontData,                -- ^ default font used in game
        gamedataGUIShade :: !GUIShade,                -- ^ shader for GUI
        gamedataGUIData :: !GUIData,                  -- ^ data for GUI
        gamedataRunData :: !RunData,                  -- ^ data for Run
        gamedataGridData :: !GridData,                -- ^ data for Grid
        gamedataLevelPuzzleData :: !LevelPuzzleData,  -- ^ data for LevelPuzzle
        gamedataMemoryData :: !MemoryData             -- ^ data for Memory
    }



loadGameData :: IO GameData
loadGameData = do

    -- Font --
    -- FontShade
    path <- fileStaticData "Font"
    fontsh <- loadFontShade path

    -- FontData (this is magic)
    path <- fileStaticData "fonts/font_game"
    fontdata <- makeFontData path 16 8 0.62 0.95 32

    -- GUI --
    -- GUIShade
    path  <- fileStaticData "GUI"
    guish <- loadGUIShade path

    -- GUIData
    path <- fileStaticData "guis/zero"
    guidata <- loadGUIData fontsh path


    -- RunData --
    rundata <- loadRunData

    -- GridData --
    griddata <- loadGridData

    -- LevelPuzzleData --
    levelpuzzledata <- loadLevelPuzzleData

    -- MemoryData --
    memorydata <- loadMemoryData
    

    return $  GameData
              {
                  gamedataFontShade = fontsh,
                  gamedataFontData = fontdata,
                  gamedataGUIShade = guish,
                  gamedataGUIData = guidata,
                  gamedataGridData = griddata,
                  gamedataRunData = rundata,
                  gamedataLevelPuzzleData = levelpuzzledata,
                  gamedataMemoryData = memorydata

              }
 
    where
      loadGUIData fontsh pathStyle =
          makeGUIData GUIMake
                      {
                          mkGUIStyle = pathStyle,
                          mkGUIFillTexRepeat = 8.0,

                          mkGUIFontShade = fontsh,
                          mkGUIFontColor = makeFontColorGLubyte 0x70 0x00 0xff 0xff,
                          mkGUIFontSize = 0.036
                      }
     

-- fixme!
unloadGameData :: GameData -> IO ()
unloadGameData gamedata =
    fixme "unloadGameData"





