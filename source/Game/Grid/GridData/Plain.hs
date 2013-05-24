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
module Game.Grid.GridData.Plain
  (
    GridData (..),
    
    loadGridData,
    unloadGridData,

    module Game.Grid.GridData.Plain.ShadeSpaceBox,
    module Game.Grid.GridData.Plain.ShadePath,
    module Game.Grid.GridData.Plain.SoundPath,
    module Game.Grid.GridData.Plain.ColorMap,

  ) where

import MyPrelude
import File

import Game.Grid.GridData.Plain.ShadeSpaceBox
import Game.Grid.GridData.Plain.ShadePath
import Game.Grid.GridData.Plain.SoundPath
import Game.Grid.GridData.Plain.ColorMap


data GridData = 
    GridData
    {
        griddataShadeSpaceBox :: !ShadeSpaceBox,
        griddataShadePath :: !ShadePath,
        griddataSoundPath :: !SoundPath,
        griddataColorMap :: !ColorMap

    }


loadGridData :: IO GridData
loadGridData = do
    shSpaceBox <- loadShadeSpaceBox
    shPath <- loadShadePath
    sndPath <- loadSoundPath
    colormap <- loadColorMap

    return $  GridData
              {
                  griddataShadeSpaceBox = shSpaceBox,
                  griddataShadePath = shPath,
                  griddataSoundPath = sndPath,
                  griddataColorMap = colormap
              }


unloadGridData :: GridData -> IO ()
unloadGridData griddata = do
    unloadColorMap $ griddataColorMap griddata
    unloadSoundPath $ griddataSoundPath griddata
    unloadShadePath $ griddataShadePath griddata
    unloadShadeSpaceBox $ griddataShadeSpaceBox griddata


