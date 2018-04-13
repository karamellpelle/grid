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
module Game.Grid.GridData.Fancy
  (
    GridData (..),
    
    loadGridData,
    unloadGridData,

    module Game.Grid.GridData.Fancy.ShadeSpace,
    module Game.Grid.GridData.Fancy.ShadePath,
    module Game.Grid.GridData.Fancy.SoundPath,

    module Game.Grid.GridData.Fancy.ColorMap,
    module Game.Grid.GridData.Fancy.ShadeGeneral,

  ) where

import MyPrelude
import File

import Game.Grid.GridData.Fancy.ShadeSpace
import Game.Grid.GridData.Fancy.ShadePath
import Game.Grid.GridData.Fancy.SoundPath
import Game.Grid.GridData.Fancy.ColorMap
import Game.Grid.GridData.Fancy.ShadeGeneral


data GridData = 
    GridData
    {
        griddataShadeSpace :: !ShadeSpace,
        griddataShadePath :: !ShadePath,
        griddataSoundPath :: !SoundPath,

        -- these should probably be moved elsewhere (RunData?)
        griddataColorMap :: !ColorMap,
        griddataShadeGeneral :: !ShadeGeneral

    }


loadGridData :: IO GridData
loadGridData = do
    shSpace <- loadShadeSpace
    shPath <- loadShadePath
    sndPath <- loadSoundPath
    colormap <- loadColorMap
    shGeneral <- loadShadeGeneral

    return $  GridData
              {
                  griddataShadeSpace = shSpace,
                  griddataShadePath = shPath,
                  griddataSoundPath = sndPath,

                  griddataColorMap = colormap,
                  griddataShadeGeneral = shGeneral
              }


unloadGridData :: GridData -> IO ()
unloadGridData griddata = do
    unloadShadeGeneral $ griddataShadeGeneral griddata
    unloadColorMap $ griddataColorMap griddata
    unloadSoundPath $ griddataSoundPath griddata
    unloadShadePath $ griddataShadePath griddata
    unloadShadeSpace $ griddataShadeSpace griddata


