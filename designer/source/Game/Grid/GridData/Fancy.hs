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
module Game.Grid.GridData.Fancy
  (
    GridData (..),
    
    loadGridData,
    unloadGridData,

    module Game.Grid.GridData.Fancy.ColorMap
  ) where

import MyPrelude
import File
import Game.Grid.GridData.Fancy.ColorMap


data GridData = 
    GridData
    {
        griddataColorMap :: !ColorMap
    }

loadGridData :: IO GridData
loadGridData = do
    colormap <- loadColorMap
    return $  GridData { griddataColorMap = colormap  }

unloadGridData :: GridData -> IO ()
unloadGridData griddata = do
    return ()
