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
module Game.Grid.GridData.Fancy.ColorMap
  (
    ColorMap,
    colormapAt,

    loadColorMap,
    unloadColorMap,
    valueColorMapSize,

    module Game.Data.Color,

  ) where

import MyPrelude
import Data.Array.IArray
import Data.Array.Base
import Game.Values
import Game.Data.Color



type ColorMap =
    Array Int Color


colormapAt :: ColorMap -> UInt -> Color
colormapAt map ix = 
    unsafeAt map (fI $ ix `mod` valueColorMapSize)


-- | note: remember to update 'valueColorMapSize'!
loadColorMap :: IO ColorMap
loadColorMap =
    return $ listArray (0, 21) [
             Color 1.0 0.0 0.0 1.0,
             Color 0.0 1.0 0.0 1.0,
             Color 0.0 0.0 1.0 1.0,
             Color 1.0 1.0 0.0 1.0,
             Color 0.0 1.0 1.0 1.0,
             Color 1.0 0.0 1.0 1.0,
             Color 0.5 0.0 0.0 1.0,
             Color 0.0 0.5 0.0 1.0,
             Color 0.0 0.0 0.5 1.0,
             Color 0.5 0.5 0.0 1.0,
             Color 0.0 0.5 0.5 1.0,
             Color 0.5 0.0 0.5 1.0,
             Color 0.5 0.0 0.0 1.0,
             Color 0.0 0.5 0.0 1.0,
             Color 0.0 0.0 0.5 1.0,
             Color 1.0 0.5 0.0 1.0,
             Color 0.0 1.0 0.5 1.0,
             Color 1.0 0.0 0.5 1.0,
             Color 0.5 1.0 0.0 1.0,
             Color 0.0 0.5 1.0 1.0,
             Color 0.5 0.0 1.0 1.0,
             Color 1.0 0.5 0.5 1.0]


unloadColorMap :: ColorMap -> IO ()
unloadColorMap map = 
    return ()



