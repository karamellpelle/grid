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
    loadColorMap,
    unloadColorMap,

    colormapAt,
    colormapAtSafe,
    colormapAt2,
    colormapAt2Safe,
    valueColorMapSize,

    module Game.Data.Color,

  ) where

import MyPrelude
import Data.Array.IArray
import Data.Array.Base
import Game.Values
import Game.Data.Color

import OpenGL
import OpenGL.Helpers



type ColorMap =
    Array Int Color




-- | number of colors in ColorMap
valueColorMapSize :: UInt
valueColorMapSize =
    22


-- | note: remember to update 'valueColorMapSize'!
loadColorMap :: IO ColorMap
loadColorMap =
    return $ listArray (0, 21) [
             Color 0.0 0.0 0.0 1.0,
             Color 1.0 1.0 1.0 1.0,
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



--------------------------------------------------------------------------------
--  


-- | color from index relative to 0
colormapAt :: ColorMap -> UInt -> Color
colormapAt map ix = 
#ifdef GRID_SAFE
    (!) map (fI $ ix)
#else
    unsafeAt map (fI $ ix)
#endif


-- | color from index relative to 2 (non-trivial colors)
colormapAt2 :: ColorMap -> UInt -> Color
colormapAt2 map ix = 
#ifdef GRID_SAFE
    (!) map (fI $ ix + 2)
#else
    unsafeAt map (fI $ ix + 2)
#endif


-- | modulo ix
colormapAtSafe :: ColorMap -> UInt -> Color
colormapAtSafe map ix = 
#ifdef GRID_SAFE
    (!) map (fI $ ix `mod` valueColorMapSize)
#else
    unsafeAt map (fI $ ix `mod` valueColorMapSize)
#endif


-- | modulo ix
colormapAt2Safe :: ColorMap -> UInt -> Color
colormapAt2Safe map ix = 
#ifdef GRID_SAFE
    (!) map (fI $ 2 + ix `mod` (valueColorMapSize - 2))
#else
    unsafeAt map (fI $ 2 + ix `mod` (valueColorMapSize - 2))
#endif




