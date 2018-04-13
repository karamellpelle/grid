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
module Game.Memory.Do
  (
    doShow,
    doPlay,
    doFailure,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Memory
import Game.Memory.Do.Grid



doShow :: s -> MemoryWorld -> b -> MEnv' (s, MemoryWorld, b)
doShow = \s mem b -> do
    let mem' = memoryClearEvents $ memoryModifyCamera mem $ \cam -> 
                                   cameraToPathTranslate cam (memoryPath mem)

    -- do GridWorld
    (s', grid', mem'') <- doGridShow s (memoryGrid mem') mem'
    
    return (s', mem'' { memoryGrid = grid' }, b)



doPlay :: s -> MemoryWorld -> b -> MEnv' (s, MemoryWorld, b)
doPlay = \s mem b -> do
    let mem' = memoryClearEvents $ memoryModifyCamera mem $ \cam -> 
                                   cameraToPath cam (memoryPath mem)

    -- do GridWorld
    (s', grid', mem'') <- doGridPlay s (memoryGrid mem') mem'
    
    return (s', mem'' { memoryGrid = grid' }, b)



doFailure :: s -> MemoryWorld -> b -> MEnv' (s, MemoryWorld, b)
doFailure = \s mem b -> do
    let mem' = memoryClearEvents $ memoryModifyCamera mem $ \cam -> 
                                   cameraToPath cam (memoryPath mem)

    -- do GridWorld
    (s', grid', mem'') <- doGridFailure s (memoryGrid mem') mem'
    
    return (s', mem'' { memoryGrid = grid' }, b)



