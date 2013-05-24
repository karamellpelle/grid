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
module Game.Memory.Helpers
  (
    memoryCamera,
    memoryClearEvents,
    memoryPath,
    memoryModifyGrid,
    memoryModifyCamera,

    memorySetLevelBegin,
    memorySetLevelNext,

  ) where

import MyPrelude
import Game

import Game.Memory.MemoryWorld
import Game.Grid.GridWorld
import Game.Grid.GridWorld.Make
import Game.Grid.Helpers




--------------------------------------------------------------------------------
--  

memoryCamera :: MemoryWorld -> Camera
memoryCamera =  
    gridCamera . memoryGrid


memoryClearEvents :: MemoryWorld -> MemoryWorld
memoryClearEvents mem =
    mem { memoryEvents = [] }


memoryPath :: MemoryWorld -> Path
memoryPath =
    gridPath . memoryGrid


memoryModifyCamera :: MemoryWorld -> (Camera -> Camera) -> MemoryWorld
memoryModifyCamera mem f =
    memoryModifyGrid mem $ \grid -> gridModifyCamera grid f


memoryModifyGrid :: MemoryWorld -> (GridWorld -> GridWorld) -> MemoryWorld
memoryModifyGrid mem f =
    mem { memoryGrid = f (memoryGrid mem) }



--------------------------------------------------------------------------------
--  

memorySetLevelBegin :: MemoryWorld -> MEnv' MemoryWorld
memorySetLevelBegin mem = do
    grid' <- beginGrid $ memoryGrid mem
    return  mem
            {
                memoryFood = memoryLevelCurrent mem,
                memoryGrid = grid',
                memoryIsFailure = False
            }

    where
      beginGrid grid = do
          path' <- pathClear (gridPath grid)
          let grid' = grid
                      {   
                          gridPath = path',
                          gridEvents = []
                      }
              grid'' = gridModifyCamera grid' $ \cam -> cameraSetPath cam path'

          return grid''



memorySetLevelNext :: MemoryWorld -> MEnv' MemoryWorld
memorySetLevelNext mem =                    
    case memoryLevelNexts mem of
        []            -> 
            error "memoryNextLevel: logic error (no more levels)"

        (next:nexts)  -> do
            let size = fI $ length next + 1
                turn = cameraTurn $ memoryCamera mem

            grid' <- makeGridWorldCameraView size $ gridCamera $ memoryGrid mem
            destroyGridWorld $ memoryGrid mem

            return  mem
                    {
                        memoryGrid = gridModifyPath grid' $ \path ->
                                     path { pathSpeed = valueMemoryPathSpeed },
                        memoryLevelIx = memoryLevelIx mem + 1,
                        memoryLevelSize = size,
                        memoryLevelCurrent = next,
                        memoryLevelNexts = nexts,
                        memorySpaceRef = nextSpaceRef (memorySpaceRef mem) turn
                                    
                    }
    where
      nextSpaceRef ref turn =
          ref `mappend` (turnInverse turn)


