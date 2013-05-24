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
module Game.Grid.Do
  (
    --pathWait,
    postStepDTModify,
    pathEatWait,
    pathEatContinue,

  ) where


import MyPrelude
import Game
import Game.Grid.GridWorld
import Game.Grid.Modify
import Game.Grid.Helpers


-- | to be done after StepDT
postStepDTModify :: s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
postStepDTModify s grid b = 
    case pathEvents $ gridPathA grid of
        (EventNewSegment:_) -> do
            -- if new segment, reset reference pos
            grid' <- keysTouchHandlePointVector grid $ \_ _ pos' -> 
                     grid { gridControlPosRef = pos' }
            return (s, grid', b)
        
        -- fixme: (e:es) -> f es (but currently only 1 PathEvent)

        _                   -> do
            return (s, grid, b)



--------------------------------------------------------------------------------
--  onPathNode


-- | eat node and wait for defined turn
pathEatWait :: Path -> IO Path
pathEatWait path =
    case pathTurnState path of
        -- no defined turn, eat and wait
        Nothing     -> do
            path' <- pathEatTurn path straightTurn
            return $ path' { pathWaiting = True }

        -- continue in direction defined by turn
        Just turn     -> do
            path' <- pathEatTurn path turn
            return $ path' { pathTurnState = Nothing }

-- | continue straight ahead if no defined turn
pathEatContinue :: Path -> IO Path
pathEatContinue path = do
    path' <- pathEatTurn path (maybe straightTurn id $ pathTurnState path)
    return $ path' { pathTurnState = Nothing }




