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
module Game.Run.Iteration.AtFace
  (
    iterationAtFace,

  ) where


import MyPrelude
import Game

import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Helpers

import Game.Grid.Helpers
import Game.Run.Iteration.Iteration
import Game.Run.Iteration.State



--------------------------------------------------------------------------------
-- iterationAtFace: do something at face. 


iterationAtFace :: Face -> IterationStack' RunWorld -> Iteration' RunWorld
iterationAtFace face top = 
    makeIteration' $ \run -> do

        let view = cameraCurrentView $ runCamera run
            view' = viewFromRunFace run
            turn' = turnFromFace face
            run' = run { runTurn = turn' }
            run'' = runModifyCamera run' $ \cam ->
                    cameraToTurnView cam speed turn' view'

            s = makeAtFaceState face view

        iteration' (iterationAtFace' top s) run''

    where
      speed = valueRunFaceSpeed



iterationAtFace' :: IterationStack' RunWorld -> AtFaceState -> Iteration' RunWorld
iterationAtFace' top s =
    mainIteration s outputAtFace' $ defaultStep doEmpty $ \s run b -> 

        -- Camera finished => do iteration
        if cameraIsAtView $ runCamera run
          then return (run, b, top ++ [iterationAtFace'' s])
          else return (run, b, [iterationAtFace' top s]) 



iterationAtFace'' :: AtFaceState -> Iteration' RunWorld
iterationAtFace'' s =
    makeIteration' $ \run -> do

        let run' = runModifyCamera run $ \cam -> 
                   cameraToView cam speed (atfacestateView s)

        iteration' (iterationAtFace''' s) run'
        
    where
      speed = valueRunFaceSpeed


iterationAtFace''' :: AtFaceState -> Iteration' RunWorld
iterationAtFace''' s =
    mainIteration s outputAtFace''' $ defaultStep doEmpty $ \s run b -> 

        if cameraIsAtView $ runCamera run
          then return (run, b, [])
          else return (run, b, [iterationAtFace''' s])




