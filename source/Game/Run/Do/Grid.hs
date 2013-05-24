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
module Game.Run.Do.Grid
  (
    doGrid,
    doGridCamera,

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.Grid.Modify
import Game.Grid.StepDT
import Game.Grid.Do
import Game.Run.RunWorld
import Game.Run.Helpers
import Game.Run.Do.Grid.Modify



doGrid :: s -> GridWorld -> RunWorld -> MEnv' (s, GridWorld, RunWorld)
doGrid =
    defaultDo (controlGrid controlEmpty)
              (tickClockRunGet, tickClockRunSet)
              (defaultStepDT collisionMain)
              (noBreakModify)
              (noDefaultModify)


doGridCamera :: s -> GridWorld -> RunWorld -> MEnv' (s, GridWorld, RunWorld)
doGridCamera =
    defaultDo (controlGrid controlRunCamera)
              (tickClockRunGet, tickClockRunSet)
              (defaultStepDT collisionMain)
              (noBreakModify)
              (noDefaultModify)





--------------------------------------------------------------------------------
--  on path node
--

--next = 
--   (a:as)  -> msg' = nextA msg a as
--              push ...
--   []      -> 
--       (as:ass)  -> msg' = nextAS msg as ass -- (step ref)
--                    next ...
--       []        -> 
--           (ass:asss)  -> msg' = nextASS msg ass asss
--                          next ...
--           []          -> 
--               case replicate charWth dummySegment of
--                   (a:as)  -> step ref
--                              nextA msg a as
--                              next ...
--                   []      -> undefined
--
collisionMain :: Collision s RunWorld
collisionMain  =
    Collision { onPathNode = next }
    
    where
      next path s grid run = 
          case messageAS (runMessage run) of

              -- next A, push segment, end recursion
              (a:as)  -> do
                  -- push a
                  let msg' = nextA (runMessage run) a as
                      run' = run { runMessage = msg' }
                  path' <- pathPushCurrent path $ 
                           refAppend (messageRefPlus msg' `mappend` messageRef msg')
                                     (messageA msg')
                  return (path', s, grid, run') 

              []      -> 
                  case messageASS (runMessage run) of
                      
                      -- next AS, begin recursion
                      (as:ass)  ->
                          let msg' = nextAS (runMessage run) as ass
                              run' = run { runMessage = msg' }
                          in  next path s grid run'

                      []        -> 
                          case messageASSS (runMessage run) of

                              -- next ASS, begin recursion
                              (ass:asss)  ->
                                  let msg' = nextASS (runMessage run) ass asss
                                      run' = run { runMessage = msg' }
                                  in  next path s grid run'

                              -- just waitin', end recursion
                              []          ->
                                return (path, s, grid, run)

                              -- instead of waiting, push invisible segments
                              --[]          -> 
                              --    let msg' = nextDummyAS (runMessage run)
                              --        run' = run { runMessage = msg' }
                              --    --in  (path, s, grid, run')
                              --    in  next path s grid run'
      -- next segment
      nextA msg a as = 
          msg
          {
              messageA = a,
              messageAS = as
          }

      -- next character (handle special characters appropriate)
      nextAS msg as ass =
          case as of
              '\0'    -> msg
                         {
                            messageAS = charToSegments as,
                            messageASS = ass
                         }

              -- unless character is 0, also set new Ref
              as      -> msg
                         {
                             messageAS = charToSegments as,
                             messageASS  = ass,
                             messageRef = nextRef (messageRef msg)
                         }

      -- next string
      nextASS msg ass asss = 
          msg
          {
              messageASS = ass,
              messageASSS = asss
          }

      nextDummyAS msg = 
          msg
          {
              messageAS = replicate (fI valueRunMessageCharWth) dummySegment,
              messageRef = nextRef (messageRef msg)
          }


-- | Ref defines character starting point and direction. we want to draw characters
--   around RunCube. (maybe Ref should be character point instead)
nextRef :: Segment -> Segment
nextRef seg = 
    let node'@(Node x y z) = segmentNodeNextCount seg valueRunMessageCharWth
    in  case direction (segmentTurn seg) of
            Dir 0 0 1     -> if (fI valueRunMessageRadius) <= z 
                             then Segment node' straightTurn
                             else Segment node' (segmentTurn seg)
            Dir 1 0 0     -> if (fI valueRunMessageRadius) <= x
                             then Segment node' leftTurn
                             else Segment node' (segmentTurn seg)
            Dir 0 0 (-1)  -> if z <= (-fI valueRunMessageRadius)
                             then Segment node' backTurn
                             else Segment node' (segmentTurn seg)
            Dir (-1) 0 0  -> if x <= (-fI valueRunMessageRadius)
                             then Segment node' rightTurn
                             else Segment node' (segmentTurn seg)
            _             -> Segment node' (segmentTurn seg)
        


refAppend :: Segment -> Segment -> Segment
refAppend (Segment n0 t0) 
          (Segment n1 t1) =
    Segment ((t0 `turnMultNode` n1) `mappend` n0) (t1 `mappend` t0)


