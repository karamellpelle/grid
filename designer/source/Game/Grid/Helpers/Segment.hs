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
module Game.Grid.Helpers.Segment
  (
    segmentEatTurn,
    segmentAppendNode,
    segmentAppendTurn,
    segmentNodeNext,
    segmentNodeNextCount,
    dummySegment,

  ) where

import MyPrelude
import Game
import Game.Grid.GridWorld


segmentEatTurn :: Segment -> Turn -> Segment
segmentEatTurn seg@(Segment node turn) t =
    let node' = segmentNodeNext seg
        turn' = t `mappend` turn
    in (Segment node' turn')


segmentAppendTurn :: Segment -> Turn -> Segment
segmentAppendTurn (Segment node turn ) t =
    Segment node (t `mappend` turn)


segmentAppendNode :: Segment -> Node -> Segment
segmentAppendNode (Segment node turn) n =
    Segment (n `mappend` node) turn


segmentNodeNext :: Segment -> Node
segmentNodeNext (Segment (Node x y z) (Turn x0 x1 x2 _ _ _ _ _ _)) =
    Node (x + fI x0) (y + fI x1) (z + fI x2)


segmentNodeNextCount :: Segment -> UInt -> Node
segmentNodeNextCount (Segment (Node x y z) (Turn x0 x1 x2 _ _ _ _ _ _)) n =
    Node (x + fI n * fI x0) (y + fI n * fI x1) (z + fI n * fI x2)


segmentNext :: Segment -> Segment
segmentNext seg =
    Segment (segmentNodeNext seg) (segmentTurn seg)


segmentNextCount :: Segment -> UInt -> Segment
segmentNextCount seg n =
    Segment (segmentNodeNextCount seg n) (segmentTurn seg)



--------------------------------------------------------------------------------
--  

dummySegment :: Segment
dummySegment =
    Segment (Node 0 0 0) (Turn 0 0 0 0 0 0 0 0 0)
