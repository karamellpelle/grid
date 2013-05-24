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
module Game.Grid.GridWorld.Segment
  (
      Segment (..),
      segmentInverse, 
  
  ) where


import MyPrelude
import Game.Grid.GridWorld.Node
import Game.Grid.GridWorld.Turn


data Segment =
    Segment
    {
        segmentNode :: !Node,        -- ^ begin node      (relative to something)
        segmentTurn :: !Turn         -- ^ turn at node    (relative to something)
    } deriving (Eq)


instance Monoid Segment where
    mempty = Segment mempty mempty
    mappend (Segment n1 t1) (Segment n0 t0) = Segment (n1 `mappend` n0) (t1 `mappend` t0)


segmentInverse :: Segment -> Segment
segmentInverse (Segment node turn) =
    Segment (nodeInverse node) (turnInverse turn)

--------------------------------------------------------------------------------
--  tmp:
instance Show Segment where
    show (Segment node turn) = "Segment( " ++
                               show node ++
                               ", " ++ show turn ++ " )"
