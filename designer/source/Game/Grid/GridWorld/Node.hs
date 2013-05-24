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
module Game.Grid.GridWorld.Node
  (
      Node (..),
      
      nodeDiff,
      nodeInverse,
      nodeAbs,
      nodeCross,
      nodeInner,

  ) where


import MyPrelude
import Data.Int


-- | save space with Int16! (well, in GHC they still are of word size)
data Node =
    Node !Int16 !Int16 !Int16
    deriving (Eq)



-- | the difference node -> node'
nodeDiff :: Node -> Node -> Node 
nodeDiff (Node x y z) (Node x' y' z') =
    Node (x' - x) (y' - y) (z' - z)


instance Monoid Node where
    mempty = Node 0 0 0
    mappend (Node x1 y1 z1) (Node x0 y0 z0) = Node (x1 + x0) (y1 + y0) (z1 + z0)


nodeInverse :: Node -> Node
nodeInverse (Node x y z) =
   Node (negate x) (negate y) (negate z)


--------------------------------------------------------------------------------
--  various functions

nodeAbs :: Node -> Int16
nodeAbs (Node x y z) =
    abs x + abs y + abs z

nodeCross :: Node -> Node -> Node
nodeCross (Node x0 y0 z0) (Node x1 y1 z1) =
    Node (y0 * z1 - z0 * y1) (z0 * x1 - x0 * z1) (x0 * y1 - y0 * x1)

nodeInner :: Node -> Node -> Int16
nodeInner (Node x0 y0 z0) (Node x1 y1 z1) =
        x0 * x1 + y0 * y1 + z0 * z1


--------------------------------------------------------------------------------
--  tmp:

instance Show Node where
    show (Node x y z) = "Node: " ++ show x ++ " " ++ show y ++ " " ++ show z
