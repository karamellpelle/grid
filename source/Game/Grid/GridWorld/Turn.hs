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
module Game.Grid.GridWorld.Turn
  (
      Turn (..),
      Z3,
      
      straightTurn,
      downTurn,
      upTurn,
      leftTurn,
      rightTurn,
      clockTurn,
      anticlockTurn,
      backTurn,
      turnInverse,
      turnDiff,

      Dir (..),
      direction,
      leftDir,
      rightDir,
      backwardDir, 
      forwardDir, 
      downDir,
      upDir,

  ) where

import MyPrelude
import Data.Int

-- what can be said about Turn?
-- 
-- Turn is generated by straightTurn, downTurn, upTurn, leftTurn, rightTurn:
-- ok, by idea.
--
-- Turn is a group:
-- ok.
-- 
-- |Turn| = 24:
-- we have 6 directions. every direction has 4 neighbour directions. this gives |Turn| <= 6*4 = 24.
-- I can however count 24 elements of Turn (4+4 in zx-plane, 4+4 in xy-plane, 4+4 in yz-plane). 
-- hence |Turn| = 24.
-- 
-- Turn is not abelian:
-- up * left != left * up
--
--
--
--
-- ok. lets model the group Turn as the subset of M3x3( Z3 ) generated by straightTurn, 
-- downTurn, upTurn, leftTurn, rightTurn.


-- GHC note: data Int8 = I8# Int#
type Z3 = 
    Int8  

data Turn =
    Turn !Z3 !Z3 !Z3
         !Z3 !Z3 !Z3
         !Z3 !Z3 !Z3
    deriving Eq

-- (actually, we can define Z3 using 2 bits, and put a whole 4x4 matrix into Word32. the matrix
--  operations can be implemented by multiplication, bitshifts, bitmasks, bitors. a nice property
--  is that each row and column consists of all zeros except one. but I found that this requires more 
--  instructions than the above.)

-- a more important property of Turn is that we have a 0-element to start with. actually,
-- this structure of Turn turns out to be a monoid.

instance Monoid Turn where
    mempty = straightTurn
    mappend = appendTurn


-- from right to left
-- (fixme: a* -> b*, b* -> a*)
appendTurn :: Turn -> Turn -> Turn
appendTurn (Turn ax0 ax1 ax2
                 ay0 ay1 ay2
                 az0 az1 az2)
           (Turn bx0 bx1 bx2
                 by0 by1 by2
                 bz0 bz1 bz2) = 

    Turn (bx0 * ax0 + by0 * ax1 + bz0 * ax2)
         (bx1 * ax0 + by1 * ax1 + bz1 * ax2)
         (bx2 * ax0 + by2 * ax1 + bz2 * ax2)
             
         (bx0 * ay0 + by0 * ay1 + bz0 * ay2)
         (bx1 * ay0 + by1 * ay1 + bz1 * ay2)
         (bx2 * ay0 + by2 * ay1 + bz2 * ay2)
             
         (bx0 * az0 + by0 * az1 + bz0 * az2)
         (bx1 * az0 + by1 * az1 + bz1 * az2)
         (bx2 * az0 + by2 * az1 + bz2 * az2)

--------------------------------------------------------------------------------
--  



straightTurn :: Turn
straightTurn =
    Turn 1 0 0
         0 1 0
         0 0 1

leftTurn :: Turn 
leftTurn =
    Turn 0 0 (-1)
         0 1 0
         1 0 0


rightTurn :: Turn
rightTurn =
    Turn 0 0 1
         0 1 0
         (-1) 0 0 

upTurn :: Turn
upTurn =
    Turn 0 1 0
         (-1) 0 0 
         0 0 1

downTurn :: Turn
downTurn =
    Turn 0 (-1) 0
         1 0 0
         0 0 1

clockTurn :: Turn
clockTurn =
    Turn 1 0 0
         0 0 1
         0 (-1) 0


anticlockTurn :: Turn
anticlockTurn =
    Turn 1 0 0
         0 0 (-1)
         0 1 0


backTurn :: Turn -- ( the only one with two (-1) )
backTurn =
    Turn (-1) 0 0 
         0 1 0
         0 0 (-1)



-- | it happens that the inverse is just the transpose :)
turnInverse :: Turn -> Turn
turnInverse (Turn x0 x1 x2
                  y0 y1 y2
                  z0 z1 z2) = 
    Turn x0 y0 z0
         x1 y1 z1
         x2 y2 z2



-- | the difference turn -> turn'
turnDiff :: Turn -> Turn -> Turn
turnDiff turn turn' =
    turn' `mappend` (turnInverse turn)



--------------------------------------------------------------------------------
--  direction


data Dir = 
    Dir
    {
        dirX :: !Z3,
        dirY :: !Z3,
        dirZ :: !Z3
    } deriving Eq


direction :: Turn -> Dir
direction (Turn x0 x1 x2
                _  _  _
                _  _  _) =
    Dir x0 x1 x2


leftDir :: Dir
leftDir =
    Dir 0 0 (-1)

rightDir :: Dir
rightDir =
    Dir 0 0 1

backwardDir :: Dir
backwardDir =
    Dir (-1) 0 0

forwardDir :: Dir
forwardDir = 
    Dir 1 0 0

downDir :: Dir
downDir =
    Dir 0 (-1) 0

upDir :: Dir
upDir =
    Dir 0 1 0


-- tmp
instance Show Turn where
    show (Turn x0 x1 x2 y0 y1 y2 z0 z1 z2) = 
        "Turn (" ++ show x0 ++ " " ++ show x1 ++ " " ++ show x2 ++ ") (" ++
                    show y0 ++ " " ++ show y1 ++ " " ++ show y2 ++ ") (" ++
                    show z0 ++ " " ++ show z1 ++ " " ++ show z2 ++ ")"


instance Show Dir where
    show (Dir x y z) =
        "Dir " ++ show x ++ " " ++ show y ++ " " ++ show z ++ " "