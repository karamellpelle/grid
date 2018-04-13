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
module Game.Memory.Helpers.Make
  (
    makeMemoryWorld,
    destroyMemoryWorld,

  ) where


import MyPrelude
import Game

import Game.Grid
import Game.Memory.MemoryWorld
import Game.Memory.MemoryWorld.OutputState

import Data.Word
import Control.Monad.State


-- | create the memory world, starting at level 'ix'
makeMemoryWorld :: UInt -> MEnv' MemoryWorld
makeMemoryWorld mix = do
    case drop mix theLevels of
        []          -> 
            error $ "makeMemoryWorld: no level ix " ++ show mix

        (next:nexts) -> do
            let size = length' next + 1
            grid <- makeGridWorldView size valueMemoryGridView
            state <- makeOutputState mix

            return  MemoryWorld
                    {
                        memoryLevelIx = mix,
                        memoryLevelSize = size,
                        memoryLevelCurrent = next,
                        memoryLevelNexts = nexts,

                        memoryGrid = gridModifyPath grid $ \path ->
                                     path { pathSpeed = valueMemoryPathSpeed },
                        memoryFood = [],
                        memoryEvents = [],
                        memorySpaceRef = mempty,

                        memoryIsFailure = False,
                        memoryFailureIx = 0,
                        memoryFailureSegment = mempty,
                        memoryFailureTick = 0.0,

                        memoryOutputState = state
                    }

    where
        drop i as = case i of
            0   -> as
            i   -> case as of
                (a:as)  -> drop (i - 1) as
                []      -> error $ "makeMemoryWorld: no level " ++ show mix



--------------------------------------------------------------------------------
--  

destroyMemoryWorld :: MemoryWorld -> MEnv' ()
destroyMemoryWorld mem = do
    destroyGridWorld $ memoryGrid mem 




--------------------------------------------------------------------------------
--  

theLevels :: [ [Turn] ]
theLevels = 
    seedMyGen mygenSeed $ helper 0 
    where
      helper ix = do 
          l <- genLevel ix
          ls <- helper (ix + 1)
          return (l:ls)

newtype MyGen =
    MyGen Word32


mygenSeed :: Word32
mygenSeed = 
    0

mygenA :: Word32
mygenA = 
    302002570

mygenB :: Word32
mygenB =
    42351223

mygenM :: Word32
mygenM = 
    230002570

makeMyGen :: Word32 -> MyGen
makeMyGen seed =
    MyGen seed

{-
-- debug in ghci:
mid n = sum (seedMyGen mySeed $ replicateM n uniform) / fromIntegral n
columns = 112
mapPoint alpha = replicate (truncate $ alpha * fromIntegral columns) ' ' ++ "|"
draw n = mapM_ putStrLn $ map mapPoint $ seedMyGen mySeed $ replicateM n uniform
-}


type MyGenM = 
    State MyGen


seedMyGen :: Word32 -> MyGenM a -> a
seedMyGen seed ma = 
    evalState ma (makeMyGen seed)


uniform :: MyGenM Float
uniform = 
    state $ \(MyGen s) -> (fromIntegral s / fromIntegral mygenM, MyGen $ (mygenA * s + mygenB) `mod` mygenM)


-- | create level ix
genLevel :: UInt -> MyGenM [Turn]
genLevel ix =
    let turnsSize = subgroup ix + 1
        size' = subgroupSize (subgroup ix)
        jx' = subgroupIx ix
        size = groupSize (group ix)
        jx = subgroupBegin (subgroup ix) + (size' - 1 - jx')
        alpha = fromIntegral (size - jx) / fromIntegral size
        alpha' = (1 - alpha) * 0.4 + alpha
    in  genMakeLevel turnsSize alpha'

-- | make level of difficulty 'alpha' with 'size' turns. alpha in [0, 1] with 0 easiest, 1 difficultst.
genMakeLevel :: UInt -> Float -> MyGenM [Turn]
genMakeLevel size alpha =
    myReplicateM size $ uniform >>= \u -> 
        let lim = alpha * 1
            lim0 = lim * 0.25 --0.2
            lim1 = lim * 0.5  --0.4
            lim2 = lim * 0.75 --0.6
            lim3 = lim * 1.0  --0.8
            ret | u < lim0  = leftTurn
                | u < lim1  = rightTurn
                | u < lim2  = downTurn
                | u < lim3  = upTurn
                | otherwise = straightTurn

        in return ret

myReplicateM :: Monad m => UInt -> m a -> m [a]
myReplicateM n ma =
    sequence $ helper n ma
    where
        helper 0 ma = []
        helper n ma = ma : helper (n - 1) ma



{-
level n =
    let turnsSize = subgroup n + 1
        size' = subgroupSize (subgroup n)
        ix' = subgroupIx n
        size = groupSize (group n)
        ix = subgroupBegin (subgroup n) + (size' - 1 - ix')
        alpha = fromIntegral (size - ix) / fromIntegral size
        alpha' = (1 - alpha) * 0.2 + alpha
    in  (turnsSize, alpha) --genMakeLevel turnsSize alpha'
-}


-- the following functions can also be written with formulas:


-- | size of subgroup 'subgroup'
subgroupSize :: UInt -> UInt
subgroupSize subgroup =
    subgroup + 1

-- | size of group 'group'
groupSize :: UInt -> UInt
groupSize group =
    sum (map subgroupSize [0..group])

-- | begin index of group
groupBegin :: UInt -> UInt
groupBegin group =
    case group of 
        0     -> 0
        group -> groupSize (group - 1) + groupBegin (group - 1)

-- | containing group
group :: UInt -> UInt
group n =
    helper 0 n
    where
        helper m n =
            if n < groupBegin (m + 1) then m
                                      else helper (m + 1) n

-- | index relative to containing group 
groupIx :: UInt -> UInt
groupIx n =
    n - groupBegin (group n)


-- | begin index of subgroup, relative to group
subgroupBegin :: UInt -> UInt
subgroupBegin subgroup =
    case subgroup of
        0         -> 0
        subgroup  -> subgroupSize (subgroup - 1) + subgroupBegin (subgroup - 1)

-- | containing subgroup
subgroup :: UInt -> UInt
subgroup n = 
    helper 0 (groupIx n)
    where
      helper m ix = 
          if ix < subgroupBegin (m + 1) then m
                                        else helper (m + 1) ix

-- | index relative to containing subgroup
subgroupIx :: UInt -> UInt
subgroupIx n =
    groupIx n - subgroupBegin (subgroup n)



