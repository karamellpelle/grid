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
{-# LANGUAGE FlexibleContexts #-}
module Game.LevelPuzzle.Do.Grid.StepDT
  (
    collisionPlay,
    collisionComplete,

  ) where

import MyPrelude
import Game

import Game.Grid
import Game.Grid.StepDT
import Game.Grid.Do
import Game.LevelPuzzle


collisionPlay :: Collision s LevelPuzzleWorld
collisionPlay =
    Collision
    {   
        onPathNode = \path s grid lvl -> do
            -- push segment
            path' <- case levelpuzzleIsPuzzle lvl of
                     False -> pathEatContinue path
                     True  -> pathEatWait path 

            -- handle collisions
            handleCollisions path' lvl >>= \(path'', lvl') -> 
                    
                -- decrease number of remaining segments
                case levelpuzzleSegmentsCount lvl' of

                    0   -> return (path'', s, grid, handleZero lvl')

                    1   -> return (path'', s, grid, handleOne lvl')

                    n   -> return (path'', s, grid, handleN lvl')

    }
    where 
      handleZero lvl =
          lvl
      handleOne lvl =
          worldPushEvent (lvl {levelpuzzleSegmentsCount = levelpuzzleSegmentsCount lvl - 1})
                         EventNullSegments
      handleN lvl =
          lvl { levelpuzzleSegmentsCount = levelpuzzleSegmentsCount lvl - 1 }


collisionComplete :: Collision s LevelPuzzleWorld
collisionComplete =
    Collision
    {   
        onPathNode = \path s grid lvl -> do
            path' <- case levelpuzzleIsPuzzle lvl of
                     False -> pathEatContinue path
                     True  -> pathEatWait path 

            return (path', s, grid, lvl)

    }






--------------------------------------------------------------------------------
--  handleCollisions

-- | checkColDotPlain   -?-> 
--   checkColDotBonus   -?->
--   checkColDotTele    -?->
--   checkColDotFinish  -?->
--   checkColWall       -?->

handleCollisions :: Path -> LevelPuzzleWorld -> IO (Path, LevelPuzzleWorld)
handleCollisions path lvl =
    case levelpuzzleContent lvl of
        cnt -> do
          room <- roomarrayAt (contentRooms cnt) (contentRoom cnt)
          checkColDotPlain path lvl room 



--------------------------------------------------------------------------------
--  DotPlain

checkColDotPlain :: Path -> LevelPuzzleWorld -> Room -> IO (Path, LevelPuzzleWorld)
checkColDotPlain path lvl room = 
    helper path lvl room (roomDotPlainSize room) 0
    where
      helper path lvl room size ix = 
          if ix == size then checkColDotBonus path lvl room

            else do
              dot <- dotplainarrayAt (roomDotPlain room) ix
              if isCol path dot
                then do
                  path' <- pathEat path dot
                  lvl' <- levelpuzzleEat lvl dot
                  dotplainarrayWrite (roomDotPlain room) ix $ dotEat dot
                  return (path', lvl')

                else helper path lvl room size (ix + 1)

      isCol path dot =
          (dotplainCount dot /= 0) && (pathNode path == dotplainNode dot)
      
      dotEat dot = 
          dot { dotplainCount = dotplainCount dot - 1 }

      pathEat path dot = 
          return path

      levelpuzzleEat lvl dot = do
          -- push (begin, end) to room
          let cnt = levelpuzzleContent lvl
              path = gridPath $ contentGrid cnt
              begin = contentEatPathBegin cnt
              end = ((pathArrayEnd path) + 1) `mod` pathArraySize path
          roomarrayModifyAt (contentRooms cnt) (contentRoom cnt) $ \room -> 
              room
              {
                  roomPathBeginEnd = roomPathBeginEnd room ++ [(begin, end)]
              }

          -- update content
          let lvl' = levelpuzzleModifyContent lvl $ \cnt -> 
                     cnt
                     {
                        contentRoom = dotplainRoom dot,
                        contentEatRoom = contentRoom cnt,
                        contentEatTick = worldTick lvl,
                        contentEatPathBegin = end
                     }

          return $ worldPushEvent lvl' (EventPathEatDotPlain dot)

--------------------------------------------------------------------------------
--  DotBonus

checkColDotBonus :: Path -> LevelPuzzleWorld -> Room -> IO (Path, LevelPuzzleWorld)
checkColDotBonus path lvl room = 
    helper path lvl room (roomDotBonusSize room) 0
    where
      helper path lvl room size ix = 
        if ix == size then checkColDotTele path lvl room

          else do
            dot <- dotbonusarrayAt (roomDotBonus room) ix
            if isCol path dot
              then do
                path' <- pathEat path dot
                lvl' <- levelpuzzleEat lvl dot
                dotbonusarrayWrite (roomDotBonus room) ix $ dotEat dot
                return (path', lvl')

              else helper path lvl room size (ix + 1)

      isCol path dot =
          (dotbonusCount dot /= 0) && (pathNode path == dotbonusNode dot)

      dotEat dot =
          dot { dotbonusCount = dotbonusCount dot - 1 }
      
      pathEat path dot = 
          return path

      levelpuzzleEat lvl dot = do
          let lvl' = lvl 
                     {
                        levelpuzzleSegmentsCount = levelpuzzleSegmentsCount lvl + 
                                                   dotbonusAdd dot
                     }
          return $ worldPushEvent lvl' (EventPathEatDotBonus dot)


--------------------------------------------------------------------------------
--  DotTele

checkColDotTele :: Path -> LevelPuzzleWorld -> Room -> IO (Path, LevelPuzzleWorld)
checkColDotTele path lvl room = 
    helper path lvl room (roomDotTeleSize room) 0
    where
      helper path lvl room size ix = 
          if ix == size then checkColDotFinish path lvl room

            else do
              dot <- dottelearrayAt (roomDotTele room) ix
              if isCol path dot
                -- collision DotPlain
                then do
                  path' <- pathEat path dot
                  lvl' <- levelpuzzleEat lvl dot
                  dottelearrayWrite (roomDotTele room) ix $ dotEat dot
                  return (path', lvl')

                else helper path lvl room size (ix + 1)

      isCol path dot = 
          (dotteleCount dot /= 0) && (pathNode path == dotteleNode dot)
      
      dotEat dot =
          dot { dotteleCount = dotteleCount dot - 1 }

      pathEat path dot =
          return $ pathSetNode path $ dotteleNode' dot
      
      levelpuzzleEat lvl dot = 
          return $ worldPushEvent lvl (EventPathEatDotTele dot)


--------------------------------------------------------------------------------
--  DotFinish

checkColDotFinish :: Path -> LevelPuzzleWorld -> Room -> IO (Path, LevelPuzzleWorld)
checkColDotFinish path lvl room = 
    helper path lvl room (roomDotFinishSize room) 0
    where
      helper path lvl room size ix = 
          if ix == size then checkColWall path lvl room

            else do
              dot <- dotfinisharrayAt (roomDotFinish room) ix
              if isCol path dot
                -- collision DotPlain
                then do
                  path' <- pathEat path dot
                  lvl' <- levelpuzzleEat lvl dot
                  
                  return (path', lvl')

                else helper path lvl room size (ix + 1)

      isCol path dot = 
          (pathNode path == dotfinishNode dot)
      
      pathEat path dot = 
          return path  { pathWaiting = True }
      
      levelpuzzleEat lvl dot =
          return $ worldPushEvent lvl (EventPathEatDotFinish dot)
          

--------------------------------------------------------------------------------
--  Wall

checkColWall :: Path -> LevelPuzzleWorld -> Room -> IO (Path, LevelPuzzleWorld)
checkColWall path lvl room = 
    helper path lvl room (roomWallSize room) 0
    where
      helper path lvl room size ix = 
          if ix == size then return (path, lvl)

            else do
              let wall = wallarrayAt (roomWall room) ix
              if isCol path wall
                then do
                  path' <- pathEat path wall
                  lvl' <- levelpuzzleEat lvl wall

                  return (path', lvl')

                else helper path lvl room size (ix + 1)

      isCol path wall = 
        case nodeDiff (wallNode wall) (pathNode path) of
          p ->  let x = wallX wall 
                    y = wallY wall
                    n = nodeCross x y
                    ix = nodeInner p x
                    iy = nodeInner p y
                in  nodeInner p n == 0 && 0 <= ix && ix <= nodeInner x x
                                       && 0 <= iy && iy <= nodeInner y y
                                       && if wallIsDouble wall 
                                          then True
                                          else isNotWallDir path n

      isNotWallDir path (Node n0 n1 n2) =
          let Turn a0 a1 a2 _ _ _ _ _ _ = segmentTurn $ path0Last path
          in  fI a0 * n0 + fI a1 * n1 + fI a2 * n2 <= 0

      pathEat path wall = 
          return path
      
      levelpuzzleEat lvl wall = 
          return $ worldPushEvent lvl (EventPathEatWall wall)










