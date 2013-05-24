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
-- fixme: strict arguments!
module Game.Grid.Helpers.Path
  (
    gridPathStart,
    gridPathWait,
    gridModifyPath,
    gridModifyPathM,

    pathSize,
    pathSegmentAt,
    path0Last,
    pathClear,
    pathClearEvents,
    pathModifyTurn,
    pathPushCurrent,
    pathPushSegment,
    pathPushTurn,
    pathPushNode,
    pathEatTurn,
    pathAppendSegment,
    pathAppendNode,
    pathAppendTurn,
    pathSetCurrent,
    pathSetNode,
    pathSetTurn,
    pathHasEventNewSegment,
    pathNodeNext,

  ) where

import MyPrelude
import Game
import Game.Grid.GridWorld
import Game.Grid.GridWorld.SegmentArray
import Game.Grid.Helpers.Segment



gridModifyPath :: GridWorld -> (Path -> Path) -> GridWorld
gridModifyPath grid f =
    grid { gridPath = f $ gridPath grid } 


gridModifyPathM :: GridWorld -> (Path -> MEnv' Path) -> MEnv' GridWorld
gridModifyPathM grid f = do
    path' <- f $ gridPath grid
    return grid { gridPath = path' }

gridPathStart :: GridWorld -> GridWorld
gridPathStart grid =
    grid { gridPath = (gridPath grid) { pathWaiting = False } }


gridPathWait :: GridWorld -> GridWorld
gridPathWait grid =
    grid { gridPath = (gridPath grid) { pathWaiting = True } }

    
pathModifyTurn :: Path -> (Turn -> Turn) -> Path
pathModifyTurn path f =
    let seg = pathCurrent path
        seg' = seg { segmentTurn = f $ segmentTurn seg }

    in path { pathCurrent = seg' }

pathNodeNext :: Path -> Node
pathNodeNext path =
    segmentNodeNext (pathCurrent path)


-- | size of path
pathSize :: Path -> UInt
pathSize path =
    let size = pathArraySize path
        begin = pathArrayBegin path
        end = pathArrayEnd path
    in  (end + size - begin) `mod` size


-- | segment at index (path1 not included)
pathSegmentAt :: Path -> UInt -> Segment
pathSegmentAt path ix =
    let begin = pathArrayBegin path
        size = pathArraySize path
        array = pathArray path
    in  segmentarrayRead array ((begin + ix) `mod` size)


path0Last :: Path -> Segment
path0Last path =
    let end = pathArrayEnd path
        size = pathArraySize path
        array = pathArray path
    in segmentarrayRead array ((end + (size - 1)) `mod` size)


-- | reset path
pathClear :: Path -> MEnv' Path
pathClear path = do
    po' <- clearPathOutput $ pathPathOutput path
    return path
           {
               pathCurrent = mempty,
               pathWaiting = False,
               pathAlpha = 0.0,
               pathArrayBegin = 0,
               pathArrayEnd = 0,
               pathEvents = [],
               pathPathOutput = po',

               pathTurnState = [],
               pathTurnStateX = 0.0,
               pathTurnStateY = 0.0,
               pathTurnStateHandled = False
           }

pathClearEvents :: Path -> Path
pathClearEvents path =
    path { pathEvents = [] }


-- | push new path1 of path, adding previous path1 to path0
pathPushCurrent :: Path -> Segment -> IO Path
pathPushCurrent path seg = do
    path' <- pathPush path $ pathCurrent path
    return path'
           {
              pathCurrent = seg,
              pathAlpha = 0.0,
              --pathAlpha = if 1.0 <= pathAlpha path 
              --            then pathAlpha path - 1.0
              --            else 0.0,
              pathEvents = pathEvents path ++ [EventNewSegment]
           }

pathPushTurn :: Path -> Turn -> IO Path
pathPushTurn path t =
    pathPushCurrent path $ segmentAppendTurn (pathCurrent path) t 

pathPushNode :: Path -> Node -> IO Path
pathPushNode path n =
    pathPushCurrent path $ segmentAppendNode (pathCurrent path) n

pathPushSegment :: Path -> Segment -> IO Path
pathPushSegment path seg =
    pathPushCurrent path $ seg `mappend` (pathCurrent path)


-- | let path take a step in direction defined by 't'
pathEatTurn :: Path -> Turn -> IO Path
pathEatTurn path t =
    let node' = segmentNodeNext $ pathCurrent path
        turn' = t `mappend` pathTurn path
    in pathPushCurrent path $ Segment node' turn'



-- | append segment to path current
pathAppendSegment :: Path -> Segment -> Path
pathAppendSegment path seg =
    path { pathCurrent = seg `mappend` pathCurrent path }

pathAppendTurn :: Path -> Turn -> Path
pathAppendTurn path t =
    path { pathCurrent = segmentAppendTurn (pathCurrent path) t }

pathAppendNode :: Path -> Node -> Path
pathAppendNode path n =
    path { pathCurrent = segmentAppendNode (pathCurrent path) n }


-- | set segment as path current
pathSetCurrent :: Path -> Segment -> Path
pathSetCurrent path seg =
    path { pathCurrent = seg }

pathSetNode :: Path -> Node -> Path
pathSetNode path node =
    path { pathCurrent = (pathCurrent path) { segmentNode = node } }


pathSetTurn :: Path -> Turn -> Path
pathSetTurn path turn =
    path { pathCurrent = (pathCurrent path) { segmentTurn = turn } }


pathHasEventNewSegment :: Path -> Bool
pathHasEventNewSegment path =
    -- assuming only 1 event!
    case pathEvents path of
        []    -> False
        _     -> True

