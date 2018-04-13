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
module Game.Grid.GridWorld.Path
  (
      Path (..),
      PathEvent (..),

      makePath,
      makePathNull,
      destroyPath,

      pathNode,
      pathTurn,
      pathPush,


      module Game.Grid.GridWorld.Segment,
      module Game.Grid.GridWorld.SegmentArray,

  ) where


import MyPrelude
import Game.MEnv
import Game.Values

import Game.Grid.GridWorld.Node
import Game.Grid.GridWorld.Turn
import Game.Grid.GridWorld.Segment
import Game.Grid.GridWorld.SegmentArray


data Path = 
    Path
    {
        -- path1
        pathCurrent :: !Segment,        -- ^ current
        pathAlpha :: !Float,            -- ^ current alpha, [0, 1) typically
   
        -- path0
        pathArray :: SegmentArray,      -- ^ segments before current 
        pathArraySize :: !UInt,         -- ^ size of pathArray
        pathArrayBegin :: !UInt,        -- ^ begin ix
        pathArrayEnd :: !UInt,          -- ^ end ix


        pathSpeed :: !Float,            -- ^ speed of alpha
        pathWaiting :: !Bool,           -- ^ pause
        pathEvents :: [PathEvent],      -- ^ events

        -- control state (pushed next physical state difference)
        pathTurnState :: !(Maybe Turn)   -- ^ maybe next turn relative to path

    }


data PathEvent =
    EventNewSegment



-- | current node
pathNode :: Path -> Node
pathNode =
    segmentNode . pathCurrent


-- | current turn relative to world
pathTurn :: Path -> Turn
pathTurn = 
    segmentTurn . pathCurrent


makePath :: UInt -> MEnv' Path
makePath maxSize = do
    let size = maxSize + 1
    return  Path
            {
                pathCurrent = mempty,
                pathAlpha = 0.0,
                pathArray = makeSegmentArray size,
                pathArraySize = size,
                pathArrayBegin = 0,
                pathArrayEnd = 0,
                pathSpeed = valueGridPathSpeed,
                pathWaiting = False,
                pathTurnState = Nothing,
                pathEvents = []
            }

-- | null path
makePathNull :: MEnv' Path
makePathNull =
    makePath 0


destroyPath :: Path -> MEnv' ()
destroyPath path = do
    return ()

pathPush :: Path -> Segment -> IO Path
pathPush path seg = do
    let size = pathArraySize path
        begin = pathArrayBegin path
        end = pathArrayEnd path
        end' = succMod size end
        begin' = if end' == begin then succMod size begin else begin
        array' = segmentarrayWrite (pathArray path) end seg
    

    return path
           { 
              pathArray = array',        
              pathArrayBegin = begin',
              pathArrayEnd = end'
           }
    
    where
      succMod size ix =
          (ix + 1) `mod` size
