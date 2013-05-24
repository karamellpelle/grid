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

#ifdef GRID_STYLE_FANCY
      module Game.Grid.GridWorld.Path.Fancy,
#endif
#ifdef GRID_STYLE_PLAIN
      module Game.Grid.GridWorld.Path.Plain,
#endif

  ) where


import MyPrelude
import Game

import Game.Grid.GridWorld.SegmentArray
import Game.Grid.GridWorld.Segment
import Game.Grid.GridWorld.Node
import Game.Grid.GridWorld.Turn

#ifdef GRID_STYLE_FANCY
import Game.Grid.GridWorld.Path.Fancy
#endif
#ifdef GRID_STYLE_PLAIN
import Game.Grid.GridWorld.Path.Plain
#endif


data Path = 
    Path
    {
        -- Path1
        pathCurrent :: !Segment,        -- ^ current
        pathAlpha :: !Float,            -- ^ current alpha, [0, 1) typically
   
        -- Path0
        pathArray :: SegmentArray,      -- ^ segments before current 
        pathArraySize :: !UInt,         -- ^ size of pathArray
        pathArrayBegin :: !UInt,        -- ^ begin ix
        pathArrayEnd :: !UInt,          -- ^ end ix

        pathOverflowCount :: !UInt,     -- ^ counting overwrites
        pathSpeed :: !Float,            -- ^ speed of alpha
        pathWaiting :: !Bool,           -- ^ pause
        pathEvents :: [PathEvent],      -- ^ events
        
        -- output state
        pathPathOutput :: !PathOutput,  -- ^ data used by output

        -- control state. such should ideally be extern to Path
        pathTurnState :: ![Turn],       -- ^ next turns relative to path
        pathTurnStateX :: !Float,       -- ^ X drag
        pathTurnStateY :: !Float,       -- ^ Y drag
        pathTurnStateHandled :: !Bool   -- ^ handled 
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

#ifdef DEBUG
    when (valueGridMaxPathSize < maxSize) $ 
        error ("makePath: " ++ show maxSize ++ " is too big size")
#endif

    let size = maxSize + 1
    po <- makePathOutput size
    return  Path
            {
                pathCurrent = mempty,
                pathAlpha = 0.0,
                pathArray = makeSegmentArray size,
                pathArraySize = size,
                pathArrayBegin = 0,
                pathArrayEnd = 0,
                pathPathOutput = po,
                pathOverflowCount = 0,
                pathSpeed = 1.0,
                pathWaiting = False,
                pathEvents = [],

                pathTurnState = [],
                pathTurnStateX = 0.0,
                pathTurnStateY = 0.0,
                pathTurnStateHandled = False
            }

-- | null path
makePathNull :: MEnv' Path
makePathNull =
    makePath 0


destroyPath :: Path -> MEnv' ()
destroyPath path = do
    destroyPathOutput $ pathPathOutput path


pathPush :: Path -> Segment -> IO Path
pathPush path seg = do
    let size = pathArraySize path
        begin = pathArrayBegin path
        end = pathArrayEnd path

        end' = succMod size end
        array' = segmentarrayWrite (pathArray path) end seg
    
    -- PathOutput
    po' <- writePathOutput (pathPathOutput path) size end seg

    if end' == begin
      then return path
                  {
                      pathArray = array',        
                      pathArrayEnd = end',
                      pathPathOutput = po',

                      pathArrayBegin = succMod size begin, 
                      pathOverflowCount = pathOverflowCount path + 1
                  }

      else return path
                  {
                      pathArray = array',        
                      pathArrayEnd = end',
                      pathPathOutput = po'
                  }
    
