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
module Game.Grid.Modify
  (
    controlGrid,

    controlEmpty,
    controlCamera,
    controlCameraPathWait,
    controlCameraPathContinue,

    inputCamera,
    inputPathWait,
    inputPathContinue,

  ) where


import MyPrelude
import Game
import Game.Grid


--------------------------------------------------------------------------------
--  controlGrid


controlGrid :: (s -> GridWorld -> b -> MEnv' (s, GridWorld, b)) -> 
               (s -> GridWorld -> b -> MEnv' (s, GridWorld, b))
controlGrid control = \s grid b -> do

    -- begin + control
    (s', grid', b') <- control s (gridClearEvents grid) b
    
    -- update
    return (s', updateCameraCommands grid', b')



--------------------------------------------------------------------------------
--  

updateCameraCommands :: GridWorld -> GridWorld
updateCameraCommands grid = 
    if gridCameraCommandTick grid <= worldTick grid 
      then case gridCameraCommands grid of
          []          -> grid
          (cmd:cmds)  -> 
              let grid' = gridModifyCamera grid $ \cam ->
                          cameraEatCameraCommand cam cmd
              in  grid'
                  {
                      gridCameraCommands = cmds,
                      gridCameraCommandTick = worldTick grid' + cameracommandTicks cmd,
                      gridCameraCommandCount = gridCameraCommandCount grid' + 1,
                      gridCameraCommandScale = 1.0 / rTF (cameracommandTicks cmd)
                  }
      else grid



--------------------------------------------------------------------------------
--  controls to use

-- | no control
controlEmpty :: s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
controlEmpty = \s grid b -> do
    return (s, grid, b)


-- | control camera from input
controlCamera :: s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
controlCamera = \s grid b -> do
    grid' <- inputCamera grid
    return (s, grid', b)


-- | control camera and path from input
controlCameraPathWait :: s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
controlCameraPathWait = \s grid b -> do
    grid' <- inputCamera grid
    grid'' <- inputPathWait grid'
    return (s, grid'', b)


-- | control camera and path from input
controlCameraPathContinue :: s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
controlCameraPathContinue = \s grid b ->  do
    grid' <- inputCamera grid
    grid'' <- inputPathContinue grid'
    return (s, grid'', b)





--------------------------------------------------------------------------------
-- 

-- | input for Camera
inputCamera :: GridWorld -> MEnv' GridWorld
inputCamera grid = do
    gridModifyCameraM grid $ \camera -> do

        -- save current view
        camera' <- keysTouchHandleCircleTouched camera $ \_ _ ->
            let View a b c = cameraCurrentView camera
            in  camera
                {
                    -- current view
                    cameraView = View a b c,    

                    cameraViewIdeal = View (a + 1.0) (b + 1.0) (c + 1.0),
                    cameraViewAAlpha = 0.0,
                    cameraViewAAlphaIdeal = 0.0,
                    cameraViewASpeed = valueGridCameraViewASpeed,
                    cameraViewBAlpha = 0.0,
                    cameraViewBAlphaIdeal = 0.0,
                    cameraViewBSpeed = valueGridCameraViewBSpeed,
                    cameraViewCAlpha = 0.0,
                    cameraViewCAlphaIdeal = 0.0,
                    cameraViewCSpeed = valueGridCameraViewCSpeed
                }

        -- move camera
        keysTouchHandleCircleDrag camera' $ \ticks (x, y) r (x', y') r' -> 
            let View a b c = cameraView camera'
                aAlphaIdeal = (x' - x) * valueGridCameraViewASens
                bAlphaIdeal = keepInside (bMin - b) (bMax - b)
                                         ((y' - y) * valueGridCameraViewBSens)
                cAlphaIdeal = keepInside (valueGridCameraViewCMin - c) 
                                         (valueGridCameraViewCMax - c) 
                                         ((r - r') * valueGridCameraViewCSens)
            in  camera'
                {
                    cameraViewAAlphaIdeal = aAlphaIdeal,
                    cameraViewBAlphaIdeal = bAlphaIdeal, 
                    cameraViewCAlphaIdeal = cAlphaIdeal
                }
    
    where
      gridModifyCameraM grid f = do
          cam' <- f $ gridCamera grid
          return grid { gridCamera = cam' }
          
      bMin = (-0.249 * tau)
      bMax = 0.249 * tau



-- | input for a Path waiting at each Node
inputPathWait :: GridWorld -> MEnv' GridWorld
inputPathWait grid = do
    
    gridModifyPathM grid $ \path -> do
        controlPathRoll >>= \maybeRoll -> case maybeRoll of

            -- roll
            Just roll -> do
                return $ pathModifyTurn path (mappend roll)

            -- turn
            Nothing   -> do
                inputDrag path >>= inputDrop 

    where
      inputDrag path = 
          keysTouchHandlePointDrag path $ \_ (x, y) (x', y') -> 
                                   path { pathTurnStateX = x' - x, 
                                          pathTurnStateY = y' - y }
      
      inputDrop path = 
          keysTouchHandlePointDrop path $ \_ _ _ -> 
              let dx = pathTurnStateX path
                  dy = pathTurnStateY path
                  path'  | abs dx < abs dy = pathY
                         | abs dy < abs dx = pathX
                         | otherwise       = takeTurn path straightTurn
                  pathX  | dx <= (-sens)   = takeTurn path leftTurn
                         | (sens) <= dx    = takeTurn path rightTurn
                         | otherwise       = takeTurn path straightTurn
                  pathY  | dy <= (-sens)   = takeTurn path upTurn
                         | (sens) <= dy    = takeTurn path downTurn
                         | otherwise       = takeTurn path straightTurn
              in  path' { pathTurnStateX = 0.0, pathTurnStateY = 0.0 }

      takeTurn path turn = 
          if pathWaiting path
            then pathModifyTurn (path { pathWaiting = False }) (mappend turn)
            else path { pathTurnState = pathTurnState path ++ [turn] }
      
      takeTurnStraight path = 
          path { pathWaiting = False }
      
      sens = valueGridControlWaitSens




-- | input for a Path continuing at each Node
inputPathContinue :: GridWorld -> MEnv' GridWorld
inputPathContinue grid = do

    gridModifyPathM grid $ \path -> do
        controlPathRoll >>= \maybeRoll -> case maybeRoll of

            -- roll
            Just roll -> do
                return $ pathModifyTurn path (mappend roll)

            -- turn
            Nothing   -> do
                -- (if touching then TurnStateHandled = False)
                inputDrag path >>= inputDrop

    where
      inputDrag path = 
          keysTouchHandlePointDrag path $ \_ (x, y) (x', y') -> 
                                   path { pathTurnStateX = x' - x, 
                                          pathTurnStateY = y' - y }

      inputDrop path = do
          keysTouchHandlePointDrop path $ \_ _ _ ->
              let dx = pathTurnStateX path
                  dy = pathTurnStateY path
                  path'  | pathTurnStateHandled path = path
                         | abs dx < abs dy = pathY
                         | abs dy < abs dx = pathX
                         | otherwise       = path
                  pathX  | dx <= (-sens)   = takeTurn path leftTurn
                         | (sens) <= dx    = takeTurn path rightTurn
                         | otherwise       = path
                  pathY  | dy <= (-sens)   = takeTurn path upTurn
                         | (sens) <= dy    = takeTurn path downTurn
                         | otherwise       = path

              in  path' { pathTurnStateHandled = False,
                          pathTurnStateX = 0.0,
                          pathTurnStateY = 0.0 }

      takeTurn path turn = 
          path { pathTurnState = pathTurnState path ++ [turn] }

      sens = valueGridControlContinueSens0



controlPathRoll :: MEnv' (Maybe Turn)
controlPathRoll = do
    (wth, hth) <- screenShape
    keysTouchHandlePointDrop Nothing $ \_ (x, y) _ -> 
        let maybe | x * wth < valueGridRollSize       = Just anticlockTurn
                  | (1 - x) * wth < valueGridRollSize = Just clockTurn
                  | otherwise                         = Nothing
        in  maybe





