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
import Game.Grid.GridWorld
import Game.Grid.Helpers
import Graphics.UI.GLFW

--------------------------------------------------------------------------------
--  controlGrid


controlGrid :: (s -> GridWorld -> b -> MEnv' (s, GridWorld, b)) -> 
               (s -> GridWorld -> b -> MEnv' (s, GridWorld, b))
controlGrid control = \s grid b -> do
    -- begin + control
    (s', grid', b') <- control s (gridClearEvents grid) b
    
    -- update
    return (s', updateCameraCommands grid', b')




updateCameraCommands :: GridWorld -> GridWorld
updateCameraCommands grid = 
    if gridCameraCommandTick grid <= worldTick grid 
      then case gridCameraCommands grid of
          []          -> grid
          (cmd:cmds)  -> let grid' = gridModifyCamera grid $ \cam ->
                                     cameraEatCameraCommand cam cmd
                         in  grid'
                             {
                                gridCameraCommands = cmds,
                                gridCameraCommandTick = worldTick grid + 
                                                        cameracommandTicks cmd
                             }
      else grid



--------------------------------------------------------------------------------
--  controls for use

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

inputCamera :: GridWorld -> MEnv' GridWorld
inputCamera grid = do
    gridModifyCameraM grid $ \camera -> do
        -- save current view
        camera' <- keysTouchHandleTouched camera $ \_ ->
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
        camera'' <- keysTouchHandleCircleVector camera' $ \ticks (x, y) r (x', y') r' -> 
            let View a b c = cameraView camera'
                cAlphaIdeal = keepInside (valueGridCameraViewCMin - c) 
                                         (valueGridCameraViewCMax - c) 
                                         ((r' - r) * valueGridCameraViewCSens)
            in  camera'
                {
                    cameraViewCAlphaIdeal = cAlphaIdeal
                }
        keysTouchHandlePointVector camera'' $ \ticks (x, y) (x', y') -> 
            let View a b c = cameraView camera''
                aAlphaIdeal = (x' - x) * valueGridCameraViewASens
                bAlphaIdeal = keepInside (bMin - b) (bMax - b)
                                         ((y - y') * valueGridCameraViewBSens)
                {-
                cAlphaIdeal = keepInside (valueGridCameraViewCMin - c) 
                                         (valueGridCameraViewCMax - c) 
                                         ((r - r') * valueGridCameraViewCSens)
                -}
            in  camera''
                {
                    cameraViewAAlphaIdeal = aAlphaIdeal,
                    cameraViewBAlphaIdeal = bAlphaIdeal
                    --cameraViewCAlphaIdeal = cAlphaIdeal
                }
    
    where
      gridModifyCameraM grid f = do
          cam' <- f $ gridCamera grid
          return grid { gridCamera = cam' }
          
      bMin = (-0.249 * tau)
      bMax = 0.249 * tau


inputPathWait :: GridWorld -> MEnv' GridWorld
inputPathWait grid = do
    gridModifyPathM grid $ 
        inputRoll >>>
        inputDirWait


inputPathContinue :: GridWorld -> MEnv' GridWorld
inputPathContinue grid = do
    gridModifyPathM grid $ 
        inputRoll >>>
        inputDirContinue

inputRoll = 
    (arrKey (CharKey '\'') $ \path -> 
        return (pathModifyTurn path $ \turn -> anticlockTurn `mappend` turn)) >>>
    (arrKey (CharKey 'J') $ \path -> 
        return (pathModifyTurn path $ \turn -> clockTurn `mappend` turn))

inputDirWait = 
    (\path -> case pathWaiting path of
        False   -> return path
        True    -> maybeTurnWait >>= \maybe -> case maybe of
            Nothing   -> return path
            Just turn -> let path' = pathModifyTurn path (mappend turn)
                         in  return path' { pathWaiting = False })

inputDirContinue = \path -> 
    maybeTurnContinue >>= \maybe -> return path { pathTurnState = maybe }
              
              

maybeTurnWait :: MEnv' (Maybe Turn)
maybeTurnWait = 
    
    arrM Nothing $ (arrKey (SpecialKey LEFT) $ \_ -> return $ Just leftTurn) >>>
                   (arrKey (SpecialKey RIGHT) $ \_ -> return $ Just rightTurn) >>>
                   (arrKey (SpecialKey DOWN) $ \_ -> return $ Just downTurn) >>>
                   (arrKey (SpecialKey UP) $ \_ -> return $ Just upTurn) >>>
                   (arrKey (SpecialKey RSHIFT) $ \_ -> return $ Just straightTurn)
    where
      arrM a fmb =
          fmb a

maybeTurnContinue :: MEnv' (Maybe Turn)
maybeTurnContinue = 
    
    arrM Nothing $ (arrKeyHold (SpecialKey LEFT) $ \_ -> return $ Just leftTurn) >>>
                   (arrKeyHold (SpecialKey RIGHT) $ \_ -> return $ Just rightTurn) >>>
                   (arrKeyHold (SpecialKey DOWN) $ \_ -> return $ Just downTurn) >>>
                   (arrKeyHold (SpecialKey UP) $ \_ -> return $ Just upTurn) >>>
                   (arrKeyHold (SpecialKey RSHIFT) $ \_ -> return $ Just straightTurn)
    where
      arrM a fmb =
          fmb a

arrKey key arr = \a ->
    keysKeyOnce key >>= \bool -> case bool of
        False   -> return a
        True    -> arr a

arrKeyHold key arr = \a -> do
    state <- io $ getKey key 
    case state of
        Release -> return a
        Press   -> arr a


{-
    gridModifyPathM grid $ \path -> do 
        controlPathRoll >>= \maybeRoll -> case maybeRoll of

            -- roll
            Just roll -> do
                return $ pathModifyTurn path 
                       $ \turn -> roll `mappend` turn

            -- turn
            Nothing   -> do
                case pathWaiting (gridPathA grid) of

                    -- only controllable when waiting
                    False   -> do
                        return path

                    True    -> do
                        controlPathTurnDrop >>= \maybeTurn -> case maybeTurn of
                            Nothing   -> 
                                return path 
                            
                            Just t    -> do
                                let path' = pathModifyTurn path (mappend t)
                                return path' { pathWaiting = False }
-}

{-
inputPathContinue :: GridWorld -> MEnv' GridWorld
inputPathContinue grid = do
    return grid
    -- set ControlPosRef
    grid' <- keysTouchHandlePointTouched grid $ \pos -> 
             grid { gridControlPosRef = pos }

    sens <- fmap gamedataSens resourceGameData

    gridModifyPathM grid' $ \path -> do
        controlPathRoll >>= \maybeRoll -> case maybeRoll of

            -- roll
            Just roll -> do
                return $ pathModifyTurn path 
                       $ \turn -> roll `mappend` turn

            -- turn
            Nothing   -> do
                -- drag control
                tstate' <- keysTouchHandlePointDrag (pathTurnState path) $ \_ _ (x', y') -> 
                           let (x, y) = gridControlPosRef grid'
                               dx = x' - x
                               dy = y' - y
                               tstate | abs dx < abs dy = tstateY
                                      | abs dy < abs dx = tstateX
                                      | otherwise       = pathTurnState path
                               tstateX | dx <= (-sens)  = Just leftTurn
                                       | sens <= dx     = Just rightTurn
                                       | otherwise      = pathTurnState path
                               tstateY | dy <= (-sens)  = Just downTurn
                                       | sens <= dy     = Just upTurn
                                       | otherwise      = pathTurnState path
                           in  tstate

                -- drop control
                tstate'' <- keysTouchHandlePointDrop tstate' $ \_ _ (x', y') -> 
                           let (x, y) = gridControlPosRef grid'
                               dx = x' - x
                               dy = y' - y
                               tstate | abs dx < abs dy = tstateY
                                      | abs dy < abs dx = tstateX
                                      | otherwise       = Just straightTurn
                               tstateX | dx <= (-sens)  = Just leftTurn
                                       | sens <= dx     = Just rightTurn
                                       | otherwise      = Just straightTurn
                               tstateY | dy <= (-sens)  = Just downTurn
                                       | sens <= dy     = Just upTurn
                                       | otherwise      = Just straightTurn
                           in  tstate
                
                return path { pathTurnState = tstate'' }
-}


controlPathRoll :: MEnv' (Maybe Turn)
controlPathRoll = do
    (wth, hth) <- screenShape
    keysTouchHandleReleased Nothing $ \(x, y) -> 
        let maybe | x * wth < valueGridRollSize       = Just anticlockTurn
                  | (1 - x) * wth < valueGridRollSize = Just clockTurn
                  | otherwise                         = Nothing
        in  maybe

{-
-- fixme: remove
-- | non-empty iff acceptable turn
controlPathTurnDrag :: MEnv' (Maybe Turn)
controlPathTurnDrag = do
    sens <- fmap gamedataSens resourceGet
    keysTouchHandlePointDrag Nothing $ \_ (x, y) (x', y') -> 
        let dx = x' - x
            dy = y' - y
            maybe | abs dy < abs dx = maybeX
                  | abs dx < abs dy = maybeY
                  | otherwise       = Nothing
            maybeX | sens <= dx     = Just rightTurn
                   | dx <= (-sens)  = Just leftTurn
                   | otherwise      = Nothing
            maybeY | sens <= dy     = Just upTurn
                   | dy <= (-sens)  = Just downTurn
                   | otherwise      = Nothing
        in  maybe
    
-- fixme: move into controlPathTurnWait
-- | non-empty iff acceptable turn
controlPathTurnDrop :: MEnv' (Maybe Turn)
controlPathTurnDrop = do
    sens <- fmap gamedataSens resourceGet
    keysTouchHandlePointDrop Nothing $ \_ (x, y) (x', y') -> 
        let dx = x' - x
            dy = y' - y
            maybe | abs dy < abs dx = maybeX
                  | abs dx < abs dy = maybeY
                  | otherwise       = Just straightTurn 
            maybeX | sens <= dx     = Just rightTurn
                   | dx <= (-sens)  = Just leftTurn
                   | otherwise      = Just straightTurn
            maybeY | sens <= dy     = Just upTurn
                   | dy <= (-sens)  = Just downTurn
                   | otherwise      = Just straightTurn
        in  maybe
-}
