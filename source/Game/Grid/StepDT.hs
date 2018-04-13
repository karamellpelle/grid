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
module Game.Grid.StepDT
  (
    defaultStepDT,
    cameraStepDT,

    Collision (..),
    emptyCollision,

  ) where


import MyPrelude
import Game
import Game.Grid




-- | step dt of worlds 'a' and 'b', using collision handler
defaultStepDT :: Collision s b ->
                 Tick -> s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
defaultStepDT col = \dt s grid b ->

    -- (this ideally a pure function, but we need IO for GL, typically)
    io $ do
      let dt' = rTF dt
          path' = stepDTPath dt' $ gridPath grid
          camera' = stepDTCamera dt' $ gridCamera grid
          grid' = grid 
                  {
                      gridTick = gridTick grid + dt,
                      gridPath = path',
                      gridCamera = camera' 
                  }

      -- handle collisions
      (s', grid'', b') <- handleCollisionPath col s grid' b
      
      -- handleCollisionCamera (onCameraNodeIdeal, etc.) 

      return (s', grid'', b') 



-- | only step dt of camera
cameraStepDT :: Tick -> s -> GridWorld -> b -> MEnv' (s, GridWorld, b)
cameraStepDT = \dt s grid b -> do

    -- we want pureness
    let camera' = stepDTCamera (rTF dt) $ gridCamera grid
        grid' = grid 
                {
                    gridTick = gridTick grid + dt,
                    gridCamera = camera'
                }

    return (s, grid', b)




--------------------------------------------------------------------------------
-- step dt



-- note: in pathPushCurrent instead?
-- we set alpha to maximum 1.0, to prevent camera jittering when StepDT steps
-- alpha above 1.0. 
stepDTPath :: Float -> Path -> Path
stepDTPath dt path =
    case pathWaiting path of
        --False   -> path { pathAlpha = pathAlpha path + dt * pathSpeed path }
        False   -> path { pathAlpha = min 1.0 $ pathAlpha path + dt * pathSpeed path }
        True    -> path


stepDTCamera :: Float -> Camera -> Camera
stepDTCamera dt camera = 
    let nodeAlpha = keepInside 0 1 $ cameraNodeAlpha camera + 
                                     dt * cameraNodeSpeed camera
        turnAAlpha = keepInside 0 1 $ cameraTurnAAlpha camera + 
                                      dt * cameraTurnASpeed camera
        turnBAlpha = keepInside 0 1 $ cameraTurnBAlpha camera + 
                                      dt * cameraTurnBSpeed camera
        viewAVel = cameraViewASpeed camera
        viewBVel = cameraViewBSpeed camera
        viewCVel = cameraViewCSpeed camera
    -- fixme: do not set X to XIdeal when 1.0 <= alpha?  (but then cameraXXX 
    --        needs to be fixed too)
    in  camera 
        {
            cameraNodeAlpha = nodeAlpha,
            cameraNode = if 1.0 <= nodeAlpha then cameraNodeIdeal camera 
                                             else cameraNode camera,
                 
            cameraTurnAAlpha = turnAAlpha,
            cameraTurnA = if 1.0 <= turnAAlpha then cameraTurnAIdeal camera
                                               else cameraTurnA camera,
                 
            cameraTurnBAlpha = turnBAlpha,
            cameraTurnB = if 1.0 <= turnBAlpha then cameraTurnBIdeal camera
                                               else cameraTurnB camera,
                                                    
            cameraViewAAlpha = follow viewAVel dt (cameraViewAAlpha camera)
                                                   (cameraViewAAlphaIdeal camera),
            cameraViewBAlpha = follow viewBVel dt (cameraViewBAlpha camera)
                                                   (cameraViewBAlphaIdeal camera),
            cameraViewCAlpha = follow viewCVel dt (cameraViewCAlpha camera)
                                                   (cameraViewCAlphaIdeal camera)
                                                   
        }

    where
      follow vel dt x xIdeal =
          if x <= xIdeal
            then min xIdeal (x + vel * dt)
            else max xIdeal (x - vel * dt)


--------------------------------------------------------------------------------
--  collision



-- | check for collisions of path with node
handleCollisionPath :: Collision s b -> s -> GridWorld -> b -> IO (s, GridWorld, b)
handleCollisionPath col = \s grid b -> 
    case gridPath grid of
        path  -> if 1.0 <= pathAlpha path
                 then do
                    (path', s', grid', b') <- onPathNode col path s grid b
                    return (s', grid' { gridPath = path' }, b')
                 else 
                    return (s, grid, b)
       

-- | controlling the flow of defaultStepDT
data Collision s b =
    Collision
    {
        onPathNode :: Path -> s -> GridWorld -> b -> IO (Path, s, GridWorld, b)

    }


emptyCollision :: Collision s b
emptyCollision =
    Collision { onPathNode = \path s grid b -> return (path, s, grid, b) }







