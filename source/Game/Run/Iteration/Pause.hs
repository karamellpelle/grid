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
module Game.Run.Iteration.Pause
  (
    iterationPause,

  ) where


import MyPrelude
import Game
import File

import Game.Grid.Helpers
import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Scene
import Game.Run.Helpers

import OpenGL
import OpenGL.Helpers




--------------------------------------------------------------------------------
--  iterationPause

iterationPause :: Iteration' RunWorld
iterationPause = 
    makeIteration' $ \run -> do

        io $ putStrLn "Run.iterationPause"

        -- note: we assume that current mode is current Face. but ideally, 
        --       we should not assume that, and instead know which mode, when we 
        --       take a screenshot!
        run' <- case faceFromTurn $ runTurn run of
            FaceLevelMode   -> screenshotLevelPuzzle run
            FacePuzzleMode  -> screenshotLevelPuzzle run
            FaceMemoryMode  -> screenshotMemory run
            _               -> return run

        iteration' iterationPauseBegin run'



iterationPauseBegin :: Iteration' RunWorld
iterationPauseBegin = 
    makeIteration' $ \run -> do
        let tick = worldTick run
            view = cameraCurrentView $ runCamera run
            run' = runSetCameraCmds run [ camcmdView t0 s0 view0 ]
                    
        iteration' (iterationPauseBegin' view tick) run'

    where
      t0 = rTF valueRunPauseTicks
      s0 = 1.0 / valueRunPauseTicks
      view0 = View 0.0 0.3 100.0


iterationPauseBegin' :: View -> Tick -> Iteration' RunWorld
iterationPauseBegin' view tick = 
    pauseBeginIteration tick outputMain' $ defaultStep doEmpty $ \s run b -> do

        if (tick + rTF valueRunPauseTicks <= worldTick run)
          then return (run, b, [iterationPauseMid view tick])
          else return (run, b, [iterationPauseBegin' view tick])


iterationPauseMid :: View -> Tick -> Iteration' RunWorld
iterationPauseMid view tick = 
    makeIteration' $ \run -> do

        -- throttle down screen rate
        screenSetRate 30

        iteration' (iterationPauseMid' view tick) run


iterationPauseMid' :: View -> Tick -> Iteration' RunWorld
iterationPauseMid' view tick = 
    pauseMidIteration tick outputMain' $ defaultStep doEmpty $ \s run b -> do

        -- throttle down screen rate, if begin Front
        join $ systemHandleFrontBegin (return ()) (screenSetRate 0)

        -- ButtonA => end pause
        keysTouchHandleButtonA (run, b, [iterationPauseMid' view tick]) $ \pos -> 
                               (run, b, [iterationPauseEnd view tick])


iterationPauseEnd :: View -> Tick -> Iteration' RunWorld
iterationPauseEnd view tick = 
    makeIteration' $ \run -> do
        let tick' = worldTick run + 
                    max 0.0 (tick + rTF valueRunPauseTicks - worldTick run)
            run' = runModifyCamera run $ \cam -> 
                   cameraToViewTicks cam valueRunPauseTicks view
        
        -- set screen rate back to original value
        screenSetRate 0

        iteration' (iterationPauseEnd' tick') run'


iterationPauseEnd' :: Tick -> Iteration' RunWorld
iterationPauseEnd' tick = 
    pauseEndIteration tick outputMain' $ defaultStep doEmpty $ \s run b -> do

        if tick + rTF valueRunPauseTicks <= worldTick run

          then return (run, b, [])

          else return (run, b, [iterationPauseEnd' tick])
        






--------------------------------------------------------------------------------
--  pauseIteration

pauseBeginIteration :: 
      Tick -> 
      (Tick -> RunWorld -> () -> MEnv' (Tick, RunWorld, ())) -> 
      (Tick -> RunWorld -> () -> MEnv' (RunWorld, (), IterationStack RunWorld ())) -> 
      Iteration' RunWorld
pauseBeginIteration tick output step =
    makeIteration' $ \run -> do

        -- handle screen change
        run' <- runBeginScreen run

        -- begin Scene
        scene' <- sceneBegin $ runScene run'

        -- actual iteration
        (run'', top) <- iteration' (defaultIteration tick output step) $
                        run' { runScene = scene' }

        -- output Scene
        let tweak = sceneTweak $ runScene run''
            fade = rTF (worldTick run'' - tick) * valueRunPauseTicksInv
        scene'' <- scenePresentPause (runScene run'') tweak fade

        return (run'' { runScene = scene'' }, top)



pauseMidIteration :: 
      Tick -> 
      (Tick -> RunWorld -> () -> MEnv' (Tick, RunWorld, ())) -> 
      (Tick -> RunWorld -> () -> MEnv' (RunWorld, (), IterationStack RunWorld ())) -> 
      Iteration' RunWorld
pauseMidIteration tick output step =
    makeIteration' $ \run -> do

        -- handle screen change
        run' <- runBeginScreen run

        -- begin Scene
        scene' <- sceneBegin $ runScene run'

        -- actual iteration
        (run'', top) <- iteration' (defaultIteration tick output step) $
                        run' { runScene = scene' }

        -- output Scene
        let tweak = sceneTweak $ runScene run''
        scene'' <- scenePresentPause (runScene run'') tweak 1.0

        return (run'' { runScene = scene'' }, top)



pauseEndIteration :: 
      Tick -> 
      (Tick -> RunWorld -> () -> MEnv' (Tick, RunWorld, ())) -> 
      (Tick -> RunWorld -> () -> MEnv' (RunWorld, (), IterationStack RunWorld ())) -> 
      Iteration' RunWorld
pauseEndIteration tick output step =
    makeIteration' $ \run -> do

        -- handle screen change
        run' <- runBeginScreen run

        -- begin Scene
        scene' <- sceneBegin $ runScene run'

        -- actual iteration
        (run'', top) <- iteration' (defaultIteration tick output step) $
                        run' { runScene = scene' }

        -- output Scene
        let tweak = sceneTweak $ runScene run''
            fade = 1.0 - rTF (worldTick run'' - tick) * valueRunPauseTicksInv
        scene'' <- scenePresentPause (runScene run'') tweak fade

        return (run'' { runScene = scene'' }, top)


