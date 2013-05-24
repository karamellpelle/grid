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
module Test.GL
  (
    iterationTestGL,

  ) where


import MyPrelude
import Game

import Game.Run

import Game.Grid.Do
import Game.Grid.Modify
import Game.Grid.StepDT

import Game.Run.Iteration.Iteration

import Test.GL.Output
import Test.GL.State



iterationTestGL :: Iteration' RunWorld
iterationTestGL = 
    makeIteration' $ \run -> do
        
        -- set clocks to 0.0
        tickClockRunSet 0.0

        -- set intensity
        gameSetIntensity $ runIntensity run

        s <- makeTestGLState 
        iteration' (iterationTestGL' s) $ runModifyCamera run $ \cam -> 
            cameraSetView cam (View 0.0 0.0 1.0)


iterationTestGL' :: TestGLState -> Iteration' RunWorld
iterationTestGL' s =
#ifdef GRID_TEST_NO_SCENE
    defaultIteration s outputTestGL' $ defaultStep doTestGL $ \s run b -> do
#else
    mainIteration s outputTestGL' $ defaultStep doTestGL $ \s run b -> do
#endif
        let w = tglstateWidget s
        (w', _) <- iterateGUI 1.0 w ()

        -- take Screenshot if hold
        run' <- join $ keysTouchHandleButtonB (return run) (\_ -> screenshotLevelPuzzle run)

        return (run', b, [iterationTestGL' s { tglstateWidget = w' }])
        --return (run', b, [iterationTestGL' s])

--------------------------------------------------------------------------------
--  


modifyBegin :: RunWorld -> MEnv' RunWorld
modifyBegin run = do
    return $ runClearEvents run


doTestGL :: TestGLState -> RunWorld -> b -> MEnv' (TestGLState, RunWorld, b) 
doTestGL = \s run b -> do
    -- begin modify
    run' <- modifyBegin run 

    -- fixme: push path...
    --path' <- io $ pathPush (tglstatePath s) (Segment (Node (1) (0) (-1)) mempty)
    path' <- return $ tglstatePath s

    -- do GridWorld
    (s', grid', run'') <- doGridTestGL (s { tglstatePath = path' }) (runGrid run') run'

    return (s', run'' { runGrid = grid' }, b)



doGridTestGL :: TestGLState -> GridWorld -> RunWorld ->
                MEnv' (TestGLState, GridWorld, RunWorld)
doGridTestGL =
    defaultDo (controlTestGL)
              (tickClockRunGet, tickClockRunSet)
              (cameraStepDT)
              (noBreakModify)
              (noDefaultModify)



controlTestGL :: TestGLState -> GridWorld -> RunWorld -> 
                    MEnv' (TestGLState, GridWorld, RunWorld)
controlTestGL s grid run = do
    (controlGrid controlCamera) s grid run


