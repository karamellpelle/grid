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
module Test.Tweak
  (
    iterationTestTweak,

  ) where


import MyPrelude
import Game

import Game.Run
import Game.Run.RunWorld
import Game.Run.Output
import Game.Run.Do
import Game.Run.Do.Grid
import Game.Grid.Do
import Game.Grid.Modify
import Game.Grid.StepDT

import Game.Run.Iteration.Iteration

import Test.Tweak.Output
import Test.Tweak.State
import Text.Printf

iterationTestTweak :: Iteration' RunWorld
iterationTestTweak = 
    makeIteration' $ \run -> do

        -- set clocks to 0.0
        tickClockRunSet 0.0

        -- set intensity
        gameSetIntensity $ runIntensity run

        s <- makeTestTweakState run
        iteration' (iterationTestTweak' s) $ runModifyCamera run $ \cam -> 
            cameraSetView cam (View 0.0 0.0 1.0)


iterationTestTweak' :: TestTweakState -> Iteration' RunWorld
iterationTestTweak' s =
    mainIteration s outputTestTweak' $ defaultStep doTestTweak $ \s run b -> do
        let w = ttstateWidget s
            a = ttstateA s
        (w', a') <- iterateGUI 1.0 w a
        let run' = runSetTweakValues run a'
        if aTouch a' 
          then do
            let s' = s { ttstateWidget = w', ttstateA = a' { aTouch = False } }
            return (run', b, [iterationTestTweak' s'])
          else do
            ix' <- keysTouchHandlePointDrop (ttstateColorIx s) $ \t (x, y) (x', y') -> 
                let diffx = x' - x
                    ix' | diffx < 0.0 = (ttstateColorIx s + valueColorMapSize - 1) `mod`
                                        valueColorMapSize
                        | 0.0 < diffx = (ttstateColorIx s + 1) `mod` 
                                        valueColorMapSize
                        | otherwise   = ttstateColorIx s
                in  ix'
            tick <- tickGet
            run'' <- keysTouchHandleButtonB run' $ \_ -> 
                      resetTweak run' tick

            run''' <- keysTouchHandleButtonA run'' $ \_ -> runTweakPulse0Push run'' 1.0
            let s' = s { ttstateColorIx = ix', 
                          ttstateWidget = w', 
                          ttstateA = a' { aTouch = False } }
            return (run''', b, [iterationTestTweak' s'])
            


--------------------------------------------------------------------------------
--  


modifyBegin :: RunWorld -> MEnv' RunWorld
modifyBegin run = do
    return $ runClearEvents run


doTestTweak :: TestTweakState -> RunWorld -> b -> 
               MEnv' (TestTweakState, RunWorld, b) 
doTestTweak = \s run b -> do
    -- begin modify
    run' <- modifyBegin run 

    -- do GridWorld
    (s', grid', run'') <- doGridTestTweak s (runGrid run') run'

    join $ keysTouchHandlePointDrop (return ()) $ \t pos pos' -> io $ do
        let (x, y) = pos
            (x', y') = pos'
        putStrLn $ "PointDrop. pos: " ++ showPos pos ++ "pos': " ++ showPos pos'
        putStrLn $ "max abs: " ++ showFloat (max (abs $ x' - x) (abs $ y' - y))

    return (s', run'' { runGrid = grid' }, b)

    where
      showPos (x, y) = 
         "(" ++ printf ("%.1f") x ++ ", " ++
                printf ("%.1f") y ++ ")"
      showFloat = 
          printf ("%.3f")

doGridTestTweak :: TestTweakState -> GridWorld -> RunWorld ->
                   MEnv' (TestTweakState, GridWorld, RunWorld)
doGridTestTweak =
    defaultDo (controlTestTweak)
              (tickClockRunGet, tickClockRunSet)
              (cameraStepDT)
              (noBreakModify)
              (noDefaultModify)



controlTestTweak :: TestTweakState -> GridWorld -> RunWorld -> 
                    MEnv' (TestTweakState, GridWorld, RunWorld)
controlTestTweak s grid run = do
    (controlGrid controlCamera) s grid run


runSetTweakValues :: RunWorld -> TweakA -> RunWorld
runSetTweakValues run a = 
    let scene = runScene run
        tweak = sceneTweak scene
        tweak' = tweak 
                 {  
                    tweakConstI = aConstI a,
                    tweakConstJ = aConstJ a,
                    tweakConstK = aConstK a,
                    tweakConstL = aConstL a
                 }
        scene' = scene { sceneTweak = tweak' }
        run' = run { runScene = scene' }

    in  run'

resetTweak :: RunWorld -> Tick -> RunWorld
resetTweak run tick = 
    let scene = runScene run
        scene' = scene { sceneTweak = makeTweak' }
    in  run { runScene = scene' }
    where
      makeTweak' = makeTweak { tweakTick = tick }
