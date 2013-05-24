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
module Test.Font
  (
    iterationTestFont,

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

import Game.Run.Iteration.Iterations

import Test.Font.Output
import Test.Font.Widget


iterationTestFont :: Iteration' RunWorld
iterationTestFont = 
    makeIteration' $ \run -> do

        -- set clocks to 0.0
        tickClockRunSet 0.0

        -- set intensity
        gameSetIntensity $ runIntensity run

        gd <- resourceGUIData
        let w = makeFontWidget gd 
            a = makeFontA 
            s = makeFontS w a
        iteration' (iterationTestFont' s) $ runModifyCamera run $ \cam -> 
            cameraSetView cam (View 0.0 0.0 1.0)



iterationTestFont' :: FontS -> Iteration' RunWorld
iterationTestFont' s =
    mainIteration s outputTestFont' $ defaultStep doTestFont $ \s run b -> do
        let w = fontsWidget s
            a = fontsA s
        (w', a') <- iterateGUI 1.0 w a
        
        let run' = runSetFontValues run a'

        return (run', b, [iterationTestFont' (w', a')])



--------------------------------------------------------------------------------
--  


modifyBegin :: RunWorld -> MEnv' RunWorld
modifyBegin run = do
    return $ runClearEvents run


doTestFont :: s -> RunWorld -> b -> MEnv' (s, RunWorld, b) 
doTestFont = \s run b -> do
    -- begin modify
    run' <- modifyBegin run 

    -- do GridWorld
    (s', grid', run'') <- doGridTestFont s (runGrid run') run'

    return (s', run'' { runGrid = grid' }, b)

doGridTestFont :: s -> GridWorld -> RunWorld -> MEnv' (s, GridWorld, RunWorld)
doGridTestFont =
    defaultDo (controlGrid controlCamera)
              (tickClockRunGet, tickClockRunSet)
              (cameraStepDT)
              (noBreakModify)
              (postStepDTModify)


runSetFontValues :: RunWorld -> FontA -> RunWorld
runSetFontValues run a = 
    let scene = runScene run
        tweak = sceneFont scene
        tweak' = tweak 
                 {  
                    tweakForA = aForA a, 
                    tweakForB = aForB a, 
                    tweakForC = aForC a,
                    tweakForD = aRotD a, 
                    tweakRotA = aRotA a, 
                    tweakRotB = aRotB a, 
                    tweakRotC = aRotC a,
                    tweakRotD = aRotD a
                 }
        scene' = scene { sceneFont = tweak' }
        run' = run { runScene = scene' }

    in  run'
