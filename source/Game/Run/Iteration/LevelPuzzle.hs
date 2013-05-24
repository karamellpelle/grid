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
module Game.Run.Iteration.LevelPuzzle
  (
    iterationLevelMode,
    iterationPuzzleMode,

  ) where


import MyPrelude
import Game

import Game.Run.RunWorld
import Game.Run.Do
import Game.Run.Output
import Game.Run.Scene
import Game.Run.Helpers
import Game.Run.Helpers.World
import Game.Run.Helpers.Make
import Game.Run.Iteration.Iteration

import Game.Grid.Helpers
import Game.LevelPuzzle.LevelPuzzleWorld
import Game.LevelPuzzle.Helpers

import OpenGL
import OpenGL.Helpers



--------------------------------------------------------------------------------
--  

-- | begin LevelMode
iterationLevelMode :: Iteration' RunWorld
iterationLevelMode = 
    makeIteration' $ \run -> do
        outputLevelMode run
       
        -- iterate with IsPuzzle set to False
        iteration' (iterationLevelPuzzleMode' ()) $ 
                   runModifyLevelPuzzleWorld run $ \lvl -> 
                      let lvl' = levelpuzzleModifyGrid lvl $ gridPathStart
                          lvl'' = levelpuzzleModifyPath lvl' $ \path -> 
                                  path { pathSpeed = valueLevelPuzzlePathSpeedLevel }

                      in  lvl'' { levelpuzzleIsPuzzle = False }



-- | begin PuzzleMode
iterationPuzzleMode :: Iteration' RunWorld
iterationPuzzleMode = 
    makeIteration' $ \run -> do
        outputPuzzleMode run

        -- iterate with IsPuzzle set to True
        iteration' (iterationLevelPuzzleMode' ()) $ 
                   runModifyLevelPuzzleWorld run  $ \lvl -> 
                      let lvl' = levelpuzzleModifyPath lvl $ \path ->
                                 path { pathSpeed = valueLevelPuzzlePathSpeedPuzzle }
                      in  lvl' { levelpuzzleIsPuzzle = True }


    
iterationLevelPuzzleMode' :: s -> Iteration' RunWorld
iterationLevelPuzzleMode' s =
    modeIteration s noOutput $ defaultStep doLevelPuzzle $ \s run b -> do

        -- update Peak
        let run' = run { runLevelPuzzlePeak = 
                         max (runLevelPuzzlePeak run)
                             (levelpuzzleLevelIx $ runLevelPuzzleWorld run) }

        -- look at LevelPuzzleWorld 
        case runLevelPuzzleStack run' of

            -- LevelPuzzleWorld is finished
            []  -> do
                
                -- grab screenshot
                run'' <- screenshotLevelPuzzle run'

                -- save RunWorld
                saveRunWorld run''

                return (run'', b, [])


            _   -> do
                
                -- handle escape
                sceneHandleEscape (runScene run') (onNoEscape s run' b) (onEscape s run' b)


    where
      onNoEscape = \s run b ->

          -- handle switch AMode -> BMode
          handleAllEventsM run (run, b, [iterationLevelPuzzleMode' s])
                   $ \rbt@(run, b, top) e -> case e of

                      EventLevelMode  -> do
                          if (not $ levelpuzzleIsPuzzle (runLevelPuzzleWorld run))
                            then return rbt
                            else do
                                run' <- screenshotLevelPuzzle run
                                return (run', b, [iterationPuzzleModeToLevelMode])
                                
                      EventPuzzleMode -> do
                          if levelpuzzleIsPuzzle (runLevelPuzzleWorld run)
                            then return rbt
                            else do
                                run' <- screenshotLevelPuzzle run
                                return (run', b, [iterationLevelModeToPuzzleMode])

                      _               -> 
                          return rbt

      onEscape = \s run b -> do
          -- grab screenshot
          run' <- screenshotLevelPuzzle run

          -- save RunWorld
          saveRunWorld run'
          
          run'' <- commentLevelPuzzleEscape run'

          return (run'', b, [])



--------------------------------------------------------------------------------
--  

iterationLevelModeToPuzzleMode :: Iteration' RunWorld
iterationLevelModeToPuzzleMode = 
    makeIteration' $ \run -> do
        let view = cameraCurrentView $ runCamera run
            run' = runModifyGrid run $ \grid -> 
                   gridPushCameraCmds grid [ camcmdViewAdd (rTF t) s v,
                                             camcmdViewAdd (rTF t) s (View pi 0.0 0.0),
                                             camcmdViewAdd (rTF t) s (viewInv v) ]
        iteration' (iterationLevelModeToPuzzleMode' ()) run'
        
    where
      v = valueRunAModeToBModeView
      t = valueRunAModeToBModeTicks * 0.3333333
      s = 1.0 / t


iterationLevelModeToPuzzleMode' :: s -> Iteration' RunWorld
iterationLevelModeToPuzzleMode' s =
    mainIteration s outputAToB $ defaultStep doEmpty $ \s run b -> 
        if gridCameraCmdsIsComplete $ runGrid run
          then let run' = runSetTurnView run turnFacePuzzleMode $ viewFromRunFace run
               in  return (run', b, [iterationPuzzleMode])

          else return (run, b, [iterationLevelModeToPuzzleMode' s])

    where
      

--------------------------------------------------------------------------------
--  iterationLevelModeToPuzzleMode

iterationPuzzleModeToLevelMode :: Iteration' RunWorld
iterationPuzzleModeToLevelMode = 
    makeIteration' $ \run -> do
        let view = cameraCurrentView $ runCamera run
            run' = runModifyGrid run $ \grid -> 
                   gridPushCameraCmds grid [ camcmdViewAdd (rTF t) s v,
                                             camcmdViewAdd (rTF t) s (View pi 0.0 0.0),
                                             camcmdViewAdd (rTF t) s (viewInv v) ]
        iteration' (iterationPuzzleModeToLevelMode' ()) run'
        
    where
      v = valueRunAModeToBModeView
      t = valueRunAModeToBModeTicks * 0.3333333
      s = 1.0 / t


iterationPuzzleModeToLevelMode' :: s -> Iteration' RunWorld
iterationPuzzleModeToLevelMode' s =
    mainIteration s outputAToB $ defaultStep doEmpty $ \s run b -> 
        if gridCameraCmdsIsComplete $ runGrid run
          then let run' = runSetTurnView run turnFaceLevelMode $ viewFromRunFace run
               in  return (run', b, [iterationLevelMode])

          else return (run, b, [iterationPuzzleModeToLevelMode' s])




valueRunAModeToBModeView :: View
valueRunAModeToBModeView = 
    View 0.0 0.3 32.0

valueRunAModeToBModeTicks :: Float
valueRunAModeToBModeTicks =
    4.0

